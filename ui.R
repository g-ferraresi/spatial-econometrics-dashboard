#  ui.R

ui <- dashboardPage(
  
  dashboardHeader(title = "Italian Provinces", titleWidth = 260),
  
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("Descriptive overview", tabName = "descrittive", icon = icon("map")),
      menuItem("Spatial analysis",     tabName = "esda",        icon = icon("project-diagram")),
      menuItem("Models",               tabName = "modelli",     icon = icon("calculator")),
      menuItem("Effects and spillovers", tabName = "impatti",   icon = icon("share-nodes")),
      menuItem("Methodological notes", tabName = "note",        icon = icon("circle-info"))
    ),
    hr(),
    # Global controls: apply to all tabs
    selectInput("anno", "Year", choices = ANNI, selected = max(ANNI)),
    selectInput("matrice", "Weights matrix", choices = MATRICI, selected = "W_knn5")
  ),
  
  dashboardBody(
    tabItems(
      
      # ---------------------------------------------------------------- #
      tabItem(
        tabName = "descrittive",
        
        fluidRow(
          box(
            title = "Selection", status = "primary", solidHeader = TRUE, width = 6,
            column(
              6, style = "padding-left:0;",
              selectInput("var_desc", "Variable", choices = VARIABILI,
                          selected = DIPENDENTE_DEFAULT)
            ),
            column(
              6, style = "padding-right:0;",
              selectizeInput("prov", "Province", choices = PROVINCE,
                             selected = PROVINCIA_DEFAULT, multiple = FALSE)
            )
          ),
          box(
            title = "How to read", background = "light-blue", width = 6,
            collapsible = TRUE,
            "Panel of 107 provinces for the years 2018-2022",
            "Source: Istat, Territorial indicators for development policies. ",
            "The map shows the year selected in the sidebar.",
            br(), br(),
            "The selected province remains highlighted in red in all charts. "
          )
        ),
        
        fluidRow(
          valueBoxOutput("box_prov", width = 3),
          valueBoxOutput("box_media", width = 3),
          valueBoxOutput("box_max", width = 3),
          valueBoxOutput("box_min", width = 3)
        ),
        
        fluidRow(
          box(title = "Territorial distribution", width = 7, collapsible = TRUE,
              withSpinner(plotlyOutput("mappa_desc", height = "520px"))),
          tabBox(
            title = "Comparisons", width = 5,
            tabPanel("By macro-area", plotlyOutput("box_rip", height = "460px")),
            tabPanel("Time series", plotlyOutput("serie", height = "460px")),
            tabPanel(
              "Correlation",
              plotlyOutput("scatter", height = "390px"),
              selectInput("var_desc2", "Second variable", choices = VARIABILI,
                          selected = "servizi_infanzia_presa_carico")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Comparison across provinces", width = 12, collapsible = TRUE,
            radioButtons("ambito", NULL, choices = AMBITI_CONFRONTO,
                         selected = "tutte", inline = TRUE),
            withSpinner(plotlyOutput("confronto", height = "430px")),
            div(style = "font-size:12px; color:#777; padding-top:6px;",
                "Provinces ordered from highest to lowest value. ",
                "The selected province is shown in red, while the dashed line represents ",
                "the national average for the year.")
          )
        )
      ),
      
      # ---------------------------------------------------------------- #
      tabItem(
        tabName = "esda",
        
        fluidRow(
          box(title = "Variable", status = "primary", solidHeader = TRUE, width = 4,
              selectInput("var_esda", NULL, choices = VARIABILI,
                          selected = DIPENDENTE_DEFAULT),
              sliderInput("signif_lisa", "LISA significance threshold",
                          min = 0.01, max = 0.10, value = 0.05, step = 0.01),
              radioButtons("base_lisa", "LISA and scatterplot computed on",
                           choices = c("Raw variable" = "raw",
                                       "Residuals (net of macro-areas)" = "resid"),
                           selected = "raw")),
          box(title = "Spatial autocorrelation", background = "light-blue",
              width = 8, collapsible = TRUE,
              "Moran's I measures how similar nearby provinces are. ",
              "Values close to 1 indicate strong similarity, while values close to 0 indicate no spatial structure.",
              br(), br(),
              tags$b("Note: "),
              "a very high value may reflect a large-scale gradient ",
              "(the North-South divide) rather than interaction between neighbouring provinces. ",
              "The box on the right separates the two: it removes the ",
              "differences between macro-areas and recalculates the index on what remains. ",
              "The radio button on the left applies the same correction to the scatterplot and the cluster map.")
        ),
        
        fluidRow(
          valueBoxOutput("box_moran", width = 4),
          valueBoxOutput("box_moran_res", width = 4),
          valueBoxOutput("box_lisa", width = 4)
        ),
        
        fluidRow(
          box(title = "Moran scatterplot", width = 6, collapsible = TRUE,
              withSpinner(plotOutput("moran_plot", height = "440px"))),
          box(title = "Local clusters (LISA)", width = 6, collapsible = TRUE,
              withSpinner(plotlyOutput("mappa_lisa", height = "440px")))
        ),
        
        fluidRow(
          box(title = "Moran's I by year", width = 12, collapsible = TRUE,
              plotlyOutput("moran_anni", height = "280px"))
        )
      ),
      
      # ---------------------------------------------------------------- #
      tabItem(
        tabName = "modelli",
        
        fluidRow(
          box(
            title = "Specification", status = "primary", solidHeader = TRUE,
            width = 6, collapsible = TRUE,
            selectInput("dip", "Dependent variable", choices = VARIABILI,
                        selected = DIPENDENTE_DEFAULT),
            # Initial choices set here (not only in the server) so that the
            # models estimated at startup already have their regressors.
            selectInput("indep", "Independent variables",
                        choices = regressori_ammessi(DIPENDENTE_DEFAULT),
                        selected = REGRESSORI_DEFAULT, multiple = TRUE),
            checkboxInput("fe_rip", "Geographical macro-area fixed effects",
                          value = TRUE),
            checkboxInput("std_indep",
                          "Standardize independent variables (z-scores) before estimation",
                          value = FALSE),
            uiOutput("avviso_std"),
            uiOutput("avviso_spec")            ,
            hr(),
            checkboxGroupInput(
              "modelli_sel_stima", "Models to estimate",
              choices  = c("SAR", "SEM", "SDM", "SLX", "SDEM", "SAC", "GNS"),
              selected = c("SAR", "SEM", "SDM", "SLX", "SDEM", "SAC", "GNS"),
              inline   = TRUE),
            div(style = "font-size:12px; color:#555; margin-bottom:8px;",
                "OLS is always estimated: it is the baseline for the LM tests."),
            actionButton("stima", "Estimate models",
                         icon = icon("play"), class = "btn-primary"),
            uiOutput("avviso_stale")
          ),
          box(
            title = "Models being compared", background = "light-blue",
            width = 6, collapsible = TRUE,
            tags$ul(
              tags$li(tags$b("OLS"), ": no spatial structure."),
              tags$li(tags$b("SAR"), ": the dependent variable in one province responds to that of its neighbours."),
              tags$li(tags$b("SEM"), ": dependence lies in unobserved factors."),
              tags$li(tags$b("SDM"), ": spatial lag on the dependent variable and regressors."),
              tags$li(tags$b("SLX"), ": spatial lag only on the regressors."),
              tags$li(tags$b("SAC"), ": spatial lag and spatial error together."),
              tags$li(tags$b("SDEM"), ": spatial lag on the regressors and spatial error."),
              tags$li(tags$b("GNS"), ": the most general specification, all three components together.")
            ),
            "Model choice combines the LM tests (which form of dependence is present) with the AIC (which specification fits best)."
          )
        ),
        
        fluidRow(
          valueBoxOutput("box_modello", width = 4),
          valueBoxOutput("box_rho", width = 4),
          valueBoxOutput("box_moran_resid", width = 4)
        ),
        
        fluidRow(
          box(
            title = "LM specification tests", width = 12, collapsible = TRUE,
            fluidRow(
              column(6, verbatimTextOutput("lm_tests")),
              column(
                6,
                uiOutput("lm_raccomandazione"),
                br(),
                div(style = "font-size:12px; color:#555;",
                    "The tests are computed on the OLS residuals of the specification
                     selected above, macro-area fixed effects included. The standard
                     tests indicate whether dependence is present; when both are
                     significant, the robust versions discriminate which of the two
                     forms is genuine and which is an artefact of the other.")
              )
            )
          )
        ),
        
        fluidRow(
          box(title = "Model comparison (AIC)", width = 5, collapsible = TRUE,
              withSpinner(plotlyOutput("aic_plot", height = "340px"))),
          box(title = "Coefficients of the selected model", width = 7,
              collapsible = TRUE,
              selectInput("modello_sel", "Model",
                          choices = c("OLS", "SAR", "SEM", "SDM", "SLX",
                                      "SDEM", "SAC", "GNS"),
                          selected = "SAR"),
              plotlyOutput("coef_plot", height = "300px"))
        ),
        
        fluidRow(
          box(
            title = "Comparison metrics", width = 12, collapsible = TRUE,
            tableOutput("metriche_out"),
            div(style = "font-size:12px; color:#555;",
                "Moran resid. is the residual spatial autocorrelation left by each
                 model: a well-specified model should bring it close to zero. For OLS
                 the test accounts for the estimated coefficients (lm.morantest);
                 for the maximum-likelihood models no analytical equivalent exists,
                 so the p-value should be read as indicative.")
          )
        ),
        
        fluidRow(
          box(
            title = "Nested model hierarchy (LR tests)", width = 12,
            collapsible = TRUE,
            withSpinner(plotlyOutput("lr_plot", height = "420px")),
            div(style = "font-size:12px; color:#555;",
                "Each bar tests a restricted model against a more general one that
                 nests it. A green bar means the restriction is rejected, so the extra
                 component is doing work. Only nested pairs can be compared this way:
                 SAR and SEM, for instance, are not nested in each other, which is why
                 the AIC comparison above remains necessary.")
          )
        ),
        
        fluidRow(
          box(title = "Estimation table", width = 12, collapsible = TRUE,
              verbatimTextOutput("tab_modello"))
        )
      ),
      
      # ---------------------------------------------------------------- #
      tabItem(
        tabName = "impatti",
        
        fluidRow(
          box(title = "Direct, indirect, and total effects",
              background = "light-blue", width = 12, collapsible = TRUE,
              "In models with a spatial lag, coefficients are not marginal effects. ",
              "A change in one province propagates to neighbouring provinces and feeds back. ",
              tags$b("Direct"), ": effect on the province itself. ",
              tags$b("Indirect"), ": effect on other provinces (spillover). ",
              tags$b("Total"), ": the sum.",
              br(), br(),
              "The dependent variable in this section depends on the specification selected in ",
              tags$b("Models"), ".")
        ),
        
        fluidRow(
          box(title = "Decomposition of effects", width = 7, collapsible = TRUE,
              withSpinner(plotlyOutput("impatti_plot", height = "400px"))),
          box(title = "Estimated values", width = 5, collapsible = TRUE,
              verbatimTextOutput("impatti_tab"))
        )
      ),
      
      # ---------------------------------------------------------------- #
      tabItem(
        tabName = "note",
        
        fluidRow(
          box(
            title = "Dashboard user guide", width = 6, collapsible = TRUE,
            status = "primary", solidHeader = TRUE,
            
            tags$p(tags$b("Sidebar. "),
                   "Year and weights matrix apply to all tabs: ",
                   "changing them recalculates maps, indices, and models. ",
                   "The matrix determines which provinces are considered \"neighbours\" of each province."),
            
            tags$p(tags$b("Descriptive overview. "),
                   "Select a variable and a province. The four ",
                   "boxes at the top show the province's value and its ",
                   "rank, the national average, the maximum, and the ",
                   "minimum. Below: the map, three comparisons (by macro-area, ",
                   "over time, and against a second variable), and the complete ",
                   "ranking of provinces."),
            
            tags$p(tags$b("Spatial analysis. "),
                   "Moran's I indicates whether nearby provinces are similar. ",
                   "The LISA map shows where high values ",
                   "(high-high) and low values (low-low) cluster; the significance ",
                   "threshold determines how many provinces remain highlighted."),
            
            tags$p(tags$b("Models. "),
                   "Select the dependent variable, the regressors, and which models to ",
                   "estimate, then press Estimate. The LM tests indicate which form of ",
                   "spatial dependence is present, the AIC and BIC compare the fit of ",
                   "all specifications, and the LR tests compare the pairs that are ",
                   "nested in each other. The model shown in the coefficient plot and ",
                   "table is selected separately, so it is not necessarily the one ",
                   "preferred by AIC."),
            
            tags$p(tags$b("Effects and spillovers. "),
                   "It uses the model selected in ", tags$i("Models"),
                   " and decomposes its effects. This is meaningful only for SAR, SDM, SAC, SLX, SDEM, and GNS."),
          ),
          
          box(title = "Useful information", width = 6, collapsible = TRUE, status = "primary",
              solidHeader = TRUE,
              tags$p("Panel of 107 Italian provinces, 2018-2022, built by combining ",
                     "thematic tables from the Istat database ",
                     tags$i("Territorial indicators for development policies"),
                     ": Labour, Care services, Information society, Culture."),
              tags$p("Cartography: Istat, boundaries of administrative units for statistical ",
                     "purposes as of 1 January 2023, generalized version, WGS84 UTM 32N."),
              
              tags$p(tags$b("Ultra-broadband: "), "there is a body of literature on the effect of ",
                     "home internet connectivity on female labour supply that finds a positive effect ",
                     "of residential broadband on labour-force participation among married women, ",
                     "through the mechanisms of working from home and reduced time spent on ",
                     "household production."),
              tags$p(tags$b("Sardinia and Sicily: "), "with queen contiguity, the islands ",
                     "form separate components. With k-nearest neighbours, they are connected to the ",
                     "mainland based on centroid proximity, a geometric rather than ",
                     "economic link.")
          )
        )
      )
    )
  )
)