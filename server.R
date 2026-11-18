#  server.R

etichetta <- function(v) {
  e <- names(VARIABILI)[match(v, VARIABILI)]
  ifelse(is.na(e), v, e)
}

# From the Istat code to the province name, for titles and legends.
nome_provincia <- function(cod) {
  n <- names(PROVINCE)[match(cod, PROVINCE)]
  ifelse(is.na(n), cod, n)
}

ROSSO <- "#c62828"   # single colour for the selected province
GRIGIO <- "#90a4ae"

# Extracts the estimates/standard errors matrix.
# spatialreg Sarlm objects (SAR, SEM, SDM, SAC) expose it as $Coef;
# objects derived from lm (OLS, SLX) expose it as $coefficients, whereas for the former it is
# only the estimates vector. Both conventions need to be handled.
coef_matrice <- function(m) {
  s <- summary(m)
  cf <- if (!is.null(s$Coef)) s$Coef else s$coefficients
  if (is.null(dim(cf))) return(NULL)
  data.frame(termine = rownames(cf), stima = cf[, 1], se = cf[, 2],
             row.names = NULL)
}

# Interactive choropleth map. ggplotly works with geom_sf, but the tooltip
# must be constructed manually using aes(text = ...) and tooltip = "text".
# 'evidenzia' is the code of the province to outline in red.
mappa_coropletica <- function(d, variabile, titolo_legenda, evidenzia = NULL) {
  d$.val <- d[[variabile]]
  d$.txt <- paste0(d$nome_mappa, "<br>", round(d$.val, 1))
  
  p <- ggplot(d) +
    geom_sf(aes(fill = .val, text = .txt), colour = "grey30", linewidth = 0.1) +
    scale_fill_viridis_c(name = titolo_legenda, option = "viridis") +
    theme_void() +
    theme(legend.position = "right")
  
  if (!is.null(evidenzia)) {
    sel <- d[d$cod_provincia %in% evidenzia, ]
    if (nrow(sel) > 0) {
      p <- p + geom_sf(data = sel, aes(text = .txt), fill = NA,
                       colour = ROSSO, linewidth = 0.9)
    }
  }
  
  ggplotly(p, tooltip = "text")
}

# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # ---- basic reactive objects -------------------------------------------
  
  W_sel <- reactive({
    switch(input$matrice, "W_queen" = W_queen, W_knn5)
  })
  
  NB_sel <- reactive({
    switch(input$matrice, "W_queen" = nb_queen, nb_knn5)
  })
  
  # Annual slice, already aligned with the order of the weights matrix
  d_anno <- reactive({
    dati_anno(as.integer(input$anno))
  })
  
  
  # TAB 1 - DESCRIPTIVE OVERVIEW ####
  
  # Row index of the selected province within d_anno().
  riga_prov <- reactive({
    req(input$prov)
    match(input$prov, d_anno()$cod_provincia)
  })
  
  output$box_prov <- renderValueBox({
    d <- st_drop_geometry(d_anno())
    v <- d[[input$var_desc]]
    i <- riga_prov()
    
    if (is.na(i) || is.na(v[i])) {
      return(valueBox("-", paste(nome_provincia(input$prov), "- missing data"),
                      icon = icon("location-dot"), color = "light-blue"))
    }
    
    posizione <- rank(-v, na.last = "keep", ties.method = "min")[i]
    valueBox(
      round(v[i], 1),
      sprintf("%s: %d\u00b0 out of %d provinces",
              d$nome_mappa[i], posizione, sum(!is.na(v))),
      icon = icon("location-dot"), color = "light-blue"
    )
  })
  
  output$box_media <- renderValueBox({
    v <- d_anno()[[input$var_desc]]
    valueBox(round(mean(v, na.rm = TRUE), 1), "National average",
             icon = icon("chart-simple"), color = "aqua")
  })
  
  output$box_max <- renderValueBox({
    d <- d_anno(); v <- d[[input$var_desc]]
    i <- which.max(v)
    valueBox(round(v[i], 1), paste("Maximum:", d$nome_mappa[i]),
             icon = icon("arrow-up"), color = "green")
  })
  
  output$box_min <- renderValueBox({
    d <- d_anno(); v <- d[[input$var_desc]]
    i <- which.min(v)
    valueBox(round(v[i], 1), paste("Minimum:", d$nome_mappa[i]),
             icon = icon("arrow-down"), color = "red")
  })
  
  output$mappa_desc <- renderPlotly({
    mappa_coropletica(d_anno(), input$var_desc, etichetta(input$var_desc),
                      evidenzia = input$prov)
  })
  
  output$box_rip <- renderPlotly({
    d <- st_drop_geometry(d_anno())
    i <- riga_prov()
    
    p <- plot_ly(d, x = ~ripartizione, y = d[[input$var_desc]], type = "box",
                 color = ~ripartizione, text = ~nome_mappa)
    
    # The selected province is shown as a red point above the boxplot of its macro-area
    if (!is.na(i) && !is.na(d[[input$var_desc]][i])) {
      p <- p %>%
        add_markers(x = as.character(d$ripartizione[i]),
                    y = d[[input$var_desc]][i],
                    inherit = FALSE,
                    marker = list(color = ROSSO, size = 11),
                    text = d$nome_mappa[i], hoverinfo = "text+y")
    }
    
    p %>%
      layout(showlegend = FALSE,
             yaxis = list(title = etichetta(input$var_desc)),
             xaxis = list(title = ""))
  })
  
  output$serie <- renderPlotly({
    serie <- panel %>%
      group_by(anno) %>%
      summarise(media = mean(.data[[input$var_desc]], na.rm = TRUE),
                .groups = "drop")
    
    # Series for the selected province, overlaid on the national average.
    sp <- panel %>%
      dplyr::filter(cod_provincia == input$prov) %>%
      dplyr::arrange(anno)
    
    p <- plot_ly() %>%
      add_trace(x = serie$anno, y = serie$media, type = "scatter",
                mode = "lines+markers", name = "National average",
                line = list(color = GRIGIO, dash = "dot"),
                marker = list(color = GRIGIO))
    
    if (nrow(sp) > 0) {
      p <- p %>%
        add_trace(x = sp$anno, y = sp[[input$var_desc]], type = "scatter",
                  mode = "lines+markers", name = nome_provincia(input$prov),
                  line = list(color = ROSSO), marker = list(color = ROSSO))
    }
    
    p %>%
      layout(legend = list(orientation = "h", y = -0.15),
             xaxis = list(title = "", dtick = 1),
             yaxis = list(title = etichetta(input$var_desc)))
  })
  
  output$scatter <- renderPlotly({
    d <- st_drop_geometry(d_anno())
    x <- d[[input$var_desc2]]; y <- d[[input$var_desc]]
    i <- riga_prov()
    
    validate(need(input$var_desc2 != input$var_desc,
                  "Choose two different variables."))
    
    ok <- !is.na(x) & !is.na(y)
    validate(need(sum(ok) > 2, "Insufficient data for the regression line."))
    
    fit <- lm(y[ok] ~ x[ok])
    xs <- sort(x[ok])
    ys <- coef(fit)[1] + coef(fit)[2] * xs
    
    p <- plot_ly(x = x, y = y, type = "scatter", mode = "markers",
                 text = d$nome_mappa, hoverinfo = "text+x+y",
                 marker = list(color = "steelblue", size = 8, opacity = 0.7)) %>%
      add_lines(x = xs, y = ys, inherit = FALSE,
                line = list(color = "firebrick"), showlegend = FALSE)
    
    if (!is.na(i) && !is.na(x[i]) && !is.na(y[i])) {
      p <- p %>%
        add_markers(x = x[i], y = y[i], inherit = FALSE,
                    marker = list(color = ROSSO, size = 13),
                    text = d$nome_mappa[i], hoverinfo = "text+x+y")
    }
    
    p %>%
      layout(xaxis = list(title = etichetta(input$var_desc2)),
             yaxis = list(title = etichetta(input$var_desc)),
             showlegend = FALSE)
  })
  
  ## Comparison between provinces ####
  
  # Ranking of all provinces for the selected variable, with the selected one
  # highlighted in red. With 107 bars, labels are unreadable: hence
  # the two alternative subsets (same macro-area, top and bottom ten).
  
  output$confronto <- renderPlotly({
    d <- st_drop_geometry(d_anno())
    d$valore <- d[[input$var_desc]]
    d <- d[!is.na(d$valore), ]
    validate(need(nrow(d) > 0, "No data for this variable."))
    
    media_naz <- mean(d$valore, na.rm = TRUE)
    rip_sel <- d$ripartizione[match(input$prov, d$cod_provincia)]
    
    d_plot <- switch(
      input$ambito,
      "ripartizione" = {
        validate(need(!is.na(rip_sel),
                      "Macro-area not available for the selected province."))
        d[!is.na(d$ripartizione) & d$ripartizione == rip_sel, ]
      },
      "estremi" = {
        ord <- d[order(-d$valore), ]
        tieni <- unique(c(head(ord$cod_provincia, 10),
                          tail(ord$cod_provincia, 10),
                          input$prov))
        d[d$cod_provincia %in% tieni, ]
      },
      d
    )
    
    d_plot <- d_plot[order(-d_plot$valore), ]
    colori <- ifelse(d_plot$cod_provincia == input$prov, ROSSO, GRIGIO)
    
    # With few bars, labels fit; with 107 they do not: they are hidden.
    mostra_etichette <- nrow(d_plot) <= 40
    
    plot_ly(
      x = d_plot$nome_mappa,
      y = d_plot$valore,
      type = "bar",
      marker = list(color = colori),
      text = paste0(d_plot$nome_mappa, "<br>", round(d_plot$valore, 1)),
      hoverinfo = "text"
    ) %>%
      layout(
        bargap = 0.15,
        xaxis = list(title = "", categoryorder = "array",
                     categoryarray = d_plot$nome_mappa,
                     showticklabels = mostra_etichette,
                     tickangle = -60, tickfont = list(size = 10)),
        yaxis = list(title = etichetta(input$var_desc)),
        shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                           y0 = media_naz, y1 = media_naz,
                           line = list(dash = "dot", color = "grey40"))),
        annotations = list(list(xref = "paper", x = 1, y = media_naz,
                                text = sprintf("National average %.1f", media_naz),
                                showarrow = FALSE, xanchor = "right",
                                yanchor = "bottom",
                                font = list(size = 11, color = "grey40")))
      )
  })
  
  
  # TAB 2 - SPATIAL ANALYSIS ####
  
  moran_glob <- reactive({
    moran.test(d_anno()[[input$var_esda]], W_sel(), zero.policy = TRUE)
  })
  
  # Moran's test on residuals after removing differences between macro-areas.
  # This test distinguishes local dependence from a North-South gradient.
  moran_resid <- reactive({
    m <- lm(costruisci_formula(input$var_esda, "ripartizione"), data = d_anno())
    lm.morantest(m, W_sel(), zero.policy = TRUE)
  })
  
  output$box_moran <- renderValueBox({
    m <- moran_glob()
    valueBox(round(m$estimate[[1]], 3),
             sprintf("Global Moran (p = %.4f)", m$p.value),
             icon = icon("globe"), color = "purple")
  })
  
  output$box_moran_res <- renderValueBox({
    m <- moran_resid()
    valueBox(round(m$estimate[[1]], 3),
             sprintf("Moran on residuals (p = %.4f)", m$p.value),
             icon = icon("filter"), color = "yellow")
  })
  
  # Vector on which the local analysis is computed: either the raw variable
  # or the residuals of the regression on macro-area dummies.
  # na.exclude keeps the length aligned with the rows of the map.
  y_esda <- reactive({
    d <- d_anno()
    if (identical(input$base_lisa, "resid")) {
      m <- lm(costruisci_formula(input$var_esda, "ripartizione"),
              data = d, na.action = na.exclude)
      as.numeric(residuals(m))
    } else {
      d[[input$var_esda]]
    }
  })
  
  etichetta_esda <- reactive({
    if (identical(input$base_lisa, "resid"))
      paste0(etichetta(input$var_esda), " (net of macro-areas)")
    else
      etichetta(input$var_esda)
  })
  
  lisa_calc <- reactive({
    d <- d_anno()
    y <- y_esda()
    W <- W_sel()
    
    lisa <- localmoran_perm(y, W, nsim = 999, zero.policy = TRUE,
                            na.action = na.exclude)
    # FDR correction: one test per province, 107 tests in total.
    pval <- p.adjust(lisa[, "Pr(folded) Sim"], method = "fdr")
    
    y_c   <- y - mean(y, na.rm = TRUE)
    lagsp <- lag.listw(W, y, zero.policy = TRUE, NAOK = TRUE)
    lag_c <- lagsp - mean(lagsp, na.rm = TRUE)
    
    q <- rep("not significant", length(y))
    q[y_c > 0 & lag_c > 0] <- "high-high"
    q[y_c < 0 & lag_c < 0] <- "low-low"
    q[y_c > 0 & lag_c < 0] <- "high-low"
    q[y_c < 0 & lag_c > 0] <- "low-high"
    q[is.na(pval) | pval > input$signif_lisa] <- "not significant"
    
    d$quadrante <- factor(q, levels = c("high-high", "low-low", "high-low",
                                        "low-high", "not significant"))
    d
  })
  
  output$box_lisa <- renderValueBox({
    tb <- table(lisa_calc()$quadrante)
    valueBox(sum(tb[1:4]), "Provinces in significant clusters (FDR)",
             icon = icon("layer-group"), color = "orange")
  })
  
  output$moran_plot <- renderPlot({
    d <- d_anno()
    par(mar = c(4.4, 4.4, 1.5, 1))
    moran.plot(y_esda(), W_sel(), labels = d$nome_mappa,
               zero.policy = TRUE,
               xlab = etichetta_esda(),
               ylab = "Neighbour average", main = "")
  })
  
  output$mappa_lisa <- renderPlotly({
    d <- lisa_calc()
    colori <- c("high-high" = "#d73027", "low-low" = "#4575b4",
                "high-low" = "#fdae61", "low-high" = "#abd9e9",
                "not significant" = "#f0f0f0")
    
    p <- ggplot(d) +
      geom_sf(aes(fill = quadrante, text = paste0(nome_mappa, "<br>", quadrante)),
              colour = "grey30", linewidth = 0.1) +
      scale_fill_manual(values = colori, name = "Cluster", drop = FALSE) +
      theme_void()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$moran_anni <- renderPlotly({
    W <- W_sel()
    grezzo <- sapply(ANNI, function(a) {
      moran.test(dati_anno(a)[[input$var_esda]], W,
                 zero.policy = TRUE)$estimate[[1]]
    })
    residui <- sapply(ANNI, function(a) {
      m <- lm(costruisci_formula(input$var_esda, "ripartizione"),
              data = dati_anno(a))
      lm.morantest(m, W, zero.policy = TRUE)$estimate[[1]]
    })
    
    plot_ly(x = ANNI) %>%
      add_trace(y = round(grezzo, 3), name = "Raw variable",
                type = "scatter", mode = "lines+markers") %>%
      add_trace(y = round(residui, 3), name = "Net of macro-areas",
                type = "scatter", mode = "lines+markers") %>%
      layout(xaxis = list(title = "", dtick = 1),
             yaxis = list(title = "Moran's I", range = c(0, 1)),
             legend = list(orientation = "h", x = 0, y = -0.15))
  })
  
  
  # TAB 3 - MODELS ####
  
  # The allowed regressors depend on the selected dependent variable:
  # labour-market outcomes are linked to each other by accounting
  # identities and cannot appear together.
  observeEvent(input$dip, {
    ammessi <- regressori_ammessi(input$dip)
    updateSelectInput(session, "indep", choices = ammessi,
                      selected = intersect(REGRESSORI_DEFAULT, ammessi))
  }, ignoreInit = FALSE)
  
  output$avviso_spec <- renderUI({
    if (input$dip %in% ESITI_LAVORO) {
      div(style = "color:#8a6d3b; background:#fcf8e3; padding:8px;
                   border-radius:4px; font-size:12px;",
          "The other labour-market indicators are excluded ",
          "from the list: they are linked to the dependent variable by accounting identities.")
    }
  })
  
  output$avviso_std <- renderUI({
    if (isTRUE(input$std_indep)) {
      div(style = "color:#31708f; background:#d9edf7; padding:8px;
                   border-radius:4px; font-size:12px; margin-bottom:10px;",
          "Model fit, AIC, t-statistics, p-values, rho and lambda do not change: ",
          "the z-score is a linear transformation. It changes the scale of the ",
          "coefficients, which can now be interpreted as the effect of ",
          tags$b("+1 standard deviation"), " of the regressor and are ",
          "comparable with each other. The mean and standard deviation are calculated ",
          "over the 107 provinces of the selected year, so the values are not ",
          "comparable across different years. The dependent variable and the ",
          "macro-area dummies remain on their original scale.")
    }
  })
  
  # Data used for estimation: identical to d_anno(), with only the regressors
  # selected optionally standardized to z-scores.
  d_modello <- reactive({
    d <- d_anno()
    if (isTRUE(input$std_indep) && length(input$indep) > 0) {
      d <- standardizza(d, input$indep)
    }
    d
  })
  
  # Suffix for axis labels, so the plot always indicates the
  # units of the displayed values.
  unita_coef <- reactive({
    if (isTRUE(input$std_indep)) "per +1 std. dev." else "in original units"
  })
  
  formula_sel <- reactive({
    req(input$indep)
    reg <- input$indep
    if (isTRUE(input$fe_rip)) reg <- c(reg, "ripartizione")
    costruisci_formula(input$dip, reg)
  })
  
  # The weights matrix is frozen at the moment of estimation: if the user changes
  # it in the sidebar without re-estimating, the tests and the impacts would be
  # computed on a W different from the one used to fit the models.
  W_stima <- eventReactive(input$stima, { W_sel() }, ignoreNULL = FALSE)
  
  # ignoreNULL = FALSE: the models are estimated once at startup, then only
  # when the button is pressed.
  modelli <- eventReactive(input$stima, {
    d <- d_modello(); W <- W_sel(); frm <- formula_sel()
    frm_durbin <- as.formula(paste("~", paste(input$indep, collapse = " + ")))
    sel <- input$modelli_sel_stima
    
    stima <- function(expr) tryCatch(expr, error = function(e) NULL)
    
    withProgress(message = "Estimating models...", value = 0, {
      passo <- 1 / (length(sel) + 1)
      out <- list()
      
      out$OLS <- lm(frm, data = d); incProgress(passo)
      
      if ("SAR"  %in% sel) { out$SAR  <- stima(lagsarlm(frm, data = d, listw = W, zero.policy = TRUE)); incProgress(passo) }
      if ("SEM"  %in% sel) { out$SEM  <- stima(errorsarlm(frm, data = d, listw = W, zero.policy = TRUE)); incProgress(passo) }
      if ("SDM"  %in% sel) { out$SDM  <- stima(lagsarlm(frm, data = d, listw = W, Durbin = frm_durbin, zero.policy = TRUE)); incProgress(passo) }
      if ("SLX"  %in% sel) { out$SLX  <- stima(lmSLX(frm, data = d, listw = W, Durbin = frm_durbin)); incProgress(passo) }
      if ("SDEM" %in% sel) { out$SDEM <- stima(errorsarlm(frm, data = d, listw = W, Durbin = frm_durbin, zero.policy = TRUE)); incProgress(passo) }
      if ("SAC"  %in% sel) { out$SAC  <- stima(sacsarlm(frm, data = d, listw = W, zero.policy = TRUE)); incProgress(passo) }
      if ("GNS"  %in% sel) { out$GNS  <- stima(sacsarlm(frm, data = d, listw = W, Durbin = frm_durbin, zero.policy = TRUE)); incProgress(passo) }
      
      out[!sapply(out, is.null)]
    })
  }, ignoreNULL = FALSE)
  
  # The model menu only offers what was actually estimated.
  observeEvent(modelli(), {
    disp <- names(modelli())
    sel  <- if (isolate(input$modello_sel) %in% disp) isolate(input$modello_sel) else disp[1]
    updateSelectInput(session, "modello_sel", choices = disp, selected = sel)
  })
  
  # Warns when the specification has changed but the models have not been re-estimated.
  firma <- reactive({
    paste(input$anno, input$matrice, input$dip,
          paste(sort(input$indep), collapse = "|"),
          isTRUE(input$fe_rip), isTRUE(input$std_indep),
          paste(sort(input$modelli_sel_stima), collapse = "|"))
  })
  firma_stimata <- reactiveVal(NULL)
  observeEvent(modelli(), { firma_stimata(isolate(firma())) })
  
  output$avviso_stale <- renderUI({
    req(firma_stimata())
    if (!identical(firma(), firma_stimata())) {
      div(style = "margin-top:8px; padding:8px; background:#fcf8e3;
                   border-left:4px solid #f0ad4e; border-radius:4px; font-size:12px;",
          "The specification has changed. The results shown below refer to the
           previous estimation: press ", tags$b("Estimate models"), " to update them.")
    }
  })
  
  # ---- LM specification tests ------------------------------------------
  # spdep >= 1.3 renamed lm.LMtests() into lm.RStests() (Rao score tests) and
  # changed the test names. Both versions are handled so the app runs on either.
  # The tests are computed on the OLS with the SAME specification used
  # everywhere else, macro-area fixed effects included.
    lm_tests_calc <- reactive({
    ols <- modelli()$OLS
    W   <- W_stima()
    req(!is.null(ols))
    
    # "all" is the only argument accepted by every spdep version: the names of
    # the individual tests changed with the switch from lm.LMtests() to
    # lm.RStests() (LMerr -> RSerr, RLMlag -> adjRSlag, and so on).
    out <- if ("lm.RStests" %in% getNamespaceExports("spdep")) {
      spdep::lm.RStests(ols, W, zero.policy = TRUE, test = "all")
    } else {
      spdep::lm.LMtests(ols, W, zero.policy = TRUE, test = "all")
    }
    
    # Takes the first name present among the possible aliases.
    p_di <- function(...) {
      cand <- c(...)
      hit  <- names(out)[names(out) %in% cand]
      if (length(hit) == 0) return(NA_real_)
      v <- out[[hit[1]]]$p.value
      if (is.null(v)) NA_real_ else as.numeric(v)
    }
    
    list(out    = out,
         p_err  = p_di("LMerr",  "RSerr"),
         p_lag  = p_di("LMlag",  "RSlag"),
         p_rerr = p_di("RLMerr", "adjRSerr"),
         p_rlag = p_di("RLMlag", "adjRSlag"))
  })
  
  output$lm_tests <- renderPrint({
    summary(lm_tests_calc()$out)
  })
  
  # Anselin-Florax decision rule: first the standard tests, then the robust
  # versions to discriminate when both are significant.
  output$lm_raccomandazione <- renderUI({
    t <- lm_tests_calc()
    a <- 0.05
    
    riquadro <- function(testo, sfondo, bordo) {
      div(style = paste0("padding:10px; background:", sfondo,
                         "; border-left:4px solid ", bordo,
                         "; border-radius:4px; font-size:13px;"),
          HTML(testo))
    }
    
    if (any(is.na(c(t$p_err, t$p_lag)))) {
      return(riquadro("Tests not available for this specification.",
                      "#fcf8e3", "#f0ad4e"))
    }
    
    err <- t$p_err < a; lag <- t$p_lag < a
    
    if (!err && !lag) {
      riquadro("Neither test is significant: <b>OLS</b> is adequate,
                there is no residual spatial dependence to model.",
               "#dff0d8", "#5cb85c")
    } else if (lag && !err) {
      riquadro("Only the lag test is significant: <b>SAR</b>.", "#d9edf7", "#31708f")
    } else if (err && !lag) {
      riquadro("Only the error test is significant: <b>SEM</b>.", "#d9edf7", "#31708f")
    } else {
      rerr <- !is.na(t$p_rerr) && t$p_rerr < a
      rlag <- !is.na(t$p_rlag) && t$p_rlag < a
      if (rlag && !rerr) {
        riquadro("Both standard tests are significant; among the robust ones only
                  the lag test survives: <b>SAR</b>.", "#d9edf7", "#31708f")
      } else if (rerr && !rlag) {
        riquadro("Both standard tests are significant; among the robust ones only
                  the error test survives: <b>SEM</b>.", "#d9edf7", "#31708f")
      } else if (rerr && rlag) {
        riquadro("Both robust tests are significant: the two forms of dependence
                  coexist. Consider <b>SAC</b> or <b>SDM</b>, and compare them by AIC.",
                 "#fcf8e3", "#f0ad4e")
      } else {
        riquadro("The standard tests are significant but neither robust test is:
                  the evidence does not identify a single form of dependence.
                  Rely on the AIC comparison.", "#fcf8e3", "#f0ad4e")
      }
    }
  })
  metriche_tab <- reactive({
    m <- modelli()
    m <- m[!sapply(m, is.null)]
    W <- W_stima()
    
    righe <- lapply(names(m), function(nm) {
      mod <- m[[nm]]
      r   <- as.numeric(residuals(mod))
      
      # For OLS the correct test is lm.morantest, which accounts for the
      # estimated coefficients. For ML spatial models there is no equivalent
      # analytical correction: moran.test is used and its p-value is indicative.
      # two.sided: the default alternative is "greater", which would return a
      # p-value close to 1 for any negative index, hiding an over-correction.
      mor <- tryCatch(
        if (nm == "OLS") lm.morantest(mod, W, zero.policy = TRUE,
                                      alternative = "two.sided")
        else moran.test(r, W, zero.policy = TRUE,
                        alternative = "two.sided"),
        error = function(e) NULL)
      
      data.frame(
        Model    = nm,
        LogLik   = round(as.numeric(logLik(mod)), 2),
        AIC      = round(AIC(mod), 2),
        BIC      = round(BIC(mod), 2),
        RMSE     = round(sqrt(mean(r^2, na.rm = TRUE)), 4),
        `Moran resid.` = if (is.null(mor)) NA_real_ else round(as.numeric(mor$estimate[[1]]), 3),
        `p-value`      = if (is.null(mor)) NA_real_ else round(mor$p.value, 4),
        check.names = FALSE
      )
    })
    
    out <- do.call(rbind, righe)
    out[order(out$AIC), ]
  })
  
  # Likelihood ratio tests between nested pairs. Each pair imposes a zero
  # restriction on one component: OLS -> SAR sets rho = 0, SLX -> SDM sets rho = 0,
  # SAR -> SDM sets theta = 0, and so on. A significant test rejects the
  # restricted model in favour of the general one.
  lr_tab <- reactive({
    m <- modelli()
    m <- m[!sapply(m, is.null)]
    
    coppie <- list(
      c("OLS", "SLX"),  c("OLS", "SAR"),   c("OLS", "SEM"),
      c("SLX", "SDM"),  c("SLX", "SDEM"),
      c("SAR", "SDM"),  c("SAR", "SAC"),
      c("SEM", "SDEM"), c("SEM", "SAC"),
      c("SDM", "GNS"),  c("SDEM", "GNS"),  c("SAC", "GNS")
    )
    
    righe <- lapply(coppie, function(cp) {
      if (!all(cp %in% names(m))) return(NULL)
      tryCatch({
        ll_r <- logLik(m[[cp[1]]]); ll_g <- logLik(m[[cp[2]]])
        stat <- max(0, 2 * (as.numeric(ll_g) - as.numeric(ll_r)))
        gdl  <- max(1, abs(attr(ll_g, "df") - attr(ll_r, "df")))
        data.frame(confronto = paste(cp[1], "\u2192", cp[2]),
                   LR = stat, df = gdl,
                   p  = pchisq(stat, gdl, lower.tail = FALSE))
      }, error = function(e) NULL)
    })
    
    out <- do.call(rbind, righe)
    validate(need(!is.null(out), "No nested pairs available."))
    out[order(out$LR), ]
  })
  
  output$lr_plot <- renderPlotly({
    d <- lr_tab()
    d$esito <- ifelse(d$p < 0.05, "General model preferred",
                      "Restricted model sufficient")
    
    plot_ly(d, x = ~LR, y = ~factor(confronto, levels = confronto),
            color = ~esito, type = "bar", orientation = "h",
            colors = c("General model preferred" = "#2e7d32",
                       "Restricted model sufficient" = "#90a4ae"),
            hovertext = ~sprintf("%s<br>LR = %.2f (df = %d)<br>p = %.4f",
                                 confronto, LR, df, p),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "LR statistic"),
             yaxis = list(title = ""),
             legend = list(orientation = "h", x = 0, y = -0.15))
  })
  
  aic_tab <- reactive({
    d <- metriche_tab()
    data.frame(modello = d$Model, AIC = d$AIC, row.names = NULL)
  })
  
  output$metriche_out <- renderTable({
    d <- metriche_tab()
    d$Best <- ifelse(d$AIC == min(d$AIC, na.rm = TRUE), "\u2713", "")
    d
  }, striped = TRUE, bordered = TRUE, na = "-", digits = 4)
  
  output$box_modello <- renderValueBox({
    a <- aic_tab()
    best <- a$modello[which.min(a$AIC)]
    valueBox(best, "AIC-preferred model",
             icon = icon("trophy"), color = "green")
  })
  
  output$box_rho <- renderValueBox({
    m <- modelli()[[input$modello_sel]]
    val <- if (is.null(m)) NA
    else if (!is.null(m$rho)) m$rho
    else if (!is.null(m$lambda)) m$lambda
    else NA
    valueBox(if (is.na(val)) "-" else round(val, 3),
             "Spatial parameter (rho or lambda)",
             icon = icon("arrows-left-right"), color = "blue")
  })
  
  output$box_moran_resid <- renderValueBox({
    mt <- lm.morantest(modelli()$OLS, W_stima(), zero.policy = TRUE)
    valueBox(round(mt$estimate[[1]], 3),
             sprintf("Moran on OLS residuals (p = %.4f)", mt$p.value),
             icon = icon("wave-square"), color = "maroon")
  })
  
  output$aic_plot <- renderPlotly({
    a <- aic_tab()
    a$colore <- ifelse(a$AIC == min(a$AIC), "#2e7d32", "#90a4ae")
    
    plot_ly(a, x = ~reorder(modello, AIC), y = ~AIC, type = "bar",
            marker = list(color = ~colore),
            text = ~round(AIC, 1), hoverinfo = "text") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "AIC",
                          range = c(min(a$AIC) - 15, max(a$AIC) + 5)))
  })
  
  output$coef_plot <- renderPlotly({
    m <- modelli()[[input$modello_sel]]
    validate(need(!is.null(m), "Model cannot be estimated with this specification."))
    
    tb <- coef_matrice(m)
    validate(need(!is.null(tb), "Coefficients not available for this model."))
    
    # intercept and dummies: these are not the coefficients of interest
    tb <- tb[tb$termine != "(Intercept)" &
               !grepl("^ripartizione", tb$termine), ]
    validate(need(nrow(tb) > 0, "No coefficients to display."))
    
    plot_ly(tb, x = ~stima, y = ~reorder(termine, stima),
            type = "scatter", mode = "markers",
            error_x = ~list(array = 1.96 * se),
            marker = list(color = "#1565c0", size = 9)) %>%
      layout(xaxis = list(title = paste0("Coefficient ", unita_coef(),
                                         " (IC 95%)")),
             yaxis = list(title = ""),
             shapes = list(list(type = "line", x0 = 0, x1 = 0,
                                y0 = -0.5, y1 = nrow(tb) - 0.5,
                                line = list(dash = "dot", color = "grey"))))
  })
  
  output$tab_modello <- renderPrint({
    m <- modelli()[[input$modello_sel]]
    if (is.null(m)) cat("Model cannot be estimated with this specification.")
    else {
      if (isTRUE(input$std_indep)) {
        cat("Regressors standardized (z-score) across the provinces of year",
            input$anno, "\n\n")
      }
      summary(m)
    }
  })
  
  
  # TAB 4 - EFFECTS AND SPILLOVERS ####
  
  impatti_calc <- reactive({
    m <- modelli()[[input$modello_sel]]
    validate(need(!is.null(m), "Model not available."))
    validate(need(input$modello_sel %in% c("SAR", "SDM", "SAC", "SLX", "SDEM", "GNS"),
                  "Direct effects exist only in models with a spatial lag (SAR, SDM, SAC, SLX, SDEM, GNS)."))
    
    W <- W_stima()
    
    # SLX and SDEM have no autoregressive term in y: the impacts are read
    # directly off the coefficients and require neither the trace matrix nor simulations.
    if (input$modello_sel %in% c("SLX", "SDEM")) {
      impacts(m, listw = W)
    } else {
      trMat <- trW(as(W, "CsparseMatrix"), type = "mult")
      impacts(m, tr = trMat, R = 200)
    }
  })
  
  output$impatti_plot <- renderPlotly({
    im <- summary(impatti_calc(), zstats = TRUE, short = TRUE)
    
    # LagImpact objects do not always expose variable names.
    # They are not needed: impacts follow the order of the formula, and input$indep
    # comes before the macro-area dummies. The first k values are sufficient.
    val <- if (!is.null(im$res)) im$res else im
    dir <- as.numeric(val$direct)
    ind <- as.numeric(val$indirect)
    
    k <- length(input$indep)
    validate(need(length(dir) >= k, "Impacts not available."))
    
    df <- data.frame(
      variabile = rep(etichetta(input$indep), 2),
      tipo = rep(c("Direct", "Indirect"), each = k),
      valore = c(dir[seq_len(k)], ind[seq_len(k)])
    )
    
    plot_ly(df, x = ~valore, y = ~variabile, color = ~tipo, type = "bar",
            orientation = "h",
            colors = c("Direct" = "#1565c0", "Indirect" = "#ef6c00")) %>%
      layout(barmode = "group",
             xaxis = list(title = paste0("Estimated effect ", unita_coef())),
             yaxis = list(title = ""))
  })
  
  output$impatti_tab <- renderPrint({
    if (isTRUE(input$std_indep)) {
      cat("Effects for +1 standard deviation of the regressor.\n\n")
    }
    summary(impatti_calc(), zstats = TRUE, short = TRUE)
  })
}