#  global.R
#  Female employment and care services in Italian provinces, 2018-2022

# Libraries ####

library(shiny)
library(shinydashboard)
library(readxl)
library(sf)
library(spdep)
library(spatialreg)
library(plotly)
library(ggplot2)
library(broom)


if (requireNamespace("shinycssloaders", quietly = TRUE)) {
  library(shinycssloaders)
} else {
  withSpinner <- function(ui_element, ...) ui_element
  message("shinycssloaders not installed: spinners disabled. ",
          "To get them: install.packages('shinycssloaders')")
}

library(dplyr)

# 1. PATHS ####

FILE_DATI <- "data/panel_province_lavoro_cura.xlsx"
FILE_MAPPA <- "data/ProvCM01012023_g_WGS84.shp"

EPSG_METRICO <- 32632   # WGS84 / UTM 32N: coordinates in meters, for the centroids

# 2. PANEL ####

panel <- read_excel(
  FILE_DATI,
  sheet = "panel_province",
  col_types = c("text", "text", "numeric", rep("numeric", 9))
) %>%
  dplyr::mutate(
    cod_provincia = sprintf("%03d", as.integer(cod_provincia)),
    anno = as.integer(anno),
  )

ANNI <- sort(unique(panel$anno))

# Readable labels for the UI menus
VARIABILI <- c(
  "Female employment rate"                = "tasso_occupazione_F",
  "M-F employment gap"                    = "gap_occupazione_MF",
  "M-F activity gap"                      = "gap_attivita_MF",
  "Female unemployment rate"              = "tasso_disoccupazione_F",
  "Labor market participation"            = "partecipazione_ml",
  "Childcare services: availability"      = "servizi_infanzia_diffusione",
  "Childcare services: enrollment rate"   = "servizi_infanzia_presa_carico",
  "Elderly in home care"                  = "anziani_assistenza_domiciliare",
  "Ultra-broadband coverage"              = "banda_ultra_larga"
)

DIPENDENTE_DEFAULT <- "tasso_occupazione_F"
REGRESSORI_DEFAULT <- c("servizi_infanzia_presa_carico",
                        "anziani_assistenza_domiciliare",
                        "banda_ultra_larga")

# 3. CARTOGRAPHY ####

mappa <- st_read(FILE_MAPPA, quiet = TRUE) %>%
  st_transform(EPSG_METRICO) %>%
  st_make_valid()

# st_make_valid() fixes invalid geometries but can return a GEOMETRYCOLLECTION.
# Just one mixed geometry is enough for the column to become class sfc_GEOMETRY,
# which ggplotly() cannot convert. Everything is cast back to MULTIPOLYGON.
if (!all(st_geometry_type(mappa) %in% c("POLYGON", "MULTIPOLYGON"))) {
  mappa <- st_collection_extract(mappa, "POLYGON")
}
mappa <- st_cast(mappa, "MULTIPOLYGON")

col_cod <- intersect(c("COD_PROV", "COD_UTS"), names(mappa))[1]
if (is.na(col_cod)) {
  stop("No code field found in the shapefile. Available fields: ",
       paste(names(mappa), collapse = ", "))
}

col_nome <- intersect(c("DEN_UTS", "DEN_PROV", "NAME"), names(mappa))[1]

# TIPO_UTS distinguishes provinces from metropolitan cities.
# COD_RIP is the geographic macro-area (1 North-West,
# 2 North-East, 3 Center, 4 South, 5 Islands): used for the
# macro-area fixed effects in the spatial trend test.
col_tipo <- intersect(c("TIPO_UTS"), names(mappa))[1]
col_rip <- intersect(c("COD_RIP"), names(mappa))[1]

ETICHETTE_RIP <- c("1" = "North-West", "2" = "North-East", "3" = "Center",
                   "4" = "South", "5" = "Islands")

mappa <- mappa %>%
  dplyr::mutate(
    cod_provincia = sprintf("%03d", as.integer(.data[[col_cod]])),
    nome_mappa = as.character(.data[[col_nome]]),
    tipo_uts = if (is.na(col_tipo)) NA_character_ else as.character(.data[[col_tipo]]),
    ripartizione = if (is.na(col_rip)) NA_character_ else
      factor(ETICHETTE_RIP[as.character(.data[[col_rip]])], levels = ETICHETTE_RIP)
  ) %>%
  dplyr::select(cod_provincia, nome_mappa, tipo_uts, ripartizione, geometry) %>%
  dplyr::arrange(cod_provincia)


# 4. MATCH CHECK ####

solo_dati  <- setdiff(unique(panel$cod_provincia), mappa$cod_provincia)
solo_mappa <- setdiff(mappa$cod_provincia, unique(panel$cod_provincia))

if (length(solo_dati) > 0 || length(solo_mappa) > 0) {
  stop(
    "Mismatch between data and cartography.\n",
    "  In the data but not in the map: ",
    paste(solo_dati, collapse = ", "), "\n",
    "  In the map but not in the data: ",
    paste(solo_mappa, collapse = ", "), "\n"
  )
}

message(sprintf("Match successful: %d provinces, years %s.",
                nrow(mappa), paste(range(ANNI), collapse = "-")))


# 4.2. LIST OF PROVINCES AND COMPARISON OPTIONS ####
#    Vector with names = labels shown in the menu, values = Istat code.

PROVINCE <- stats::setNames(mappa$cod_provincia, mappa$nome_mappa)
PROVINCE <- PROVINCE[order(names(PROVINCE))]

PROVINCIA_DEFAULT <- if ("Brescia" %in% names(PROVINCE)) {
  unname(PROVINCE[["Brescia"]])
} else {
  unname(PROVINCE[1])
}

# Scope of the "Province comparison" chart: with 107 bars the labels
# become unreadable, so two narrower subsets are offered.
AMBITI_CONFRONTO <- c(
  "All provinces"                    = "tutte",
  "Same macro-area only"             = "ripartizione",
  "Top and bottom ten"               = "estremi"
)

# 5. SPATIAL WEIGHTS MATRICES ####
#    Built ONLY ONCE using the order of 'mappa'. Every annual slice
#    must respect this same order (see dati_anno()).

nb_queen <- poly2nb(mappa, queen = TRUE)

centroidi <- suppressWarnings(st_centroid(st_geometry(mappa)))

nb_knn5 <- knn2nb(knearneigh(st_coordinates(centroidi), k = 5))

W_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)
W_knn5  <- nb2listw(nb_knn5,  style = "W", zero.policy = TRUE)

MATRICI <- c("5 nearest neighbors" = "W_knn5", "Queen contiguity" = "W_queen")

# Diagnostics: with queen contiguity the Italian graph splits into three
# components (mainland, Sicily, Sardinia). No province is left isolated,
# but the islands never connect to the mainland. The knn stitches the Strait
# of Messina back together; Sardinia still remains separate, it is too far.

comp_queen <- n.comp.nb(nb_queen)
message(sprintf("Queen: %d components, %d provinces with no neighbors, %.2f average neighbors.",
                comp_queen$nc, sum(card(nb_queen) == 0), mean(card(nb_queen))))
message(sprintf("KNN5:  %d components.", n.comp.nb(nb_knn5)$nc))


# 6. EXTRACTING A SINGLE YEAR ####
#    Returns an sf object with one row per province, in the same order as the
#    weights matrix. The stopifnot is the safety net: if one day an
#    NA or a filter makes a row disappear, the error surfaces here instead of
#    propagating silently into lagsarlm().

dati_anno <- function(a) {
  d <- panel %>%
    dplyr::filter(anno == a) %>%
    dplyr::arrange(cod_provincia)
  
  stopifnot(
    "Number of provinces differs from the weights matrix" =
      nrow(d) == nrow(mappa),
    "Province order not aligned with the weights matrix" =
      identical(d$cod_provincia, mappa$cod_provincia)
  )
  
  mappa %>%
    dplyr::left_join(d, by = "cod_provincia")
}

pesi_sottoinsieme <- function(nb, tenere) {
  nb2listw(subset(nb, subset = tenere), style = "W", zero.policy = TRUE)
}


# 7. DYNAMIC FORMULA ####

costruisci_formula <- function(dipendente, regressori) {
  if (length(regressori) == 0) {
    return(as.formula(paste(dipendente, "~ 1")))
  }
  as.formula(paste(dipendente, "~", paste(regressori, collapse = " + ")))
}


# 8. ADMISSIBLE REGRESSORS ####
#    The five labor market outcomes are linked to each other by accounting
#    identities or are simultaneously determined: the M-F gap contains the
#    female employment rate, participation is employed plus unemployed over
#    the population. Putting one on the right-hand side of another produces
#    an R^2 close to 1 that means nothing. The dashboard must prevent this.

ESITI_LAVORO <- c("tasso_occupazione_F", "gap_occupazione_MF",
                  "gap_attivita_MF", "tasso_disoccupazione_F",
                  "partecipazione_ml")

regressori_ammessi <- function(dipendente) {
  if (dipendente %in% ESITI_LAVORO) {
    VARIABILI[!VARIABILI %in% ESITI_LAVORO]
  } else {
    VARIABILI[VARIABILI != dipendente]
  }
}

# 9. STANDARDIZATION OF REGRESSORS (z-score) ####
#
#    (x - mean) / standard deviation, computed on the cross-section
#    of the selected year: 107 provinces, not the whole panel.

z_score <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(x - m)   # zero variance: centering only
  (x - m) / s
}

standardizza <- function(d, variabili) {
  for (v in intersect(variabili, names(d))) {
    if (is.numeric(d[[v]])) d[[v]] <- z_score(d[[v]])
  }
  d
}