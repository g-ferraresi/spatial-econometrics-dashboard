# Installs the packages the dashboard needs. Run once:  source("install_packages.R")
pkgs <- c("shiny", "shinydashboard", "shinycssloaders", "readxl", "sf", "spdep",
          "spatialreg", "plotly", "ggplot2", "broom", "dplyr")
install.packages(setdiff(pkgs, rownames(installed.packages())))
