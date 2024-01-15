# setup script for processing functions

#Install (if necessary) and load all required packages

packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }

# vector of packages to load
packages <- c('tidyverse',
              'sf',
              'httr',
              'jsonlite',
              'nhdplusTools',
              'rnoaa',
              "mapview",
              "ggpubr",
              "kableExtra",
              "plotly",
              "DT",
              "igraph",
              "nngeo",
              #"USAboundaries",
              "tigris")


packageLoad(packages)

# source all functions

purrr::map(list.files(
  path = "src/",
  pattern = "*.R",
  full.names = TRUE,
  recursive = TRUE
),
source)

# set noaa key (don't think this is needed anymore)
#noaa_key <-  "VAOckuKZRAFPIOuippCpMBAUPTiAtVMn"
