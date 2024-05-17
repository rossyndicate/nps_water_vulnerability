# setup script for processing functions

#Install (if necessary) and load all required packages ----------------

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
packages <- c(
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
              "dataRetrieval",
              "tigris",
              "data.table",
              "terra",
              "stars",
              "remotes",
              'devtools',
              'ggfortify',
              'ggrepel',
              "stars",
              "ggforce",
              "janitor",
              "rvest",
              "lwgeom",
              "zoo",
              "gridExtra",
              "akima")


packages <- c(packages, "tidyverse")


packageLoad(packages)

## GitHub package installs ----------------

# suggested install of github version for climateR
# remotes::install_github("mikejohnson51/AOI") # suggested! But I don't think we need it..
if (!"climateR" %in% installed.packages()) {
  remotes::install_github("mikejohnson51/climateR")
}
library(climateR)


if (!"cft" %in% installed.packages()) {
  devtools::install_github("earthlab/cft")
}
library(cft)

# source all functions --------------------------

purrr::map(list.files(
  path = "src/",
  pattern = "*.R",
  full.names = TRUE,
  recursive = TRUE
),
source)

# set noaa key (don't think this is needed anymore)
#noaa_key <-  "VAOckuKZRAFPIOuippCpMBAUPTiAtVMn"
