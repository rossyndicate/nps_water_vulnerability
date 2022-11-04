#' Import Omernik II Ecoregion shapefile
#' 
#' This function pulls Omernik III ecoregion shapefiles within the park(s) of interest. 
#'
#' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The buffer distance (in decimal degrees) around park boundaries to filter data to
#' @param save Whether to save (TRUE) the resulting shapefile or not (FALSE)
#' @param path If `save = TRUE`, the file path to save the shapefile
#' 
#' @return A spatial sf object of Omernik III ecoregions for each specified park boundary
#' 
#' @seealso [getParkBoundary()]
downloadOmernik <- function(aoi, save=FALSE, path){
  
  if(!dir.exists(paste0(getwd(),"/data/all/nps_structures/"))){dir.create(file.path(getwd(),"data/all/nps_structures/"), showWarnings = FALSE)}
  
  call <- "https://mapservices.nps.gov/arcgis/rest/services/NationalDatasets/NPS_Public_Buildings/MapServer?f=lyr&v=9.3"
  
  #download boundary 
  temp1 <- tempfile()
  download.file(paste0(call), destfile = "data/all/nps_structures/")#, method = "curl")
  unzip(temp1,  exdir = "data/all/nps_structures/")
  
  
  sf::sf_use_s2(FALSE)
  
  aoi <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  ecoregs <- sf::st_read(dsn = temp1)# %>%
    sf::st_transform(4269) %>%
    .[aoi,]
  
  if(save == TRUE){
    sf::st_write(poi, paste0(path, "/omernik_region.shp"))
    
  }
  
  return(ecoregs)
}
install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
