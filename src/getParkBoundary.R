#' Import park boundary shapefile
#' 
#' This function uses the Data Store REST Api to download national park boundary 
#' shapefiles.
#' 
#' @param park The 4 digit national park code(s) for parks of interest
#' @param save Whether to save (TRUE) the resulting shapefile or not (FALSE)
#' @param path If `save == TRUE`, the file path to save the shapefile. Must end with "/"
#' 
#' @return A spatial sf object for each specified park boundary
#' 
#' @seealso [pullDataStore()]
getParkBoundary <- function(park, save = FALSE, path = NULL){
  
  call <- "https://irmaservices.nps.gov/datastore/v4/rest"
  
  
  temp1 <- tempfile()
  download.file(paste0(call, "/DownloadFile/", 673366), destfile = temp1, method = "curl")
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  
  parks <- sf::st_read(dsn = temp2)
  
  poi <- parks %>% 
    dplyr::filter(UNIT_CODE %in% park)
    
  
  if(save == TRUE){
    sf::st_write(poi, paste0(path, "park_boundary.shp"))
    
  }
  
  return(poi)
  
}