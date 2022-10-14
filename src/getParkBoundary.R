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
  
  #pull resource ID using reference ID of the park boundaries landing page
  resID <- httr::GET(paste0(call, "/Reference/2225713/DigitalFiles")) %>% 
    httr::content("text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON( flatten = TRUE) %>% 
    dplyr::as_tibble() %>% 
    filter(str_detect(fileName, "nps_boundary")) %>% 
    pull(resourceId) %>% 
    as.numeric()
  
  #download boundary 
  temp1 <- tempfile()
  download.file(paste0(call, "/DownloadFile/", resID), destfile = temp1, method = "curl")
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