#' Import NPS buildings

getBuildings <- function(aoi, save=FALSE, path){
  
  #install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
  
  temp1 <- paste0('data/all/Public_Buildings_py_wm_wgs84.shp')
  
  sf::sf_use_s2(FALSE)
  
  aoi <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  buildings <- sf::st_read(temp1) %>%
    sf::st_transform(4269) %>%
    .[aoi,]
  
  if(save == TRUE){
    sf::st_write(aoi, paste0(path, "/buildings.shp"))
    
  }
  
  return(buildings)
}
