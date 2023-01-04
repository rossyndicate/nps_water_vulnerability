#' Import NPS buildings

getBuildings <- function(aoi, dist=0, save=FALSE, path){
  
  #install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
  
  temp1 <- paste0('data/all/Public_Buildings_py_wm_wgs84.shp')
  
  sf::sf_use_s2(FALSE)
  
  buffer <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  buildings <- sf::st_read(temp1) %>%
    sf::st_transform(4269) %>%
    .[buffer,]
  
  if(save == TRUE){
    sf::st_write(buildings, paste0(path, "/buildings.shp"))
    
  }
  
  return(buildings)
}
