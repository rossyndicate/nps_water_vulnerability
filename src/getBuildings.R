#' Import NPS buildings
#' 
#' https://public-nps.opendata.arcgis.com/datasets/nps::nps-buildings-web-mercator/explore?location=21.734632%2C-117.659450%2C4.08

getBuildings <- function(aoi, dist=0){
  
  #install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
  call <- "https://opendata.arcgis.com/api/v3/datasets/2bf22e15d269431e9e3380382c0a1546_2/downloads/data?format=shp&spatialRefId=3857&where=1%3D1"
 
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  sf::sf_use_s2(FALSE)
  
  buffer <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  buildings <- rgdal::readOGR(temp2, layer ="Public_Buildings_py_wm_wgs84") %>%
    sf::st_as_sf(.) %>%
    sf::st_cast(., "POLYGON") %>%
    sf::st_transform(4269) %>%
    sf::st_join(., dplyr::select(buffer, UNIT_CODE)) %>%
    dplyr::filter(!is.na(UNIT_CODE))
  
  return(buildings)
}

