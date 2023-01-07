#' Import and clean Nevada Water Rights Spatial Data. Function still local -- need to change this!
#' 
#'
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units used in  `park`) to buffer around spatial object `park`
#' 
#' @return An sf object of points of diversion within given spatial boundary and attributes
getPODNevada <- function(aoi, dist){
  
  # add buffer around park boundary  
  aoi_buffer <- aoi %>%
    sf::st_buffer(dist = dist) 
  
  #read in POD attribute table
  table <- read_csv('data/all/Water_Rights_Points_of_Diversion.csv')
  
  #read in spatial points of diversion
  nv_wr <- sf::st_read(paste0('data/all/POD_Permits_All.shp')) %>%
    sf::st_drop_geometry() %>%
    inner_join(.,table, by="app") %>%
    sf::st_as_sf(.,coords=c("longitude","latitude"), crs = 4269) %>%
    sf::st_transform(crs=4269) %>%
    sf::st_intersection(., aoi_buffer) %>%
    #sf::st_join(.,dplyr::select(aoi,within=UNIT_CODE),left=TRUE) %>%
    dplyr::mutate(PARK_RIGHT=ifelse(grepl("National Park", owner_name, ignore.case=T), "Park", "Non-park"))
    #within_park=ifelse(UNIT_CODE==within,"Within","Outside")) %>%
    #select(-within)
  
  return(nv_wr)
  
}


