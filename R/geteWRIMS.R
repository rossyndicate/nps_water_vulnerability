#' Import and clean California Water Rights Spatial Data
#' 
#' This function imports geospatial data from the eWRIMS database (https://waterrightsmaps.waterboards.ca.gov/viewer/index.html?viewer=eWRIMS.eWRIMS_gvh#)
#' 
#' @param x An sf polygon object, often a park boundary retrieved from `getParkBoundary()`
#' @param dist The distance (in the same units used by `x`) to buffer around spatial object `x`
#' @param data_path The file path to the eWRIMS geodatabase
#' 
#' @return An sf object of points of diversion within given spatial boundary and attributes
geteWRIMS <- function(x, dist, data_path){
  
  #ca_pod <- arcpullr::get_spatial_layer("https://gispublic.waterboards.ca.gov/arcgis/rest/services/Water_Rights/Points_of_Diversion/FeatureServer/0")
  
  # add buffer around park boundary
  aoi <- sf::st_buffer(x, dist = dist)
  
  #read in spatial points of diversion
  ca_wr <- sf::st_read(data_path) %>%
    sf::st_transform(crs=4269)
  
  #read in POD attribute table
  table <- sf::st_read(data_path, layer = "POD_Attributes")
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_wr <- ca_wr %>%
    dplyr::select(POD_ID,LATITUDE,LONGITUDE,DIVERSION_SITE_NAME,SOURCE_NAME) %>%
    dplyr::inner_join(., table, by=c("POD_ID"="CORE_POD_ID")) %>%
    sf::st_intersection(., aoi) %>%
    dplyr::mutate(RELATIONSHIP=ifelse(grepl("National Park", LAST_NAME, ignore.case=T), "PARK","NONPARK"))
  
  return(nearby_wr)
  
}


