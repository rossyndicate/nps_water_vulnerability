#' Import and clean Utah Water Rights Point of Diversion Data
#' This function imports geospatial data from the Utah Geospatial Resource Center (https://opendata.gis.utah.gov/datasets/utahDNR::utah-points-of-diversion/explore?location=40.989324%2C-114.748636%2C-1.00)
#' 
#' col descriptions for POD_supply @ 
#' https://www.arcgis.com/sharing/rest/content/items/5d530e62e6ca42528dd13e0a453a3b73/info/metadata/metadata.xml?format=default&output=html
#' Uses: 
#'      D = Domestic 
#'      I = Irrigation 
#'      M = Municipal 
#'      O = Other 
#'      P = Power 
#'      S = Stockwatering 
#'      X = Mining
#' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units used in  `park`) to buffer around spatial object `park`
#' 
#' @return An sf object of points of diversion within given spatial boundary and attributes
getPODUtah <- function(aoi, dist = 0){
  
  call <- "https://opendata.arcgis.com/api/v3/datasets/5d530e62e6ca42528dd13e0a453a3b73_0/downloads/data?format=shp&spatialRefId=26912&where=1%3D1"
  #download boundary 
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  sf::sf_use_s2(FALSE)
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  ut_wr <- sf::st_read(dsn = temp2) %>%
    sf::st_transform(4269)
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_wr <- ut_wr %>%    
    sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(park_right = ifelse(grepl("National Park", OWNER, ignore.case = T), "Park","Non-park"),
                  within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
    dplyr::select(-within)
  
  return(nearby_wr)
  
}
