#' Import and clean California Water Rights Spatial Data
#' 
#' This function imports geospatial data from the eWRIMS database (https://waterrightsmaps.waterboards.ca.gov/viewer/index.html?viewer=eWRIMS.eWRIMS_gvh#)
#' 
#' @param park The 4-digit unit code(s) for national parks of interest. Must be in ALL CAPS.
#' @param dist The distance (in the same units used in  `park`) to buffer around spatial object `park`
#' @param data_path The file path to the eWRIMS geodatabase
#' 
#' @return An sf object of points of diversion within given spatial boundary and attributes
geteWRIMS <- function(park, dist, data_path="data/all/eWRIMS_Data.gdb"){
  
  #ca_pod <- arcpullr::get_spatial_layer("https://gispublic.waterboards.ca.gov/arcgis/rest/services/Water_Rights/Points_of_Diversion/FeatureServer/0")
  
  # #RSelenium .... I CANNOT GET FILES TO DOWNLOAD IN ANY OTHER FOLDER EXVCEPT DOWLOADS AND I DONT KNOW WHY
  # url <- "https://gispublic.waterboards.ca.gov/portal/home/item.html?id=746870b4aa1f47579ca1bf5d6c9924a8"
  # 
  # file_path <- getwd() %>% str_replace_all("/", "\\\\\\\\")
  # 
  # dl <- RSelenium::makeFirefoxProfile(list("directory_upgrade" = TRUE,
  #                                          "download.default_directory" = paste0(file_path, "\\\\data\\\\all")))
  # 
  # remDr <- RSelenium::remoteDriver(port=669L,browser = c("firefox"), extraCapabilities = dl)
  # remDr$open()
  # remDr$navigate(url)
  # 
  # dropper <- remDr$findElement(using='xpath',value='/html/body/div[2]/div/div[2]/div/div[2]/div/main/div[1]/aside/div/button[6]')
  # dropper$clickElement()
  
  # add buffer around park boundary
  park <- getParkBoundary(park)
  
  aoi <- park %>%
    sf::st_buffer(dist = dist) 
  
  #read in spatial points of diversion
  ca_wr <- sf::st_read(data_path) %>%
    sf::st_transform(crs=4269)
  
  #read in POD attribute table
  table <- sf::st_read(data_path, layer = "POD_Attributes")
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_wr <- ca_wr %>%
    dplyr::select(POD_ID,LATITUDE,LONGITUDE,DIVERSION_SITE_NAME,SOURCE_NAME) %>%
    dplyr::inner_join(., table, by=c("POD_ID"="CORE_POD_ID")) %>%
    sf::st_intersection(., dplyr::select(aoi,UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(park,UNIT_CODE),left=TRUE) %>%
    dplyr::mutate(RELATIONSHIP=ifelse(grepl("National Park", LAST_NAME, ignore.case=T), "PARK","NONPARK"),
                  WITHIN_PARK_BOUNDARY=ifelse(UNIT_CODE.x==UNIT_CODE.y,"WITHIN","OUTSIDE"))
  
  return(nearby_wr)
  
}


