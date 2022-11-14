#' Import and clean California Water Rights Spatial Data
#' 
#' This function imports geospatial data from the eWRIMS database (https://waterrightsmaps.waterboards.ca.gov/viewer/index.html?viewer=eWRIMS.eWRIMS_gvh#)
#' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units used in  `park`) to buffer around spatial object `park`
#' 
#' @return An sf object of points of diversion within given spatial boundary and attributes
geteWRIMS <- function(aoi, dist){
  
  # create a temporary directory for all raw datafiles to be sent to
  dir.create(file.path(getwd(),"/temp_ewrims/"), showWarnings = FALSE)
  
  downloadPath <- file.path(getwd(),"/temp_ewrims/") %>% stringr::str_replace_all("/", "\\\\\\\\")
  
  # make sure server downloads data into appropriate folder (i.e., our temporary folder)
  fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = downloadPath,
                                              browser.download.folderList = 2L,
                                              browser.download.manager.showWhenStarting = FALSE,
                                              browser.helperApps.neverAsk.openFile = "text/csv/gdb/zip",
                                              browser.helperApps.neverAsk.saveToDisk = "text/csv/gdb/zip"))
  
  exCap <- list(firefox_profile = fprof$firefox_profile, 
                "moz:firefoxOptions" = list(args = list('--headless')))
  
  # start up RSelenium session:
  rD <- RSelenium::rsDriver(browser = "firefox", port = sample.int(1000, 1), extraCapabilities = exCap)
  remDr <- rD$client
  
  # get list of all Aquarius sites from the portal
  remDr$navigate('https://gispublic.waterboards.ca.gov/portal/home/item.html?id=746870b4aa1f47579ca1bf5d6c9924a8')
  Sys.sleep(5) # provide time for site navigation
  remDr$screenshot(display = TRUE)
  
  dropper <- remDr$findElement(using = 'xpath', value = '/html/body/div[2]/div/div[2]/div/div[2]/div/main/div[1]/aside/div/button[6]')
  Sys.sleep(3) # provide time for navigation and selection
  dropper$clickElement()
  Sys.sleep(15)
  # OLD? <- /html/body/div[3]/div/div[2]/div/div[2]/div/main/div[1]/aside/div/button[6]
  temp <- list.files(path = paste0(getwd(),"/temp_ewrims"), pattern = "*.zip", full.names = T)
  
  # unzip all your downloaded data files (one for every site)
  try(unzip(temp,exdir=paste0(getwd(),"/temp_ewrims/eWRIMS.gdb")))
  
  # add buffer around park boundary  
  aoi_buffer <- aoi %>%
    sf::st_buffer(dist = dist) 
  
  #read in spatial points of diversion
  try(ca_wr <- sf::st_read(paste0(getwd(),"/temp_ewrims/eWRIMS.gdb")) %>%
    sf::st_transform(crs=4269))
  
  #read in POD attribute table
  try(table <- sf::st_read(paste0(getwd(),"/temp_ewrims/eWRIMS.gdb"), layer = "POD_Attributes"))
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_wr <- ca_wr %>%
    dplyr::select(POD_ID,LATITUDE,LONGITUDE,DIVERSION_SITE_NAME,SOURCE_NAME) %>%
    dplyr::inner_join(., table, by=c("POD_ID"="CORE_POD_ID")) %>%
    sf::st_intersection(., dplyr::select(aoi_buffer)) %>%
    #sf::st_join(.,dplyr::select(aoi,within=UNIT_CODE),left=TRUE) %>%
    dplyr::mutate(park_right=ifelse(grepl("National Park", LAST_NAME, ignore.case=T), "Park","Non-park"))#,
                  #within_park=ifelse(UNIT_CODE==within,"Within","Outside")) %>%
    #select(-within)
  
  # end the RSelenium server session
  remDr$close()
  rD[["server"]]$stop()
  
  # delete the temporary folder
  unlink(file.path(getwd(), "/temp_ewrims/"), recursive = T)
  return(nearby_wr)
  
}


