getSprings <- function(park, aoi, system = "Mac", sites = TRUE){
  
if(sites == FALSE){

  dir.create(file.path(getwd(), paste0("/data/park/", park, "/ssi/")), showWarnings = FALSE)
  downloadPath <- file.path(getwd(), paste0("/data/park/", park, "/ssi/"))
    
  if(system == "PC") {downloadPath <- downloadPath %>% stringr::str_replace_all("/", "\\\\\\\\")}
  
    # make sure server downloads data into appropriate folder (i.e., our temporary folder)
    fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = downloadPath,
                                                browser.download.folderList = 2L,
                                                browser.download.manager.showWhenStarting = FALSE,
                                                browser.helperApps.neverAsk.openFile = "text/csv",
                                                browser.helperApps.neverAsk.saveToDisk = "text/csv"))
    
    exCap <- list(firefox_profile = fprof$firefox_profile)#, 
                  # REMOVE  "Pop-up" CONSOLE:
                  #"moz:firefoxOptions" = list(args = list('--headless')))
    
    # start up RSelenium session:
    rD <- RSelenium::rsDriver(browser = "firefox", port = sample.int(1000, 1), extraCapabilities = exCap)
    remDr <- rD$client

    # log in to Springs Online using username and password
    remDr$navigate('https://springsdata.org/profile/index.php?refurl=/index.php')
    Sys.sleep(3) # provide time for site navigation
    dropper <- remDr$findElement(using = 'xpath', value = '//*[@id="f_b284aff0-5395-4e01-8043-4b644f5b5026"]') # '//*[@id="login"]')
    Sys.sleep(3) # provide time for navigation and selection
    dropper$sendKeysToElement(list("krw21719", key = "enter"))
    Sys.sleep(3)
    dropper <- remDr$findElement(using = 'xpath', value = '//*[@id="f_93d50f4a-c1d5-4232-8b19-8b5a7bcd7f67"]') # '//*[@id="login"]')
    Sys.sleep(3) # provide time for navigation and selection
    dropper$sendKeysToElement(list("Fcs25739!", key = "enter"))
    Sys.sleep(3)
    dropper <- remDr$findElement(using = 'xpath', value = '/html/body/div[1]/div[3]/div/div/div[1]/div[2]/button')
    Sys.sleep(3) # provide time for navigation and selection
    dropper$clickElement()
    
    
    
    remDr$screenshot(display = TRUE)
    
    # go to site search
    remDr$navigate('https://springsdata.org/search/index.php')
    Sys.sleep(3)
    remDr$screenshot(display = TRUE)
    # select all sites in USA
    option <- remDr$findElement(using = 'xpath', '//*[@id="Country"]')
    Sys.sleep(2)
    option$clickElement()
    Sys.sleep(2)
    option <- remDr$findElement(using = 'xpath', "/html/body/div[1]/div[5]/form/div[7]/select/option[3]")
    Sys.sleep(2)
    option$clickElement()
    Sys.sleep(2)
    remDr$screenshot(display = TRUE)
    # click "search"
    search <- remDr$findElement(using = 'xpath', "/html/body/div[1]/div[5]/form/div[2]/div[1]/input")
    Sys.sleep(2)
    search$clickElement()
    Sys.sleep(5)
    Sys.sleep(5)
    Sys.sleep(5)
    Sys.sleep(5)
    
    #remDr$screenshot(display = TRUE)
  
    dropper <- remDr$findElement(using = 'xpath', value = '//*[@id="querydownloadselect"]')
    Sys.sleep(3) # provide time for navigation and selection
    dropper$clickElement()
    Sys.sleep(3) # provide  time for download to start
    dropper <- remDr$findElement(usin = 'xpath', value = "/html/body/div/div[4]/div[3]/div/div[4]/div[1]/div[1]/select/option[2]")
    Sys.sleep(2)
    dropper$clickElement()
    Sys.sleep(3) # provide  time for download to start
    dropper <- remDr$findElement(usin = 'xpath', value = "/html/body/div/div[4]/div[3]/div/div[4]/div[1]/div[2]/button")
    Sys.sleep(2)
    dropper$clickElement()
    Sys.sleep(15)
    
    # end the RSelenium server session
    remDr$close()
    rD[["server"]]$stop()
  
}
 
  transform_site_locations <- function(sites, crs_out = "NAD83"){
    
    message("Attempting to harmonize different site CRS...")
    
    # Check that crs_out is one of two allowable entries
    if(!crs_out %in% c("WGS84","NAD83"))
      stop("crs_out must be either 'WGS84' or 'NAD83'")
    
    # Define EPSG code for output data frame 
    epsg_out <- case_when(crs_out == "NAD83" ~ 4269,
                          crs_out == "WGS84" ~ 4326)
    
    # Transform sites into a consistent crs defined by epsg_out
    sites_transformed <- sites %>%
      # transform the sites data frame into a list where all sites within each 
      # list element share the same crs/datum
      rename(datum = Datum) %>%
      split(.,.$datum) %>%
      # Each list element contains sites with different crs. Transform the sites 
      # within each element to the desired crs
      lapply(., function(x){
        # assume unknown crs (datum equals "UNKNWN", "OTHER") correspond with WGS84
        epsg_in <- case_when(unique(x$datum) == "NAD83" ~ 4269,
                             unique(x$datum) == "NAD 83" ~ 4269,
                             unique(x$datum) == "WGS84" ~ 4326,
                             unique(x$datum) == "WGS 84" ~ 4326,
                             unique(x$datum) == "NAD27" ~ 4267,
                             unique(x$datum) == "NAD 27" ~ 4267,
                             unique(x$datum) == "UNKWN" ~ 4326,
                             unique(x$datum) == "OTHER" ~ 4326)
        
        # convert data frame to sf object and transform to desired crs
        if(!is.na(epsg_in)){
          sites_transformed <- x %>%
            sf::st_as_sf(coords=c("LongitudeDD", "LatitudeDD"), crs = epsg_in) %>%
            sf::st_transform(epsg_out) %>%
            mutate(lon_new = as.numeric(sf::st_coordinates(.)[,1]),
                   lat_new = as.numeric(sf::st_coordinates(.)[,2]),
                   datum_new = crs_out) %>%
            sf::st_drop_geometry() %>%
            select(-datum) %>%
            rename("LongitudeDD" = "lon_new", "LatitudeDD" = "lat_new", "datum" = "datum_new")
        } else {
          sites_transformed <- x
          message(sprintf("Unable to transform sites with datum = %s; returning sites untransformed",
                          unique(x$datum)))
        }
        return(sites_transformed)
      }) %>%
      # combine transformed sites into a single data frame
      bind_rows()
    
    return(sites_transformed)
    
  }
  
  
  
  nearby_springs <- read_csv(list.files(downloadPath, full.names = TRUE)) %>%
    transform_site_locations(sites = ., crs_out = "NAD83") %>%
  sf::st_as_sf(coords = c("LongitudeDD", "LatitudeDD"), crs = 4269) %>%
    st_intersection(., aoi)
  
  return(nearby_springs)
   
}
