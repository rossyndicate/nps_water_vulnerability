#' Inventory of Aquarius dcontinuous data associated with any given park unit
#' 
#' This function imports all available continuous data from the NPS Aquarius Data Portal (https://irma.nps.gov/aqwebportal)
#' This function is incredibly bulky and unsophisticated!
#' 
#' @param park The 4 digit national park code(s) for parks of interest
downloadAquarius <- function(park){
  
  # create a temporary directory for all raw datafiles to be sent to
  dir.create(file.path(getwd(),"data/aquarius/temp/"), showWarnings = FALSE)
  dir.create(file.path(getwd(),"data/aquarius/raw_yuck/"), showWarnings = FALSE)
  
  downloadPath <- file.path(getwd(), "data/aquarius/temp") %>% stringr::str_replace_all("/", "\\\\\\\\")
  
  # make sure server downloads data into appropriate folder (i.e., our temporary folder)
  fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = downloadPath,
                                   browser.download.folderList = 2L,
                                   browser.download.manager.showWhenStarting = FALSE,
                                   browser.helperApps.neverAsk.openFile = "text/csv",
                                   browser.helperApps.neverAsk.saveToDisk = "text/csv"))
  
  exCap <- list(firefox_profile = fprof$firefox_profile, 
                "moz:firefoxOptions" = list(args = list('--headless')))
  
  # start up RSelenium session:
  rD <- RSelenium::rsDriver(browser = "firefox", port = sample.int(1000, 1), extraCapabilities = exCap)
  remDr <- rD$client
  
  # get list of all Aquarius sites from the portal
  remDr$navigate('https://irma.nps.gov/aqwebportal/Data/List/Parameter/NoParameter/Location/DataSetsNo/Interval/Latest')
  Sys.sleep(5) # provide time for site navigation
  remDr$screenshot(display = TRUE)
  
  dropper <- remDr$findElement(using = 'xpath', value = '/html/body/section/section/section/div[3]/div[2]/div[1]/div[2]/div[2]/div[6]/div/button')
  Sys.sleep(3) # provide time for navigation and selection
  dropper$clickElement()
  Sys.sleep(1)
  dropper <- remDr$findElement(using = 'xpath', value = '/html/body/section/section/section/div[3]/div[2]/div[1]/div[2]/div[2]/div[6]/div/ul/li[2]/a')
  Sys.sleep(3) # provide time for navigation and selection
  dropper$clickElement()
  Sys.sleep(3) # provide  time for download to start
  
  sites <- read.csv(list.files(path = "data/aquarius/temp/", pattern = "*.csv", full.names = TRUE)[1], skip = 1)
  
  # select list of sites associated with our park(s) of interest
  sites <- sites %>% dplyr::filter(grepl(paste0(paste(park, collapse = "|")), Identifier))
  urls <- tibble::tibble(sites = paste0('https://irma.nps.gov/aqwebportal/Data/Location/Summary/Location/', sites$Identifier))
  locations <- urls$sites
  
  # function to download all data from each site's individual webpage on the data portal
  web_runner <- function(locations){
    
    url <- urls %>%
      dplyr::filter(sites == locations)
    
    remDr$navigate(url$sites)
    
    Sys.sleep(5) # provide time for navigation 
    
    remDr$screenshot(display = TRUE) # shows webpage for each site

    dropper <- remDr$findElement(using = 'xpath', value = '/html/body/section/section/section/div[3]/div[4]/div[1]/div[6]/div/div[1]/div[1]/div[1]/div/div[2]/div/div[18]/div/a[3]')  
    Sys.sleep(5)
    dropper$clickElement()
    Sys.sleep(3)
    dropper <- remDr$findElement(using = 'xpath', value = 'html/body/section/section/section/div[3]/div[4]/div[1]/div[6]/div/div[1]/div[1]/div/div/div[2]/div/div[18]/div/a[4]')
    Sys.sleep(5)
    dropper$clickElement()
    Sys.sleep(10) # provide ample time for download to configure
  }
  
  purrr::map(locations,
      possibly(web_runner, otherwise=1+1))
  
  Sys.sleep(10) # ensure all site data has been downloaded... 
  
  # buy more time if they haven't all been fully downloaded
  if(n_distinct(list.files(path = "data/aquarius/temp/", pattern = "*.zip", full.names = TRUE)) < n_distinct(locations)) {Sys.sleep(15)}
  
  temp <- list.files(path = "data/aquarius/temp/", pattern = "*.zip", full.names = TRUE)
  
  # unzip all your downloaded data files (one for every site)
  plyr::ldply(.data = temp, .fun = unzip, exdir = 'data/aquarius/raw_yuck/')
  
  # end the RSelenium server session
  remDr$close()
  rD[["server"]]$stop()
  
  # delete the temporary folder
  unlink(file.path(getwd(), "data/aquarius/temp/"), recursive = T)

}
