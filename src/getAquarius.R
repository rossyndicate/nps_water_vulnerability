#' Inventory of Aquarius dcontinuous data associated with any given park unit
#' 
#' This function imports all available continuous data from the NPS Aquarius Data Portal (https://irma.nps.gov/aqwebportal)
#' This function is incredibly bulky and unsophisticated! Uses Rselenius so java and Firefox need to be installed on 
#' local computer.
#' 
#' @param park The 4 digit national park code(s) for parks of interest
#' 
#' @return A dataset containing tidied Aquarius data
getAquarius <- function(park, path = "data/all/", system = "Mac"){
  
  #park = "SHEN"
  #path = "data/all/aquarius/"
  #system = "Mac"
  
full_data <- vector("list", length = length(park))
  
  for(i in 1:length(park)){
    
    dir.create(file.path(getwd(), paste0("/", path, "/", park[i], "/aquarius/")), showWarnings = FALSE)
    
    # Create the resting place for each park's Aquarius data:
    dir.create(file.path(getwd(), paste0("/", path, "/", park[i], "/aquarius/temp/")), showWarnings = FALSE)
    
    # create a temporary directory for all raw datafiles to be sent to
    dir.create(file.path(getwd(), paste0("/", path, "/", park[i], "/aquarius", "/temp/temp/")), showWarnings = FALSE)
    dir.create(file.path(getwd(), paste0("/", path, "/", park[i], "/aquarius", "/temp/raw_yuck/")), showWarnings = FALSE)
    dir.create(file.path(getwd(), paste0("/", path, "/", park[i], "/aquarius", "/temp/coords/")), showWarnings = FALSE)
    
    downloadPath <- file.path(getwd(), paste0("/", path, "/", park[i], "/aquarius", "/temp/temp"))
    
    if(system == "PC") {downloadPath <- downloadPath %>% stringr::str_replace_all("/", "\\\\\\\\")}
    
    # make sure server downloads data into appropriate folder (i.e., our temporary folder)
    fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = downloadPath,
                                                browser.download.folderList = 2L,
                                                browser.download.manager.showWhenStarting = FALSE,
                                                browser.helperApps.neverAsk.openFile = "text/csv",
                                                browser.helperApps.neverAsk.saveToDisk = "text/csv"))
    
    exCap <- list(firefox_profile = fprof$firefox_profile, 
                  # REMOVE  "Pop-up" CONSOLE:
                  "moz:firefoxOptions" = list(args = list('--headless')))
    
    # start up RSelenium session:
    rD <- RSelenium::rsDriver(browser = "firefox", port = sample.int(1000, 1), extraCapabilities = exCap)
    remDr <- rD$client
    
    # get list of all Aquarius sites from the portal
    remDr$navigate('https://irma.nps.gov/aqwebportal/Data/List/Parameter/NoParameter/Location/DataSetsNo/Interval/Latest')
    Sys.sleep(3) # provide time for site navigation
    remDr$screenshot(display = TRUE)
    
    dropper <- remDr$findElement(using = 'xpath', value = '/html/body/section/section/section/div[3]/div[2]/div[1]/div[2]/div[2]/div[6]/div/button')
    Sys.sleep(3) # provide time for navigation and selection
    dropper$clickElement()
    Sys.sleep(3)
    dropper <- remDr$findElement(using = 'xpath', value = '/html/body/section/section/section/div[3]/div[2]/div[1]/div[2]/div[2]/div[6]/div/ul/li[2]/a')
    Sys.sleep(3) # provide time for navigation and selection
    dropper$clickElement()
    Sys.sleep(3) # provide  time for download to start
    
    sites <- read.csv(list.files(path = downloadPath, pattern = "*.csv", full.names = TRUE)[1], skip = 1)
    
    # select list of sites associated with our park(s) of interest
    sites <- sites %>% dplyr::filter(grepl(paste0(park[i]), Identifier)) %>%
      mutate(sites=paste0('https://irma.nps.gov/aqwebportal/Data/Location/Summary/Location/', Identifier))
    locations <- sites$sites
    
    print(paste0(n_distinct(locations), ' Aquarius sites associated with ', park[i]))
    
    # Create empty tibble for storing for loop data into if not sites exist for a park. 
    if(nrow(sites) == 0){
      
      full_data[[i]] <- tibble(c("timestamp","siteID","parameter","val","units","UNIT_CODE","Long","Lat","EPSG","Location.Type"),
                               c( NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
      
      print(paste0(park, " does not have any Aquarius data."))
    }
    
    if(nrow(sites) != 0) {
      
      # function to download all data from each site's individual webpage on the data portal
      web_runner <- function(locations){
        
        url <- sites %>%
          dplyr::filter(sites == locations)
        
        remDr$navigate(url$sites)
        
        Sys.sleep(5) # provide time for navigation 
        
        remDr$screenshot(display = TRUE) # shows webpage for each site
        
        try(dropper <- remDr$findElement(using = 'xpath', value = '/html/body/section/section/section/div[3]/div[4]/div[1]/div[6]/div/div[1]/div[1]/div[1]/div/div[2]/div/div[18]/div/a[3]'),
            silent=T)  
        Sys.sleep(4)
        try(dropper$clickElement())
        Sys.sleep(4)
        try(dropper <- remDr$findElement(using = 'xpath', value = 'html/body/section/section/section/div[3]/div[4]/div[1]/div[6]/div/div[1]/div[1]/div/div/div[2]/div/div[18]/div/a[4]'),
            silent=T)
        Sys.sleep(4)
        try(dropper$clickElement())
        Sys.sleep(4) # provide ample time for download to configure. totally dependent on web connection
                     # I need to find code that waits until you know the data has finished downloading. 
        
        # Site coordinates... not listed in data table, so pull from site's meta-data table on webpage
        try(coords <- remDr$findElement(using = 'xpath', value = '/html/body/section/section/section/div[3]/div[4]/div[1]/div[6]/div/div[1]/div[1]/div/div/div[2]/div/div[4]/div[2]'))
        Sys.sleep(3)
        try(
          coord <- coords$getElementText()[[1]] %>%
            as_tibble() %>%
            tidyr::separate(., value, into=c('Long', 'Lat'), sep = c(",")) %>%
            tidyr::separate(., Lat, c("non", "Lat", "EPSG", "P2"), sep = c(" ")) %>%
            dplyr::mutate(EPSG = paste0(EPSG, P2)) %>%
            dplyr::select(-c(P2, non)) %>%
            mutate(site = url$Identifier) %>%
            write_csv(paste0(path, "/", park[i], "/aquarius", "/temp/coords/", url$Identifier, '.csv')),
          silent=TRUE)
        
      }
      
      purrr::map(locations,
                 possibly(web_runner, otherwise = 1 + 1))
      
      # function to download all data from each site's individual webpage on the data portal... the slooooow way.
      web_runner <- function(locations){
        
        url <- sites %>%
          dplyr::filter(sites == locations)
        
        remDr$navigate(url$sites)
        
        Sys.sleep(5) # provide time for navigation 
        
        remDr$screenshot(display = TRUE) # shows webpage for each site
        
        try(dropper <- remDr$findElement(using = 'xpath', value = '/html/body/section/section/section/div[3]/div[4]/div[1]/div[6]/div/div[1]/div[1]/div[1]/div/div[2]/div/div[18]/div/a[3]'),
            silent = T)  
        Sys.sleep(3)
        try(dropper$clickElement())
        Sys.sleep(60)
        try(dropper <- remDr$findElement(using = 'xpath', value = 'html/body/section/section/section/div[3]/div[4]/div[1]/div[6]/div/div[1]/div[1]/div/div/div[2]/div/div[18]/div/a[4]'),
            silent=T)
        Sys.sleep(p3)
        try(dropper$clickElement())
        Sys.sleep(60) # provide ample time for download to configure
        
        # Site coordinates
        try(coords <- remDr$findElement(using = 'xpath', value = '/html/body/section/section/section/div[3]/div[4]/div[1]/div[6]/div/div[1]/div[1]/div/div/div[2]/div/div[4]/div[2]'))
        Sys.sleep(3)
        try(
          coord <- coords$getElementText()[[1]] %>%
            as_tibble() %>%
            tidyr::separate(., value, into = c('Long', 'Lat'), sep = c(",")) %>%
            tidyr::separate(., Lat, c("non", "Lat", "EPSG", "P2"), sep = c(" ")) %>%
            dplyr::mutate(EPSG = paste0(EPSG, P2)) %>%
            dplyr::select(-c(P2, non)) %>%
            mutate(site = url$Identifier) %>%
            write_csv(paste0(path, "/", park[i], "/aquarius", '/temp/coords/', url$Identifier, '.csv')),
          silent=TRUE)
        
      }
      
      # buy more time if they haven't all been fully downloaded
      slow_locy = sub(paste0("-",lubridate::year(Sys.Date()),".*"), "",
                      sub(".*LocationExport-", "",
                          list.files(path = downloadPath, full.names = TRUE))) %>%
        subset(grepl(park[i],. ))
      
      slow_locy = filter(sites, !Identifier %in% slow_locy)
      
      purrr::map(slow_locy$sites,
                 possibly(web_runner, otherwise = 1 + 1))
      
      Sys.sleep(10) # ensure all site data has been downloaded... 
      
      # Which sites didn't seem to work?
      if(n_distinct(list.files(path = downloadPath, pattern = "*.zip", full.names = TRUE)) < n_distinct(locations)) {print(paste0('Unable to download the following sites: ', slow_locy$Identifier))}
      
      # end the RSelenium server session
      remDr$close()
      rD[["server"]]$stop()
      
      temp <- list.files(path = paste0(downloadPath, "/"), pattern = "*.zip", full.names = TRUE)
      
      # unzip all your downloaded data files (one for every site)
      try(plyr::ldply(.data = temp, .fun = unzip, exdir = paste0(path, "/", park[i], "/aquarius","/temp/raw_yuck/")), silent = TRUE)
      
      try(coords <- plyr::ldply(list.files(path = paste0(path, "/", park[i], "/aquarius","/temp/coords/"), pattern = "*.csv", full.names = TRUE), read.csv)  %>%
            dplyr::mutate(EPSG=stringr::str_extract(string = EPSG,
                                                    pattern = "(?<=\\().*(?=\\))")), silent = TRUE)
      
      # delete the temporary folder
      unlink(file.path(downloadPath), recursive = T)
      
      
      # Tidy Aquarius data into one data frame
      try(aquarius_files <- list.files(path=paste0(path, "/", park[i], "/aquarius","/temp/raw_yuck/"), pattern = "*.csv", full.names = TRUE, recursive = TRUE) %>%
            tibble::as_tibble())
      
      aquarius <- function(aquarius_union) {
        
        tidier <- function(dataset){
          
          file <- aquarius_files %>%
            dplyr::filter(value == dataset)
          
          site_name <- stringr::str_sub(read.csv(paste0(file$value), header=F)[1,1],20)
          
          unit <- gsub("Value..","",names(read.csv(paste0(file$value), skip = 1) %>% select(3)))  
          
          read.csv(paste0(file$value), skip = 1) %>%
            dplyr::mutate(parm_site = site_name) %>%
            tidyr::separate(parm_site, c("parameter","siteID"), "@") %>%
            dplyr::rename(timestamp = 1,
                          val = 3) %>%
            dplyr::mutate(timestamp = as.character(timestamp),
                          units = unit)
        }
        
        ali <- purrr::map_dfr(aquarius_union, tidier)
        
      }
      
      full_data[[i]] <- aquarius_files %>%
        dplyr::mutate(my_pretty = map(value, aquarius)) %>%
        tidyr::unnest(cols=my_pretty) %>%
        dplyr::mutate(UNIT_CODE=substr(siteID, 1, 4)) %>%
        dplyr::select(c(timestamp, siteID, parameter, val, units, UNIT_CODE)) %>%
        dplyr::left_join(.,coords, by = c("siteID" = "site")) %>%
        dplyr::left_join(.,dplyr::select(sites, c(Identifier, Location.Type)), by = c("siteID" = "Identifier"))
      
      write_csv(full_data, paste0(path, "/", park[i], "/aquarius", "/", park[i], "_aquarius_data.csv"))
      
      unlink(file.path(getwd(), paste0(path, "/", park[i], "/aquarius", "/temp/")), recursive = T)
      
      print(paste0(park[i], "'s Aquarius data downloaded."))
    }
  }
  
  final_data <- bind_rows(full_data)
  
  return(final_data)
  
}
