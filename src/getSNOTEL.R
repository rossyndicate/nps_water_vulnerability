#' Pull weather station data for parks
#' 
#' This function imports SNOTEL and SCAN site data
#' 
#' @param aoi An sf polygon/multipolygon object, preferably a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units as the aoi object) to buffer around park boundary
#' @param years The years to pull data for. Default just pulls current year, but can be a string of years 
#' @param save Whether to save the resulting file (TRUE/FALSE)
#' @param path If `save == TRUE`, the file path to save to
#' @return An sf object of weather stations within given spatial boundary and attributes
getSNOTEL <-
  function(aoi,
           dist = 0,
           years = substr(Sys.Date(), 1, 4),
           save = FALSE,
           path = NULL) {
    sf::sf_use_s2(FALSE)
    
    # add buffer around area of interest
    aoi <- aoi %>%
      sf::st_buffer(dist = dist)
    
    #read in all stations and make spatial
    data('SCAN_SNOTEL_metadata', package = 'soilDB')
    
    sites <-
      st_as_sf(
        SCAN_SNOTEL_metadata,
        coords = c("Longitude", "Latitude"),
        crs = 4326
      )
    
    
    #filter to park
    site_aoi <- sites %>%
      st_transform(st_crs(aoi)) %>%
      st_filter(aoi) %>%
      distinct(Site, .keep_all = TRUE) %>%
      #st_join(aoi) %>%
      dplyr::select(Name,
                    Site,
                    State,
                    County,
                    Network,
                    Elevation_ft,
                    HUC,
                    #UNIT_CODE,
                    geometry)
    # add option to filter stations/vars that are logged up to a certain year?
    
    #download data for each site
    siteID <- unique(site_aoi$Site)
    
    dat <- vector("list", length(siteID))
    
    for (i in 1:length(siteID)) {
      dat[[i]] <- fetchSCAN(site.code = siteID[i], year = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"))#c("1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022")) %>%
        bind_rows() %>%
        as_tibble() %>%
        #remove metadata columns
        dplyr::select(-(Name:pedlabsampnum))
    }
    
      
    for(i in 1:nrow(site_aoi)){
      fetchSCAN(site.code = site_aoi$SiteID[i], year = c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"))#c("1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022")) %>%
      bind_rows() %>%
        as_tibble() %>%
        dplyr::select(-(Name:pedlabsampnum)) %>%
        write_csv(paste0('data/all/snotel/', site_aoi$SiteID[i], '.csv'))}
    
    list.files(path = "data/all/snotel", pattern = "*.csv", full.names = TRUE) %>%
      plyr::ldply(., .fun = read.csv) %>%
      left_join(site_aoi, by = "Site") %>% #tie back to site metadata
      st_as_sf()
    
    
    
    final_data <- bind_rows(dat) %>%
      left_join(site_aoi, by = "Site") %>% #tie back to site metadata
      st_as_sf()
    
    
    if (save == TRUE) {
      write_sf(final_data, path)
      
    }
    
    return(final_data)
    
  }

