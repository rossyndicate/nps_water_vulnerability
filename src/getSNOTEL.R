#' Pull weather station data for parks
#' 
#' This function imports SNOTEL and SCAN site data
#' 
#' @param aoi An sf polygon/multipolygon object, preferably a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units as the aoi object) to buffer around park boundary
#' @param years The years to pull data for. Default just pulls current year, but can be a string of years 
#' @param timeseries either 'Daily' or 'Hourly
#' @param save Whether to save the resulting file (TRUE/FALSE)
#' @param path If `save == TRUE`, the file path to save to
#' @return An sf object of weather stations within given spatial boundary and attributes
getSNOTEL <-
  function(aoi,
           dist = 0,
           years = substr(Sys.Date(), 1, 4),
           timeseries = "Daily",
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
      sf::st_join(aoi) %>%
      dplyr::select(Name,
                    Site,
                    State,
                    County,
                    Network,
                    Elevation_ft,
                    HUC,
                    #UNIT_CODE, # Only for Park Units
                    geometry)
    # add option to filter stations/vars that are logged up to a certain year?
    
    #download data for each site
    siteID <- unique(site_aoi$Site)
    
    dat <- vector("list", length(siteID))
    
    for (i in 1:length(siteID)) {
      dat[[i]] <- soilDB::fetchSCAN(site.code = siteID[i], 
                                    year = years,
                                    timeseries = "Daily") %>%
        bind_rows() %>%
        as_tibble() %>%
        #remove metadata columns
        dplyr::select(-(Name:pedlabsampnum))
    }
    
    
    final_data <- bind_rows(dat) %>%
      left_join(site_aoi, by = "Site") %>% #tie back to site metadata
      st_as_sf()
    
    
    if (save == TRUE) {
      st_write(final_data, paste0(path, "/SNOTEL.shp"), append = FALSE)
      
    }
    
    return(final_data)
    
  }

