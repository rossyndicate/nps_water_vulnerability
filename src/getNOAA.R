#' Pull weather station data for parks
#' 
#' This function imports NOAA weather station data
#' 
#' @param aoi An sf polygon/multipolygon object, preferably a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param startDate minimum date for data retrieval. Either 'first' (default), which calculated the minimum year for sites of interest,
#' or specify a minimum year in "YYYY-MM-DD" format
#' @param endDate maximum date for data retrieval
#' @param noaa_key Your API NOAA key retrieved (for free) from: https://www.ncdc.noaa.gov/cdo-web/token 
#' @param save Whether to save the resulting file (TRUE/FALSE)
#' @param path If `save == TRUE`, the file path to save to
#' @return An sf object of weather stations within given spatial boundary and attributes
getNOAA <-
  function(aoi,
           dist = 0,
           startDate = "first",
           endDate = Sys.Date(),
           noaa_key,
           save = FALSE,
           path = NULL) {
    
    
    if (is.null(noaa_key)) {
      print("Must supply your NOAA key")
      stop()
    }
    

    options(noaakey = noaa_key)
    
    sf::sf_use_s2(FALSE)
    
    
    #pull all stations and make spatial
    all <- rnoaa::ghcnd_stations() %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    
    #filter to park(s)
    station_aoi <- all %>%
      sf::st_transform(st_crs(aoi)) %>%
      sf::st_filter(aoi) %>%
      dplyr::distinct(id, .keep_all = TRUE) %>%
      sf::st_join(aoi)
    
    
    # if startDate = 'first', find min year in the station metadata
    if (startDate == 'first'){
      startDate <- paste(min(station_aoi$first_year), "01", "01", sep = "-")
    }
    
    #get weather data for all stations (depending on dates and how many stations this can take a while)
    weather_data <- rnoaa::meteo_pull_monitors(station_aoi$id, date_min = startDate,
                                                           date_max = endDate)
 
    
    #tie back to coordinates
    final_data <- weather_data %>%
      dplyr::left_join(station_aoi, by = "id") %>%
      sf::st_as_sf() #add argument on whether to return sf or tibble?

    
    if (save == TRUE) {
      sf::write_sf(final_data, path)
      
    }
    
    return(final_data)
    
  }
