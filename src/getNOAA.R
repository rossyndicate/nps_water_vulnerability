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
           park,
           startDate = "first",
           endDate = Sys.Date(),
           #noaa_key,
           variables = "all",
           save = FALSE,
           path = NULL) {
    
    # if (is.null(noaa_key)) {
    #   print("Must supply your NOAA key")
    #   stop()
    # }
    
    
    #options(noaakey = noaa_key)
    
    sf::sf_use_s2(FALSE)
    
    
    #pull all stations and make spatial
    all <- rnoaa::ghcnd_stations() %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    
    #filter to aoi
    station_aoi <- all %>%
      sf::st_transform(st_crs(aoi)) %>%
      sf::st_filter(aoi) %>%
      dplyr::group_by(id, elevation, name, geometry, first_year) %>%
      dplyr::summarize() %>%
      sf::st_join(aoi) %>%
      distinct(.keep_all = TRUE)
    
    
    # if startDate = 'first', find min year in the station metadata
    if (startDate == 'first'){
      startDate <- paste(min(station_aoi$first_year), "01", "01", sep = "-")
    }
    
    station_aoi <- station_aoi %>%
      dplyr::group_by(id, elevation, name, geometry) %>%
      dplyr::summarize() %>%
      distinct(.keep_all = TRUE)
    
    #get weather data for all stations (depending on dates and how many stations this can take a while)
    weather_data <- rnoaa::meteo_pull_monitors(station_aoi$id, date_min = startDate,
                                               date_max = endDate,
                                               var = variables)
    
    
    #tie back to coordinates
    final_data <- weather_data %>%
      left_join(., station_aoi, by = "id") %>%
      sf::st_as_sf() #add argument on whether to return sf or tibble?
    
    if (save == TRUE) {
      
        # save park boundary in park folder
        sf::st_write(final_data,
                     paste0(path, "/", park, "_noaa.shp"),
                     append = FALSE
                     )
        
        
      }
      
      print(paste("NOAA data for specified dates saved to respective NPS park folder in:", paste0(getwd(), "/", path)))
    
    return(final_data)
    
  }
