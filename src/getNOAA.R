#' Pull weather station data for parks
#' 
#' This function imports NOAA weather station data
#' 
#' @param aoi An sf polygon/multipolygon object, preferably a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units as the aoi object) to buffer around park boundary
#' @param startDate minimum date for data retrieval
#' @param endDate maximum date for data retrieval
#' @param save Whether to save the resulting file (TRUE/FALSE)
#' @param path If `save == TRUE`, the file path to save to
#' @return An sf object of weather stations within given spatial boundary and attributes
getNOAA <-
  function(aoi,
           dist = 0,
           startDate = "2010-01-01",
           endDate = Sys.Date(),
           save = FALSE,
           path = NULL) {
    # put these next 2 lines in a set up script eventually !!
    options(noaakey = "VAOckuKZRAFPIOuippCpMBAUPTiAtVMn")
    
    sf::sf_use_s2(FALSE)
    
    # add buffer around area of interest
    aoi <- aoi %>%
      sf::st_buffer(dist = dist)
    
    #pull all stations and make spatial
    all <- rnoaa::ghcnd_stations() %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    
    #filter to park(s)
    station_aoi <- all %>%
      sf::st_transform(st_crs(aoi)) %>%
      sf::st_filter(aoi) %>%
      dplyr::distinct(id, .keep_all = TRUE) %>%
      sf::st_join(aoi) %>%
      dplyr::select(id, elevation, state, name, geometry)
    
    
    #get weather data for all stations (depending on dates and how many stations this can take a while)
    weather_data <- rnoaa::meteo_pull_monitors(station_aoi$id, date_min = startDate,
                                                           date_max = endDate)
 
     # Katie code for saving all the data... for if/when it crashes :-(
    #   for(i in 1:nrow(station_aoi)){
    #   meteo_pull_monitors(station_aoi$id[i], date_min = "1979-10-01",
    #                       date_max = "2022-09-30") %>%
    #   write_csv(paste0('data/all/noaa/', station_aoi$id[i], '.csv'))}
    # weather_data <- list.files(path = "data/all/noaa", pattern = "*.csv", full.names = TRUE) %>%
    #   plyr::ldply(., .fun = read.csv) %>%
    #   distinct(.keep_all=TRUE)
    
    #tie back to coordinates
    final_data <- weather_data %>%
      dplyr::left_join(station_aoi, by = "id") %>%
      sf::st_as_sf() #add argument on whether to return sf or tibble?

    
    if (save == TRUE) {
      sf::write_sf(final_data, path)
      
    }
    
    return(final_data)
    
  }
