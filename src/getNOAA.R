#' Pull weather station data for parks
#' 
#' This function imports weather station data from NOAA, SNOTEL and PRISM databases
#' 
#' @param aoi An sf polygon/multipolygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units used by `getParkBoundary()`) to buffer around park boundary
#' @param startDate 
#' @param endDate
#' @param save Whether to save the resulting file (TRUE/FALSE)
#' @param path If `save == TRUE`, the file path to save to
#' @return An sf object of weather stations within given spatial boundary and attributes
getNOAA <- function(aoi, dist = 0, startDate = "2010-01-01", endDate = Sys.Date(), save = FALSE, path = NULL){
  
  # put these next 2 lines in a set up script eventually
  options(noaakey = "VAOckuKZRAFPIOuippCpMBAUPTiAtVMn")
  
  sf::sf_use_s2(FALSE)
  
  # add buffer around area of interest
  aoi <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  #pull all stations and make spatial
  all <- ghcnd_stations() %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


    #filter to park
    station_aoi <- all %>% 
      st_transform(st_crs(aoi)) %>% 
      st_filter(aoi) %>% 
      distinct(id, .keep_all = TRUE) %>% 
      st_join(aoi) %>% 
      dplyr::select(id, elevation, state, name, UNIT_CODE, geometry)
    # add option to filter stations/vars that are logged up to a certain year?
    
    
    #get weather data for all stations (depending on dates and how many stations this can take a while)
    weather_data <- meteo_pull_monitors(station_aoi$id, date_min = startDate,
                                        date_max = endDate) 
    
    #tie back to coordinates
    final_data <- weather_data %>% 
      left_join(station_aoi, by = "id") %>% 
      st_as_sf() #add argument on whether to return sf or tibble?
    
    if(save == TRUE){
      
      write_sf(final_data, path)
      
    }
    
    return(final_data)
    
}
  
