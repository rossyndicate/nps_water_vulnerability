#' This function pulls geologic layers from the Macrostrat API that intersect a user-specified point (or points) of interest. 
#' 
#' @param points An `sf` object of points.
#' 
#' @return An `sf` object of Macrostrat (https://macrostrat.org/) geologic layers that intersect the point object(s). 

get_rockd <- function(points){

  #https://macrostrat.org/api/v2/geologic_units/map?lat=38.7806&lng=-109.5936&adjacents=true&format=geojson_bare
  # Transform points to 4326 if not already 4326, then 
  # grab the coordinates for later steps:
  if(st_crs(points)$epsg != 4326) {
    aoi <- points %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates() %>%
      dplyr::as_tibble() %>%
      dplyr::distinct(X, Y) %>%
      tibble::rowid_to_column()
  
    } else {
      # Grab the coordinates for later steps:
    aoi <- points %>%
      sf::st_coordinates() %>%
      dplyr::as_tibble() %>%
      dplyr::distinct(X, Y) %>%
      tibble::rowid_to_column()
  }
  
  sf_temp <- list()
  
  for(i in 1:nrow(aoi)) {
    # grab geologic layers that intersect the coordinates of the points of interest
    y <- RCurl::getURL(paste0("https://macrostrat.org/api/v2/geologic_units/map/?lat=",aoi$Y[i],
                              "&lng=",
                              aoi$X[i],
                              "&adjacents=true&format=geojson_bare"),#geologic_units/map?strat_name_id=2683&format=geojson_bare",
                       .opts = RCurl::curlOptions(ssl.verifypeer=FALSE, verbose=TRUE))
    
    sf_temp[[i]] <- geojsonsf::geojson_wkt(y) %>%
      dplyr::mutate(aoi$rowid[i])
  }
  
  sf_temp <- sf_temp %>% dplyr::bind_rows() %>%
    sf::st_as_sf(., wkt = 'geometry', crs = 4326)
  
  return(sf_temp)
  
}

