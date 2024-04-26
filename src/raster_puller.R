raster_puller <- function(data, point = NULL, aoi = NULL){
  
  # if trying to get just a point...
  if(is.null(aoi)){
    
    point <- point %>%
      dplyr::select(geometry) %>%
      sf::st_transform(4326)
    
    tab_data <- data %>% 
      dplyr::distinct() %>%
      sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(4326)) %>%
      tibble::as_tibble()
    
    # create unique identifier for every coordinate:
    ID_raster <- data %>%
      dplyr::select(x, y) %>%
      dplyr::distinct() %>%
      sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>%
      tibble::rowid_to_column()
    
    final <- point %>%
      # grab nearest raster centroid to point of interest's ID:
      dplyr::mutate(rowid = sf::st_nearest_feature(., ID_raster)) %>%
      # Using that id, grab the actual coordinates of that raster cell:
      dplyr::inner_join(., tibble::as_tibble(ID_raster) , by = "rowid") %>%
      # ... to use as a join for all the raster data:
      dplyr::inner_join(tab_data, by = c("geometry.y" = "geometry")) %>%
      # drop the geometry of the point
      sf::st_drop_geometry() %>%
      # remove the unnecessary raster ID:
      dplyr::select(-rowid) %>%
      # preserve the coordinates of the raster grid (NOT the point of interest's coordinates)
      dplyr::rename(geometry_grid = geometry.y)
    
  }
  
  # if trying to get an AOI ...
  # THIS DOESN'T REALLY WORK APPROPRIATELY, AS IT ONLY SELECTS GRIDS WHOSE CENTROIDS ARE WITHIN THE AOI
  if(is.null(point)){
    
    aoi <- aoi %>%
      sf::st_transform(4326)
    
    # grab all data within the AOI
    final <- data %>%
      dplyr::mutate(X = x, Y = y) %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
      #sf::st_intersection(aoi) #%>%
      .[aoi,] %>%
      sf::st_drop_geometry()
  }
  
  return(final)
  
}