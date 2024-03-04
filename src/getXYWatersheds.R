
getXYWatersheds <- function(sf = NULL, coordinates = c(-112.2124, 37.63281)){
  
  sf::sf_use_s2(FALSE)
  
  if(is.null(sf)){
    
    # Create a data frame with a column named 'geometry'
    df <- tibble(long = coordinates[1],
                 lat = coordinates[2])
    
    aoi <- sf::st_as_sf(df, coords = c("long", "lat"), crs = 4326)
  }
  
  if(is.null(coordinates)){
    
    aoi <- sf %>% sf::st_transform(4326)

  }
    
    aoi <- aoi %>%
      nhdplusTools::get_nhdplus(AOI = .)
  
  # Use the NHDPlus digital elevation model to find the nearest downslope  
  # NHD flowline for any point in space (here, our point of interest)
  # Not working anymore -
  # trace <- get_raindrop_trace(aoi, direction = "up") 
  #
  # "Snap" our site to the nearest NHD flowline feature
  # snap_point <- sf::st_sfc(sf::st_point(trace$intersection_point[[1]][1:2]),
  #                          crs=4326)
  #
  # Clip/split our catchment to only include the portion of the
  # catchment upstream of our site:
  # better_termination <- get_split_catchment(snap_point, upstream = F)[2,]
  
  # read in the complete NHD (in tabular form) to make for much more efficient nhd crawling. 
  # This data in tabular form doesn't exist anywhere online that I know of... -_-
  nhd <- read_csv('data/all/nhd_flow_network.csv') 
  
  upstream <- nhdplusTools::get_UT(nhd, aoi$comid) %>% #upstream trace function in nhdplusTools
    tibble::as_tibble() %>%
    dplyr::rename(comid_list = value)  %>%
    distinct(comid_list, .keep_all = TRUE)
  
  nhd_catch <-  upstream$comid_list %>%
    map(~nhdplusTools::get_nhdplus(comid = .,
                                   realization='catchment',
                                   t_srs = 4269)) %>%
    bind_rows() %>%
    distinct(featureid,.keep_all=TRUE) %>%
    group_by(featureid) %>%
    summarize()
  
  return(nhd_catch)

  }


