#' @param sf A shapefile of your area(s) of interest for pulling in GridMET data. Can be either point of polygon AOIs.
#' @param col_name The column identifying the name of each area of interest.
#' @param start Start date for pulling in data. Default is "1979-01-01".
#' @param end End data for pulling in data. Default is "2023-12-31".
#' @param vars Vector of variables you would like to pull data for. Options include: "tmmx", "tmmn", "pr", "rmax", "rmin", "pet", "etr", "vpd", "vs". Default is "tmmx", "tmmn", "pr", and "pet"
#' @return a data frame of requested data for each area of interest.
#'
get_climate_historic <- function(sf,
                                 col_name,
                                 start = "1979-01-01",
                                 end = "2023-12-31",
                                 vars = c("tmmx", "tmmn", "pr")) {
  
  sf <- sf %>%
    dplyr::rename("join_index" = {{col_name}})
  
  all_climate_data <- vector("list", length = nrow(sf))
  
if(any(unique(sf::st_geometry_type(sf)) %in% c("POLYGON", "MULTIPOLYGON"))){
  
  for (i in 1:nrow(sf)) {
    
    aoi <- sf[i,]
    
    print(paste0('Downloading GridMET for ', aoi$join_index, "."))
    
    clim <- climateR::getGridMET(AOI = aoi,
                                 varname = vars,
                                 startDate = start,
                                 endDate = end)
    
    
    if(is.list(clim) | inherits(clim[[1]], "SpatRaster")){

    
    clim_crs <- crs(clim[[1]])
    
    if(st_crs(clim[[1]]) != st_crs(sf)){
      
      clim <- clim %>%
        purrr::map(
          # getGridMET defaults AOI to bbox - so crop / mask results to sf extent
          ~terra::crop(., st_transform(aoi, crs = clim_crs), mask = TRUE),
          crs = clim_crs)
    } else {
      
      clim <- clim %>%
        purrr::map(
          # getGridMET defaults AOI to bbox - so crop / mask results to sf extent
          ~terra::crop(., aoi, mask = TRUE),
          crs = clim_crs)
      
    }
    
    all_climate_data[[i]] <- clim %>%
      purrr::map_dfr(~ as.data.frame(., xy = TRUE)) %>%
      data.table() %>%
      pivot_longer(-(x:y),
                   names_to = "var_temp",
                   values_to = "val") %>%
      separate_wider_delim(var_temp, "_", names = c("var", "date")) %>%
      drop_na(val) %>%
      group_by(x, y, date) %>%
      pivot_wider(names_from = "var", values_from = "val") %>%
      dplyr::mutate(date = as.Date(date),
                    ppt_mm = pr,
                    tmax_C = tmmx - 273.15,
                    tmin_C = tmmn - 273.15,
                    tmean_C = (tmax_C + tmin_C)/2,
                    join_index = aoi$join_index) %>%
      dplyr::select(-c("tmmx", "tmmn", "pr"))
    
    } else {
      
      all_climate_data[[i]] <- clim %>%
        data.table() %>%
        # since polygon grabbed a single grid, gridMET does not provide the coordinates
        # of the gridMET cell, so we fill in x and y with the coordinates
        # of the sf object:
        dplyr::mutate(x = sf::st_coordinates(aoi)[[1]],
                      y = sf::st_coordinates(aoi)[[2]]) %>%
        # Then do all other cleaning steps done for polygon sf objects:
        dplyr::mutate(date = as.Date(date),
                      ppt_mm = pr,
                      tmax_C = tmmx - 273.15,
                      tmin_C = tmmn - 273.15,
                      tmean_C = (tmax_C + tmin_C)/2,
                      join_index = aoi$join_index) %>%
        dplyr::select(-c("tmmx", "tmmn", "pr"))
      
    }
  }

  all_climate_data <- all_climate_data %>%
    bind_rows()
  
  # Rename the join_index column
  colnames(all_climate_data)[colnames(all_climate_data) == "join_index"] <- {{col_name}}
  
  return(all_climate_data)
  
} else if(unique(sf::st_geometry_type(sf)) == "POINT"){
  
  for (i in 1:nrow(sf)) {
    
    aoi <- sf[i,]
    
    print(paste0('Downloading GridMET for ', aoi$join_index, "."))
    
    clim <- climateR::getGridMET(AOI = aoi,
                                 varname = vars,
                                 startDate = start,
                                 endDate = end)
    
    all_climate_data[[i]] <- clim %>%
      data.table() %>%
      # since point pulls from gridMET do not provide the coordinates
      # of the gridMET cell, we fill in x and y with the coordinates
      # of the sf object:
      dplyr::mutate(x = sf::st_coordinates(aoi)[[1]],
                    y = sf::st_coordinates(aoi)[[2]]) %>%
      # Then do all other cleaning steps done for polygon sf objects:
      dplyr::mutate(date = as.Date(date),
                    ppt_mm = pr,
                    tmax_C = tmmx - 273.15,
                    tmin_C = tmmn - 273.15,
                    tmean_C = (tmax_C + tmin_C)/2,
                    join_index = aoi$join_index) %>%
      dplyr::select(-c("tmmx", "tmmn", "pr"))
    
  }
  
  all_climate_data <- all_climate_data %>%
    bind_rows()
  
  # Rename the join_index column
  colnames(all_climate_data)[colnames(all_climate_data) == "join_index"] <- {{col_name}}
  
  return(all_climate_data)

} else {stop("Your sf feature is neither a polygon nor point feature, or it needs to be made valid.")}
  
}

# start = Sys.time()
# # Get climate data (precipitation, max and min temp, PET)
# clim_all <- get_climate(sf = nldi_watershed)
# end = Sys.time()
# end-start
#
# gridMET_pts <- unique(clim_all[,c("x","y","join_index")]) %>%
#   ungroup() %>%
#   dplyr::mutate(pt_num = row_number()) %>%
#   st_as_sf(., coords = c("x", "y"),
#            crs = 4326,
#            remove = FALSE)
#
# clim_all <- clim_all %>%
#   left_join(., st_drop_geometry(gridMET_pts), by = c("x","y","join_index"))


