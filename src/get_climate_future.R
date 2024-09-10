#' @param sf A shapefile of your area(s) of interest for pulling in MACA data. Can be either point of polygon AOIs.
#' @param col_name The column identifying the name of each area of interest.
#' @param start Start date for pulling in data. Default is "1979-01-01".
#' @param end End data for pulling in data. Default is "2023-12-31".
#' @param vars Vector of variables you would like to pull data for. Options include: "tmmx", "tmmn", "pr", "rmax", "rmin", "pet", "etr", "vpd", "vs". Default is "tmmx", "tmmn", "pr", and "pet"
#' @param GCM Vector of which climate models (GCMs) to pull.
#' @return a data frame of requested data for each area of interest.
#'
get_climate_future <- function(sf,
                                 col_name,
                                 start = "2023-01-01",
                                 end = "2070-01-01",
                                 vars = c("tasmax", "tasmin", "pr"),
                                 GCM = select_cfs$GCM) {
  
  
  #sf <- watersupply_watershed
  #col_name <- "name"
  #start <- "1979-01-01"
  #end <- "1980-01-01"
  #vars <- c("tasmax", "tasmin", "pr")
  #GCM <- select_cfs$GCM
  
  
  
  sf <- sf %>%
    dplyr::rename("join_index" = {{col_name}})
  
  all_climate_data <- vector("list", length = nrow(sf) * length(GCM))
  idx <- 0
  models <- str_extract(GCM, "^[^.]+")     # Get models to run
  scenarios <- str_extract(GCM, "(?<=\\.).*")     # Get scenarios (rcps) 
  
if(any(unique(sf::st_geometry_type(sf)) %in% c("POLYGON", "MULTIPOLYGON"))) {
  
  for (i in 1:nrow(sf)) {
      
    aoi <- sf[i,]
      
    for (j in 1:length(GCM)) {
      
      idx <- idx + 1
      
      print(paste0('Downloading MACA ', models[j], '-', scenarios[j], ' for ', aoi$join_index, "."))
        
      clim <- climateR::getMACA(AOI = aoi, 
                              varname = vars,#,"pet","vs"),
                              model = models[j],
                              scenario = scenarios[j],
                              startDate = start,
                              endDate = end) 
        
      # If the result is a spatraster (i.e., AOI spanned more than 1 grid cell)
      if(inherits(clim[[1]], "SpatRaster")) {
        
        clim_crs <- st_crs(clim[[1]])
    
        if(st_crs(clim[[1]]) != st_crs(sf)) {
          
          clim <- clim %>%
            purrr::map(
              # getGridMET defaults AOI to bbox - so crop / mask results to sf extent
              ~terra::crop(., st_transform(aoi, crs = clim_crs), mask = TRUE),
              crs = clim_crs)
          } else {
            
            clim <- clim %>%
              purrr::map(
                # defaults AOI to bbox - so crop / mask results to sf extent
                ~terra::crop(., aoi %>% 
                               st_transform(., crs = clim_crs), 
                             mask = TRUE),
                crs = clim_crs)
            
          }
    
          all_climate_data[[idx]] <- clim %>%
            purrr::map_dfr(~ as.data.frame(., xy = TRUE)) %>%
            data.table() %>%
            pivot_longer(-(x:y),
                         names_to = "var_temp",
                         values_to = "val") %>%
            separate_wider_delim(var_temp, "_", 
                                 names = c("var","date","GCM",
                                           "Ensemble","RCP")) %>%
            drop_na(val) %>%
            group_by(x, y, date) %>%
            pivot_wider(names_from = "var", 
                        values_from = "val") %>%
            dplyr::mutate(date = as.Date(date),
                   ppt_mm = pr,
                   tmax_C = tasmax - 273.15,
                   tmin_C = tasmin - 273.15,
                   tmean_C = (tmax_C + tmin_C)/2,
                   join_index = aoi$join_index) %>%
            dplyr::select(-c("tasmax","tasmin","pr"))
    
    } else {
      
      all_climate_data[[idx]] <- clim %>%
        data.table() %>%
        # names of columns include va_mode_rcp so must rename
        rename_with(~ str_split(.x, "_", n = 2) %>% map_chr(1)) %>%
        # since polygon grabbed a single grid, does not provide the coordinates
        # of the gridMET cell, so we fill in x and y with the coordinates
        # of the sf object:
        dplyr::mutate(x = sf::st_coordinates(aoi)[[1]],
                      y = sf::st_coordinates(aoi)[[2]]) %>%
        # Then do all other cleaning steps done for polygon sf objects:
        dplyr::mutate(date = as.Date(date),
                      ppt_mm = pr,
                      tmax_C = tasmax - 273.15,
                      tmin_C = tasmin - 273.15,
                      tmean_C = (tmax_C + tmin_C)/2,
                      join_index = aoi$join_index,
                      GCM = models[j],
                      RCP = scenarios[j],
                      Ensemble = "r1i1p1") %>%
        dplyr::select(-c("tasmax","tasmin","pr"))
      
     }
    }
  }
  
  all_climate_data <- all_climate_data %>%
    bind_rows()
  
  # Rename the join_index column
  colnames(all_climate_data)[colnames(all_climate_data) == "join_index"] <- {{col_name}}
  
  return(all_climate_data)
  
  
  # IF the input geometry is a point...
} else if(unique(sf::st_geometry_type(sf)) == "POINT") {
  
    for (i in 1:nrow(sf)) {
      
      aoi <- sf[i,]
      
      for (j in 1:length(GCM)) {
        
        idx <- idx + 1
        
        print(paste0('Downloading MACA for ', aoi$join_index, "."))
        
        clim <- climateR::getMACA(AOI = aoi, 
                                  varname = vars,#,"pet","vs"),
                                  model = models[j],
                                  scenario = scenarios[j],
                                  startDate = start,
                                  endDate = end) 
    
        all_climate_data[[idx]] <- clim %>%
          data.table() %>%
          # names of columns include va_mode_rcp so must rename
          rename_with(~ str_split(.x, "_", n = 2) %>% map_chr(1)) %>%
          # since point pulls from gridMET do not provide the coordinates
          # of the gridMET cell, we fill in x and y with the coordinates
          # of the sf object:
          dplyr::mutate(x = sf::st_coordinates(aoi)[[1]],
                        y = sf::st_coordinates(aoi)[[2]]) %>%
          # Then do all other cleaning steps done for polygon sf objects:
          dplyr::mutate(date = as.Date(date),
                        ppt_mm = pr,
                        tmax_C = tasmax - 273.15,
                        tmin_C = tasmin - 273.15,
                        tmean_C = (tmax_C + tmin_C)/2,
                        join_index = aoi$join_index,
                        GCM = models[j],
                        RCP = scenarios[j],
                        Ensemble = "r1i1p1") %>%
          dplyr::select(-c("tasmax","tasmin","pr"))
        
      }
    }
  
  all_climate_data <- all_climate_data %>%
    bind_rows()
  
  # Rename the join_index column
  colnames(all_climate_data)[colnames(all_climate_data) == "join_index"] <- {{col_name}}
  
  return(all_climate_data)

} else {
  stop("Your sf feature is neither a polygon nor point feature, or it needs to be made valid.")
}
  
}
  



