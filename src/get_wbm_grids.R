#' Import daily WBM rasters
#' 
#' This function pulls data directly from the FUTURE and Historical THREDDS servers for a specified variable, years, and climate models at a daily timestep
#' and crops the imported rasters to an area of interest.
#' 
#' See 'scratch/explore_wbm_data_pull.Rmd' for examples
#' 
#' @param park The 4 digit national park code(s) for parks of interest
#' @param aoi An sf object to use as the crop extent
#' @param aoi_name A character string used to name the file, often a description of the AOI pulled
#' @param future_years A numeric vector of all future years to pull daily data for
#' @param historic_years A numeric vector of all historic years to pull daily data for
#' @param macas The name of the GCMs to pull future data from (ideally pulled from `select_GCMs()`)
#' @param wb_var The water balance variable to pull data for: ONE of runoff, soil_water, rain, accumswe, PET, deficit, AET
#' @param path The file path to save the .tif of all stacked rasters. Automatically creates a subfolder called 'wbm_daily_rasters'
#' 
#' @return A SpatRaster with a layer for each daily value of specified wbm variable
get_wbm_grids <- function(park, 
                          aoi,
                          aoi_name = "AOI",
                          future_years = 2024:2070, 
                          historic_years = 1980:2023,
                          macas, #= selected_GCMs[c(1,4),]$GCM, 
                          wb_var, #= "runoff", 
                          path = "data/park/") {
  start <- Sys.time()
  # check that path to save files to exists
  if (!dir.exists(paste0(getwd(), "/", path, "/", park))) {
    dir.create(file.path(paste0(
      getwd(), "/", path, "/", park
    )), showWarnings = FALSE)
  }
  if (!dir.exists(paste0(getwd(), "/", path, "/", park, "/wbm_daily_rasters/"))) {
    dir.create(file.path(paste0(
      getwd(), "/", path, "/", park, "/wbm_daily_rasters/"
    )), showWarnings = FALSE)
  }
  
  # create df of all variable, year, scenario combinations
  map_dat <- tidyr::crossing(future_years, wb_var, macas) %>%
    tidyr::separate_wider_delim(macas, delim = ".", names = c("maca", "rcp"))
  
  
  # transform aoi to crs of WBM data
  aoi <- st_transform(aoi, crs = "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  
  # download each raster file to temp directory
  future_rasters <- map(1:nrow(map_dat), function(x) {
    call <- paste0(
      "/vsicurl/",
      "http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/gcm/",
      map_dat$rcp[1],
      "/",
      map_dat$maca[1],
      "/V_1_5_",
      map_dat$future_years[1],
      "_",
      map_dat$maca[1],
      "_",
      map_dat$rcp[1],
      "_",
      map_dat$wb_var[1],
      ".nc4"
    )
    
    r <- terra::rast(call)
    
    # assign informative names to each raster
    names(r) <- paste(map_dat$maca[x], wb_var, as.Date(paste0(map_dat$future_years[x], "-", sub(".*_", "", names(r))), format = "%Y-%j"), sep = "_")
    
    return(r)
    
  })
  
  ## combine all to single stack
  final_future_stack <- terra::rast(future_rasters)
  
  ## saved CRS and extent from raw downloaded file, that info does not attach to files pulled directly from URL
  crs(final_future_stack) <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  
  ext(final_future_stack) <- c(-2060750, 2639250, -1915500, 1384500)
  
  # crop to aoi
  final_future_stack <- terra::crop(final_future_stack, aoi)
  
  # save final raster stack to file
  terra::writeRaster(
    final_future_stack,
    filename = paste0(
      getwd(),
      "/",
      path,
      "/",
      park,
      "/wbm_daily_rasters/",
      park,
      "_",
      aoi_name,
      "_",
      wb_var,
      "_selected_cfs_",
      future_years[1],
      "_",
      future_years[length(future_years)],
      ".tif"
    ),
    overwrite = TRUE
  )
  
  # create df of all variable, year, scenario combinations
  map_dat <- tidyr::crossing(historic_years, wb_var)
  
  # transform aoi to crs of WBM data
  aoi <- st_transform(aoi, crs = "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  
  # download each raster file to temp directory
  historic_rasters <- map(1:nrow(map_dat), function(x) {
    call <- paste0(
      "/vsicurl/",
      "http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/v2_historical/gridmet_v_1_5_historical/V_1_5_",
      map_dat$historic_years[x],
      "_gridmet_historical_",
      map_dat$wb_var[x],
      ".nc4"
    )
    
    r <- terra::rast(call)
    
    # assign informative names to each raster
    # assign informative names to each raster
    names(r) <- paste(wb_var, as.Date(paste0(map_dat$historic_years[x], "-", sub(".*_", "", names(r))), format = "%Y-%j"), sep = "_")
    
    
    return(r)
    
  })
  
  ## combine all to single stack
  final_historic_stack <- terra::rast(historic_rasters)
  
  ## saved CRS and extent from raw downloaded file, that info does not attach to files pulled directly from URL
  crs(final_historic_stack) <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  
  ext(final_historic_stack) <- c(-2060750, 2639250, -1915500, 1384500)
  
  # crop to aoi
  final_historic_stack <- terra::crop(final_historic_stack, aoi)
  
  # save final raster stack to file
  terra::writeRaster(
    final_historic_stack,
    filename = paste0(
      getwd(),
      "/",
      path,
      "/",
      park,
      "/wbm_daily_rasters/",
      park,
      "_",
      aoi_name,
      "_",
      wb_var,
      "_historic_gridmet_",
      historic_years[1],
      "_",
      historic_years[length(historic_years)],
      ".tif"
    ),
    overwrite = TRUE
  )
  
  end <- Sys.time()
  
  end - start
  
  return(list("future" = final_future_stack, "historic" = final_historic_stack))
  
}

