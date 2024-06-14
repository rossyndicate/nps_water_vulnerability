#' Import daily WBM rasters
#' 
#' This function pulls data directly from the FUTURE THREDDS server for specified variable, years, and climate models at a daily timestep
#' and crops the imported rasters to an area of interest.
#' 
#' See 'scratch/explore_wbm_data_pull.Rmd' for examples
#' 
#' @param park The 4 digit national park code(s) for parks of interest
#' @param aoi An sf object to use as the crop extent
#' @param years A numeric vector of all years to pull daily data for
#' @param macas The name of the GCMs to pull data from (ideally pulled from `selected_gcms`)
#' @param wb_var The water balance variable to pull data for: ONE of runoff, soil_water, rain, accumswe, PET, deficit, AET
#' @param path The file path to save the .tif of all stacked rasters. Automatically creates a subfolder called 'wbm_daily_rasters'
#' 
#' @return A SpatRaster with a layer for each daily value of specified wbm variable
get_future_wbm <- function(park, aoi, years = 2023:2070, macas, wb_var, path = "data/park/") {
  # check that path to save files to exists
  if (!dir.exists(paste0(getwd(), "/", path, "/", park, "/wbm_daily_rasters/"))) {
    dir.create(file.path(paste0(
      getwd(), "/", path, "/", park, "/wbm_daily_rasters/"
    )), showWarnings = FALSE)
  }
  
  # create df of all variable, year, scenario combinations
  map_dat <- tidyr::crossing(years, wb_var, macas) %>%
    tidyr::separate_wider_delim(macas, delim = ".", names = c("maca", "rcp"))
  
  
  # transform aoi to crs of WBM data
  aoi <- st_transform(aoi, crs = "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  
  # download each raster file to temp directory
  rasters <- map(1:nrow(map_dat), function(x) {
    call <- paste0(
      "/vsicurl/",
      "http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/gcm/",
      map_dat$rcp[x],
      "/",
      map_dat$maca[x],
      "/V_1_5_",
      map_dat$years[x],
      "_",
      map_dat$maca[x],
      "_",
      map_dat$rcp[x],
      "_",
      map_dat$wb_var[x],
      ".nc4"
    )
    
    r <- terra::rast(call)
    
    # assign informative names to each raster
    names(r) <- paste(map_dat$maca[x], map_dat$years[x], names(r), sep = "_")
    
    return(r)
    
  })
  
  ## combine all to single stack
  final_stack <- terra::rast(rasters)
  
  ## saved CRS and extent from raw downloaded file, that info does not attached to files pulled directly from URL
  crs(final_stack) <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  
  ext(final_stack) <- c(-2060750, 2639250, -1915500, 1384500)
  
  # crop to aoi
  final_stack <- terra::crop(final_stack, aoi)
  
  # save final raster stack to file
  terra::writeRaster(
    final_stack,
    filename = paste0(
      getwd(),
      "/",
      path,
      "/",
      park,
      "/wbm_daily_rasters/",
      park,
      "_",
      wb_var,
      "_selected_cfs_",
      years[1],
      "_",
      years[length(years)],
      ".tif"
    ),
    overwrite = TRUE
  )
  
  return(final_stack)
  
}