#' This function returns the 30-year statistic rasters (annual means) for the NPS gridded
#' water balance mode based on user input area of interest, WBM variable(s), year ranges and GCMs and
#' returns a raster stack of all .tifs
#' 
#' @param aoi area of interest to return 30-year statistics. Should be an sf object
#' @param variable the WBM variables to pull data for, can be a single variable or list.
#' @param cf A list of the GCMs to pull data for
#' @param historic_years A character range of historic years separated by "_". Choose from: "1980_1999", "1980_2019", "1981_2010", "2000_2019"
#' @param future_years A character range of future years separated by "_". Default is all future ranges: c("2040_2069", "2070_2099")
#' @param save TRUE/FALSE, whether or not to save the raster stack output
#' @param path directory to save raster stack to
#' @param filename_prefix a name to append to filename that describes the AOI for the saved raster stack
#' @return A SpatRaster stack of all downloaded rasters
get30yearWBMGridMET <- function(aoi,
                                variable = c("runoff"),
                                cf,
                                historic_years = c("1981_2010"),
                                future_years = c("2040_2069", "2070_2099"),
                                save = TRUE,
                                path = "data/all",
                                filename_prefix) {
  # Set lookup vars
  scenarios <- c('rcp45', 'rcp85', "historical")
  rcp <- gsub("^.*\\.", "", cf)
  cfs <- gsub("\\.", "_", cf)
  call_vars <- tidyr::crossing(variable, cfs, years = future_years) %>%
    mutate(rcp = gsub("^.*\\_", "", cfs)) %>%
    bind_rows(
      tidyr::crossing(
        variable,
        rcp = "historical",
        cfs = "gridmet_historical",
        years = historic_years
      )
    ) %>% 
    # fix name for "MIROC-ESM-CHEM" to make sure it matches what WBM link says
    ## also note, for now since WBM does not have MIROC-ESM for rcp 4.5, this will
    ## instead use rcp 8.5 if that was a selected model
    mutate(cfs = if_else(str_detect(cfs, "MIROC-ESM"), "MIROC-ESM-CHEM_rcp85", cfs))
  
  # Define web call URLs, only pulls annual means
  base_call <- "http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/"
  all_calls <- paste0(
    base_call,
    call_vars$variable,
    "/",
    call_vars$rcp,
    "/V_1_5_annual_",
    call_vars$cfs,
    "_",
    call_vars$variable,
    "_",
    call_vars$years,
    "_annual_means_cropped_units_mm.tif"
  )
  
  #download and crop each individual .tif and save as list
  tif_list <- purrr::map(all_calls, function(x) {
    #download a single .tif
    temp1 <- tempfile()
    download.file(x, destfile = temp1, method = "curl")
    
    # read and crop raster to AOI
    wb_dat <- terra::rast(temp1) %>%
      terra::crop(sf::st_transform(aoi, st_crs(wb_dat)))
    
    # assign name to raster layer
    names(wb_dat) <- gsub(".*V_1_5_annual_", "", x)
    
    return(wb_dat)
    
  })
  
  # bind all rasters into raster stack
  tif_stack <- terra::rast(tif_list)
  
  # save raster stack as single .tif (with named layers)
  if (save == TRUE) {
    # Set output directory
    out_dir <- paste0(path, "/wbm_gridmet_30_year/")
    
    if (!dir.exists(path)) {
      dir.create(path)
    }
    
    if (!dir.exists(out_dir)) {
      dir.create(out_dir)
    }
    
    writeRaster(
      tif_stack,
      filename = paste0(out_dir, filename_prefix, "_wbm_30yr_annual_rasters.tif"),
      overwrite = TRUE
    )
  }
  
  return(tif_stack)
  
}

# # Test function -----------------
# source("setup.R")
# ## get aoi
# brca <- getParkBoundary("BRCA")
# aoi <- brca %>% st_buffer(20000)
# 
# ## variable
# variable <- c("runoff")
# 
# ## get cfs
# brca_cfs <- select_GCMs("BRCA", region = "IMR")
# cf <- brca_cfs %>% filter(centroid == "BRCA_future") %>% pull(GCM)
# 
# brca_wbm_30y <- get30yearWBMGridMET(aoi = aoi,
#                                     variable = variable,
#                                     cf = cf,
#                                     save = TRUE,
#                                     path = "data/all",
#                                     filename_prefix = "BRCA")
# 
# # inspect maps
# library(leaflet)
# leaflet() %>% addTiles() %>% addRasterImage(brca_wbm_30y[[9]])

