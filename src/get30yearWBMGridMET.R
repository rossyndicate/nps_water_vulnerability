#' This function returns the 30-year statistic sf_rasters for the NPS gridded
#' water balance model.
#' @param aoi area of interest to return 30-year statistics
#' @param path directory to download data
#' 
get30yearWBMGridMET <- function(aoi,
                        path = "data/all",
                        wb_var = c("runoff","accumswe,AET"),cf = select_cfs$GCM) {
  
  # Test vars
  path <- "data/all"
  wb_var <- c("runoff")
  aoi <- park_boundary %>% sf::st_buffer(., dist = .1) %>% sf::st_bbox()
  
  # Set lookup vars
  scenarios <- c('rcp45','rcp85',"historical")
  rcp <- gsub("^.*\\.","",cf)
  cfs <- gsub("\\.","_",cf)
  call_vars <- tidyr::crossing(wb_var,cfs) %>%
    mutate(rcp = gsub("^.*\\_","",cfs),
           years = "2040_2069") %>%
    bind_rows(tidyr::crossing(wb_var,
                              rcp = "historical",
                              cfs = "gridmet_historical",
                              years = "1981_2010"))
  
  # Set out dir
  out_dir <- paste0(path,"/wbm_gridmet_30_year/")
  
  # Define web call URLs
  base_call <- "http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/"
  full_call <- paste0(base_path,call_vars$wb_var,"/",call_vars$rcp,
                      "/V_1_5_annual_",call_vars$cfs,"_",
                      call_vars$wb_var,
                      "_",call_vars$years,"_annual_means_cropped_units_mm.tif")
  
  
  
  # Conus-wide so common directory
      # CM:can't create multiple sub directories with dir.create() so split it up
    if (!dir.exists(path)) {
      dir.create(path)
    }
    
    if (!dir.exists(out_dir)) {
      dir.create(out_dir)
      }
    
   get_30y_wbm <- function(full_path) { 
     
     call <- full_path
     
     out_name <- gsub(".*V_1_5_annual_","",full_path)
   
     out_path <- paste0(out_dir,out_name)
     
     if (!file.exists(out_path)) {
        download.file(call, destfile = out_path)
     }
     
     wb_dat <- terra::rast(out_path) 
     
     aoi_sf <- aoi %>%
       sf::st_as_sfc() %>%
       sf::st_transform(st_crs(wb_dat))
     
     wb_dat2 <- wb_dat %>%
       terra::crop(aoi_sf) %>% 
       stars::st_as_stars() %>% 
       sf::st_as_sf()
     
     return(wb_dat2)
     
     }
  
   a<- get_30y_wbm(full_call[1])
   raster_list <- purrr::map(full_call, get_30y_wbm) %>% append()
   combined_sf <- do.call(rbind, raster_list)
  
  return(full_call)
   
}
  

