#' Import or download daily WBM rasters for an AOI and summarize to mean daily 
#' values in a dataframe.
#' 
#' This function uses the get_wbm_grids() function to download or import 
#' gridded WBM rasters for an AOI. Each daily raster is then summarized using
#' the daily mean, to return a timeseries of daily mean WBM variables for the
#' AOI. 
#' 
#' @param park The 4 digit national park code(s) for parks of interest
#' @param aoi An sf object to use as the crop extent
#' @param aoi_name A character string used to name the file, often a description of the AOI pulled
#' @param gcm A character vector containing selected GCMs for which to download future WBM
#' @param download A TRUE/FALSE
#' 
#' @return A data.frame containing daily timeseries of the spatial mean for 
#'          each water balance variable formatted like the 
#'          get_centroid_wbm_data output.

get_wbm_aoi <- function(park, aoi, aoi_name, gcm = select_cfs$GCM, download = FALSE) {
  
  if (download == TRUE) {
    print("Downloading gridded WBM. This may take several minutes to hours...\n\n")
    
    walk(
      c(
        "soil_water",
        "runoff",
        "rain",
        "accumswe",
        "PET",
        "Deficit",
        "AET"
      ),
      ~ get_wbm_grids(
        park = park,
        aoi_name = aoi_name,
        aoi = aoi,
        wb_var = .x,
        macas = gcms
      )
    )
    
    print("Download complete!\n\n")
  }
  
  wbm_ws_dir <- paste0("data/park/",park,"/wbm_daily_rasters/")
  
  rast_read <- function(rast_list) {
    
    rast(rast_list) %>% 
      as.data.frame(., xy = TRUE) %>%
      pivot_longer(-(x:y),
                   names_to = "var",
                   values_to = "val") %>%
      # Add historic to name
      mutate(var = 
               ifelse(
                 grepl('^accumswe|^runoff|^rain|^PET|^AET|^soil_water|^Deficit', var),
                 paste0("gridmet.historical_",var),var),
             val = val / (25.4*10) ) %>% # 10 = conversion factor, 25.4 = mm to in
      mutate(var = gsub("soil_water", "soilwater", var)) %>%
      # KEC - separate is VERY slow - can anyone suggest something quicker?
      separate(var, 
               into = c("gcm", "wbm_var", "date"), 
               sep = "_") %>%
      mutate(wbm_var = 
               str_replace_all(
                 wbm_var, 
                 c("runoff" = "runoff_in", 
                   "rain" = "rain_in", 
                   "AET" = "aet_in",
                   "PET" = "pet_in",
                   "accumswe" = "accumswe_in",
                   "soilwater" = "soil_water_in",
                   "Deficit" = "deficit_in")),
             date = as.Date(date)) 
  }
  
  
  ws_wbm <- list.files(wbm_ws_dir,full.names = TRUE) %>% 
            grep(aoi_name, ., value=TRUE) 
  
  if (length(ws_wbm) < 1) {
    
    stop("Must download wbm files first! Try function argument: download = TRUE.")
  
    } else {
    
    ws_wbm <- ws_wbm %>%
    purrr::map_dfr(~ rast_read(.)) %>%
    dplyr::select(-c(x,y)) %>%
    group_by(date, wbm_var, gcm) %>%
    summarize_all(.funs = 
                    list(val = ~ mean(val, na.rm = TRUE)), 
                  .groups = "keep") %>%
    pivot_wider(names_from = "wbm_var", values_from = "val") 
  
    return(ws_wbm)
    
    }
  
}
