# This function pulls in historic daymet data (https://daymet.ornl.gov/) for a selected time frame and area
# of interest.

get_dayMET <- function(# time = "monthly",
  path = "data/",
  park = "misc",
  aoi = final_aoi, #st_transform(summarize(watersupply_watershed), 4326),
  vars = c("swe", "srad", "dayl", "vp", "tmin", "tmax", "prcp")){
  
  years <- 1980:2023
  
  map_dat <- tidyr::crossing(years, vars)
  
  bbox <- st_bbox(aoi)
  
  if(!dir.exists(paste0(getwd(),"/", path, "/", park))){
    dir.create(file.path(paste0(getwd(),"/", path, "/", park)), showWarnings = FALSE)}
  
  if(!dir.exists(paste0(getwd(),"/", path, "/", park, "/daymet_hist_monthly/"))){
    dir.create(file.path(paste0(getwd(),"/", path, "/", park, "/daymet_hist_monthly/")), showWarnings = FALSE)}
  
  if(!dir.exists(paste0(getwd(),"/", path, "/", park, "/daymet_hist_monthly/temp/"))){
    dir.create(file.path(paste0(getwd(),"/", path, "/", park, "/daymet_hist_monthly/temp/")), showWarnings = FALSE)}
  
  for (i in 1:nrow(map_dat)){
    
    # folder to save temp netcdf in
    if (!dir.exists("templeod_dataall/")) {
      dir.create("templeod_dataall/")
    }
    
    # not sure why, but cannot pull SWE, SRAD, and DAYL on a monthly timestep:
    freq <- ifelse(map_dat$vars[i] %in% c("swe", "srad", "dayl"), "daily", "monthly")
    
    print(paste0("Downloading ", freq, " data for ", map_dat$vars[i]))
    
    # download data by year/var combo at appropriate frequency
    daymetr::download_daymet_ncss(location = c(bbox[[4]], bbox[[1]], bbox[[2]], bbox[[3]]),
                                  start = map_dat$years[i],
                                  end = map_dat$years[i],
                                  param = map_dat$vars[i],
                                  frequency = paste(freq),
                                  path = "templeod_dataall/")
    
    # read in netCDF
    wb_dat <- stars::read_ncdf(list.files("templeod_dataall/", full.names = TRUE))
    
    # transform crs of aoi (faster than transforming netCDF)
    aoi <- aoi %>%
      sf::st_transform(st_crs(wb_dat))
    
    # crop dayMET data to AOI and clean for export
    wb_dat_clean <- sf::st_crop(wb_dat, aoi, crop = TRUE) %>%
      terra::as.data.frame(., xy = TRUE) %>%
      dplyr::mutate(ym = lubridate::ym(str_sub(as.character(time), start = 1, end = 7)),
                    var = map_dat$vars[i]) %>%
      dplyr::rename(val = 4) %>%
      # filter NAs (this will return just the points within the AOI)
      dplyr::filter(!is.na(val)) %>%
      dplyr::select(x, y, ym, var, val) %>%
      sf::st_as_sf(coords = c("x", "y"), crs = st_crs(wb_dat)) %>%
      sf::st_transform(crs = 4326)
    
    wb_dat_clean <- st_coordinates(wb_dat_clean$geometry) %>%
      cbind(wb_dat_clean) %>%
      dplyr::rename(x = X,
                    y = Y) %>%
      # drop old geometry column
      sf::st_drop_geometry()
    
    
    if(freq == "daily"){
      wb_dat_clean <- wb_dat_clean %>%
        dplyr::group_by(x, y, ym, var) %>%
        # summarize the data to monthly
        dplyr:: summarize(sum = sum(as.numeric(val), na.rm=T),
                          mean = mean(as.numeric(val), na.rm=T)) %>%
        dplyr::mutate(value = case_when(var %in% c("srad", "dayl") ~ mean,
                                        # is this the right move??
                                        var %in% c("swe") ~ max)) %>%
        dplyr::select(x, y, ym, var, value)
    }
    
    # back up because this download is slow and seems to break a lot....
    write_csv(wb_dat_clean,
              file = paste0(
                path,
                "/",
                park,
                "/daymet_hist_monthly/temp/",
                park,
                "_", map_dat$vars[i],
                "_", map_dat$years[i],
                "_daymet_hist.csv"
              ))
    
    unlink("templeod_dataall", recursive = TRUE)
    
  }
  
  # return all daymet data:
  all_daymet <- list.files(paste0(path, "/", park, "/daymet_hist_monthly/temp"), recursive = TRUE, full.names = TRUE) %>%
    map_dfr(~data.table::fread(.)) %>%
    select(x, y, ym, variable, value) %>%
    pivot_wider(names_from = "variable", values_from = "value") 
  
  return(all_daymet)
  
}









