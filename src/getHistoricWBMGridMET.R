# Downloads WBM outputs derived from GridMET. Found at:
# http://www.yellowstone.solutions/thredds/catalog/daily_or_monthly/v2_historical/gridmet_v_1_5_historical/catalog.html

getHistoricWBMGridMET <- function(time = "monthly",
                                  park,
                                  aoi,
                                  path = "data/",
                                  wb_vars = c("soil_water",
                                              "runoff",
                                              "rain",
                                              "accumswe",
                                              "PET",
                                              "Deficit",
                                              "AET")) {
  
  
  start <- Sys.time()
  
  years <- 1980:2023
  
  map_dat <- tidyr::crossing(years, wb_vars)
  
  if (time == "monthly") {
    
    # CM:can't create multiple sub directories with dir.create() so split it up
    if (!dir.exists(paste0(path, "/", park))) {
      dir.create(paste0(path, "/", park))
    }
    
    if (!dir.exists(paste0(path, "/", park, "/wbm_gridmet_hist_monthly/"))) {
      dir.create(paste0(path, "/", park, "/wbm_gridmet_hist_monthly/"))
    }
    
    if (!dir.exists(paste0(path, "/", park, "/wbm_gridmet_hist_monthly/temp/"))) {
      dir.create(paste0(path, "/", park, "/wbm_gridmet_hist_monthly/temp/"))
    }
    
    table <- vector("list", length = nrow(map_dat))
    
    for (i in 1:nrow(map_dat)) {
      
      # folder to save temp netcdf in
      if (!dir.exists("templeod_dataall/")) {
        dir.create("templeod_dataall/")
      }
      
      call <- paste0(
        "http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/v2_historical/gridmet_v_1_5_historical/V_1_5_",
        map_dat$years[i],
        "_gridmet_historical_",
        map_dat$wb_vars[i],
        "_monthly.nc4")
      
      download.file(call, destfile = "templeod_dataall/data.nc4")
      
      # read in netCDF
      wb_dat <- stars::read_ncdf("templeod_dataall/data.nc4")
      
      # transform crs of aoi (faster than transforming netCDF)
      aoi <- aoi %>%
        sf::st_transform(st_crs(wb_dat))
      
      # crop gridmet data to park and clean for export
      wb_dat_clean <- st_crop(wb_dat, aoi, crop = TRUE) %>%
        terra::as.data.frame(., xy = TRUE) %>%
        dplyr::mutate(ym = ym(str_sub(time, 1, 7)),
                      var = map_dat$wb_vars[i]) %>%
        dplyr::rename(val = 4) %>%
        dplyr::mutate(val = as.numeric(val)) %>%
        # filter NAs (this will return just the points within the AOI)
        dplyr::filter(!is.na(val)) %>%
        dplyr::select(x, y, ym, var, val) %>%
        sf::st_as_sf(coords = c("x", "y"), crs = st_crs(wb_dat)) %>%
        # Transform coordinates to WGS84
        sf::st_transform(crs = 4326) 
      
      wb_dat_clean <- st_coordinates(wb_dat_clean$geometry) %>%
        # bind new x and y coords to the data frame
        cbind(wb_dat_clean) %>%
        dplyr::rename(x = X,
                      y = Y) %>%
        # drop old geometry column
        sf::st_drop_geometry()
      
      # back up because this download is slow and seems to break a lot....
      readr::write_csv(
        wb_dat_clean,
        file = paste0(
          path,
          "/",
          park,
          "/wbm_gridmet_hist_monthly/temp/",
          park, "_", map_dat$wb_vars[i],
          "_", map_dat$years[i],
          "_wbm_gridmet_hist_monthly.csv"
        ))
      
      table[[i]] <- wb_dat_clean
      
      unlink("templeod_dataall", recursive = TRUE)
      
      print(i)
      
    }
    
    table_all <- dplyr::bind_rows(table)
    
    readr::write_csv(
      table_all,
      file = paste0(
        path,
        "/",
        park,
        "/wbm_gridmet_hist_monthly/",
        park,
        "_wb_gridmet_hist_monthly.csv"
      )
    )
    
  }
  
  if (time == "daily") {
    
    # CM:can't create multiple sub directories with dir.create() so split it up
    if (!dir.exists(paste0(path, "/", park))) {
      dir.create(paste0(path, "/", park))
    }
    
    if (!dir.exists(paste0(path, "/", park, "/wbm_gridmet_hist_daily/"))) {
      dir.create(paste0(path, "/", park, "/wbm_gridmet_hist_daily/"))
    }
    
    if (!dir.exists(paste0(path, "/", park, "/wbm_gridmet_hist_daily/temp/"))) {
      dir.create(paste0(path, "/", park, "/wbm_gridmet_hist_daily/temp/"))
    }
    
    table <- vector("list", length = nrow(map_dat))
    
    for (i in 1:nrow(map_dat)) {
      
      # folder to save temp netcdf in
      if (!dir.exists("templeod_dataall/")) {
        dir.create("templeod_dataall/")
      }
      
      call <- paste0(
        "http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/v2_historical/gridmet_v_1_5_historical/V_1_5_",
        map_dat$years[i],
        "_gridmet_historical_",
        map_dat$wb_vars[i],
        ".nc4")
      
      download.file(call, destfile = "templeod_dataall/data.nc4")
      
      # read in netCDF
      wb_dat <- stars::read_ncdf("templeod_dataall/data.nc4")
      
      # transform crs of aoi (faster than transforming netCDF)
      aoi <- aoi %>%
        sf::st_transform(st_crs(wb_dat))
      
      # crop gridmet data to park and clean for export
      wb_dat_clean <- st_crop(wb_dat, aoi, crop = TRUE) %>%
        terra::as.data.frame(., xy = TRUE) %>%
        dplyr::mutate(ym = ym(str_sub(time, 1, 7)),
                      date = as_date(str_sub(time, 1, 10)),
                      var = map_dat$wb_vars[i]) %>%
        dplyr::rename(val = 4) %>%
        dplyr::mutate(val = as.numeric(val)) %>%
        # filter NAs (this will return just the points within the AOI)
        dplyr::filter(!is.na(val)) %>%
        dplyr::select(x, y, ym, date, var, val) %>%
        sf::st_as_sf(coords = c("x", "y"), crs = st_crs(wb_dat)) %>%
        # Transform coordinates to WGS84
        sf::st_transform(crs = 4326) 
      
      wb_dat_clean <- st_coordinates(wb_dat_clean$geometry) %>%
        # bind new x and y coords to the data frame
        cbind(wb_dat_clean) %>%
        dplyr::rename(x = X,
                      y = Y) %>%
        # drop old geometry column
        sf::st_drop_geometry()
      
      # back up because this download is slow and seems to break a lot....
      readr::write_csv(
        wb_dat_clean,
        file = paste0(
          path,
          "/",
          park,
          "/wbm_gridmet_hist_daily/temp/",
          park, "_", map_dat$wb_vars[i],
          "_", map_dat$years[i],
          "_wbm_gridmet_hist_daily.csv"
        ))
      
      table[[i]] <- wb_dat_clean
      
      unlink("templeod_dataall", recursive = TRUE)
      
      print(i)
      
    }
    
    table_all <- dplyr::bind_rows(table)
    
    readr::write_csv(
      table_all,
      file = paste0(
        path,
        "/",
        park,
        "/wbm_gridmet_hist_daily/",
        park,
        "_wb_gridmet_hist_daily.csv"
      )
    )
    
  }
  
  end <- Sys.time()
  
  print(end - start)
  
  return(table_all)
  
}
