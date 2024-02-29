# Downloads WBM outputs derived from DayMET (most recent iteration of the WBM). Found at:
# http://www.yellowstone.solutions/thredds/catalog/daily_or_monthly/v2_historical/monthly/catalog.html

getHistoricWBMDayMET <- function(park, aoi, path = "data/", 
                                 wb_vars = c("soil_water", "runoff", "rain", "agdd", "accumswe", "PET", "Deficit", "AET")
) {
  
  options(timeout = 900)
  sf::sf_use_s2(FALSE)
  
  if(!dir.exists(paste0(getwd(),"/templeod_dataall/"))){dir.create(file.path(getwd(),"/templeod_dataall/"), showWarnings = FALSE)}
  if(!dir.exists(paste0(getwd(),"/", path, "/", park, "/wbm_daymet_hist/"))){dir.create(file.path(paste0(getwd(),"/", path, "/", park, "/wbm_daymet_hist/")), showWarnings = FALSE)}
  
  years <- 1980:2022
  
  map_dat <- tidyr::crossing(years, wb_vars)
  
  start <- Sys.time()
  
  for(i in 1:nrow(map_dat)){
    #http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/v2_historical/gridmet_v_1_5_historical/V_1_5_2022_gridmet_historical_rain.nc4
    call <- paste0("http://www.yellowstone.solutions/thredds/fileServer/daily_or_monthly/v2_historical/monthly/v2_", 
                   map_dat$years[i], "_", map_dat$wb_vars[i], "_monthly.nc4")
    
    temp1 <- tempfile()
    download.file(paste0(call), destfile = "templeod_dataall/data.nc4")#, method = "curl")
    
    #wb_crop <- ncdf4::nc_open("templeod_dataall/data.nc4")
    #wb_crop <- terra::rast("templeod_dataall/data.nc4")
    wb_wb <- stars::read_ncdf("templeod_dataall/data.nc4")
    # Set the CRS for the stars object. For whatever reason, a handful are unknown CRS.
    # Assuming they are the same as the others in the pull:
    crs <- 4326
    st_crs(wb_wb) <- crs
    wb_lat <- stars::read_ncdf("templeod_dataall/data.nc4", var = "lat") %>% as.data.frame()
    wb_lon <- stars::read_ncdf("templeod_dataall/data.nc4", var = "lon") %>% as.data.frame() 
    
    aoi <- aoi %>%
      sf::st_transform(st_crs(wb_wb))
    
    wb_wb_crop <- st_crop(wb_wb, aoi, crop = TRUE) %>% raster::as.data.frame(., xy = TRUE)
    
    table <- wb_wb_crop %>%
      left_join(wb_lat, by = c("x", "y")) %>%
      left_join(wb_lon, by = c("x", "y")) %>%
      # cant get the projections to line up -_-
      #st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>%
      mutate(var = names(.)[4]) %>%
      rename(val = 4) %>%
      filter(!is.na(val))
    
    #mapview(table) + mapview (aoi)
    
    write_csv(table, file = paste0(getwd(), "/", path, "/", park, "/wbm_daymet_hist/", park, "_daymet_", map_dat$wb_vars[i], "_", map_dat$years[i], ".csv"))
    
    Sys.sleep(1)
    
  }
  
  end <- Sys.time()
  
  end - start
  
  unlink(paste0(getwd(),"/templeod_dataall"), recursive = TRUE)
}


# scratch code:
#   wb_crop <- wb_crop %>%
#     terra::crop(aoi, mask = TRUE)
#   
#   terra::writeRaster(wb_crop, filename = paste0(getwd(), "/", path, "/", park, "/wbm_hist/",
#                                                 park, "_", map_dat$wb_vars[i], "_", map_dat$years[i], ".nc4"),
#                      overwrite = TRUE)
# }
# 
