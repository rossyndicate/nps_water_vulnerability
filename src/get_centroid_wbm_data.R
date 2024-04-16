get_centroid_wbm_data <- function(park,
                                  path = "data/park"){
  
  call <- "https://parkfutures.s3.us-west-2.amazonaws.com/maca-tprh-data/0Climate_Zones_by_Unit_cleaned.csv"
  
  if(!dir.exists(paste0(getwd(),"/templeod_dataall/"))){
    dir.create(file.path(getwd(),"/templeod_dataall/"), showWarnings = FALSE)}
  
  if(!dir.exists(paste0(getwd(), "/", path, "/", park))){
    dir.create(file.path(paste0(getwd(), "/", path, "/", park)), showWarnings = FALSE)}
  
  if(!dir.exists(paste0(getwd(), "/", path, "/", park, "/centroid/"))){
    dir.create(file.path(paste0(getwd(), "/", path, "/", park, "/centroid/")), showWarnings = FALSE)}
  
  if(!dir.exists(paste0(getwd(), "/", path, "/", park, "/centroid/wbm/"))){
    dir.create(file.path(paste0(getwd(), "/", path, "/", park, "/centroid/wbm/")), showWarnings = FALSE)}
  
  download.file(paste0(call), destfile = "templeod_dataall/park_list.csv") 
  
  # park_koppen <- read_csv("templeod_dataall/park_list.csv", skip = 1) %>%
  #   filter(UNIT_CODE == park) %>%
  #   .$Unit_ClimateZone %>%
  #   c(park) %>%
  #   as_tibble() %>%
  #   tidyr::crossing(., tibble(period = c("_historical", "_future"))) %>%
  #   mutate(name = paste0(value, period)) %>%
  #   .$name
  
  park_koppen_coords <- read_csv("templeod_dataall/park_list.csv", skip = 1) %>%
    dplyr::filter(UNIT_CODE == park) %>%
    dplyr::select(Unit_ClimateZone, Lat, Lon)
  
  download.file("https://parkfutures.s3.us-west-2.amazonaws.com/park-centroid-wb/0COORDINATES_USED.csv", destfile = "templeod_dataall/park_list.csv") 
  
  park_centroid_coords <- read_csv("templeod_dataall/park_list.csv") %>% 
    dplyr::filter(Place == park) %>%
    dplyr::select(Unit_ClimateZone = Place,
                  Lat = found_lat,
                  Lon = found_lon) %>%
    dplyr::bind_rows(park_koppen_coords) %>%
    tidyr::crossing(., tibble(period = c("_historical", "_future")))
  
  
  downloader <- function(Unit_ClimateZone, period) {
    centroids <- paste0("https://parkfutures.s3.us-west-2.amazonaws.com/park-centroid-wb/", Unit_ClimateZone, "_water_balance", period, ".zip")
    # centroids <- paste0("https://parkfutures.s3.us-west-2.amazonaws.com/maca-tprh-data/", Unit_ClimateZone, period, ".zip")
    
    
    #download boundary 
    temp1 <- tempfile()
    download.file(centroids, destfile = temp1, method = "curl")
    temp2 <- tempfile()
    unzip(temp1, exdir = temp2)
    
    download.file(paste0(centroids), destfile = "templeod_dataall/park_list.csv")
    
    list <- data.table::fread(list.files(temp2, full.names = TRUE)) %>% 
      dplyr::mutate(Unit_ClimateZone = Unit_ClimateZone,
                    period =  period) %>%
      left_join(park_centroid_coords, by = c("Unit_ClimateZone", "period"))
    
    write_csv(list, paste0(path, "/", park, "/centroid/wbm/", Unit_ClimateZone, period, ".csv"))
    
    return(list)
    
  }
  
  all_centroid_data <- pmap(dplyr::select(park_centroid_coords, Unit_ClimateZone, period), downloader) %>%
    bind_rows()
  
  unlink("templeod_dataall", recursive = TRUE)
  
  return(all_centroid_data)
  
}
