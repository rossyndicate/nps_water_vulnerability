# Water Rights of Montana in file geodatabase format (6 feature classes). The data 
# consists of estimated locations of water right points of diversion (POD) and places 
# of use (POU) from the Department of Natural Resources and Conservation (DNRC)
# Water Rights Database. There are six separate spatial layers: Points of Diversion (x2), 
# Places of Use polygons (x2), Points of Reservoirs (water rights associated with a
# reservoir; x2) For administrative purposes, the original version of all water rights
# is given (feature classes for original version water rights are named with a "1":
# WR1DIV; WR1POU; WR1RES). If a change of information about a particular 
# water right has occurred over time, there will be an additional version of the water 
# right, and depending on how many changes, there will only be one other version
# number (greater than 1), which will represent the "current operating authority" 
# version. IN MOST CASES, THE CURRENT OPERATING AUTHORITY DATA SHOULD BE USED 
# (THAT IS, THE FEATURE CLASSES WITHOUT A "1" IN THE FILENAME: WRDIV, WRPOU, AND WRRES). 
# The WRKEY is the unique identifier for a water right. It consists of the WRID hyphenated 
# with the version number (VERID). Date are updated approximately twice annually.
# 
# http://ftpgeoinfo.msl.mt.gov/Data/Spatial/NonMSDI/DNRC_WR/MTWaterRights_Metadata.xml
#'
#' @param aoi An sf polygon object used to grab Montana water rights data.
#' @param dist The distance (in the same units used by `aoi`) to buffer around the area of interest.
#' 
#' @return A list of `sf` objects: Montana points of diversions, places of use, and reservoirs within the user-supplied aoi and buffer area.

getWaterRightsMontana <- function(aoi, dist = 0){
  
  call <- "https://ftpgeoinfo.msl.mt.gov/Data/Spatial/NonMSDI/DNRC_WR/MTWaterRights.gdb.zip"
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
 
  POD <- st_read(list.files(temp2, full.names = TRUE), layer = "WRDIV")
  POU <- st_read(list.files(temp2, full.names = TRUE), layer = "WRPOU")
  RES <- st_read(list.files(temp2, full.names = TRUE), layer = "WRRES")
  
  # These are layers containing the historic data (i.e., if any rights were
  # moved around or modififed, this layer shows the original owner, etc.)
  # POU1 <- st_read(list.files(temp2, full.names = TRUE), layer = "WR1POU")
  # POD1 <- st_read(list.files(temp2, full.names = TRUE), layer = "WR1DIV")
  # RES1 <- st_read(list.files(temp2, full.names = TRUE), layer = "WR1RES")
  
  sf::sf_use_s2(FALSE)
  
  aoi <- aoi  %>%
    st_transform(crs(POD))
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>% 
    sf::st_zm() %>%
    sf::st_buffer(dist = dist)
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_pods <- POD %>%    
    sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
                  within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
    dplyr::select(-within)
  
  nearby_pous <- POU %>%
    sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
                  within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
    dplyr::select(-within)

  nearby_res <- RES %>%
    sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
                  within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
    dplyr::select(-within)
  
  # nearby_pod1 <- POD1 %>%
  #   sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
  #   sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
  #   dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
  #                 within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
  #   dplyr::select(-within)
  
  # nearby_pou1 <- POU1 %>%
  #   sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
  #   sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
  #   dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
  #                 within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
  #   dplyr::select(-within)
  
  # nearby_res1 <- RES %>%
  #   sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
  #   sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
  #   dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
  #                 within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
  #   dplyr::select(-within)
  
  return(list("points_of_diversion" = nearby_pods, 
              "places_of_use" = nearby_pous,
              "reservoirs" = nearby_res))
  
}