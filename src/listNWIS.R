#' Inventory of USGS flow data near park units
#' 
#' This function imports an inventory of available flow data from the USGS National Water Information System (https://waterdata.usgs.gov/nwis)
#' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units used by `getParkBoundary()`) to buffer around park boundary
#' @return An sf object of USGS gaging stations within given spatial boundary and attributes
listNWIS <- function(aoi, dist){
  
  sf::sf_use_s2(FALSE)
  
  # add buffer around area of interest
  aoi_buffer <- aoi %>% 
    sf::st_buffer(., dist = dist)
  
  # aoi <- getParkBoundary("DEVA")
  
  gage_sites <- vector("list", length = nrow(aoi_buffer))
  
  for (i in 1:nrow(aoi_buffer)){
    
    bbox <- sf::st_bbox(aoi_buffer[i]) %>%
      as.vector() %>%
      round(., digits=7) %>% 
      paste(collapse = ",")
    
    gage_sites[[i]] <- dataRetrieval::whatNWISdata(bBox = bbox)
    
  }
  
  gage_sites <- dplyr::bind_rows(gage_sites) %>%
    sf::st_as_sf(coords=c('dec_long_va','dec_lat_va'),crs=4269)
  
  tables <- rvest::read_html('https://help.waterdata.usgs.gov/parameter_cd?group_cd=%') %>%
    rvest::html_nodes('table') %>%
    rvest::html_table()
  
  pcodes <- tables[[1]] %>%
    janitor::clean_names() %>%
    dplyr::mutate(parm_cd=stringr::str_pad(as.character(parameter_code),5,pad="0"))
  
  inventory <- gage_sites %>%
    dplyr::left_join(pcodes,by="parm_cd") %>%
    sf::st_intersection(.,aoi_buffer) %>%
    # sf::st_join(., dplyr::select(aoi, UNIT_CODE), left=T) %>%
    # dplyr::mutate(within_park=ifelse(is.na(UNIT_CODE.y), "Outside","Within")) %>%
    dplyr::select(c(site_no,
                    site_name=station_nm,
                    data_type_cd,
                    site_type_cd=site_tp_cd,
                    n_obs=count_nu,
                    begin_date,
                    end_date,
                    parameter=parameter_name_description,
                    code=parm_cd))
    
  site_url <- 'https://maps.waterdata.usgs.gov/mapper/help/sitetype.html'
  
  table <- rvest::read_html(site_url) %>%
    rvest::html_nodes('table') %>%
    rvest::html_table() #%>%
  
  table <- rbind(table[[1]],table[[2]],table[[3]],table[[4]],table[[5]]) %>%
    dplyr::select(site_type_cd = 1,
           site_type = 2)
  
  inventory <- left_join(inventory,table,by='site_type_cd') %>%
    mutate(data_type=case_when(data_type_cd=="dv" ~ "Daily",
                               data_type_cd=="uv" ~ "Unit",
                               data_type_cd=="qw" ~ "Water Quality",
                               data_type_cd=="gw" ~ "Groundwater Levels",
                               data_type_cd=="iv" ~ "Unit",
                               data_type_cd=="sv" ~ "Site Visits",
                               data_type_cd=="pk" ~ "Peak Measurements",
                               data_type_cd=="ad" ~ "USGS Annual Water Data Report",
                               data_type_cd=="aw" ~ "Active Groundwater Level Network",
                               data_type_cd=="id" ~ "Historic Instantaneous")) %>%
    sf::st_join(., dplyr::select(aoi_buffer, UNIT_CODE))
  
  return(inventory)
  
}

