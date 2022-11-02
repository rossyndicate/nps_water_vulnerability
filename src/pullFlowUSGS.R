#' Inventory of USGS flow data near park units
#' 
#' This function imports an inventory of available flow data from the USGS National Water Information System (https://waterdata.usgs.gov/nwis)
#' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param dist The distance (in the same units used by `getParkBoundary()`) to buffer around park boundary
#' @param save Whether to save the resulting file (TRUE/FALSE)
#' @param path If `save = TRUE`, the file path to save to
#' @return An sf object of USGS gaging stations within given spatial boundary and attributes
pullFlowUSGS <- function(aoi, dist,  save = FALSE, path = NULL){
  
  sf::sf_use_s2(FALSE)
  
  # add buffer around area of interest
  aoi_buffer <- aoi %>% 
    sf::st_buffer(., dist = dist)
  
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
    sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(., dplyr::select(aoi, UNIT_CODE), left=T) %>%
    dplyr::mutate(within_park=ifelse(is.na(UNIT_CODE.y), "Outside","Within")) %>%
    dplyr::select(c(site_no,
                      site_name=station_nm,
                      data_type_cd,
                      site_type_cd=site_tp_cd,
                      n_obs=count_nu,
                      begin_date,
                      end_date,
                      parameter=parameter_name_description,
                      code=parm_cd,
                      within_park,
                      UNIT_CODE=UNIT_CODE.x)) %>%
    # flow-related data only for now
    dplyr::filter(grepl("discharge|flow|height|level|width",parameter,ignore.case=T)) %>%
    dplyr::filter(!grepl("sediment",parameter,ignore.case=T))
  
  site_url <- 'https://maps.waterdata.usgs.gov/mapper/help/sitetype.html'
  
  table <- rvest::read_html(site_url) %>%
    rvest::html_nodes('table') %>%
    rvest::html_table() #%>%
  
  table <- rbind(table[[1]],table[[2]],table[[3]]) %>%
    select(site_type_cd = 1,
           site_type = 2)
  
  inventory <- left_join(inventory,table,by='site_type_cd') %>%
    mutate(data_type=case_when(data_type_cd=="dv" ~ "Daily",
                               data_type_cd=="uv" ~ "Instantaneous",
                               data_type_cd=="qw" ~ "Water Quality",
                               data_type_cd=="gw" ~ "Groundwater Levels"))

  if(save == TRUE)
    {write_csv(inventory, paste0(path,'/nwis_flow_inventory.csv'))}
  
  
  return(inventory)
  
}

