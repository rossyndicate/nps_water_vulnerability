#' Inventory of USGS flow data near park units
#' 
#' This function imports an inventory of available flow data from the USGS National Water Information System (https://waterdata.usgs.gov/nwis)
#' 
#' @param park The 4-digit unit code(s) for national parks of interest. Must be in ALL CAPS.
#' @param dist The distance (in the same units used by `getParkBoundary()`) to buffer around park boundary
#' @param save Whether to save the resulting file (TRUE/FALSE)
#' @param path If `save == TRUE`, the file path to save to, including name of csv
#' @return An sf object of USGS gaging stations within given spatial boundary and attributes
pullFlowUSGS <- function(park, dist,  save = FALSE, path = NULL){
  
  sf::sf_use_s2(FALSE)
  
  # add buffer around park boundary
  aoi <- getParkBoundary(park) %>% 
    sf::st_buffer(., dist = dist)
  
  gage_sites <- vector("list", length = length(aoi))
  
  for (i in 1:length(aoi)){
    
    bbox <- sf::st_bbox(aoi[i]) %>%
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
  
  nwis_list <- gage_sites %>%
    dplyr::left_join(pcodes,by="parm_cd") %>%
    dplyr::select(c(site_no,
                    site_name=station_nm,
                    n_obs=count_nu,
                    begin_date,
                    end_date,
                    parameter=parameter_name_description,
                    code=parm_cd)) %>%
    sf::st_intersection(.,dplyr::select(aoi, UNIT_CODE)) %>%
    dplyr::filter(grepl("discharge|flow|height|level|width",parameter,ignore.case=T))

  if(save == TRUE){
    readr::write_csv(nwis_list, paste0(path,'.csv'))
    
  }
  
  
  return(nwis_list)
  
}

