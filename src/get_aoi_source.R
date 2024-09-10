#' This function takes in an sf version of the NPS water supply database "source"
#' table filtered for select sites and returns an area of interest for each 
#' source. The AOI is either the watershed associated with the source location
#' (if the source is shallow GW or SW) or the geological formation extent if
#' the source is deep groundwater, spring, or karst aquifer.
#' Inputs:
#' @param sf sf dataframe containing point geometry AND
#'           source_type column. Valid source types include:
#'           "Shallow Groundwater"
#'           "River/Stream"
#'           "Karst Aquifer and Spring"
#'           "Spring"
#'           "Deep Groundwater"
#'           Note, this will most likely be source_table_locs
#'           
#' @return sf input dataframe with additional "aoi" columns containing
#'         sf geometry and metadata for source area    
get_aoi_source <- function(sf = source_table_locs) {
  
  # for testing
  #sf <- source_table_locs
  
  sf_crs <- st_crs(sf)
  
 sf <- sf %>%
      dplyr::mutate(
        method = 
          ifelse(source_type %in% c("Shallow Groundwater", "River/Stream"),
                 "SHALLOW METHOD",  
                 #               ifelse(source_type %in% c("Karst Aquifer and Spring", "Spring"), 
                 #                     "SPRING METHOD", 
                 ifelse(source_type %in% c("Deep Groundwater", 
                                           "Spring", 
                                           "Karst Aquifer and Spring"), 
                        "DEEP METHOD", 
                        "OTHER")))
  
  # Initiate empty aoi vector
  aoi <- vector("list", nrow(sf))  
  
  # Populate each row with aoi
  for(i in 1:nrow(sf)) {
    
    #i <- 2
    
    # Shallow method - get watershed
    if (sf[i,][["method"]] == "SHALLOW METHOD") {
      
      # First get watershed
      ws <- sf[i,]["geometry"] %>% 
        getXYWatersheds(sf = ., coordinates = NULL) %>%
        dplyr::summarize() %>%
        st_transform(., crs = sf_crs) %>%
        dplyr::mutate(type = "Watershed",
                      name = "Watershed") 
      aoi[[i]] <- ws
      
      if (sf[i,][["type"]] == "Well") {
        
        # Then get formation extent- shallow so use min age geologic unit as 
        # filter
        all_formations <- sf[i,] %>%
          get_rockd(points = .) %>%
          dplyr::filter(b_int_age == min(b_int_age)) %>%
          st_make_valid() %>%
          dplyr::mutate(type = "Formation")
        
        if (nrow(all_formations) > 1) {
          
          combined_unit <- st_union(all_formations)
          
          combined_unit <- st_sf(geometry = combined_unit, 
                                 all_formations[1, 
                                                !names(all_formations) %in% "geometry"])
          all_formations <- combined_unit
        }
        
        aoi[[i]] <- bind_rows(ws, all_formations)
      }
      
      
      
      # Deep GW method - get intersecting geologic units
    } else if (sf[i,"method"][["method"]] == "DEEP METHOD") {
      
      # extract some info about the formation
      form_type <- word(sf[i,]$aquifer_name,-1)
      form_name_long <- sf[i,]$aquifer_name
      form_name_short <- word(form_name_long, 1)
      
      # get all formations intersecting with point
      all_formations <- sf[i,] %>%
        get_rockd(points = .)
      
      # Now, check if metadata available by searching macrostrat using the
      # formation name.  filter all_formations to match metadata
      # found using formation name
      
      # Clear "form_info" variable from previous loop just incase
      if (exists("form_info")) {rm("form_info")}
      
      # Search macrostat database for formation name
      if (!is.na(form_name_short)) {
        form_info <- get_formation_info(gsub(" ","_",form_name_short), form_type) #%>%
        #dplyr::filter(grepl(form_name,Fm)) 
      }
      
      if (exists("form_info")) {
        # Extract age from that info
        form_min_age <- form_info$t_age %>% min(., na.rm = TRUE)
        form_max_age <- form_info$t_age %>% max(., na.rm = TRUE)
        
        # Use formation name to filter all_formations
        select_formation <- all_formations %>%
          dplyr::filter(grepl(form_name_long, name))
        
        # If nothing returned, use formation age to filter all_formations
        if(nrow(select_formation) < 1) {
          start_buffer = 5
          while(nrow(select_formation) <1) {
            select_formation <- all_formations %>%
              dplyr::filter(t_age >= form_min_age-start_buffer & 
                              t_age <= form_max_age+start_buffer)
            start_buffer <- start_buffer + 5
            if (start_buffer > 50) break # break if nothing matched with +/- 50 years
          }
        } 
        
        # If still nothing returned, return all_formations (for now)
        # Will need to investigate further. 
        if (nrow(select_formation) < 1) {
          select_formation <- all_formations
        }
      } else {
        # if no metadata available, then assign select_formation as all formations
        select_formation <- all_formations
      }
      
      select_formation <- select_formation %>%
        st_transform(., sf_crs) %>%
        dplyr::mutate(type = "Formation",
                      area = as.numeric(st_area(geometry))/(1000*1000)) %>%
        dplyr::filter(area < 1e4) # remove aoi > 10,000 km2
      
      aoi[[i]] <- select_formation
    }
  }
  
  sf$aoi <- purrr::map(aoi, bind_rows)
  #sf['aoi'] <- aoi
  
  return(sf)
  
}


#' Get formation info - used in get_aoi_source()
#' This function pulls geologic layers from the Macrostrat API that intersect 
#' a user-specified point (or points) of interest. 
#' 
#' @param points An `sf` object of points.
#' 
#' @return An `sf` object of Macrostrat (https://macrostrat.org/) geologic 
#' layers that intersect the point object(s). 

get_formation_info <- function(formation_name, rock_type){
  
  # grab geologic layers that intersect the coordinates of the points of interest
  # y <- RCurl::getURL(paste0("https://macrostrat.org/api/units?strat_name=",formation_name,"&format=json"),
  #                   .opts = RCurl::curlOptions(ssl.verifypeer=FALSE, verbose=TRUE)) %>%
  #  fromJSON()
  
  #table_result <- y$success$data
  
  #https://macrostrat.org/api/units?strat_name=cedar_mesa
  
  if(exists("table_result")){rm("table_result")}
  
  tryCatch({
    # First try formation name search (without rock type)
    table_result <- RCurl::getURL(paste0("https://macrostrat.org/api/units?strat_name=",formation_name,"&format=geojson_bare&s"),
                                  .opts = RCurl::curlOptions(ssl.verifypeer=FALSE, verbose=TRUE)) %>%
      geojsonsf::geojson_wkt(.) %>%
      dplyr::bind_rows() %>%
      sf::st_as_sf(., wkt = 'geometry', crs = 4326)
    # Check if successful (no error)
    if (!is.null(table_result)) return(table_result)
    
    # If first try fails, attempt with rock type (assuming error handling in case both fail)
    table_result <- RCurl::getURL(paste0("https://macrostrat.org/api/units?strat_name=",formation_name,"&lith_group=",rock_type,"&format=geojson_bare"),
                                  .opts = RCurl::curlOptions(ssl.verifypeer=FALSE, verbose=TRUE)) %>%
      geojsonsf::geojson_wkt(.) %>%
      dplyr::bind_rows() %>%
      sf::st_as_sf(., wkt = 'geometry', crs = 4326)
    return(table_result)
  }, error = function(e) {
    # do nothing
    table_result <- NULL
  })
  
  return(table_result)
  
}
