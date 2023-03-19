#' Create watershed boundary shapfile(s) for an area of interest.
#' 
#' This function uses the nhdplusTools package to import flowline, catchments and
#' water body spatial data for parks, within a specified buffer distance around
#' the park boundary
#' 
#' @param aoi An sf polygon object, often a park boundary (or boundaries) retrieved from `getParkBoundary()`.
#' @param save Whether to save (TRUE) the resulting shapefile(s) or not (FALSE)
#' @param path If `save = TRUE`, the file path to save the shapefile(s) to.
#' 
#' @return Watershed shapefile(s) for the aoi
#' 
#' @seealso [getParkBoundary()]
getWatersheds <- function(aoi, save = TRUE, path = "data/all/"){
  
  sf::sf_use_s2(FALSE)
  
  aois <- aoi
  
  park_names <- aois$UNIT_CODE
  
  # read in the complete NHD (in tabular form) to make for much more efficient nhd crawling. 
  # This data in tabular form doesn't exist anywhere online that I know of... -_-
  nhd <- read_csv('data/all/nhd_flow_network.csv') 
  
  multiple <- function(park_names){
    
    #dir.create(file.path(paste0(getwd(),"/data/all/ws_trace/",park_names,"/")), showWarnings = FALSE)
    dir.create(file.path(paste0(getwd(),"/", path, "/", park_names,"/")), showWarnings = FALSE)
    
    aoi <- filter(aois, UNIT_CODE == park_names)
    
    # download flowlines for entire aoi
    nhd_flowlines <- nhdplusTools::get_nhdplus(AOI = aoi, 
                                               realization='flowline')
    
    # add `tocomid` field to ID flowlines that cross over the aoi
    # this step massively speeds up run time by reducing the number
    # of watersheds that need to be created
    park_flowlines <- nhd_flowlines  %>%
      dplyr::select(-id) %>%
      dplyr::distinct(comid, .keep_all=TRUE) %>%
      nhdplusTools::get_tocomid(., add=TRUE)
    
    # minimize number of origin points by selecting only those that cross park boundary
    outsiders <- park_flowlines %>%
      filter(tocomid==0) %>%
      tibble::rowid_to_column(., "index")
    
    # FUNCTION THAT, FOR EVERY POUR POINT, IDENTIFIES ALL UPSTREAM AND DOWNSTREAM FLOWLINES
    watersheds <- function(spid_union){
      
      tracer <- function(samples){
        outsiders <- as_tibble(outsiders)
        outlet <- outsiders %>%
          dplyr::filter(index == samples)
        upstream <- nhdplusTools::get_UT(nhd, outlet$comid) %>% #upstream trace function in nhdplusTools
          tibble::as_tibble() %>%
          dplyr::rename(comid_list = value) 
        distinct(upstream, comid_list, .keep_all = TRUE) %>%
          dplyr::filter(comid_list != outlet$comid) #%>%
      }
      
      ws <- map_dfr(spid_union, tracer)
    
      }
    
    park_ws <- outsiders %>%
      dplyr::mutate(comid_list = map(index, watersheds)) %>%
      unnest(cols = comid_list) %>%
      distinct(comid_list) %>%
      sf::st_drop_geometry() %>%
      rename(comid = comid_list)
    
    splitter_flowlines <- nhdplusTools::get_nhdplus(comid = outsiders$comid,
                                                    realization='flowline',
                                                    t_srs = 4269) %>%
      select(comid)
    
    nhd_flowlines <- nhdplusTools::get_nhdplus(comid = park_ws$comid,
                                               realization='flowline',
                                               t_srs = 4269) %>%
      dplyr::select(comid) %>%
      bind_rows(splitter_flowlines) %>%
      distinct(comid,.keep_all=TRUE)
    
    grouper <- igraph::components(igraph::graph.adjlist(sf::st_touches(nhd_flowlines)))[[1]] %>%
      as_tibble() %>%
      rename(relationship=value)
    
    grouped_flowlines <- cbind(nhd_flowlines,grouper) %>% 
      group_by(relationship) %>%
      summarize()
    
    # include catchments in the park that are not contained in trace (no NHD flowlines associated with them)
    nhd_empty_catchments <- nhdplusTools::get_nhdplus(AOI = aoi, 
                                                      realization='catchment') %>%
      distinct(featureid, .keep_all=TRUE) %>%
      filter(!featureid %in% nhd_flowlines$comid) %>%
      filter(!featureid %in% outsiders$comid) %>%
      mutate(relationship = 0) %>%
      select(relationship) %>%
      sf::st_intersection(.,aoi)
    
    splitters <- nhdplusTools::get_nhdplus(comid = outsiders$comid,
                                           realization = 'catchment',
                                           t_srs = 4269) %>%
      sf::st_intersection(.,aoi)
    
    grouped_catchments <- nhdplusTools::get_nhdplus(comid = park_ws$comid,
                                                    realization = 'catchment',
                                                    t_srs = 4269) %>%
      filter(!featureid %in% outsiders$comid) %>%
      bind_rows(splitters) %>%
      sf::st_join(.,grouped_flowlines) %>%
      group_by(relationship) %>%
      summarize() %>%
      bind_rows(nhd_empty_catchments) %>%
      summarize() %>%
      nngeo::st_remove_holes() %>%
      sf::st_join(aoi) 
    
    if(save == TRUE){
      st_write(grouped_catchments, paste0(getwd(),"/", path, "/", park_names,"/", park_names, "_watersheds.shp"), 
               append = FALSE)
    }
    
    print(paste0(park_names, " watershed delineated!"))
    
    return(grouped_catchments)
  }
  
  obj <- map(park_names,multiple) %>%
    bind_rows()
  
  return(obj)
  
}