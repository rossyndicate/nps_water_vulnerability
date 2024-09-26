#' this function calculates aquifer volume for a given depth to water (dtw) 
#' assuming the aquifer takes the form of a triangular shaped prism, i.e., 
#' an alluvial or valley aquifer. 
#' Inputs:
#' @param aquifer_extent sf_shapefile of the aquifer extent when full 
#' @param full_thickness_ft Thickness of the deepest part of the aquifer
#' @param theta_deg The angle between the aquifer ground surface and the plung-
#' ing aquifer sides   
#' @param dtw_ft The depth to groundwater in feet
#' @param the specific yield of the aquifer
#' 
#' Returns:
#' @param water_volume The volume of water in the aquifer in cubic feet

aquifer_volume <- function(dtw_ft,
                           aquifer_extent = alluvial_main_stem,
                           full_thickness_ft = 60,
                           theta_deg = 10,
                           sy = 0.2) {
  
  # Test inputs
  # aquifer_extent <- alluvial_main_stem
  #full_thickness_ft <- 30 # ft
  # theta_deg <- 10 # degrees
  # dtw_ft <- 10
  # sy <- .2
  
  if(is.na(dtw_ft)) {
    return(NA)
  } else {
    
    dtw_ft <- abs(dtw_ft) # often dtw is negative by convention even though
                          # it should be positive. Check this if applied
                          # anywhere other than BRCA
    
    # Calculate buffer distance for aquifer surface -- as the groundwater table
    # deepens, the effective "area" of the aquifer shrinks. This uses the approx.
    # valley angle reported by Marine (1962) to calculate the effective decrease
    # in aquifer width for a change in groundwater depth.
    buffer_length <- dtw_ft/tan(theta_deg*pi/180) 
    
    # Use that buffer distance to remove buffer distance from the original
    # aquifer extent.
    new_aquifer_extent <- 
      suppressMessages(
        suppressWarnings(
          st_buffer(aquifer_extent, 
                    buffer_length/-366002.44))) # ft to decimal degrees
    
    # Calculate the area of the new aquifer extent in ft2
    new_aquifer_area <- 
      as.numeric(st_area(new_aquifer_extent)) * 10.764 # m2 to ft2
    
    # Calculate the total "solid" volume of the new aquifer assuming a 
    # triangular prism.
    new_aquifer_volume <- 
      0.5 * new_aquifer_area * (full_thickness_ft - dtw_ft)
    
    # Calculate the water volume in the aquifer assuming a given specific yield.
    water_volume <- 
      new_aquifer_volume * sy 
    
    return(water_volume)
    
  }
}

#test <- well_data %>%
#  dplyr::ungroup() %>%
#  dplyr::filter(well == "Well 1") %>%
#  dplyr::mutate(aq_volume_ft3 = 
#                  map_dbl(static_ft_c, ~aquifer_volume(dtw_ft = .)))