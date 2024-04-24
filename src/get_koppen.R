get_koppen <- function(aoi, buffer_dist) {
  # This function returns an sf_polygon object (raster) of 
  #' @param park_boundary sf_object of park boundary from getParkBoundary
  #' @param aoi area of interest to return koppen raster, can be park_boundary
  # Path to CONUS-wide koppen climate raster
  #koppen_path <- "data/all/koppen_geiger_tif/1991_2020/koppen_geiger_0p00833333.tif"
  koppen_path <- "data/all/Beck_KG_V1/Beck_KG_V1_present_0p0083.tif"
  
  # Table with Koppen numbers, names, and colors
  #koppen_df <- data.table::fread("data/all/koppen_geiger_tif/Koppen_df.csv") %>%
  koppen_df <- data.table::fread("data/all/Beck_KG_V1/Koppen_df.csv") %>%
    mutate("color" = rgb(r/255,g/255,b/255)) %>%
    mutate("label" = paste0(Koppen_sym, " (", Koppen_num,")"))
  
  
  koppen <- terra::rast(koppen_path) %>% 
            terra::crop(sf::st_bbox(
              sf::st_buffer(aoi, 
                            dist = buffer_dist))) %>% 
            stars::st_as_stars() %>% 
            sf::st_as_sf()
    
  
  names(koppen) <- c('K_vals','geometry')
  
  koppen <- left_join(koppen,koppen_df,
                      by = c("K_vals" = "Koppen_num"))
  
}