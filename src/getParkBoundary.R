#' Import park boundary shapefile
#' 
#' This function uses the Data Store REST Api to download national park boundary 
#' shapefiles. 
#' Use this resource to get updated REST URL: https://irmaservices.nps.gov/datastore/v6/documentation/datastore-api.html#/Search_By_Reference_Code/ReferenceCodeSearch_Get
#' And this to find updated download links: https://irma.nps.gov/DataStore/Reference/Profile/2224545?lnv=True 
#' 
#' @param park The 4 digit national park code(s) for parks of interest
#' @param save Whether to save (TRUE) the resulting shapefile or not (FALSE)
#' @param path If `save = TRUE`, the file path to save the shapefile
#' 
#' @return A spatial sf object for each specified park boundary
getParkBoundary <- function(park, save = FALSE, path = NULL){
  
  call <- "https://irmaservices.nps.gov/datastore/v6/rest"
  
  #pull resource ID using reference ID of the park boundaries landing page (this no longer has download link...)
  # downloadLink <- httr::GET(paste0(call, "/ReferenceCodeSearch?q=2301261")) %>% 
  #   httr::content("text", encoding = "UTF-8") %>% 
  #   jsonlite::fromJSON(.,flatten = TRUE) %>% 
  #   dplyr::as_tibble() %>% 
  #   filter(str_detect(fileName, "nps_boundary")) %>% 
  #   pull(downloadLink)
  downloadLink <- "https://irmaservices.nps.gov/datastore/v6/rest/DownloadFile/693182"
  
  #download boundary 
  temp1 <- tempfile()
  download.file(downloadLink, destfile = temp1, method = "curl")
  temp2 <- tempfile()
  unzip(temp1, exdir = temp2)
  
  #sf::sf_use_s2(FALSE)
  
  parks <- sf::st_read(paste0(temp2, "/Administrative_Boundaries_of_National Park_System_Units.gdb"))
  
  poi <- parks %>% 
    dplyr::filter(UNIT_CODE %in% park) %>%
    dplyr::group_by(UNIT_CODE,REGION,STATE) %>%
    dplyr::summarize()
    
  if(save == TRUE){
    
    for (i in 1:length(park)){
      
      #create park folder if does not already exist in 'path'
      if (!dir.exists(paste0(path, park[i]))) {
        
        dir.create(paste0(path, park[i]))
      }
      
      # save park boundary in park folder
      sf::write_sf(filter(park_boundary, UNIT_CODE == park[i]), 
                   paste0(path, park[i], "/park_boundary_", park[i], ".shp"))
      
      
    }
    
    print(paste("Park boundary shapefiles saved to respective folders in:", paste0(getwd(), "/", path)))
    
  }
  
  return(poi)
  
}
