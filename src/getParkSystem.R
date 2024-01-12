#' Import park boundary shapefile
#' 
#' This function uses the Data Store REST Api to download national park boundary 
#' shapefiles. 
#' Use this resource to get updated REST URL: https://irmaservices.nps.gov/datastore/v6/documentation/datastore-api.html#/Search_By_Reference_Code/ReferenceCodeSearch_Get
#' And this to find updated download links: https://irma.nps.gov/DataStore/Reference/Profile/2224545?lnv=True 
#' 
#' @return A spatial sf object for each specified park boundary
getParkSystem <- function(save = FALSE, path = NULL){
  
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
  
  return(parks)
  
}
