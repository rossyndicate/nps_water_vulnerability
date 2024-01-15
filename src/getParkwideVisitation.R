#' Retrieve NPS parkwide visitation data
#' 
#' This function uses the IRMA STATS REST Api to pull visitation data for National Parks
#' 
#' @param startYear The start year for visitation data.
#' @param endYear The end year for visitation data.
#' 
#' @return A single dataframe/tibble of visitation statistics for all National Parks.
#' Dataframe contains visitation numbers for each month of each year.
getParkwideVisitation <- function(startYear, endYear){
  
  visits <- function(year){
    
    # pull in the data
    raw_data <- httr::GET(url = 
                            # parse out year so that it can be chosen with the "year" argument, using paste0()
                            paste0("https://irmaservices.nps.gov/v3/rest/stats/total/", year))
    
    # convert content to text
    extracted_data <- httr::content(raw_data, as = "text", encoding = "UTF-8") 
    
    # parse text from JSON to data frame
    final_data <- jsonlite::fromJSON(extracted_data)
    
    return(final_data)
  }
  
  years <- (startYear:endYear)
  
  parkwide <- years %>% 
    map(~ visits(year = .)) %>% 
    bind_rows()
  
  return(parkwide)
}
