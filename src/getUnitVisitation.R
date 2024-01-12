#' Retrieve NPS park visitation data
#' 
#' This function uses the IRMA STATS REST Api to pull visitation data for Nation Parks
#' 
#' @param units The park unit(s) 4 digit code to get visitation for. Can supply a string of park codes or single code.
#' @param startMonth The start month for visitation data, values 1-12. Default is 1 (January).
#' @param startYear The start year for visitation data. 
#' @param endMonth The end month for visitation data, values 1-12. Default is 12 (December).
#' @param endYear The end year for visitation data.
#' 
#' @return A single dataframe/tibble of visitation statistics for specified parks.
#' Dataframe contains visitation numbers for each month of each year.
getUnitVisitation <- function(
                          units, 
                          startMonth = 1,
                          startYear,
                          endMonth = 12,
                          endYear) {
  
  #base URL call
  call <- "https://irmaservices.nps.gov/v3/rest/stats"
    
    units <- paste0(units, collapse = ",")
    
    dat <- httr::GET(paste0(call, "/visitation?unitCodes=", units, "&startMonth=",
                             startMonth, "&startYear=", startYear, "&endMonth=",
                             endMonth, "&endYear=", endYear))
    
    
    # convert content to text
    dat_text <- httr::content(dat, "text", encoding = "UTF-8")
    
    #parse data in JSON
    dat_json <- jsonlite::fromJSON(dat_text, flatten = TRUE)
    
    #convert items to data.frame
    final_df <- as_tibble(dat_json)
  
  return(final_df)
  
  
}
