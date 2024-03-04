getWaterUseUtah <- function(WRID){

url <- paste0("https://www.waterrights.utah.gov/asp_apps/viewEditPWS/pwsView.asp?SYSTEM_ID=", WRID)

tables <- url %>%
  rvest::read_html() %>%
  rvest::html_nodes('table')

tables_nes <- vector("list", length = length(tables))

for(i in 1:length(tables)){
  
  try(tables_nes[[i]] <- tables[[i]] %>%
        rvest::html_table()
  )
}

tables_tes <- tables_nes %>%
  vctrs::list_drop_empty() %>%
  purrr::keep(., ~ncol(.x) == 15)

return(tables_tes)

}
