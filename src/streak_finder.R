# For a selected variable (var) in `centroid_annual` (df), finds instances where the 
# variable's annual values fall outside the user-selected lower (q1) and upper
# (q2)'s historical quantiles.
# This function returns a data frame listing all instances in the CF's when this occured. 
# And, if there were consecutive instances across years, provides the time 
# frame and number of years it occured for.

streak_finder <- function(df = centroid_annual, var = "runoff_in", q1 = 0.2, q2 = 0.8){

  historic <- df %>%
    dplyr::filter(cf == "Historical") %>%
    dplyr::rename("variable" = var) %>%
    ungroup() %>%
    dplyr::select(variable) %>%
    dplyr::summarize(QLOW = quantile(variable, probs = as.numeric(q1)),
                     QHIGH = quantile(variable, probs = as.numeric(q2)),
                     .groups = "keep")
  
  # Assuming all_raw is already defined as per your provided code
  all_low <- df %>%
    dplyr::rename("variable" = var) %>%
    mutate(low_year = ifelse(variable <= historic$QLOW, 1, 0),
           high_year = ifelse(variable >= historic$QHIGH, 1, 0)) %>%
    split(f = .$cf) %>%
    map(~ .x %>%
          arrange(y) %>%
          mutate(lag_low_year = lag(low_year, default = 0),
                 group = cumsum(low_year != lag_low_year & low_year == 1)) %>%
          filter(low_year == 1) %>%
          group_by(group) %>%
          dplyr::summarize(start_year = min(y),
                           end_year = max(y),
                           streak_length = n(),
                           .groups = "keep") %>%
          arrange(desc(streak_length))) %>%
    imap(~ mutate(.x, cf = .y)) %>%
    bind_rows() %>%
    mutate(variable = var,
           stat = "Low Year Streak") 
  
  all_high <- df %>%
    dplyr::rename("variable" = var) %>%
    mutate(low_year = ifelse(variable <= historic$QLOW, 1, 0),
           high_year = ifelse(variable >= historic$QHIGH, 1, 0)) %>%
    split(f = .$cf) %>%
    map(~ .x %>%
          arrange(y) %>%
          mutate(lag_high_year = lag(high_year, default = 0),
                 group = cumsum(high_year != lag_high_year & high_year == 1)) %>%
          filter(high_year == 1) %>%
          group_by(group) %>%
          dplyr::summarize(start_year = min(y),
                           end_year = max(y),
                           streak_length = n(),
                           .groups = "keep") %>%
          arrange(desc(streak_length))) %>%
    imap(~ mutate(.x, cf = .y)) %>%
    bind_rows() %>%
    mutate(variable = var,
           stat = "High Year Streak")
  
  bound_n_tidied <- bind_rows(all_low, all_high) %>%
    ungroup() %>%
    dplyr::select(cf, variable, stat, start_year, end_year, streak_length) 
  
  return(bound_n_tidied)
  
}
