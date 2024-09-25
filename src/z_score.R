#' z_score - this function converts a vector (x) to the z-score 
#' @param x vector representing variable through time at regular interval
#' @returns zscore of x 
z_score <- function(x) {
  # if NA values, fill. If fist
  # val is NA, replace with first 
  # non-NA value.
  if (is.na(x[1])) {
    x[1] <- first(x[!is.na(x)])
  }
  x <- zoo::na.approx(x, na.rm = FALSE)
  mean_x <- mean(x, na.rm = TRUE)
  stdev_x <- sd(x, na.rm = TRUE)
  zscore <- (x - mean_x)/stdev_x
}
