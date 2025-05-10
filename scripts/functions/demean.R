demean <- function(observed) {
  result <- (observed - mean(observed, na.rm = TRUE))
  return(result)
}