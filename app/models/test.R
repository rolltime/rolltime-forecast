#
# Test for the plumber API service.
#


#' @get /mean
normalMean <- function(samples=10){
  data <- rnorm(samples)
  data <- data.frame(data = data, id = c('a','b'))
  return(data)
}
