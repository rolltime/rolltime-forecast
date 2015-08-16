#
# Run the model on an API.
#
#
# Author: Luis Capelo | luiscape@gmail.com
#

library(plumber)

RunAPI <- function(prepare_data=FALSE, api_port=6000, flag='development') {

  cat('--------------------------------\n')
  m = paste('Model API running on:', flag, '\n')
  cat(m)
  cat('--------------------------------\n')

  #
  # Setting up API parameters.
  #
  r <- plumb(paste0(getwd(), '/app/models/auto_arima.R'))
  r$run(port=api_port)
}

args <- commandArgs(trailingOnly = TRUE)
RunAPI(flag=args)
