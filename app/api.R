#
# Run the model on an API.
#
#
# Author: Luis Capelo | luiscape@gmail.com
#

# library(dplyr)
library(plumber)

# source('app/helpers/read_table.R')

RunAPI <- function(prepare_data=FALSE, api_port=6000, flag='development') {

  m = paste('Model API running on:', flag, '\n')
  cat(m)

  #
  # Preparing data.
  #
  if (prepare_data) ProcessData()

  #
  # Load data.
  #
  # data <- ReadTable('station_processed', deploy=FALSE)

  #
  # Setting up API parameters.
  #
  r <- plumb(paste0(getwd(), '/app/models/test.R'))
  r$run(port=api_port)
}

args <- commandArgs(trailingOnly = TRUE)
RunAPI(flag=args)
