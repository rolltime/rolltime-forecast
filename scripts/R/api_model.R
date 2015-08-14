#
# Run the model on an API.
#
#
# Author: Luis Capelo | luiscape@gmail.com
#

library(dplyr)
library(rapier)

source('scripts/R/helpers/read_table.R')

RunAPI <- function(prepare_data=FALSE, api_port=8000) {

  #
  # Preparing data.
  #
  if (prepare_data) ProcessData()

  #
  # Load data.
  #
  data <- ReadTable('station_processed', deploy=FALSE)

  #
  # Setting up API parameters.
  #
  r <- rapier('scripts/R/model.R')
  r$run(port=api_port)
}

RunAPI()
