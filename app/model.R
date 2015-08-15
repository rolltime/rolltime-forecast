#
# Model what time I would have to
# leave to catch a bike or a dock.
#
#
# Author: Luis Capelo | luiscape@gmail.com
#

library(dplyr)
library(RJSONIO)

source('scripts/R/helpers/read_table.R')
source('scripts/R/helpers/write_table.R')
source('scripts/R/models/neural_network.R')


#
# Generate forecast data.
#

#' @get /forecast
GenerateForecastOutput <- function() {

  #
  # Load original data.
  #
  df <- ReadTable('processed', deploy=FALSE)
  
  #
  # Load list of stations.
  #
  station_list <- sapply(fromJSON('data/station_meta.json'), function(x) x$id)
  
  #
  # Forecast table.
  #
  for (i in 1:length(station_list)) {
    it <- try(FitNeuralNetworkModel(data=df, station_id=station_list[i], production_model=TRUE))
    if (i == 1) forecast_data <- it
    else forecast_data <- rbind(forecast_data, it)
  }
  
  #
  # Writing into db.
  #
  WriteTable(as.data.frame(forecast_data), 'forecast', overwrite=TRUE)
  return(forecast_data)
  
}
