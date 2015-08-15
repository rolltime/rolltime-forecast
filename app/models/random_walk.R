#
# Random walk forecasting model.
#
# Author: Luis Capelo  luiscape@gmail.com
#

library(MASS)
library(dplyr)
library(forecast)

#
# Loading helper functions.
#
source('scripts/R/helpers/read_table.R')

FitRandomWalkModel <- function(data=NULL, production_model=FALSE, station_id=445, forecast_minutes=120, train_set_size=.80) {

  #
  # Load data.
  #
  cat('Calculating Random Walk Model model ...')
  # data <- ReadTable('station_processed', deploy=FALSE)
  data <- filter(data, id == station_id)
  
  #
  # Hack to filter the data from the beginning of
  # a day in the database.
  #
  data <- filter(data, as.POSIXct(executionTime) >= as.POSIXct('2015-06-20'))

  #
  # Organizing the model data.frame
  #
  model_data <- data$availableBikesRatio
  time_series_data <- ts(model_data, start=1, frequency=1440)

  if (production_model) {
    
    #
    # Calculate model.
    #
    random_walk_forecast <- rwf(time_series_data, h=forecast_minutes, drift=FALSE)

    #
    # Generate plot.
    #
    return(random_walk_forecast)
  }

  else {
    
    #
    # Training the model.
    #
    train_data <- time_series_data[1:round(length(time_series_data) * train_set_size)]
    test_data <- time_series_data[(round(length(time_series_data) * train_set_size) + 1):length(time_series_data)]
    random_walk_forecast <- rwf(train_data, h=forecast_minutes, drift=FALSE)
    forecast_bikes <- forecast(random_walk_forecast, h=forecast_minutes)
    random_walk_rmse <- accuracy(forecast_bikes, test_data)[,2]
    
    cat(' done.\n')
    return(data.frame(random_walk_rmse))
  }

}
