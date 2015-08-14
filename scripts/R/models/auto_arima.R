#
# Auto-arima forecasting model.
#
# Author: Luis Capelo luiscape@gmail.com
#

library(MASS)
library(dplyr)
library(forecast)

#
# Loading helper functions.
#
source('scripts/R/helpers/read_table.R')

FitArimaModel <- function(data=NULL, production_model=FALSE, station_id=445, forecast_minutes=120, train_set_size=.80) {

  #
  # Load data.
  #
  cat('Calculating ARIMA model ...')
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
  model_data = data$availableBikesRatio
  time_series_data <- ts(model_data, start=1, frequency=1440)
  

  if (production_model) {
    
    #
    # Making the model.
    #
    bikes_arima <- auto.arima(time_series_data)
    forecast_bikes <- forecast.Arima(bikes_arima, h=forecast_minutes)

    #
    # Making plot
    #
    cat(' done.\n')
    return(forecast_bikes)
  }


  else {
    
    #
    # Testing the model.
    #
    train_data <- time_series_data[1:round(length(time_series_data) * train_set_size)]
    test_data <- time_series_data[(round(length(time_series_data) * train_set_size) + 1):length(time_series_data)]
    bikes_arima <- auto.arima(train_data)
    forecast_bikes <- forecast.Arima(bikes_arima, h=forecast_minutes)
    arima_rmse <- accuracy(forecast_bikes, test_data)[,2]
    
    cat(' done.\n')
    return(data.frame(arima_rmse))
  }

}
