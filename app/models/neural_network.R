#
# Neural Networks Model
#
# Author: Luis Capelo luiscape@gmail.com
#
#
# Auto-arima forecasting model.
#
# Author: Luis Capelo luiscape@gmail.com
#

library(MASS)
library(forecast)


FitNeuralNetworkModel <- function(data=NULL, production_model=TRUE, station_id=433, forecast_minutes=1440, train_set_size=.80) {
  
  #
  # Load data.
  #
  cat(paste0('Calculating Neural Networks model for station id ', station_id, ' ...'))
  data <- filter(data, id == station_id)
  
  #
  # Hack to filter the data from the beginning of
  # a day in the database.
  #
  data <- filter(data, as.POSIXct(executionTime) >= as.POSIXct('2015-06-20'))
  latest_minute <- max(as.POSIXct(data$executionTime))
  minutes_array <- list()
  for (i in 1:forecast_minutes) {
    m = c(latest_minute + (60*i))
    if (i == 1) minutes_array <- m
    else minutes_array <- c(minutes_array, m)
  }
  
  #
  # Organizing the model data.frame
  #
  model_data = data$availableBikesRatio
  time_series_data <- ts(model_data, start=1, frequency=1440)

  
  #
  # Making the model.
  #
  if (production_model) {
    fit = nnetar(time_series_data, repeats=25)
    forecast_data <- forecast(fit, h=1440)
    
    cat(' done.\n')
    df_output <- data.frame(
      a = station_id,
      b = data.frame(forecast_data)[1],
      c = format(minutes_array, "%Y-%m-%d %H:%M"),
      d = format(minutes_array, "%Y-%m-%d")
    )
    names(df_output) <- c('id','availableBikesRatio', 'executionTime', 'day')
    row.names(df_output) <- NULL
    return(df_output)
  }
  
  #
  # Returning the model for train sets only.
  #
  else {

    train_data <- time_series_data[1:round(length(time_series_data) * train_set_size)]
    test_data <- time_series_data[(round(length(time_series_data) * train_set_size) + 1):length(time_series_data)]
    bikes_neural_nets <- nnetar(train_data)
    forecast_bikes <- forecast(bikes_neural_nets, h=forecast_minutes)
    neural_net_rmse <- accuracy(forecast_bikes, test_data)[,2]
    
    cat(' done.\n')
    return(data.frame(neural_net_rmse))  
  }
}

