#
# Auto-arima forecasting model.
#
# Author: Luis Capelo luiscape@gmail.com
#

# library(MASS)
# library(dplyr)
library(forecast)

#
# Loading helper functions.
#
source(paste0(getwd(), '/app/helpers/read_station_day.R'))

#' @get /forecast
FitArimaModel <- function(data=NULL, production_model=TRUE, station=445, forecast_minutes=120, train_set_size=.80) {


  cat('Calculating ARIMA model ...')
  #
  # Load data from DB.
  # Needs to be improved: only load
  # data that matches a single day.
  #
  data <- ReadStationDay(day='2015-07-06', station_id=station)

  #
  # Organizing the model data.frame
  #
  model_data = data$availableBikesRatio
  last_minute <- max(as.POSIXct(data$executionTime))
  time_series_data <- ts(model_data, start=1, frequency=1440)


  if (production_model) {
    
    FillTimes <- function(data=NULL) {
      minutes = c()
      for (i in 1:forecast_minutes) {
        minute = format(last_minute + (i * 60), '%Y-%m-%d %H:%M')
        minutes = c(minutes, minute)
      }
      return(
        data.frame(
          station = station, 
          availableBikesRatio = data$"Point Forecast",
          time = minutes
        )
      )
    }

    #
    # Making the model.
    #
    bikes_arima <- auto.arima(time_series_data)
    elapsed_time <- system.time(
      forecast_bikes <- forecast.Arima(bikes_arima, h=forecast_minutes)
      )[3]

    #
    # Making plot
    #
    cat(' done.\n')

    output <- list(
      success = TRUE,
      meta = data.frame(
        object = "availableBikesRatio",
        minutes = forecast_minutes,
        model = "ARIMA",
        algorithm = "forecast 6.1",
        accuracy = 0.68,  # needs to come from daily analysis.
        performance = as.numeric(elapsed_time)  # elapsed seconds.
      ),
      results = FillTimes(as.data.frame(forecast_bikes))
    )
    return(output)

  } else {

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
