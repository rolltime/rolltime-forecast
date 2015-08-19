#
# Auto-arima forecasting model.
#
# Author: Luis Capelo luiscape@gmail.com
#

# library(dplyr)
library(caret)
library(forecast)

#
# Loading helper functions.
#
source(paste0(getwd(), '/app/helpers/read_station_day.R'))

#' @get /forecast
FitArimaModel <- function(
  data=NULL,
  station=72,
  object=NULL,
  forecast_minutes=120,
  train_set_size=.80,
  production_model=TRUE
  ) {

  if (is.null(object)) stop('No forecasting object provided.')
  if (is.null(station)) stop('No station id provided.')
  if (is.null(forecast_minutes)) warn('N forecasting minutes not provided.')


  cat('Calculating ARIMA model ...')
  #
  # Load data from DB.
  # Needs to be improved: only load
  # data that matches a single day.
  #
  # With data: 2015-07-06
  # Object: availableBikesRatio
  #
  if (is.null(data)) {
    data <- ReadStationDay(day='2015-07-06', column=object, station_id=station)
  }

  #
  # Organizing the model data.frame
  #
  model_data = data$availableBikesRatio
  time_series_data <- msts(model_data, start=1, ts.frequency=1440, seasonal.periods=c(1440,7))


  if (production_model) {

    #
    # Function that fills
    # time labels based on
    # input data.
    #
    last_minute <- max(as.POSIXct(data$executionTime))
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
    # Calculating model.
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
      # Training the model.
      #
      inTrain <- createDataPartition(time_series_data, p=train_set_size, list=FALSE)
      train <- time_series_data[inTrain]
      test <- time_series_data[-inTrain]
      model_fit <- auto.arima(train, parallel=FALSE, stepwise=TRUE)

      #
      # Terminal: "sysctl -n hw.ncpu" to see the number of cores.
      #

      #
      # Measuring model.
      #
      forecast_results <- forecast(model_fit, h=forecast_minutes)
      model_accuracy <- data.frame(accuracy(forecast_results, test))

      #
      # Adding information.
      #
      model_accuracy$sets <- c('train', 'test')
      model_accuracy$name <- 'ARIMA'
      model_accuracy$station_id <- station_id

      cat(' done.\n')
      return(model_accuracy)
    }
}
