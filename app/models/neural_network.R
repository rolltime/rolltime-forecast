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

library(dplyr)
library(caret)
library(forecast)


FitNeuralNetworkModel <- function(data=NULL,
                                  production_model=FALSE,
                                  station_id=NULL,
                                  forecast_minutes=NULL,
                                  train_set_size=.80) {

  #
  # Load data.
  #
  cat('Calculating Neural Networks Model ...')
  if (is.null(data) == TRUE) {
    data <- ReadStationData(station_id=station_id)
  }

  #
  # Hack to filter the data from the beginning of
  # a day in the database.
  #
  data <- filter(data, as.POSIXct(executionTime) >= as.POSIXct('2015-06-20'))

  #
  # Organizing the model data.frame
  #
  model_data <- data$availableBikesRatio
  time_series_data <- msts(model_data, start=1, seasonal.periods=c(1440, 7), ts.frequency=1440)

  if (production_model) {

    #
    # Calculate model.
    #
    model_fit <- nnetar(time_series_data, repeats=25)
    results <- forecast(model_fit, h=forecast_minutes)
    cat(' done.\n')
    return(results)

  }

  #
  # Returning the model for train sets only.
  #
  else {

    #
    # Training the model.
    #
    inTrain <- createDataPartition(time_series_data, p=train_set_size, list=FALSE)
    train <- time_series_data[inTrain]
    test <- time_series_data[-inTrain]
    model_fit <- nnetar(train, repeats=25)

    #
    # Measuring model.
    #
    forecast_results <- forecast(model_fit, h=forecast_minutes)
    model_accuracy <- data.frame(accuracy(forecast_results, test))
    model_accuracy$sets <- c('train', 'test')
    model_accuracy$name <- 'NEURAL'
    model_accuracy$station_id <- station_id

    cat(' done.\n')
    return(model_accuracy)
  }
}
