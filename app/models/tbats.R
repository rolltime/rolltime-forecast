#
# TBATS model for multi-season forecast.
#
# Author: Luis Capelo | capelo@un.org
#

library(dplyr)
library(caret)
library(forecast)

#
# Loading helper functions.
#
source('scripts/R/helpers/read_station_data.R')

FitTbatsModel <- function(
                        data=NULL,
                        production_model=FALSE,
                        station_id=NULL,
                        forecast_minutes=NULL,
                        train_set_size=.80) {

  #
  # Load data.
  #
  cat('Calculating TBATS Model ...')
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
    model_fit <- tbats(time_series_data, use.parallel=TRUE)
    results <- forecast(model_fit, h=minutes)
    cat(' done.\n')
    return(results)
  }

  else {

    #
    # Training the model.
    #
    inTrain <- createDataPartition(time_series_data, p=train_set_size, list=FALSE)
    train <- time_series_data[inTrain]
    test <- time_series_data[-inTrain]
    model_fit <- tbats(train, use.parallel=TRUE)

    #
    # Terminal: "sysctl -n hw.ncpu" to see the number of cores.
    #

    #
    # Measuring model.
    #
    forecast_results <- forecast(model_fit, h=forecast_minutes)
    model_accuracy <- data.frame(accuracy(forecast_results, test))
    model_accuracy$sets <- c('train', 'test')
    model_accuracy$name <- 'TBATS'
    model_accuracy$station_id <- station_id

    cat(' done.\n')
    return(model_accuracy)
  }

}
