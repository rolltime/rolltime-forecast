#
# Testing the predictive models.
#
# Author: Luis Capelo  luiscape@gmail.com
#

library(dplyr)
library(reshape2)

#
# Importing models.
#
source('scripts/R/models/tbats.R')
source('scripts/R/models/auto_arima.R')
source('scripts/R/models/random_walk.R')
source('scripts/R/models/neural_network.R')
source('scripts/R/models/exponential_smoothing.R')

#
# Load helpers.
#
source('scripts/R/helpers/read_table.R')
source('scripts/R/helpers/write_table.R')

MeasureModels <- function(
  df=NULL,
  filter_id=NULL,
  print_evaluation=TRUE,
  minutes=NULL
  ) {

  if (is.null(df) == FALSE) { df <- filter(df, id==filter_id) }

  #
  # Models used.
  #
  ARIMA = FitArimaModel(data=df, station_id=id, forecast_minutes=minutes)
  NEURAL = FitNeuralNetworkModel(data=df, station_id=id, forecast_minutes=minutes)
  TBATS = FitTbatsModel(data=df, station_id=filter_id, forecast_minutes=minutes)
  RANDOMW = FitRandomWalkModel(data=df, station_id=id, forecast_minutes=minutes)
  EXPONENTIAL = FitExponentialModel(data=df, station_id=id, forecast_minutes=minutes)

  model_results <- rbind(
    ARIMA,
    NEURAL,
    RANDOMW,
    TBATS,
    EXPONENTIAL
    )


  #
  # Selecting the best model
  #
  best_model <-  melt(model_results, id.vars=c("sets", "station_id", "name")) %>%
    group_by(variable) %>%
    filter(sets == "test", variable == 'RMSE') %>%
    filter(value == min(value))

  #
  # Running results.
  #
  if (print_evaluation) print(paste('The best model is', best_model$name))
  out <- melt(model_results, id.vars=c("sets", "station_id", "name"))
  return(out)
}

RepeatModelTests <- function(sample_size=NULL, all_stations=FALSE, summarize_results=TRUE) {

  #
  # Randomizing the selection of station ids.
  #
  data <- ReadTable('station_processed', deploy=FALSE)
  station_ids = unique(data$id)
  if (all_stations) { station_id = station_ids }
  else {
    if (!is.numeric(sample_size)) { stop('Provide an integer to the fetch_samples argument.') }
    station_id = sample(station_ids, sample_size)
    }

  #
  # Iterating over the station ids.
  #
  na_result <- data.frame(variable = NA,
                          n_stations_evaluated = NA,
                          mean_rmse = NA,
                          sd_rmse = NA,
                          total_error_rmse = NA)
  t = length(station_id)
  for (i in 1:t) {
    cat('-----------------------------------\n')
    cat(paste0('Testing models: ', i, '/', t, '\n'))
    cat('-----------------------------------\n')
    it <- try(MeasureModels(df=data, filter_id=as.numeric(station_id[i]), print_evaluation=TRUE, minutes=120))
    if (i == 1) results = it
    else results = rbind(results, it)
  }

  #
  # Summarize results.
  #
  if (summarize_results) {
    grouped_output <- group_by(filter(results, Sets == 'Test set'), variable)
    grouped_output$value <- as.numeric(grouped_output$value)
    grouped_output <- summarise(grouped_output,
                                n_stations_evaluated = n(),
                                mean_rmse = mean(value, na.rm = TRUE),
                                sd_rmse = sd(value, na.rm = TRUE),
                                total_error_rmse = sum(value, na.rm = TRUE))

    return(grouped_output)
  }

  return(results)
}

#
# Measuring the best models.
#
MeasureResults <- function(df=NULL) {
  df$value <- as.numeric(df$value)
  results <- df %>%
    group_by(name) %>%
    filter(sets == 'test', value != Inf, variable == 'RMSE') %>%
    filter(value < 2*sd(value, na.rm=TRUE), value > 2 * (-1*sd(value, na.rm=TRUE))) %>%
    summarize(
      n = n(),
      mean = mean(value,na.rm=TRUE),
      sd = sd(value, na.rm=TRUE),
      max = max(value, na.rm=TRUE),
      min = min(value,na.rm=TRUE)
    )
  return(results)
}


model_results <- as.data.frame(RepeatModelTests(all_stations=TRUE, summarize_results=FALSE))
model_analysis <- MeasureResults(model_results)
write.csv(model_analysis, 'model_analysis_neural.csv', row.names=FALSE)
write.csv(model_analysis, 'model_results_neural.csv', row.names=FALSE)



#
# Cretes a histogram with the RMSE
# accuracy measurements of each model.
# Based on that informaion, ARIMA seems
# to be most appropriate, being almost 2 times
# more accuracy than the TBATS model.
#
chart_data <- filter(model_results, sets=='test', variable=='RMSE', value!=Inf)
chart_data$value <- as.numeric(chart_data$value)
s = sd(chart_data$value, na.rm=TRUE)
chart_data <- filter(chart_data, value < 1, value > 2*(-1*s))
ggplot(chart_data) + theme_bw() +
  geom_histogram(aes(value, fill=name), binwidth=.02) +
  facet_wrap(~ name)
