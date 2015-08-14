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
source('scripts/R/models/auto_arima.R')
source('scripts/R/models/random_walk.R')
source('scripts/R/models/neural_network.R')
source('scripts/R/helpers/read_table.R')
source('scripts/R/helpers/write_table.R')

MeasureModels <- function(df=NULL, print_evaluation=TRUE, id=445) {
  
  #
  # Models used.
  #
  models = c(
    FitArimaModel(data=df, station_id=id), 
    FitNeuralNetworkModel(data=df, station_id=id), 
    FitRandomWalkModel(data=df, station_id=id)
    )
  
  #
  # Generating output
  #
  for (i in 1:length(models)) {
    data <- models[i]
    if (i == 1) out <- data
    else out <- cbind(out, data)
  }
  
  #
  # Comparing
  #
  out$sets <- c("Training set", "Test set")
  out <- as.data.frame(out)
  names(out) <- c('Arima RMSE', 'Random Walk RMSE', 'Neural Networks RMSE', 'Sets')
  
  #
  # Selecting the best model
  #
  best_model <-  melt(out, id.vars="Sets") %>%
    group_by(Sets) %>%
    filter(Sets == "Test set") %>%
    filter(value == min(value))
  
  #
  # Running results.
  #
  if (print_evaluation) print(paste('The best model is', best_model$variable))
  
  out <- melt(out, id.vars=c('Sets'))
  out$station_id = id
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
    it <- try(MeasureModels(df=data, id=station_id[i], print_evaluation=TRUE))
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
# Collecting the results of models
# and storing output in database.
#
model_results <- as.data.frame(RepeatModelTests(all_stations=TRUE, summarize_results=FALSE))
write.csv(model_results, 'model_results.csv', row.names=FALSE)
WriteTable(model_results, 'model_results', overwrite=TRUE)
