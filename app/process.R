#
# Process CitiBike station data in order to model
# more effectively its predictions.
#
#
# Author: Luis Capelo | luiscape@gmail.com
#

library(dplyr)
library(sqldf)
library(lubridate)

#
# Handles bug between dplyr and sqldf.
#
setOldClass(c("tbl_df", "data.frame"))


source('scripts/R/helpers/read_table.R')
source('scripts/R/helpers/write_table.R')

#
# Prepare data for modeling.
#
CleanData <- function(df=NULL, transform_minutes=TRUE, verbose=FALSE) {

  #
  # Select variables of interest.
  #
  s = select(df, id, stationName, availableDocks, totalDocks, statusValue, availableBikes, lastCommunicationTime, executionTime)

  #
  # Transforming numeric types.
  #
  s$totalDocks <- as.numeric(s$totalDocks)
  s$availableDocks <- as.numeric(s$availableDocks)
  s$availableBikes <- as.numeric(s$availableBikes)

  #
  # Transforming time.
  # BUG: Weird time transformations. Possibly here.
  #
  s$executionTime <- ymd_hms(s$executionTime)
  s$day <- format(s$executionTime, "%Y-%m-%d")
  s$executionTime <- format(s$executionTime, "%Y-%m-%d %H:%M")
  s$lastCommunicationTime <- as.POSIXct(s$lastCommunicationTime)
  s$week <- week(s$executionTime)
  s$weekDay <- wday(s$executionTime)

  #
  # Adding ratios.
  #
  s$availableDocksRatio <- s$availableDocks / s$totalDocks
  s$availableBikesRatio <- s$availableBikes / s$totalDocks
  # s$availabilityRatio <- s$availableDocksRatio + s$availableBikesRatio
  # s$usageRatio <- 1 - (s$availableDocksRatio + s$availableBikesRatio)
  
  #
  # Clean duplicates.
  #
  s <- s[!duplicated(s), ]
  
  #
  # Fetch the latest data per minute.
  # Pessimistic approach: selects the 
  # lowest number of available bikes
  # ratio per same minute observation.
  #
  b = nrow(s)
  if (transform_minutes) {
    s <- s %>%
      group_by(id, executionTime) %>%
      filter(availableBikes == min(availableBikes)) 
  }
  
  a = nrow(s)
  
  if (verbose) cat(paste0('Rows cleaned: ', round((1-(a/b))*100, 4), '%'))

  return(s)
}

#
# Wrapper.
#
ProcessData <- function() {

  cat('----------------------------\n')
  cat('Preparing data for model.\n')
  cat('----------------------------\n')
  
  #
  # Load.
  #
  cat('Loading data | ')
  data <- ReadTable('station', verbose=FALSE, deploy=FALSE)
  cat('DONE.\n')

  #
  # Process.
  # dplyr has a bug with classes. 
  # Transforming the output of a dplyr function
  # to data.frame is necessary before storing it
  # in a data base.
  #
  # Also, if the MASS package is loaded, the dplyr select()
  # function won't work. You'll need to unload it from the namespace.
  #
  cat('Processing data (this may take a few minutes) | ')
  processed_data <- as.data.frame(CleanData(data, verbose=TRUE))
  ccat('DONE.\n')

  #
  # Store.
  #
  cat('Storing processed data in database | ')
  WriteTable(processed_data, 'metric', overwrite=TRUE)
  cat('DONE.\n')

  cat('----------------------------\n')
}
