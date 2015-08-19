#
# Explore the CitiBike station data to get
# familiar with its properties.
#
#
# Author: Luis Capelo | luiscape@gmail.com
#

library(dplyr)
library(scales)
library(ggplot2)

source('scripts/R/helpers/read_table.R')

#
# Load data.
#
data <- ReadTable('station_processed', deploy=FALSE)

#
# Checking ratios
#
qplot(data$availableDocksRatio)
qplot(filter(data, usageRatio > 0)$usageRatio)

#
# Checking bike ratio per
# observation time.
#
ggplot(filter(data, availableBikesRatio > 0)) +
  geom_bar(aes(availableBikesRatio), stat='bin') +
  facet_wrap(~ executionTime)

#
# Checking a time-series of ratios in
# a single dock.
#
CheckTimeSeries <- function(station_id=NULL) {

  filtered_data <- filter(data, id == station_id)

  filtered_data$bikeDangerLevel <- NA
  filtered_data$bikeDangerLevel <- ifelse(filtered_data$availableBikesRatio >= 0.8, 'low', filtered_data$bikeDangerLevel)
  filtered_data$bikeDangerLevel <- ifelse(filtered_data$availableBikesRatio >= 0.4 &
                                            filtered_data$availableBikesRatio < 0.8
                                          , 'medium', filtered_data$bikeDangerLevel)
  filtered_data$bikeDangerLevel <- ifelse(filtered_data$availableBikesRatio >= 0.0 &
                                            filtered_data$availableBikesRatio < 0.4
                                          , 'high', filtered_data$bikeDangerLevel)

  data <- as.POSIXct(data$executionTime)
  filtered_data$executionTime <- as.POSIXct(filtered_data$executionTime)

  time_filtered_data <- filter(filtered_data, executionTime >= as.POSIXct('2015-06-20'))

  ggplot(time_filtered_data) +
    geom_line(aes(executionTime, availableBikesRatio), stat='identity', size=1.3) +
    # geom_line(aes(executionTime, availableBikes),  stat='identity', size=1.3) +
    scale_x_datetime(breaks=date_breaks("1 hour")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  ggplot(filtered_data) +
    #geom_point(aes(executionTime, availableDocks), stat='identity', size=3) +
    geom_line(aes(executionTime, availableBikesRatio), stat='identity', size=1)
}
