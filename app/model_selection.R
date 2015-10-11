#
# Analyze model results
#
# Author: Luis Capelo | capelo@un.org
#

library(dplyr)


#
# Loading local model result data.
#
d = read.csv('model_results_80.csv')
f = filter(d, Sets == 'Test set')
f$value = as.numeric(as.character(f$value))

#
# Select the best model
# for each bike.
#
best_model <- f %>%
               group_by(variable) %>%
               group_by(station_id) %>%
               filter(value == max(value))


#
# Let's check the summary.
#
summary(best_model)

#
# All models have similar variance of about 10% of error.
#
best_model %>%
  group_by(variable) %>%
  summarize(
    n = n(),
    sd = sd(value),
    mean = mean(value)
    )


#
# Let's explore the worst performers.
#
worst_performers <- filter(best_model, value >= 0.2)

worst_performers %>%
  group_by(variable) %>%
  summarize(
    n = n(),
    sd = sd(value),
    mean = mean(value)
  )
