#
# Read tables from a SQLite database.
# Designed to work with ScraperWiki environments.
#
# Author: Luis Capelo | capelo@un.org
#

library(sqldf)
library(RPostgreSQL)


#
# Reads a table form a SQLite databse into a data.frame.
# By default, it will read form a scraperwiki.sqlite
# database in the root folder where the
# script was called.
#
ReadStationDay <- function(
                      db = '/data/historic',
                      column = NULL,
                      day = NULL,
                      station_id = NULL,
                      deploy = FALSE,
                      verbose = TRUE) {

  #
  # Sanity check.
  #
  if (is.null(db) == TRUE) stop('No database name provided.')
  if (is.null(day) == TRUE) stop('No day provided.')
  if (is.null(column) == TRUE) stop('No column name provided.')
  if (is.null(station_id) == TRUE) stop('No station id provided.')

  if (verbose) message('Read data from a database.')

  #
  # Connecting to the PostgreSQL instance.
  #
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(
    drv, 
    dbname="rolltime",
    host="postgres",
    port=5432,
    user="rolltime",
    password="rolltime"
    )

  #
  # Read values.
  #
  s = paste0("SELECT ", column, ",executionTime FROM metric WHERE day='", day, "' AND id=", station_id)
  if (verbose) { print(s) }
  data <- dbGetQuery(con, s)

  #
  # Disconnect and return data.frame.
  #
  dbDisconnect(con)
  if (verbose) message(paste0('Done! '), nrow(data), ' records read successfuly.')
  return(data)
}
