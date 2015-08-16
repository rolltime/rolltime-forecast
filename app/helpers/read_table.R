#
# Read tables from a SQLite database.
# Designed to work with ScraperWiki environments.
#
# Author: Luis Capelo | capelo@un.org
#

library(sqldf)


#
# Reads a table form a SQLite databse into a data.frame.
# By default, it will read form a scraperwiki.sqlite
# database in the root folder where the
# script was called.
#
ReadTable <- function(table_name = NULL,
                      db = '/data/historic',
                      deploy = FALSE,
                      verbose = TRUE) {

  #
  # Sanity check.
  #
  if(is.null(df) == TRUE) stop("Don't forget to provide a data.frame.")
  if(is.null(table_name) == TRUE) stop("Don't forget to provide a table name.")
  if(is.null(db) == TRUE) stop("Don't forget to provide a data base name.")

  if (verbose) message('Read data from a database.')

  # db_name <- paste0(getwd(), db, ".sqlite")
  db_name <- '/Users/luis/Documents/Programming/rolltime/rolltime-forecast/data/historic.sqlite'

  if (verbose) {
    print(db_name)
  }

  db <- dbConnect(SQLite(), dbname = db_name)

  #
  # Read values.
  #
  data <- dbReadTable(db, table_name)

  #
  # Disconnect and return data.frame.
  #
  dbDisconnect(db)
  if (verbose) message(paste0('Done!'), nrow(data), 'records read successfuly.')
  return(data)
}
