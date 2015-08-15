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
                      db = 'scraperwiki',
                      deploy = FALSE,
                      verbose = FALSE) {

  #
  # Sanity check.
  #
  if(is.null(df) == TRUE) stop("Don't forget to provide a data.frame.")
  if(is.null(table_name) == TRUE) stop("Don't forget to provide a table name.")
  if(is.null(db) == TRUE) stop("Don't forget to provide a data base name.")

  if (verbose) message('Read data from a database.')

  #
  # TODO: improve solution for finding relative paths
  #       when path is defined by a second script call.
  #
  dbPath <- function(prod=deploy) {
    local_path = "/Users/luis/Documents/Programming/rolltime/rolltime-app/"
    prod_path = "~/check-my-bike/"
    if (prod) return(prod_path)
    else return(local_path)
  }

  db_name <- paste0(dbPath(), db, ".sqlite")

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
