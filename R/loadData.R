
library(RSQLite)
library(tidyverse)
library(lubridate)


DB.NAME <- '../data/luftdaten_dwd.db'


connect_to_db <- function(){
  tryCatch(
    {
      con <- dbConnect(RSQLite::SQLite(), DB.NAME)
      return(con)
    },
    error=function(cond) {
      message(paste("could open DB connection", filename))
      return(NULL)
    }
  )
}

read_df_from_db <- function(tableName){
  con <- connect_to_db()
  
  tryCatch(
    {
      df <- (dbReadTable(con,tableName))
      dbDisconnect(con)
      #df$timestamp <- as.POSIXct(df$timestamp,  origin="1970-01-01", tz = 'CET')
      #df$datetime <- dmy_hm(df$Zeit)
     
      return (df)
    },
    error=function(cond) {
      message(paste("Database Table does not seem to exist:"))
     
      return(NULL)
    }
  ) 
}

list_tables_from_DB <- function(){
  con <- connect_to_db()
  return (dbListTables(con))
}




