
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
      message(paste("could open DB connection", DB.NAME))
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
  table.list <- dbListTables(con)
    dbDisconnect(con) 
  return (table.list)
}

load.luftdaten.data <- function(locations_id){
  con <- connect_to_db()
  sensors_on_location <-  dbGetQuery(con, 'SELECT sensor_id FROM sensors_luftdaten WHERE "location_id" in (:x)', 
                                     params = list(x = locations_id)) %>% pull(sensor_id)
  df_luftdaten <- dbGetQuery(con, 'SELECT * FROM sensors_luftdaten_data WHERE sensor_id IN (:x)', 
                             params = list(x = sensors_on_location))
  
  dbDisconnect(con) 
  return(df_luftdaten)
}

drop.table<- function(tableName){
  con <- connect_to_db()
  dbRemoveTable(con,tableName)
 dbGetQuery(con,'VACUUM;')
  dbDisconnect(con) 
  
}




