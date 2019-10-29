


library(RSQLite)
library(tidyverse)
library(lubridate)


DB.NAME <- 'data/air_challenge.db'


connect_to_db <- function() {
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), DB.NAME)
    return(con)
  },
  error = function(cond) {
    message(paste("could open DB connection", DB.NAME))
    return(NULL)
  })
}

read_df_from_db <- function(tableName) {
  con <- connect_to_db()
  
  tryCatch({
    df <- (dbReadTable(con, tableName))
    dbDisconnect(con)
    
    
    return (df)
  },
  error = function(cond) {
    message(paste0("Database Table does not seem to exist:",tableName))
    
    return(NULL)
  })
}

writeToDB <- function(df, tableName, overwrite = F,append=T) {
  con <- connect_to_db()
  dbWriteTable(con, tableName, df, overwrite = overwrite,append=T)
  dbDisconnect(con)
}

list_tables_from_DB <- function() {
  con <- connect_to_db()
  table.list <- dbListTables(con)
  dbDisconnect(con)
  return (table.list)
}

load.luftdaten.data <- function(sensors_on_location) {
  print(sensors_on_location)
  con <- connect_to_db()
  
  df_luftdaten <-
    dbGetQuery(
      con,
      'SELECT * FROM sensors_luftdaten_data WHERE sensor_id IN (:x)',
      params = list(x = sensors_on_location)
    )
  
  dbDisconnect(con)
  return(df_luftdaten)
}


load.luftdaten.data.datetime <- function(date) {
  con <- connect_to_db()
  
  df_luftdaten <-
    dbGetQuery(
      con,
      'SELECT d.timestamp_gmt,s.location_id,s.location_latitude,s.location_longitude,d.value FROM sensors_luftdaten s inner join (SELECT * FROM sensors_luftdaten_data WHERE datehour LIKE :x AND variable LIKE "P1") d on d.sensor_id=s.sensor_id',
      params = list(x = paste0(date, '%'))
    )
  
  dbDisconnect(con)
  return(df_luftdaten)
}


drop.table <- function(tableName) {
  con <- connect_to_db()
  dbRemoveTable(con, tableName)
  dbGetQuery(con, 'VACUUM;')
  dbDisconnect(con)
  
}
