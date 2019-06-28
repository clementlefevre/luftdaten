
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

  
      return (df)
    },
    error=function(cond) {
      message(paste("Database Table does not seem to exist:"))
     
      return(NULL)
    }
  ) 
}

writeToDB<- function(df,tableName,overwrite=T){
  con <- connect_to_db()
    dbWriteTable(con, tableName, df,overwrite=overwrite)
    dbDisconnect(con)
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

load.dwd.data <- function(station_code){
  con <- connect_to_db()
  dwd.data <-  dbGetQuery(con, 'SELECT * from dwd_data_1H WHERE "station_code" LIKE (:x)', 
                                     params = list(x = station_code)) 

  
  dbDisconnect(con) 
  return(dwd.data)
}

load.dwd.stations.geo <- function(pollutant){
  pollutant<- 'PM10'
  print(pollutant)
  con <- connect_to_db()
  dwd.stations.idx <- dbGetQuery(con, 'SELECT  DISTINCT station_code FROM dwd_data_1H WHERE "pollutant" LIKE :x', 
             params = list(x = pollutant)) %>% pull(station_code)
   
  dwd.stations.location <- dbGetQuery(con, 'SELECT  *  FROM stations WHERE "idx" IN (:x)', 
                                      params = list(x = dwd.stations.idx))
  
  dbDisconnect(con) 
  return(dwd.stations.location)
  
}

load.luftdaten.geo <- function(){
  
  con <- connect_to_db()
  luftdaten.locations <- dbGetQuery(con, 'SELECT DISTINCT location_id,location_latitude,location_longitude FROM sensors_luftdaten')
  dbDisconnect(con) 
  luftdaten.locations <- luftdaten.locations %>% mutate_at(c('location_longitude','location_latitude'),as.numeric)
  return(luftdaten.locations)
}

drop.table<- function(tableName){
  con <- connect_to_db()
  dbRemoveTable(con,tableName)
 dbGetQuery(con,'VACUUM;')
  dbDisconnect(con) 
  
}




