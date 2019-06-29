
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

load.luftdaten.data <- function(sensors_on_location){
  print(sensors_on_location)
  con <- connect_to_db()

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
  
  
  luftdaten.locations <- dbGetQuery(con, 'SELECT DISTINCT location_id,location_latitude,location_longitude FROM sensors_luftdaten')
  dbDisconnect(con) 
  luftdaten.locations <- luftdaten.locations %>% mutate_at(c('location_longitude','location_latitude'),as.numeric)
  return(luftdaten.locations)
}

load.luftdaten.sensors <- function(){
  con <- connect_to_db()
  luftdaten.sensors <- dbGetQuery(con, 'SELECT DISTINCT * from sensors_luftdaten s WHERE s.sensor_id  IN (SELECT DISTINCT sensor_id FROM sensors_luftdaten_data)')
  luftdaten.sensors <- luftdaten.sensors %>% filter(sensor_type_id==14 | sensor_type_id==17)
  
  
  proximity <- read_df_from_db('luftdaten_dwd_nearest_station')
  
  luftdaten.sensors <- merge(luftdaten.sensors,proximity,by='location_id',all.x=T)
  luftdaten.sensors <- luftdaten.sensors%>% mutate_at(c('location_longitude','location_latitude','location_altitude'),as.numeric)
  
  luftdaten.sensors$location_latitude_jitter <- jitter(luftdaten.sensors$location_latitude,factor= 0.01)
  luftdaten.sensors$location_longitude_jitter <- jitter(luftdaten.sensors$location_longitude,factor= 0.01)
  
  luftdaten.sensors <- luftdaten.sensors %>% arrange(distance.to.dwd)
  
  
  
  dbDisconnect(con)
  return (luftdaten.sensors)
}

drop.table<- function(tableName){
  con <- connect_to_db()
  dbRemoveTable(con,tableName)
 dbGetQuery(con,'VACUUM;')
  dbDisconnect(con) 
  
}




