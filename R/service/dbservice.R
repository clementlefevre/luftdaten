


library(RSQLite)
library(tidyverse)
library(lubridate)


DB.NAME <- 'data/luftdaten_dwd.db'


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
    message(paste("Database Table does not seem to exist:"))
    
    return(NULL)
  })
}

writeToDB <- function(df, tableName, overwrite = T) {
  con <- connect_to_db()
  dbWriteTable(con, tableName, df, overwrite = overwrite)
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

load.dwd.data <- function(station_code) {
  con <- connect_to_db()
  dwd.data <-
    dbGetQuery(
      con,
      'SELECT * from dwd_data_1H WHERE "station_code" LIKE (:x)',
      params = list(x = station_code)
    )
  
  
  dbDisconnect(con)
  return(dwd.data)
}

load.dwd.stations.geo <- function(pollutant) {
  pollutant <- 'PM10'
  print(pollutant)
  con <- connect_to_db()
  dwd.stations.idx <-
    dbGetQuery(
      con,
      'SELECT  DISTINCT station_code FROM dwd_data_1H WHERE "pollutant" LIKE :x',
      params = list(x = pollutant)
    ) %>% pull(station_code)
  
  dwd.stations.location <-
    dbGetQuery(con,
               'SELECT  *  FROM stations WHERE "idx" IN (:x)',
               params = list(x = dwd.stations.idx))
  
  dbDisconnect(con)
  return(dwd.stations.location)
  
}

load.luftdaten.geo <- function() {
  con <- connect_to_db()
  luftdaten.locations <-
    dbGetQuery(
      con,
      'SELECT DISTINCT location_id,location_latitude,location_longitude FROM sensors_luftdaten'
    )
  dbDisconnect(con)
  luftdaten.locations <-
    luftdaten.locations %>% mutate_at(c('location_longitude', 'location_latitude'), as.numeric)
  return(luftdaten.locations)
}

load.luftdaten.sensors <- function(){
  con <- connect_to_db()
  SQL <- "SELECT * FROM sensors_luftdaten"
  df <- dbGetQuery(
    con,SQL)
  dbDisconnect(con)
  return(df)
  
  
}
load.luftdaten.sensors.with.data <- function(){
  con <- connect_to_db()
  luftdaten.sensors <-
    dbGetQuery(
      con,
      'SELECT DISTINCT * from sensors_luftdaten s WHERE s.sensor_id  IN (SELECT DISTINCT sensor_id FROM sensors_luftdaten_data)'
    )
  dbDisconnect(con)
  return (luftdaten.sensors)
  
}

load.luftdaten.sensors.with.data.and.dwd.station <- function() {
 
  luftdaten.sensors <-load.luftdaten.sensors.with.data()
    luftdaten.sensors %>% filter(sensor_type_id == 14 |
                                   sensor_type_id == 17)
  
  
  proximity <- read_df_from_db('luftdaten_dwd_nearest_station')
  
  luftdaten.sensors <-
    merge(luftdaten.sensors,
          proximity,
          by = 'location_id',
          all.x = T)
  luftdaten.sensors <-
    luftdaten.sensors %>% mutate_at(c(
      'location_longitude',
      'location_latitude',
      'location_altitude'
    ),
    as.numeric)
  
  luftdaten.sensors$location_latitude_jitter <-
    jitter(luftdaten.sensors$location_latitude, factor = 0.01)
  luftdaten.sensors$location_longitude_jitter <-
    jitter(luftdaten.sensors$location_longitude, factor = 0.01)
  
  luftdaten.sensors <-
    luftdaten.sensors %>% arrange(distance.to.dwd)
  

  return (luftdaten.sensors)
}

load.luftdaten.record.history.per.sensor <- function() {
  con <- connect_to_db()
  SQL <-
    "select timestamp_gmt,sensor_type_name,count(sensor_id) as records from  luftdaten_sensors_metadata  group by timestamp_gmt,sensor_type_name"
  df <- dbGetQuery(con, SQL)
  return (df)
  
  dbDisconnect(con)
}
load.luftdaten.record.history.per.sensor.country <- function() {
  con <- connect_to_db()
  
  SQL <-
    "select timestamp_gmt, sensor_type_name, location_country,count(sensor_id) as records from (select m.timestamp_gmt,m.sensor_id,m.sensor_type_name,s.location_country from luftdaten_sensors_metadata m left join sensors_luftdaten s on s.sensor_id=m.sensor_id ) group by timestamp_gmt,sensor_type_name,location_country"
  df <- dbGetQuery(con, SQL)
  return (df)
  
  dbDisconnect(con)
}

createBME280MeasurementsTable <- function() {
  con <- connect_to_db()
  SQL <-
    'select s.location_id,s.sensor_id,count(*) as measurements from sensors_luftdaten_data d left join sensors_luftdaten s on d.sensor_id=s.sensor_id where s.sensor_type_name LIKE "BME280" group by s.location_id order by measurements DESC'
  BME280_counts <- dbGetQuery(con, SQL)
  dbWriteTable(con,
               'sensors_luftdaten_BME280_counts',
               BME280_counts,
               overwrite = TRUE)
  dbDisconnect(con)
  
}

drop.table <- function(tableName) {
  con <- connect_to_db()
  dbRemoveTable(con, tableName)
  dbGetQuery(con, 'VACUUM;')
  dbDisconnect(con)
  
}
