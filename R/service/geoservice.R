library(geosphere)




compute_proximity_table<- function(luftdaten.geo,dwd.stations.geo ){
  

  dwd.stations.geo <-  dwd.stations.geo %>% mutate(row.id =row_number())
  
  
  set_luftdaten_coords<- luftdaten.geo %>% select(location_longitude,location_latitude) 
  set_dwd_coords<- dwd.stations.geo  %>% select(longitude,latitude)  
  
  dist.mini.idx <- function(y) which.min(apply(set_dwd_coords, 1, function(x) min(distHaversine(x,y))))
  
  # very long to compute, must compute distance between each sensors and each dwd station
  dwd.row.id <- apply(set_luftdaten_coords, 1, dist.mini.idx )
  luftdaten.geo$dwd.row.id <- dwd.row.id
  
  luftdaten.dwd.geo <- merge(luftdaten.geo,dwd.stations.geo ,by.x ='dwd.row.id',by.y='row.id')
  
  # compute distance in meter between a luftdaten location and the nearest DWD station :
  coord.sensors <- luftdaten.dwd.geo%>% select(location_longitude,location_latitude) 
  coord.dwd <- luftdaten.dwd.geo %>% select(longitude,latitude)
  
  luftdaten.dwd.geo$distance.to.dwd<-distHaversine(coord.sensors,coord.dwd)
  
  return ( luftdaten.dwd.geo)
  
}


updateNearestDwdStation <- function(){
  dwd.stations.geo <- load.dwd.stations.geo('PM10')
  luftdaten.geo<- load.luftdaten.geo()
  
  df<- compute_proximity_table(luftdaten.geo,dwd.stations.geo)
  
  df.to.save <- df %>% select(location_id,idx,distance.to.dwd) %>% rename(dwd_station_id = idx)
  
  writeToDB(df = df.to.save,tableName = 'luftdaten_dwd_nearest_station', overwrite=T) 
}

prepare.data <- function(dwdStationCode,sensors_id){
  print('starting preparing data..')
 
  df.luftdaten <- load.luftdaten.data(sensors_id)
 
  df.luftdaten$datetime <- as_datetime(df.luftdaten$timestamp_gmt)
  df.luftdaten <- df.luftdaten %>% select(sensor_id,datetime,value,variable)
  df.luftdaten$sensor_id <- as.character(df.luftdaten$sensor_id)
  
  df.dwd <- load.dwd.data(dwdStationCode)
  df.dwd<- df.dwd %>% rename(sensor_id=station_code,variable=pollutant)
  df.dwd$datetime <-  as_datetime(df.dwd$timestamp_gmt)
  
  df <- bind_rows(df.dwd,df.luftdaten) %>%  distinct(sensor_id,datetime,variable, .keep_all = TRUE)

  start.data <-  df %>% filter(variable=='P1') %>% pull(datetime)
  df <- df %>% filter(datetime>=min(start.data))
  
  return (df %>% select(-timestamp_gmt))
}









