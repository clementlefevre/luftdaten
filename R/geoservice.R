library(geosphere)

source('db_service.R')


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









