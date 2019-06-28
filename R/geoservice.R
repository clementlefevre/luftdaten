library(geosphere)

source('loadData.R')


update_proximity_table<- function(luftdaten.geo,dwd.stations.geo ){
  
  dwd.stations.geo <-  dwd.stations.geo %>% mutate(row.id =row_number())
  
  
  set_luftdaten_coords<- luftdaten.geo %>% select(location_longitude,location_latitude) 
  set_dwd_coords<- dwd.stations.geo  %>% select(longitude,latitude)  
  
  dist.mini.idx <- function(y) which.min(apply(set_dwd_coords, 1, function(x) min(distHaversine(x,y))))
  
  # very long to compute, must compute distance between each sensors and each dwd station
  dwd.row.id <- apply(set_luftdaten_coords, 1, dist.mini.idx )
  luftdaten.geo$idx.nearest.dwd <- dwd.row.id
  
  df_locations_luftdaten <- merge(luftdaten.geo,dwd.stations.geo ,by.x ='dwd.row.id',by.y='row.id')
  
  # compute distance in meter between a luftdaten location and the nearest DWD station :
  coord.sensors <- luftdaten.geo%>% select(location_longitude,location_latitude) 
  coord.dwd <- luftdaten.geo %>% select(longitude,latitude)
  luftdaten.geo$distance.to.dwd<-distHaversine(coord.sensors,coord.dwd)
  
  return (luftdaten.geo)
  
}


dwd.stations.geo <- load.dwd.stations.geo('PM10')
luftdaten.geo<- load.luftdaten.geo()

df<- update_proximity_table(luftdaten.geo,dwd.stations.geo)








