library(tidyr)
library(leaflet)
library(plotly)
library(zoo)


source('service/geoservice.R')

source('service/dbservice.R')

# 1- we need a list of DWD stations with coordinates for PM10:
dwd.stations <- load.dwd.stations.geo('PM10')

# 2 we need the list of sensors with the nearest dwd station infos:
luftdaten.sensors <- load.luftdaten.sensors()

luftdaten.sensors <-
  merge(
    luftdaten.sensors,
    dwd.stations %>% select(idx, name, region),
    by.x = 'dwd_station_id',
    by.y = 'idx'
  )

spread.data <- function(df){
  
  spreado <- df  %>% dplyr::select(-sensor_id) %>% spread(variable,value)
  
  spreado$PM10.rm <-           rollmean(
    spreado$PM10,
    k = 24*1,
    fill = NA,
    na.rm = TRUE
  )
  
  
  
  spreado$P1.rm <-           rollmean(
    spreado$P1,
    k = 24*1,
    fill = NA,
    na.rm = TRUE
  )
  
  if ( c('humidity','temperature') %in% colnames(spreado) ){
    spreado$humidity.rm <-           rollmean(
      spreado$humidity,
      k = 24*1,
      fill = NA,
      na.rm = TRUE
    )
    
    spreado$temperature.rm <-           rollmean(
      spreado$temperature,
      k = 24*1,
      fill = NA,
      na.rm = TRUE
    )
    
  } else {
    spreado$humidity.rm <- 0
    spreado$temperature.rm <-0
  }
  
  return (spreado)
}


luftdaten.sensors.list <-
  as.tibble(luftdaten.sensors) %>% select(location_id, sensor_type_name, distance.to.dwd, name, region) %>% arrange(sensor_type_name) %>%
  group_by(location_id) %>% summarise(
    sensors_list = paste0(sensor_type_name, collapse = "-"),
    distance_km = round(first(distance.to.dwd) / 1000, 3),
    dwd_station = first(name),
    dwd_region = first(region),
    distance.to.dwd = first(distance.to.dwd)
  )  %>% distinct()

bme280Measurements <- read_df_from_db('sensors_luftdaten_BME280_counts')

luftdaten.sensors.list <- merge(luftdaten.sensors.list,bme280Measurements%>% select(-sensors_id),by='location_id')

luftdaten.sensors.list<- luftdaten.sensors.list  %>% filter(sensors_list=="BME280-SDS011") %>% ungroup() %>% arrange(distance.to.dwd,measurements)

pal <- colorFactor(palette = "Spectral",
                   domain = luftdaten.sensors$sensor_type_id)

m <- leaflet() %>% setView(lng = 13.3493, lat = 52.5430, zoom = 7)
m <-
  m %>% addProviderTiles(providers$Stamen.Toner) %>%   addCircleMarkers(
    data = dwd.stations,
    lng = ~ longitude,
    lat = ~ latitude,
    radius = 6,
    label = ~ paste(idx, name, sep = ' - '),
    stroke = TRUE,
    fillOpacity = 1
  ) %>%   addCircleMarkers(
    data = luftdaten.sensors,
    lng = ~ location_longitude_jitter,
    lat = ~ location_latitude_jitter,
    layerId = ~location_id,
    radius = 5,
    stroke = FALSE,
    color =  ~ pal(sensor_type_id),
    fillOpacity = 1,
    label = ~ paste0('location_id: ', as.character(location_id))
  )


