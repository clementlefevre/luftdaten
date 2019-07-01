library(tidyr)
library(leaflet)
library(plotly)


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


luftdaten.sensors.list <-
  as.tibble(luftdaten.sensors) %>% select(location_id, sensor_type_name, distance.to.dwd, name, region) %>% arrange(sensor_type_name) %>%
  group_by(location_id) %>% summarise(
    sensors_list = paste0(sensor_type_name, collapse = "-"),
    distance_km = round(first(distance.to.dwd) / 1000, 3),
    dwd_station = first(name),
    dwd_region = first(region),
    distance.to.dwd = first(distance.to.dwd)
  )  %>% distinct()

luftdaten.sensors.list<- luftdaten.sensors.list %>% mutate(label=paste(sensors_list,distance_km,dwd_station,dwd_region,sep='-') )%>% arrange(distance.to.dwd)

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


