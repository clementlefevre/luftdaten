library(geosphere)




compute_proximity_table <- function(luftdaten.geo, dwd.stations.geo) {
  dwd.stations.geo <-
    dwd.stations.geo %>% mutate(row.id = row_number())
  
  
  set_luftdaten_coords <-
    luftdaten.geo %>% select(location_longitude, location_latitude)
  set_dwd_coords <-
    dwd.stations.geo  %>% select(longitude, latitude)
  
  dist.mini.idx <-
    function(y)
      which.min(apply(set_dwd_coords, 1, function(x)
        min(distHaversine(x, y))))
  
  # very long to compute, must compute distance between each sensors and each dwd station
  dwd.row.id <- apply(set_luftdaten_coords, 1, dist.mini.idx)
  luftdaten.geo$dwd.row.id <- dwd.row.id
  
  luftdaten.dwd.geo <-
    merge(luftdaten.geo,
          dwd.stations.geo ,
          by.x = 'dwd.row.id',
          by.y = 'row.id')
  
  # compute distance in meter between a luftdaten location and the nearest DWD station :
  coord.sensors <-
    luftdaten.dwd.geo %>% select(location_longitude, location_latitude)
  coord.dwd <- luftdaten.dwd.geo %>% select(longitude, latitude)
  
  luftdaten.dwd.geo$distance.to.dwd <-
    distHaversine(coord.sensors, coord.dwd)
  
  return (luftdaten.dwd.geo)
  
}


updateNearestDwdStation <- function() {
  dwd.stations.geo <- load.dwd.stations.geo('PM10')
  luftdaten.geo <- load.luftdaten.geo()
  
  df <- compute_proximity_table(luftdaten.geo, dwd.stations.geo)
  
  df.to.save <-
    df %>% select(location_id, idx, distance.to.dwd) %>% rename(dwd_station_id = idx)
  
  writeToDB(df = df.to.save,
            tableName = 'luftdaten_dwd_nearest_station',
            overwrite = T)
}

## source : https://rstudio-pubs-static.s3.amazonaws.com/9428_1197bd003ebd43c49b429f22ea4f36e5.html
computeCCF <- function(sensor_id, dwd_station_id,location_id) {
  result <-  tryCatch({
    
    
    data <- prepare.data(dwd_station_id, c(sensor_id))
    spreado <-
      spread.data(data)  %>% filter(!is.na(humidity.rm)) %>% filter(!is.na(P1.rm))
    
    df <-
      spreado  %>% filter(!is.na(P1.rm) &
                            !is.na(PM10.rm) &
                            !is.na(temperature.rm) &
                            !is.na(humidity.rm))
    
    
    correlations <- ccf(df$PM10.rm, df$P1.rm, plot = FALSE)
    max.acf <- max(correlations$acf)
    best.lag <- correlations$lag[which.max(correlations$acf)]
    
    df <- data.frame(
      location_id = location_id,
      sensor_id = sensor_id,
      dwd_station_id = dwd_station_id,
      best.lag = best.lag,
      max.acf = max.acf
    )
    
    df
    
  },
  error = function(cond) {
    
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    NA
  })
  
  result
}




createCrossCorrelationTable <- function(){
  df <- luftdaten.sensors %>% filter(sensor_type_name == 'SDS011') 
  
  df.best.acf <-
    do.call(rbind, apply(df[, c('sensor_id', 'dwd_station_id','location_id')], 1, function(y)
      computeCCF(y['sensor_id'], y['dwd_station_id'],y['location_id'])))
  
  df.best.acf <- df.best.acf %>% na.omit(.)
  
  writeToDB(df.best.acf, 'autocorrelations', overwrite = TRUE)
  
}

prepare.data <- function(dwdStationCode = 'None', sensors_id) {
  print('starting preparing data..')
  
  df.luftdaten <- load.luftdaten.data(sensors_id)
  
  df.luftdaten$datetime <- as_datetime(df.luftdaten$timestamp_gmt)
  df.luftdaten <-
    df.luftdaten %>% select(sensor_id, datetime, value, variable)
  df.luftdaten$sensor_id <- as.character(df.luftdaten$sensor_id)
  
  if (dwdStationCode != 'None') {
    df.dwd <- load.dwd.data(dwdStationCode)
    df.dwd <-
      df.dwd %>% rename(sensor_id = station_code, variable = pollutant)
    df.dwd$datetime <-  as_datetime(df.dwd$timestamp_gmt)
    df.dwd <- df.dwd %>%  select(-timestamp_gmt)
    
    df <-
      bind_rows(df.dwd, df.luftdaten) %>%  distinct(sensor_id, datetime, variable, .keep_all = TRUE)
    
  } else{
    df<-df.luftdaten
  }
 
  
 df <- df %>% arrange(datetime)
  start.data <-  df %>% dplyr::filter(variable == 'P1') %>% pull(datetime)
  df <- df %>% dplyr::filter(datetime >= min(start.data))
  
  return (df %>% na.omit(.))
}
