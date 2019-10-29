library(lubridate)
library(jsonlite)
library(data.table)





# live data from Grafana API



# get Grafana BME280 data


getGrafanaBME280 <- function(BME280_id, sensorId, time.range = 72) {
  grafana.bme280 <-
    sprintf(
      "https://maps.luftdaten.info/grafana/api/datasources/proxy/3/query?db=feinstaub&q=SELECT \"time\",\"bme280_humidity\",\"bme280_pressure\",\"bme280_temperature\"  FROM \"feinstaub\" WHERE (\"node\" =~ /^%s$/) AND time >= now() - %sh&epoch=ms",
      BME280_id,
      time.range
    )
  
  grafana <- fromJSON(URLencode(grafana.bme280))
  
  df.grafana <- grafana$results$series
  
  DT <- as.data.frame(df.grafana[[1]]$values)
  
  if (nrow(DT) > 0) {
    names(DT) <- unlist(df.grafana[[1]]$columns, use.names = F)
    setDT(DT)
    DT[, datetime := as.POSIXct(time / 1000, origin = "1970-01-01"),]
    DT[, sensorId := sensorId]
    return(DT)
  }
  return(NULL)
  
}


# get Grafana SDS011 data :


getGrafanaSDS <- function(sds_id, sensorId, time.range=72) {
  grafana.sds <-
    sprintf(
      "https://maps.luftdaten.info/grafana/api/datasources/proxy/3/query?db=feinstaub&q=SELECT   \"time\",\"sds011_p1\",\"sds011_p2\"FROM \"feinstaub\" WHERE (\"node\" =~ /^%s$/) AND time >= now() - %sh&epoch=ms",
      sds_id,
      time.range
    )
  
  grafana <- fromJSON(URLencode(grafana.sds))
  
  df.grafana <- grafana$results$series
  
  DT <- as.data.frame(df.grafana[[1]]$values)
  if (nrow(DT) > 0) {
    names(DT) <- unlist(df.grafana[[1]]$columns, use.names = F)
    setDT(DT)
    DT[, datetime := as.POSIXct(time / 1000, origin = "1970-01-01"),]
    DT[, sensorId := sensorId]
    return(DT)
  }
  
  
  return(NULL)
  
}

getGrafanaData <- function() {
  DT.grafana.bme280 <-
    rbindlist(mapply(getGrafanaBME280, DT.sensors[, bme280], DT.sensors[, sensorId], 72, SIMPLIFY = FALSE))
  DT.grafana.sds <-
    rbindlist(mapply(getGrafanaSDS, DT.sensors[, sds011], DT.sensors[, sensorId], 72, SIMPLIFY = FALSE))

  DT.m.bme280 <-
    melt(
      DT.grafana.bme280[,!"time"],
      id.vars = c("datetime", "sensorId"),
      mesure.vars = c('bme280_pressure', 'bme280_temperature', 'bme280_humidity')
    )
  DT.m.sds <-
    melt(
      DT.grafana.sds[,!"time"],
      id.vars = c("datetime", "sensorId"),
      mesure.vars = c('sds011_p1', 'sds011_p2')
    )
  
  return (rbind(DT.m.bme280, DT.m.sds))
}
