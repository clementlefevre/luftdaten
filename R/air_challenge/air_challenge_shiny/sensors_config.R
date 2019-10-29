## sensors config

berlin.clement = c(
  name = 'clement',
  sds011 = 21523,
  bme280 = 21524,
  sensorId = 6106225
)
antony.mathilde <-
  c(
    name = 'mathilde',
    sds011 = 23784,
    bme280 = 23785,
    sensorId = 6135251
  )
berlin.thibaut <-
  c(
    name = 'thibaut',
    sds011 = 23778,
    bme280 = 23779,
    sensorId = 6093296
  )
DT.sensors <-
  data.table(rbind(berlin.clement, antony.mathilde, berlin.thibaut))
row.names(DT.sensors) <- NULL
setkey(DT.sensors,sensorId)
