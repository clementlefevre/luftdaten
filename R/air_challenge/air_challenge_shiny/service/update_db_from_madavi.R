library(lubridate)
library(jsonlite)
library(data.table)
library(magrittr)
library(rvest)
library(reprex)
library(stringr)

ROOT_DL <- "https://www.madavi.de/sensor/"

regex.files <- "^.*?\\.(zip|csv)$"
regex.csv <- "^.*?\\.(csv)$"
regex.zip <- "^.*?\\.(zip)$"

## get latest data per sensors:

DT.madavi.DB <- read_df_from_db('madavi_data') %>% setDT()
if (nrow(DT.madavi.DB > 0)) {
  DT.madavi.DB[, datetime := as.POSIXct(datetime, origin = "1970-01-01")]
  
  DT.madavi.DB[, datefile := gsub('.csv', '', str_match(filename, '([^-]+)(?:-[^-]+){2}$')[, 1])]
  
  latest.data.db <-
    DT.madavi.DB[, .(latest_data = as.Date(max(datetime)) - 1), by = sensorId]
}
## get Madavi archive files:
listMadaviFiles <- function(sensorId) {
  MADAVI <-
    sprintf("https://www.madavi.de/sensor/csvfiles.php?sensor=esp8266-%s",
            sensorId)
  
  madavi.page <- read_html(MADAVI)
  data <-
    html_nodes(madavi.page, xpath = "//a/@href") %>% html_text()
  DT.files <- data.frame(url = data) %>% setDT()
  DT.files <- DT.files[grepl(regex.files, url)]
  DT.files[, url := as.character(url)]
  DT.files[, sensorId := sensorId]
  return(DT.files[, filename := gsub(".*/", '', url), ])
}



extractDate <- function(filename) {
  filedate <- NULL
  if (str_detect(filename, ".csv")) {
    filedate <- str_match(filename, '([^-]+)(?:-[^-]+){2}$')[, 1]
    filedate <- gsub('.csv', '', filedate)
    #filedate <- as.Date(filedate,origin="1970-01-01")
    
  } else  {
    filedate <- str_match(filename, '([^-]+)(?:-[^-]+){1}$')[, 1]
    filedate <- gsub('.zip', '', filedate)
    filedate <- paste(filedate, '01', sep = '-')
    #filedate <- as.Date(filedate,origin="1970-01-01")
    
  }
  return(filedate)
}

## Download Madavi file

dl.zip.file <- function(url.zip, sensorId) {
  tmp.dir <- 'tmp'
  temp.file.zip <- tempfile()
  
  if (!dir.exists(tmp.dir)) {
    dir.create(tmp.dir)
  } else {
    print("Dir already exists!")
  }
  
  download.file(paste0(ROOT_DL, url.zip), temp.file.zip)
  
  unzip(temp.file.zip, exdir = tmp.dir)
  
  dataFls <- dir(tmp.dir, pattern = "csv$", full.names = TRUE)
  
  dataFls <- setNames(dataFls, gsub(".*/", '', dataFls))
  dt <-
    data.table::rbindlist(
      lapply(dataFls, data.table::fread, showProgress = FALSE),
      fill = T,
      idcol = 'filename'
    )
  
  dt$sensorId <- sensorId
  unlink(temp.file.zip)
  unlink(paste0(tmp.dir, '/*'))
  return (dt)
}

dl.csv.file <- function(url.csv, sensorId) {
  dt <- fread(url.csv)
  dt$sensorId <- sensorId
  return(dt)
}

getMadaviData <- function(DT.files) {
  zip.files <- DT.files[fileextension == 'zip']
  csv.files <-
    DT.files[fileextension == 'csv'][, full_url := (paste0(ROOT_DL, url)),][, full_url]
  csv.sensorId <- DT.files[grepl(regex.csv, url)][, sensorId]
  csv.files <-
    setNames(csv.files, DT.files[grepl(regex.csv, url)][, filename])
  
  
  DT.zip <-
    rbindlist(mapply(dl.zip.file, zip.files[, url], zip.files[, sensorId], SIMPLIFY = FALSE),
              fill = T)
  DT.csv <-
    rbindlist(
      mapply(dl.csv.file, csv.files, csv.sensorId, SIMPLIFY = FALSE),
      fill = T,
      idcol = 'filename'
    )
  DT.all.Madavi <- rbind(DT.zip, DT.csv)
}

updateDB <- function() {
  DT.madavi.files <-
    rbindlist(lapply(DT.sensors$sensorId, listMadaviFiles))
  
  DT.madavi.files[, filedate := unlist(lapply(DT.madavi.files$filename, extractDate)),]
  DT.madavi.files[, fileextension := str_extract(filename, "[^.]*$")]
  DT.madavi.files[, date_of_file := as.Date(filedate)]
  DT.madavi.files[, days_in_month := days_in_month(date_of_file)]
  DT.madavi.files[, filedate := NULL]
  DT.madavi.files.all.dates.zip <-
    DT.madavi.files[fileextension == 'zip' , .(date_of_file = date_of_file + 1:days_in_month - 1), by = .(filename, sensorId, url, fileextension)]
  
  DT.madavi.files.all.dates.csv <-
    DT.madavi.files[fileextension == 'csv', .(filename, sensorId, date_of_file, url, fileextension),]
  
  DT.madavi.files.all.dates <-
    rbind(DT.madavi.files.all.dates.zip,
          DT.madavi.files.all.dates.csv)
  
  
  
  
  # get list of Madavi files to download:
  
  if (nrow(DT.madavi.DB > 0)) {
    latest.data.db[, sensorId := as.character(sensorId)]
    setkey(latest.data.db, sensorId)
    DT.madavi.files.all.dates[, sensorId := as.character(sensorId)]
    setkey(DT.madavi.files.all.dates, sensorId)
    
    
    DT.madavi.files.all.dates <-
      latest.data.db[DT.madavi.files.all.dates]
    DT.madavi.files.to.dl <-
      DT.madavi.files.all.dates[date_of_file >= latest_data]
  } else{
    DT.madavi.files.to.dl <- DT.madavi.files.all.dates
  }
  DT.madavi.files.to.dl  <-
    unique(DT.madavi.files.to.dl , by = c('filename'))
  
  DT.madavi.data <- getMadaviData(DT.madavi.files.to.dl)
  colnames(DT.madavi.data)
  DT <-
    DT.madavi.data[, .(
      filename,
      Time,
      SDS_P1,
      SDS_P2,
      BME280_temperature,
      BME280_humidity,
      BME280_pressure,
      sensorId
    )]
  DT[, datetime := ymd_hms(Time)][, Time := NULL]
  setcolorder(DT, c("datetime", setdiff(names(DT), "datetime")))
  
  ## insert in db only new data
  if (nrow(DT.madavi.DB > 0)) {
    DT.missingData <- DT[!DT.madavi.DB, on = c("sensorId", "datetime")]
  } else{
    DT.missingData <- DT
  }
  
  writeToDB(DT.missingData, 'madavi_data')
  print('finished updating DB')
  
}
