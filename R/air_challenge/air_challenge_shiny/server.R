#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(ggthemes)
source('service/dbservice.R')
source('sensors_config.R')
source('service/update_db_from_madavi.R')
source('service/grafana_api_service.R')
source('service/plotService.R')


data <- NULL

DT.grafana <- getGrafanaData()
DT.grafana$aqi.good <- 13
DT.grafana$aqi.bad <- 200
DT.grafana$aqi.soso <- 50
setkey(DT.grafana, sensorId)
data$DT.grafana <- DT.sensors[DT.grafana]
DT.grafana <- NULL
print('Grafana data loaded.')

aqi.good.rgba <- 'rgba(122,217,105,.8)'
aqi.soso.rgba <- 'rgba(249,158,56,.8)'
aqi.bad.rgba <- 'rgba(227,111,72,0.8)'

updateDB()

getMadaviData <- function() {
    DT.madavi.DB <- read_df_from_db('madavi_data') %>% setDT()
    DT.madavi.DB[, datetime := as.POSIXct(datetime, origin = "1970-01-01")]
    setkey(DT.madavi.DB, sensorId)
    print("finished load DB")
    
    return(DT.sensors[DT.madavi.DB])
}

data$DT.madavi <- getMadaviData()
data$DT.madavi$aqi.good <- 13
data$DT.madavi$aqi.bad <- 200
data$DT.madavi$aqi.soso <- 50

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    output$plot1 <- renderPlotly({
        if (input$time.range == '72h') {
            p <- plotGrafana(data)
            p
        } else{
            p <- plotMadavi(data)
            p
        }
        
    })
    
})
