#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(zoo)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
 
  # text output
  output$text <- renderText({
    
    #data.plot()$dwd.station$
  })
  
  
  data.plot <- eventReactive(input$locationChoice, {
    
    luftdaten.sensors.selected <- luftdaten.sensors %>% filter(location_id == input$locationChoice) 
    
    dwd.station.id <-luftdaten.sensors.selected      %>% pull(dwd_station_id)
    sensors_id <- luftdaten.sensors.selected      %>% pull(sensor_id)
    #browser()

    data <- NULL
    data$dwd.station <- dwd.stations %>% dplyr::filter(idx %in% dwd.station.id ) %>% as.list()
    data$sensors_info <- luftdaten.sensors.selected #%>% select(region,name,dwd_station_id,distance.to.dwd,sensor_type_name,location_indoor   )
    data$df <- prepare.data(dwd.station.id, sensors_id)
    
    spreado <- data$df  %>% select(-sensor_id) %>% spread(variable,value)
    
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
   
    data$spreado <- spreado
    
    
    data
  })
  

  output$map.dwd <- renderLeaflet({
    m
  })
  
  output$map.location.selected <- renderLeaflet({
    print(data.plot()$sensors_info)
    location.longi <- head(data.plot()$sensors_info$location_longitude,1)
    location.lati <-head(data.plot()$sensors_info$location_latitude,1)
    print(location.longi)
    print(location.lati)
    
    dwd_station_id <- head(data.plot()$sensors_info$dwd_station_id,1)
    
    dwd.station.longi <- dwd.stations %>% filter(idx==dwd_station_id) %>% pull(longitude) %>% head(1)
    dwd.station.lati <- dwd.stations %>% filter(idx==dwd_station_id) %>% pull(latitude) %>% head(1)
    
    m.detailed <- leaflet() %>% setView(lng =location.longi, lat = location.lati, zoom = 15)
    m.detailed <-
      m.detailed %>% addProviderTiles(providers$Stamen.Toner)  %>%   addCircleMarkers(
        lng = location.longi,
        lat = location.lati,
        radius = 6,
        stroke = TRUE,
        fillOpacity = 1,
        color='red'
      )  %>%   addCircleMarkers(
        lng = dwd.station.longi,
        lat = dwd.station.lati,
        radius = 8,
        stroke = TRUE,
        color='blue',
        fillOpacity = 1
      ) 
    m.detailed
  })
  
  

  
  output$data <- renderDataTable(data.plot()$df)
  output$plot <- renderPlotly({
    df <- data.plot()$spreado  %>% arrange(datetime)
  
    print(head(df))
    p1 <- plot_ly(df, x = ~datetime, y = ~PM10.rm, name = 'PM10', type = 'scatter', mode = 'lines',
                 line = list(color = 'rgb(94,182,104)', width = 1)) %>%
      add_trace(y = ~P1.rm, name = 'P1', line = list(color = 'rgb(0,66,37)', width = 1))
    
    p2 <- plot_ly(df, x = ~datetime, y = ~humidity.rm,type = 'scatter', mode = 'lines',name='humidity',
                  line = list(color = 'rgb(92,184,178)', width = 1) )
    
    p3 <- plot_ly(df, x = ~datetime, y = ~temperature.rm,type = 'scatter', mode = 'lines',name='temperature',
                  line = list(color = 'rgb(255,162,10)', width = 1) )
     
    p <- subplot(p1, p2,p3,nrows = 3, shareX = TRUE)
    p
  })
  
})
