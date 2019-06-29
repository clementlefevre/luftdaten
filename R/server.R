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
    
    data$spreado <- spreado
    
    
    data
  })
  

  output$map.dwd <- renderLeaflet({
    m
  })
  
  output$map.location.selected <- renderLeaflet({
    location.longi <- head(data.plot()$sensors_info$location_longitude,1)
    location.lati <-head(data.plot()$sensors_info$location_latitude,1)
    print(location.longi)
    print(location.lati)
   
    m.detailed <- leaflet() %>% setView(lng =location.longi, lat = location.lati, zoom = 8)
    m.detailed <-
      m.detailed %>% addProviderTiles(providers$Stamen.Toner)  %>%   addCircleMarkers(
       
        lng = location.longi,
        lat = location.lati,
        radius = 6,
        
        stroke = TRUE,
        fillOpacity = 1
      ) 
    m.detailed
  })
  
  

  
  output$data <- renderDataTable(data.plot()$df)
  output$plot <- renderPlotly({
    df <- data.plot()$spreado  %>% arrange(datetime)
  
    print(head(df))
    p <- plot_ly(df, x = ~datetime, y = ~PM10.rm, name = 'PM10', type = 'scatter', mode = 'lines',
                 line = list(color = 'rgb(92,184,178)', width = 1)) %>%
      add_trace(y = ~P1.rm, name = 'P1', line = list(color = 'rgb(46,92,89)', width = 1))
    p
  })
  
})
