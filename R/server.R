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
    df <- data.plot()$df  %>% arrange(datetime)
    p <-
      ggplot(df, aes(datetime, value, color = variable)) + geom_line(size = .2) + theme_minimal()
 
    x <- c(1:100)
    random_y <- rnorm(100, mean = 0)
    data <- data.frame(x, random_y)
    
    p <- plot_ly(df, x = ~datetime, y = ~value, color=~variable,type = 'scatter', mode = 'lines')
    p
  })
  
})
