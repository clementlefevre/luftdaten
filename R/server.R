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
  observeEvent(input$map.dwd_marker_click, {
    p <- input$map.dwd_marker_click  # typo was on this line
    
    if (!is.null(input$map.dwd_marker_click$id)) {
      updateSelectInput(session,
                        "locationChoice",
                        
                        selected = input$map.dwd_marker_click$id)
    }
    
  })
  
  
  
  
  
  data.plot <-
    eventReactive(c(input$locationChoice, input$map.dwd_marker_click), {
      if (!is.null(input$map.dwd_marker_click$id)) {
        selected_location_id <- input$map.dwd_marker_click$id
        
        
      } else{
        selected_location_id <- input$locationChoice
      }
      
      luftdaten.sensors.selected <-
        luftdaten.sensors %>% filter(location_id == selected_location_id)
      
      dwd.station.id <-
        luftdaten.sensors.selected      %>% pull(dwd_station_id)
      sensors_id <-
        luftdaten.sensors.selected      %>% pull(sensor_id)
      
      data <- NULL
      data$dwd.station <-
        dwd.stations %>% dplyr::filter(idx %in% dwd.station.id) %>% as.list()
      data$sensors_info <- luftdaten.sensors.selected
      data$df <- prepare.data(dwd.station.id, sensors_id)
      
      spreado <- spread.data(data$df)
      
      
      data$spreado <- spreado
      
      
      data
    })
  
  
  output$map.dwd <- renderLeaflet({
    m
  })
  
  output$map.location.selected <- renderLeaflet({
    location.longi <-
      head(data.plot()$sensors_info$location_longitude, 1)
    location.lati <-
      head(data.plot()$sensors_info$location_latitude, 1)
    
    
    dwd_station_id <-
      head(data.plot()$sensors_info$dwd_station_id, 1)
    
    dwd.station.longi <-
      dwd.stations %>% filter(idx == dwd_station_id) %>% pull(longitude) %>% head(1)
    dwd.station.lati <-
      dwd.stations %>% filter(idx == dwd_station_id) %>% pull(latitude) %>% head(1)
    
    m.detailed <-
      leaflet() %>% setView(lng = location.longi,
                            lat = location.lati,
                            zoom = 15)
    m.detailed <-
      m.detailed %>% addProviderTiles(providers$Stamen.Toner)  %>%   addCircleMarkers(
        lng = location.longi,
        lat = location.lati,
        radius = 6,
        stroke = TRUE,
        fillOpacity = 1,
        color = 'red'
      )  %>%   addCircleMarkers(
        lng = dwd.station.longi,
        lat = dwd.station.lati,
        radius = 8,
        stroke = TRUE,
        color = 'blue',
        fillOpacity = 1
      )
    m.detailed
  })
  
  
  
  
  output$data <- renderDataTable(data.plot()$df)
  output$plot.timeline <- renderPlotly({
    df <- data.plot()$spreado  %>% arrange(datetime)
    
    
    p1 <-
      plot_ly(
        df,
        x = ~ datetime,
        y = ~ PM10.rm,
        name = 'PM10',
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'rgb(94,182,104)', width = 1)
      ) %>%
      add_trace(
        y = ~ P1.rm,
        name = 'P1',
        line = list(color = 'rgb(0,66,37)', width = 1)
      )
    
    p2 <-
      plot_ly(
        df,
        x = ~ datetime,
        y = ~ humidity.rm,
        type = 'scatter',
        mode = 'lines',
        name = 'humidity',
        line = list(color = 'rgb(92,184,178)', width = 1)
      )
    
    p3 <-
      plot_ly(
        df,
        x = ~ datetime,
        y = ~ temperature.rm,
        type = 'scatter',
        mode = 'lines',
        name = 'temperature',
        line = list(color = 'rgb(255,162,10)', width = 1)
      )
    
    p <- subplot(p1, p2, nrows = 2, shareX = TRUE)
    p
  })
  
  output$plot.relationships <- renderPlotly({
    df <- data.plot()$spreado  %>% arrange(datetime)
    df$month.period <- month(df$datetime)
    
    p <-
      plot_ly(
        data = df,
        x = ~ PM10 / P1,
        y = ~ log(1 + humidity),
        color =  ~ as.numeric(datetime),
        alpha = .5,
        marker = list()
      )
    
    p
  })
  output$plot.DWD_vs_Luftdaten <- renderPlotly({
    df <- data.plot()$spreado  %>% filter(!is.na(humidity))
  
    p <-
      plot_ly(
        data = df,
        x = ~ PM10,
        y = ~ P1,
        color =  ~ as.numeric(humidity),
        alpha = .5,
        marker = list()
      )%>%layout(xaxis = list(range = c(0, 100)),yaxis=list(range=c(0,100)))
    
    p
  })
  
  output$plot.DWD_vs_Luftdaten_Haehnel <- renderPlotly({
    
    
    df <- data.plot()$spreado  %>% filter(!is.na(humidity))
    df$rh<- df$humidity.rm/100
    beta_haehnel <- input$slider1
    df$haehnel <- df$P1*((1-df$rh)^beta_haehnel)
    
    p <-
      plot_ly(
        data = df,
        x = ~ PM10,
        y = ~ haehnel,
        color =  ~ as.numeric(humidity),
        alpha = .5,
        marker = list()
      )%>% layout(xaxis = list(range = c(0, 100)),yaxis=list(range=c(0,100)))
    
    p
  })
  
})
