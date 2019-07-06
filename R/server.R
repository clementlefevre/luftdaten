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
library(viridis)
library(gridExtra)




# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  haehnel <- NULL
  
  haehnel <-
    eventReactive(c(input$slider.haehnel.beta, input$locationChoice), {
     
      df <-  data.plot()$spreado %>% filter(!is.na(humidity.rm))
     
      rh <- df$humidity.rm / 100
      beta_haehnel <- input$slider.haehnel.beta
      haehnel <- df$P1.rm * ((1 - rh) ^ beta_haehnel)
      haehnel
    
      
    })
  
  
  observeEvent(input$map.dwd_marker_click, {
    p <- input$map.dwd_marker_click  # typo was on this line
    
    if (!is.null(input$map.dwd_marker_click$id)) {
      updateSelectInput(session,
                        "locationChoice",
                        
                        selected = input$map.dwd_marker_click$id)
    }
    
  })
  
  
  
  data.plot <-
    eventReactive(c(input$locationChoice,input$slider.lag), {
      # add input$map.dwd_marker_click if selection from map
      req(input$locationChoice)
      selected_location_id <- isolate(input$locationChoice)
      
      
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
      
      spreado <-
        spread.data(data$df)  %>% filter(!is.na(humidity.rm)) %>% filter(!is.na(P1.rm))
      
      if(input$slider.lag>=0){
        spreado <- spreado %>% mutate(PM10.rm=lag(PM10.rm,input$slider.lag))
      } else{
        spreado <- spreado %>% mutate(PM10.rm=lead(PM10.rm,input$slider.lag*-1))
      }
    
     
      data$spreado <- spreado
      
      
      
      
      data
    })
  
  
  output$map.dwd <- renderLeaflet({
    m
  })
  
  output$map.location.selected <- renderLeaflet({
    data <- data.plot()
    location.longi <-
      head(data$sensors_info$location_longitude, 1)
    location.lati <-
      head(data$sensors_info$location_latitude, 1)
    
    
    dwd_station_id <-
      head(data.plot()$sensors_info$dwd_station_id, 1)
    
    dwd.station.longi <-
      dwd.stations %>% filter(idx == dwd_station_id) %>% pull(longitude) %>% head(1)
    dwd.station.lati <-
      dwd.stations %>% filter(idx == dwd_station_id) %>% pull(latitude) %>% head(1)
    
    m.detailed <-
      leaflet() %>% setView(lng = location.longi,
                            lat = location.lati,
                            zoom = 12)
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
  
  output$plot.timeline <- renderPlotly({
   
    df <- data.plot()$spreado  %>% arrange(datetime)
   
    df$haehnel <- haehnel()
    aqi.good.rgba <- 'rgba(166,221,171,0.4)'
    aqi.soso.rgba <- 'rgba(254,255,160,0.4)'
    aqi.bad.rgba <- 'rgba(227,111,72,0.4)'
    
    df$aqi.good <- 13
    
    df$aqi.bad <- 200
    
 
    df$aqi.soso <- 50
    
    
    
    
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
      ) %>%
      add_trace(
        y = ~ haehnel,
        name = 'P1.haehnel',
        line = list(color = 'rgb(245, 176, 66)', width = 1)
      )  %>% 
      add_trace( y = ~aqi.good, name = 'AQI Good', fill = 'tozeroy',
                   fillcolor = aqi.good.rgba,
                 line = list(color = aqi.good.rgba, width = 0)) %>%
      add_trace(y = ~aqi.soso, type = 'scatter', mode = 'lines',fill = 'tonexty', fillcolor=aqi.soso.rgba, line = list(color =aqi.soso.rgba),
                                                                           showlegend = FALSE, name = 'AQI SOSO') %>%
      add_trace(y = ~aqi.bad, type = 'scatter', mode = 'lines',fill = 'tonexty', fillcolor=aqi.bad.rgba, line = list(color =aqi.bad.rgba),
                showlegend = FALSE, name = 'AQI SOSO')
    
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
    
    p <- subplot(p1, p2, nrows = 2, shareX = TRUE,heights = c(0.7, 0.3))
    p
  })
  
  output$cross_correlation_plot <- renderPlot({
    df <-
      data.plot()$spreado  %>% filter(!is.na(P1.rm) &
                                        !is.na(PM10.rm) & !is.na(temperature.rm) & !is.na(humidity.rm))
    ccf(df$PM10.rm, df$P1.rm)
    
  })
  
  
  output$plot.DWD_vs_Luftdaten <- renderPlot({
    df <- data.plot()$spreado  %>% filter(!is.na(humidity.rm))
    df$haehnel <- haehnel()
    
    model.1 <- lm(data = df, P1.rm ~ PM10.rm)
    rmse.1 <- sqrt(mean(model.1$residuals^2))
    adjusted_r2.1 <-  paste0('Adj.R2 : ',round(summary(model.1)$adj.r.squared,2) ,'- rmse: ',rmse.1)
    
    
    model.2 <- lm(data = df, haehnel ~ PM10.rm)
    rmse.2 <-sqrt(mean(model.2$residuals^2))
    adjusted_r2.2 <- paste0('Adj.R2 : ',round(summary(model.2)$adj.r.squared,2), '- rmse: ',rmse.2)
    
  
    gato <- df %>% select(datetime,PM10.rm,P1.rm,humidity.rm,haehnel)
    gato <- gato %>% gather(key,value,-datetime,-PM10.rm,-humidity.rm)
    
    dat_text <- data.frame(
      label = c(adjusted_r2.1, adjusted_r2.2),
      key   = c('P1.rm', 'haehnel')
    )


   p<- ggplot(gato,aes(PM10.rm,value))+ geom_point(aes(color=humidity.rm))+scale_color_viridis(direction=-1)+xlim(0,100)+ylim(0,100)
  
   p + geom_text(
     data    = dat_text,
     mapping = aes(x = 20, y = 90, label = label),
     hjust   = -0.1,
     vjust   = -1
   ) + facet_grid(.~key)
  })

  
})
