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
library(knitr)
source('service/plotservice.R')
source('service/growth_functions.R')



shinyServer(function(input, output, session) {
  output$markdown <- renderUI({
    withMathJax(includeMarkdown("luftdaten_history_EDA.md"))
  })
  
  output$growth.function.def <- renderUI({
    text<-switch(input$function.adjust,
           'haehnel' = "$$gf_{Haehnel} = \\frac{1}{(1-rh)^\\beta}$$",
           'soneja' = "$$gf_{Soneja} = 1 + \\frac{\\alpha rh^2}{1-rh}$$",
           'combo' = "$$gf_{combo} = 1 + \\frac{\\alpha rh^2}{(1-rh)^\\beta}$$",
           'skupin'="$$\\epsilon^2$$")
    
    
    print(text)
    
    withMathJax(helpText(text))
  }
  )
  
    
  
  
  adjusted <- NULL
  
  adjusted <-
    eventReactive(c(input$slider.alpha,input$slider.beta,input$function.adjust, input$locationChoice), {
      df <-  data.plot()$spreado  %>% filter(!is.na(humidity.rm))
      rh <- df$humidity.rm / 100
      rh[rh == 1] <- .9999
      growth.factor <- growthFunction(input$function.adjust,rh,input$slider.alpha,input$slider.beta)
      adjusted <-df$P1.rm *  growth.factor
      adjusted
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
    eventReactive(c(input$locationChoice, input$slider.lag), {
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
      
      if (input$slider.lag >= 0) {
        spreado <- spreado %>% mutate(PM10.rm = lag(PM10.rm, input$slider.lag))
      } else{
        spreado <-
          spreado %>% mutate(PM10.rm = lead(PM10.rm, input$slider.lag * -1))
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
        color = 'rgb(115,207,201)'
      )  %>%   addCircleMarkers(
        lng = dwd.station.longi,
        lat = dwd.station.lati,
        radius = 8,
        stroke = TRUE,
        color = 'rgb(45,75,155)',
        fillOpacity = 1
      )
    m.detailed
  })
  
  output$plot.timeline <- renderPlotly({
    df <- data.plot()$spreado  %>% arrange(datetime)
    df$haehnel <- adjusted()
    plotTimeLine(df)
    
   
  })
  
  output$cross_correlation_plot <- renderPlot({
    df <-
      data.plot()$spreado  %>% filter(!is.na(P1.rm) &
                                        !is.na(PM10.rm) &
                                        !is.na(temperature.rm) & !is.na(humidity.rm))
    ccf(df$PM10.rm, df$P1.rm)
    
  })
  
  
  output$plot.DWD_vs_Luftdaten <- renderPlot({
    df <- data.plot()$spreado  %>% filter(!is.na(humidity.rm))

    df$adjusted <- adjusted()
    
    model.1 <- lm(data = df, P1.rm ~ PM10.rm)
    rmse.1 <- sqrt(mean(model.1$residuals ^ 2))
    adjusted_r2.1 <-
      paste0('Adj.R2 : ',
             round(summary(model.1)$adj.r.squared, 2) ,
             '- rmse: ',
             rmse.1)
    
    
    model.2 <- lm(data = df, adjusted ~ PM10.rm)
    rmse.2 <- sqrt(mean(model.2$residuals ^ 2))
    adjusted_r2.2 <-
      paste0('Adj.R2 : ',
             round(summary(model.2)$adj.r.squared, 2),
             '- rmse: ',
             rmse.2)
    
    
    gato <-
      df %>% select(datetime, PM10.rm, P1.rm, humidity.rm, adjusted)
    gato <-
      gato %>% gather(key, value, -datetime, -PM10.rm, -humidity.rm)
    
    dat_text <- data.frame(
      label = c(adjusted_r2.1, adjusted_r2.2),
      key   = c('P1.rm', 'adjusted')
    )
    
    
    p <-
      ggplot(gato, aes(PM10.rm, value)) + geom_point(aes(color = humidity.rm,alpha=.2),size=.5) +
      scale_color_viridis(direction = -1) + xlim(0, 100) + ylim(0, 100)
    
    p + geom_text(
      data    = dat_text,
      mapping = aes(x = 20, y = 90, label = label),
      hjust   = -0.1,
      vjust   = -1
    ) + facet_grid(. ~ key)
  })
  
  output$plot.DWD_vs_Luftdaten.humidity <- renderPlot({
    df <- data.plot()$spreado  %>% filter(!is.na(humidity.rm))
    df$adjusted <- adjusted()
    
    df$ratio.2 <- df$PM10.rm / df$P1.rm
    df$ratio.1 <- df$PM10.rm / df$adjusted
    
    
    
    model.1 <- lm(data = df, ratio.1 ~ humidity.rm)
    rmse.1 <- sqrt(mean(model.1$residuals ^ 2))
    adjusted_r2.1 <-
      paste0('Adj.R2 : ',
             round(summary(model.1)$adj.r.squared, 2) ,
             '- rmse: ',
             rmse.1)
    
    df <- df %>% na.omit(.)
    
    model.2 <- lm(data = df, ratio.2 ~ humidity.rm)
    rmse.2 <- sqrt(mean(model.2$residuals ^ 2))
    adjusted_r2.2 <-
      paste0('Adj.R2 : ',
             round(summary(model.2)$adj.r.squared, 2),
             '- rmse: ',
             rmse.2)
    
    
    gato <-
      df %>% select(datetime, ratio.1, ratio.2, humidity.rm, temperature.rm)
    
    gato <-
      gato %>% gather(key, value, -datetime, -humidity.rm, -temperature.rm)
    
    p <-
      ggplot(gato, aes(humidity.rm, value)) + geom_point(aes(alpha = .2, color =
                                                               temperature.rm), size = .5) + ylim(0, 10) + geom_hline(
                                                                 yintercept = 1,
                                                                 linetype = "dashed",
                                                                 color = "red",
                                                                 size = 2
                                                               )+scale_color_viridis(option="magma")
    
    p + facet_grid(. ~ key)
  })
  
  
})
