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
           'skupin'="$$gf_{Skupin} = \\left\\{
    \\begin{array}{ll}
        \\frac{\\alpha}{(1-rh)^\\beta} & \\mbox{if } rh \\ge 0.7 \\\\
        \\frac{1}{(1-rh)^{\\beta-\\frac{log(a)}{log(0.3)}}} & \\mbox{if } rh < 0.7 \\\\
    \\end{array}
\\right.
$$")
    
    
      withMathJax(helpText(text))
  }
  )
  
    
  adjusted <- function(df){
    
        rh <- df$humidity.rm / 100
        rh[rh == 1] <- .9999
        growth.factor <- growthFunction(input$function.adjust,rh,input$slider.alpha,input$slider.beta)
        df$adjusted <-df$P1.rm *  growth.factor
        return(df)
  }
  
  observeEvent(input$map.dwd_marker_click, {
    p <- input$map.dwd_marker_click  # typo was on this line
    
    if (!is.null(input$map.dwd_marker_click$id)) {
      updateSelectInput(session,
                        "locationChoice",
                        
                        selected = input$map.dwd_marker_click$id)
    }
    
  })
  
  
  
  data.plot <-
    eventReactive(c(input$locationChoice, input$slider.lag),{
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
    df <- adjusted(df)
    
    plotTimeLine(df)
    
   
  })
  
 
  
  dataForScatterPlots <- function(){
    d<- event_data('plotly_relayout')
    
    df <- data.plot()$spreado  %>% filter(!is.na(humidity.rm))
    
    date.start <- ymd_hms(d$`xaxis.range[0]`)
    date.end <-  ymd_hms(d$`xaxis.range[1]`)
    
    if(length(date.start>0) && date.start>min(df$datetime)&& date.end>date.start){
      df <- df %>% filter(datetime>date.start & datetime <date.end)
    }
    
    df <- adjusted(df)
    return(df)
  }
  
  

  output$plot.DWD_vs_Luftdaten <- renderPlot({
    df <- dataForScatterPlots()
  plotScatter(df,input$function.adjust)
    
    
  })
  
 
  
})
