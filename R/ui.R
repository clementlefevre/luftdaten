#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
    
      selectInput("locationChoice", "Location:",
                  choices = split(luftdaten.sensors.list$location_id,paste(luftdaten.sensors.list$distance_km,"km.(",luftdaten.sensors.list$measurements,")",luftdaten.sensors.list$location_id,'_',luftdaten.sensors.list$dwd_station)),
      selected = ""
    ),
    leafletOutput("map.location.selected"),
    sliderInput("slider.haehnel.beta", label = "beta for haehnel",step=.05, min = 0, 
                max = 3, value = .5),
    sliderInput("slider.lag", label = "Lag DWD / Luftdaten",step=1, min = -5, 
                max = 5, value = 0)),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map.dwd")),
        tabPanel("Timeline", plotlyOutput("plot.timeline"), plotOutput("cross_correlation_plot")
                ),
        tabPanel("Relationships", 
                 plotOutput("plot.DWD_vs_Luftdaten"),
                 plotOutput("plot.DWD_vs_Luftdaten.humidity"))
      )
    )
  )
))
