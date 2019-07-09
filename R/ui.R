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
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("readable"),
                   
  # Application title
  titlePanel("Take my breath away."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
    
      selectInput("locationChoice", "Location:",
                  choices = split(luftdaten.sensors.list$location_id,paste(luftdaten.sensors.list$distance_km,"km.(",luftdaten.sensors.list$measurements,")",luftdaten.sensors.list$location_id,'_',luftdaten.sensors.list$dwd_station)),
      selected = ""
    ),
    leafletOutput("map.location.selected"),
    
    
    sliderInput("slider.lag", label = "Lag DWD / Luftdaten (hours)",step=1, min = -5, 
                max = 5, value = 0)),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('Rmd', uiOutput('markdown')),
        tabPanel("Map", leafletOutput("map.dwd")),
        tabPanel("Timeline", plotlyOutput("plot.timeline"), plotOutput("cross_correlation_plot")
                ),
        tabPanel("Relationships", 
                 fluidRow(column(12,
                                 fluidRow(
                                   
                                   column(4,  radioButtons("function.adjust", "Growth function :",
                                                           c("Haehnel" = "haehnel",
                                                             "Soneja" = "soneja",
                                                             "Combo" = "combo",
                                                             "Skupin" = "skupin"
                                                           ))),
                                   column(4,uiOutput('growth.function.def')
                                          
                                         ),
                                   column(4,  sliderInput("slider.alpha", label = "\u03B1",step=.05, min = 0, 
                                                          max = 30, value = .5),sliderInput("slider.beta", label = "\u03B2",step=.05, min = 0, 
                                                         max = 3, value = .5))
                                 ),
                                 hr(),
                               
                                 plotOutput("plot.DWD_vs_Luftdaten")),
                          plotOutput("plot.DWD_vs_Luftdaten.humidity")
                                 )
                          )
                
      )
    )
  )
))
