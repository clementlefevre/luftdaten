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
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("readable"),
                  
                  # Sidebar with a slider input for number of bins
                  sidebarLayout(
                    sidebarPanel(
                      width = 2,
                      selectInput(
                        "locationChoice",
                        "Location:",
                        choices = split(
                          luftdaten.sensors.list$location_id,
                          paste(
                            luftdaten.sensors.list$distance_km,
                            "km.(",
                            luftdaten.sensors.list$measurements,
                            ")",
                            luftdaten.sensors.list$location_id,
                            '_',
                            luftdaten.sensors.list$dwd_station
                          )
                        ),
                        selected = ""
                      ),
                      leafletOutput("map.location.selected"),
                      
                      
                      numericInput(
                        "slider.lag",
                        label = "Lag DWD/Luftdaten (h)",
                        step = 1,
                        min = -5,
                        max = 5,
                        value = 0
                      )
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(width=10,tabsetPanel( 
                      tabPanel('Rmd', uiOutput('markdown')),
                      tabPanel("Map", leafletOutput("map.dwd")),
                      tabPanel('Table', DT::dataTableOutput("selected"),DT::dataTableOutput('locationTable')),
                      tabPanel("Timeline",
                               
                               
                              
                                 fluidRow(
                                   column(3,
                                          selectInput(
                                            "function.adjust",
                                            "Growth function :",
                                            c(
                                              "Haehnel" = "haehnel",
                                              "Soneja" = "soneja",
                                              "Combo" = "combo",
                                              "Skupin" = "skupin"
                                            )
                                          )),
                                   column(3,
                                          uiOutput('growth.function.def')),
                                   
                                   column(
                                     3,
                                     numericInput(
                                       "slider.alpha",
                                       label = "\u03B1",
                                       step = .05,
                                       min = 0,
                                       max = 30,
                                       value = .5
                                     )
                                   ),
                                   column(
                                     3,
                                     numericInput(
                                       "slider.beta",
                                       label = "\u03B2",
                                       step = .05,
                                       min = 0,
                                       max = 3,
                                       value = .5
                                     )
                                   )
                                   
                                 )
                                 ,
                                 hr(),
                                 fluidRow(column(6, plotlyOutput("plot.timeline")),
                                          column(
                                            6, plotOutput("plot.DWD_vs_Luftdaten")
                                          ))
                                 
                                 
                               )))
                      
                      
                    
                  )))
