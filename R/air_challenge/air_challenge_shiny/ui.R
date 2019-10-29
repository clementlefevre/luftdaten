#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinymaterial)
library(plotly)
material_page(
    title = "Air challenge",
    tags$br(),
    material_row(
        material_column(
            width = 2,
            material_card(
                title = "",
                depth = 4,
               
                material_radio_button(
                    input_id = "time.range",
                    label = "Time Range",
                    choices = 
                        c("72h" = "72h",
                          "all year" = "all year")
                )
            )
           
        ),
        material_column(
            width = 9,
            material_card(
                title = "",
                depth = 4,
                plotlyOutput("plot1")
            )
        )
    )
)
