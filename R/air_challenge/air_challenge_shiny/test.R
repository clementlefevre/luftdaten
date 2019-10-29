library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
ui <- dashboardPage(
  dashboardHeader(title = "FullscreenDashboard", titleWidth = 450),
  dashboardSidebar(disable = T),
  dashboardBody(
    ### change theme
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    fillPage(
      tags$style(type = "text/css", "#plot1 {height: calc(100vh - 80px) !important;}"),
      plotOutput("plot1", width = "100%",height = "100%")
    )
  )
)

server <- function(input, output, session){
  
  output$plot1 <- renderPlot({ #reactivePlot
    dat <- data.frame(
      time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
      total_bill = c(14.89, 17.23)
    )
    #Plot
    p<-ggplot(data=dat, aes(x=time, y=total_bill, fill=time)) +
      geom_bar(stat="identity")
    p
  })
}
shinyApp(ui, server)