library(shinydashboard)
library(shiny)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "This is my dashboard"), 
  dashboardSidebar(
    sliderInput(inputId = "mu", label = "Mean for dist", 
                min = 1, max = 100, value = 50),
    sliderInput(inputId = "sd", label = "SD for dist", 
                min = 5, max = 50, value = 40),
    numericInput(inputId = "sample", label = "Sample size", 
                 min = 100, max = 10000, value = 1000)
  ), 
  dashboardBody(plotOutput("histogram"))
)


server <- function(input, output){
  
  rn <- reactive({rnorm(n = input$sample, mean = input$mu, sd = input$sd)})
  
  output$histogram <- renderPlot({
    ggplot() + geom_histogram(aes(x=rn()), bins = 50) 
  })
  output$text_median <- renderText({
    paste("Median:", median(rn()))
  })
}

shinyApp(ui, server)

#install.packages("rsconnect")
