library(shiny)
library(ggplot2)

#rn <- rnorm(n = 10000, mean = 100, sd = 15)
#ggplot() + geom_histogram(aes(x=rn), bins = 50)

## ui part
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "mu", 
        label = "Mean for distribution:", 
        min = 1, max = 100, value = 50), 
      sliderInput(
        inputId = "sd", 
        label = "SD for distribution:", 
        min = 5, max = 50, value = 40),
      numericInput(inputId = "size", label = "Mean for Dist", min = 1, max = 10000, value = 5000)),
    
    mainPanel(plotOutput("histogram"), 
              h1(textOutput("text_median")))
  )
)


## server part
server <- function(input, output){
  
  rn <- reactive({rnorm(n = input$size, mean = input$mu, sd = input$sd)})
  
  output$histogram <- renderPlot({
    ggplot() + geom_histogram(aes(x=rn()), bins = 50) 
  })
  output$text_median <- renderText({
    paste("Median:", median(rn()))
  })
}

shinyApp(ui = ui, server = server)




