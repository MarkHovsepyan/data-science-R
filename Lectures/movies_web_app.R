library(shiny)
library(ggplot2)


movies <- read.csv("movies3.csv", stringsAsFactors = F)
movies_num <- movies[, sapply(movies, is.numeric)]
#head(movies_num)

## ui part
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "first", label = "X axis",
                  choices = colnames(movies_num), selected = "gross_adjusted"), 
      selectInput(inputId = "second", label = "Y axis",
                  choices = colnames(movies_num), selected = "budget_adjusted"), 
      sliderInput(inputId = "alpha", label = "Transparency:", 
                  min = 0, max = 1, value = 0.5)),
    fluidRow(
      column(4, plotOutput("Scatterplot")), 
      column(4, plotOutput("summary"))
    )

    )
  )


ui <- fluidPage(
  
    fluidRow(
      column(1, selectInput(inputId = "first", label = "X axis",
                  choices = colnames(movies_num), selected = "gross_adjusted")), 
      column(1, selectInput(inputId = "second", label = "Y axis",
                  choices = colnames(movies_num), selected = "budget_adjusted")), 
      column(1, selectInput(inputId = "alpha", label = "Transparency:", 
                  min = 0, max = 1, value = 0.5))),
    fluidRow(
      column(4, plotOutput("Scatterplot")), 
      column(4, plotOutput("summary"))
    )
)


## server part
server <- function(input, output){
  output$scatterplot <- renderPlot({
    ggplot(data = movies_num, aes_string(x = input$first, y = input$second))+
      geom_point(alpha = input$alpha)
  })
  
  output$summary <- renderDataTable({
    movies_num[, c(input$first, input$second)]
  })
}

shinyApp(ui = ui, server = server)

