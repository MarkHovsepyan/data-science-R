## loading the shinydashboard and other packages
## make sure to have all installed and loaded
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(httr)
library(devtools)
library(stringr)
library(rvest)


## the following is an API interface for stats.NBA.com
## please install it in order to run the application

#devtools::install_github('stephematician/statsnbaR')

library(statsnbaR)                                   



## getting total player statistics by season
get_by_season_total <- function(season) {
  
  player_stat <- per_player_agg(filter_per_player(season = season))
  colnames(player_stat)[2] <- "Player"
  colnames(player_stat)[4] <- "Team"
  colnames(player_stat)[5] <- "Age"
  
  player_stat <- na.omit(player_stat)
  
  player_stat
  
}


## get the seasons
## in nba stat there was only data starting from 1996 that was available in the same format
get_seasons <- function(start = 1996) {
  seasons <- start : 2017;
}

## get the list of players
get_player_list <- function(df) {
  player_df <- df %>%
    select(Player) %>%
    unique() 
  player_df$Player <- sort(player_df$Player)
  
  player_df
}

get_player_table <- function(player, df) {
  player_table_df <- df %>%
    filter(Player == player) %>%
    select(-c(1, 3))
  player_table_df
}

## get player age
get_player_age <- function(player_table_df) {
  player_table_df["Age"] %>% unique()
}

## get player team
get_player_team <- function(player_table_df) {
  player_table_df["Team"]
}

## clear and get the stats we need
get_player_stats <- function(player_table_df) {
  player_table_df[1, -c(1, 2, 3)]
}


## getting player pictures
get_pic_link <- function(player) {
  split <- str_split(tolower(player), " ")
  name <- unlist(split)
  two <- str_sub(name[1], 1, 2)
  five <- str_sub(name[2], 1, 5)
  first <- "01"
  second <- "02"
  third <- "03"
  # placeholder
  "https://d2p3bygnnzw9w3.cloudfront.net/req/999999999/images/klecko/placeholder.jpg"
  convention <- paste0(five, two, first)
  link <- paste0("https://d2cwpp38twqe55.cloudfront.net/req/201712064/images/players/", convention, ".jpg")
  link
}


## creating my dashboard
header <- dashboardHeader(title = "Player Comparison",
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Mark Hovsepyan",
                                         message = "Welcome! Let's start!"
                                       )
                          ),
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = "Please read instructions to start",
                                         icon = icon("info-circle"),
                                         status = "success"
                                       )
                          )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
    menuItem("Compare", tabName = "compare", icon = icon("bar-chart"), badgeLabel = "app", badgeColor = "purple"),
    menuItem("Information", tabName = "contact", icon = icon("envelope-o")),
    menuItem("NBA Official Satistics", icon = icon("send", lib = 'glyphicon'), 
             href = "https://stats.nba.com")
    )
)

              

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "instructions",
            h4("1. Choose a season"),
            h4("2. Select the first player"),
            h4("3. Select the second player"),
            h4("4. Click Compare"),
            h4('5. Check out "Per Game" and "Totals" tabs'),
            h4("6. Every time you choose new player(s) click compare to update the information"),
            h4("7. Enjoy!"),
            br(),
            h4("NOTES:"),
            h4("> Expansion and explanation of the abbreviations:",
               a("Glossary", href = "https://stats.nba.com/help/glossary/")
               )
    ),
            
    # Second tab content
    tabItem(tabName = "compare",
            fluidRow(
              box(
                title = "Player 1", status = "warning", solidHeader = TRUE,
                collapsible = F, width = 4, height = 330,
                htmlOutput("player1imgOutput"),
                HTML('<center><div id="info1" class="shiny-html-output"></div></center>')
                ),
              
              box(
                title = "Controls", status = "danger", solidHeader = TRUE,
                width = 4, height = 330,
                selectInput(inputId = "seasonInput", "Choose a season:", choices = get_seasons(), selected = 2017),
                uiOutput("player1Output"),
                uiOutput("player2Output"),
                uiOutput("compareOutput")
              ),
                                  
              box(
                title = "Player 2", status = "info", solidHeader = TRUE,
                collapsible = F, width = 4, height = 330,
                htmlOutput("player2imgOutput"),
                HTML('<center><div id="info2" class="shiny-html-output"></div></center>')
                )
              ),
              
            fluidRow(
              box(
                title = "Stats",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabsetInput", height = "220px", width = 12,
                         
                tabPanel("Totals",
                         div(style = 'overflow-x: scroll', tableOutput("result1Output")),
                         div(style = 'overflow-x: scroll', tableOutput("result2Output"))
                         )
                )
              ),
            
            fluidRow(
              div(style = "height:13px; padding: 1px;"),
              plotOutput("scatterplot")
            )
            
            ),
                        
                        
    tabItem(tabName = "sources",
            h4("All the data is taken from",
               a("NBA.com", href = "https://stats.nba.com/")
               )
            ),
    tabItem(tabName = "contact",
            h4("Data Science Homework 5 by Mark Hovsepyan"),
            h4("Please feel free to share your comments and/or report any issues."),
            br(),
            h4("Mark Hovsepyan", 
               br(),
               a("Email", href="mailto:markhovsepyan98@gmail.com")
               )
            )
    )
  )

ui <- dashboardPage(header, sidebar, body, skin = 'red')

server <- function(input, output, session) {
  
  totals <- reactive( {
    get_by_season_total(as.numeric(input$seasonInput))
  })
  
  main_totals_1 <- eventReactive(input$compareInput, {
    get_player_table(input$player1Input, totals())
  })
  
  main_totals_2 <- eventReactive(input$compareInput, {
    get_player_table(input$player2Input, totals())
  })
  
  img1 <- eventReactive(input$compareInput, {
    get_pic_link(input$player1Input)
  })
  
  img2 <- eventReactive(input$compareInput, {
    get_pic_link(input$player2Input)
    
  })
  
  info1 <- eventReactive(input$compareInput, {
    age1 <- get_player_age(main_totals_1())
    team1 <- get_player_team(main_totals_1())
    cbind(team1, age1, row.names = NULL)
  })
  
  info2 <- eventReactive(input$compareInput, {
    age2 <- get_player_age(main_totals_2())
    team2 <- get_player_team(main_totals_2())
    cbind(team2, age2, row.names = NULL)
  })
  
  output$player1Output <- renderUI({
    selectInput(inputId = "player1Input", "Select player 1:", choices = get_player_list(totals()))
  })
  
  output$player2Output <- renderUI({
    selectInput(inputId = "player2Input", "Select player 2:", choices = get_player_list(totals()))
  })
  
  output$compareOutput <- renderUI({
    HTML('<center><button id="compareInput" type="button" class="btn btn-default action-button">Compare</button></center>')
  })
  
  
  output$player1imgOutput <- renderText({
    c('<center>', '<img height="180" width="120" src="', img1(), '">', '</center>')
  })
  
  output$player2imgOutput <- renderText({
    c('<center>', '<img height="180" width="120" src="', img2(), '">', '</center>')
  })
  
  output$info1 <- renderTable({
    info1()
  })
  
  output$info2 <- renderTable({
    info2()
  })
  
  output$result1Output <- renderTable({
    get_player_stats(main_totals_1())
  })
  
  output$result2Output <- renderTable({
    get_player_stats(main_totals_2())
  })
  
  ## scatterplot for player and their fields goals which depends on the selected season
  ## fgm/fga = fg%
  output$scatterplot <- renderPlot({
    ggplot(totals(), aes(x = Player, y = (fgm/fga))) +
      geom_point() +
      theme(axis.text.x = element_blank()) +
      labs(title = "Scatter Plot for players and their Field Goal Percentage", 
           x = "Player", y = "FG%")
      
  })
}

shinyApp(ui, server)