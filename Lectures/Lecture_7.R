## Data Scraping

library(rvest)

url <- "https://www.imdb.com/title/tt0299117/"

## reading from html
rdodger <- read_html(url) %>%
  html_node(css = "h1") %>%
  html_text()

rdodger # got the title

## using xpath to get rating
rating <- read_html(url) %>%
  html_node(xpath = "//strong//span") %>%
  html_text()

rating

## get attributes
read_html(url) %>%
  html_node(xpath = "//strong//span") %>%
  html_attrs()

read_html(url) %>%
  html_node("[itemprop=ratingValue]") %>%
  html_text()

## getting the cast
cast <- read_html(url) %>%
  html_nodes('#titleCast .itemprop span') %>%
  html_text()

cast

## getting the link of the poster
poster <- read_html(url) %>%
  html_nodes(".poster img") %>%
  html_attr("src")

poster


library(curl)

## downloading the poster using library 'curl'
curl_download(url = poster, destfile = "poster.jpg")


## get images of actors
actors_img <- read_html(url) %>%
  html_nodes(".primary_photo img") %>%
  html_attr("loadlate")

actors_img

paste0("actors/", cast, ".jpeg") # we will use this in the loop


for (i in 1:length(actors_img)) {
  link = actors_img[i]
  filename = paste0("actors/", cast[i], ".jpeg")
  try({
    curl_download(link, destfile = filename, mode = "wb")
  })
  
}


## foorball games results
url <- "http://www.worldfootball.net/schedule/ita-serie-a-2017-2018-spieltag/38/"

## getting tables
tables <- read_html(url) %>%
  html_table(url, header = FALSE)

View(tables[[2]])

## getting a table by its name
table <- read_html(url) %>%
  html_node(".standard_tabelle") %>%
  html_table(url, header = FALSE)

View(table)

## cleaning the table
table <- table[, -c(1, 2, 4, 7, 8, 9)]
colnames(table) <- c("Home", "Away", "Score") 

table <- as.data.frame(apply(table, 2, trimws))
head(table)


library(stringr)

## splitting the 'score' column
halves <- str_split(table$Score, " ", simplify = T)
head(halves)

## splitting again
final_s <- str_split(halves[, 1], ":", simplify = T)
final_s

## making numeric
final_s <- apply(final_s, 2, as.numeric)
final_s

## clean the first half scores
first_half <- str_remove_all(halves[, 2], "[\\(\\)]")
first_half

## alternatively we can do the following, skipping the previous steps
halves1 <- str_replace_all(table$Score, "[\\(\\)]", ":")
halves1 <- str_split(halves1, ":", simplify = T)
halves1

scores <- apply(halves1[, 1:4], 2, as.numeric)

colnames(scores) <- c("Home_final", "Away_final", "Home_fh", "Away_fh")

game_38 <- data.frame(table[, 1:2], scores)
game_38 # final result


## scraping data from 'boxofficemojo.com'
url <- "http://www.boxofficemojo.com/alltime/weekends/"

officemojo_tables <- read_html(url) %>%
  html_table(url, header = TRUE, fill = T)

top_table <- officemojo_tables[[5]]
top_table <- top_table[, -1]
top_table <- as.data.frame(top_table)


top_table[, 3] <- top_table[, 3] %>% 
  str_remove_all("[\\$,]") %>%
  as.numeric()
top_table[, 6] <- top_table[, 6] %>% 
  str_remove_all("[\\$,]") %>%
  as.numeric()
top_table[, 7] <- top_table[, 7] %>% 
  str_remove_all("[\\$,]") %>%
  as.numeric()

colnames(top_table) <- c("Title", "Studio", "Opening", "% of Total", "Theaters", "Avg.", "Total Gross", "Date")

sapply(top_table, is.numeric)

View(top_table)


library(ggplot2)

ggplot()




