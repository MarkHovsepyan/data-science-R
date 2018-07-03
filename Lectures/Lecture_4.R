## Data Cleaning

movies4 <- read.csv("movies4c.csv", stringsAsFactors = FALSE)
str(movies4)

sapply(movies4, class)

unique(movies4$imdbRating) # to spot the problematic observations

movies4 <- read.csv("movies4c.csv", stringsAsFactors = FALSE,
                    na.strings = c("NA", "#NAME?", "#DIV/0!"))

sapply(movies4, class) # now imdbRating is numeric

summary(movies4$imdbRating)

movies4$imdbRating <- ifelse(movies4$imdbRating > 10, NA, movies4$imdbRating)
summary(movies4$imdbRating)

unique(movies4$genre_first)
table(movies4$genre_first)

movies4$genre_first <- toupper(movies4$genre_first) # make upper case

movies4$genre_first <- trimws(movies4$genre_first) # trim white spaces

table(movies4$genre_first)

unique(movies4$imdbVotes)[1:50] # noticed some problem with thousand comma separator

movies4$imdbVotes <- gsub("[,]", "", movies4$imdbVotes)
movies4$imdbVotes <- as.numeric(movies4$imdbVotes)
summary(movies4$imdbVotes)



## Data Manipulation, transformation and aggregation

data(mtcars)

apply(mtcars, 2, mean) # calculate averages for columns

apply(mtcars, 1, mean) # calculate averages for rows

apply(mtcars, 2, quantile, probs = c(.25, .5, .75)) # calculate quantiles for columns

## function for normaliztion
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

mtcars_norm <- apply(mtcars, 2, normalize)
class(mtcars_norm)

mtcars_norm <- as.data.frame(mtcars_norm)
head(mtcars_norm)

## similarly
mtcars_norm1 <- apply(mtcars, 2, function(x) (x - min(x)) / (max(x) - min(x)))
mtcars_norm1 <- as.data.frame(mtcars_norm1)
head(mtcars_norm1, n = 4)

identical(mtcars_norm, mtcars_norm1) # TRUE => objects are the same

## using lapply
lapply(mtcars, mean) # returns a list

l1 <- list(
  x = matrix(rnorm(50), ncol = 5),
  y = matrix(rnorm(30), ncol = 3),
  z = 1:25
)

l1

lapply(l1, mean)

## using sapply
sapply(l1, mean)

movies <- read.csv("movies3.csv", stringsAsFactors = F)
sapply(movies, is.numeric)

movies_num <- movies[, sapply(movies, is.numeric)]
summary(movies_num)




## Data manipulation and transformation, dplyr

library(dplyr)

summer <- read.csv("summer.csv", stringsAsFactors = F)

## using pipe operator to pass the argument
summer_usa <- summer %>%
  filter(Country == "USA")

table(summer_usa$Country, summer_usa$Medal)

summer %>%
  filter(Country %in% c("USA", "FRA", "GBR")) %>%
  group_by(Country) %>%
  summarise(Count = n())


summer %>%
  filter(Country %in% c("USA", "FRA", "GBR")) %>%
  group_by(Country, Medal) %>%
  summarise(Count=n()) %>%
  ggplot(aes(x = Medal, y = Count, fill = Medal))+
  geom_bar(stat = "identity")+
  facet_grid(.~Country)

movies <- read.csv("movies3.csv")

sum_movie <- movies %>%
  group_by(genre_first) %>%
  summarise(count = n(), mean  = mean(gross_adjusted),
            standard_dev = sd(gross_adjusted)) %>%
  arrange(desc(mean)) # arranged by desceding order of means

head(sum_movie)

ggplot(sum_movie, aes(x = mean, y = standard_dev, size = count))+
  geom_point()+
  geom_text(aes(label = genre_first), size = 3)+
  labs(x = "Mean", y = "Standard deviation", title = "Mean vs Standard Deviation for genres")


nba15 <- read.csv("nba15.csv", stringsAsFactors = F)
str(nba15)

nba15$GAME_DATE_EST <- as.Date(nba15$GAME_DATE_EST, format = "%Y-%m-%d")
str(nba15)
