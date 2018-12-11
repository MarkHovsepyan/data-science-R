## R Intro

## vectors
x <- c(10, 5, 6)

x1 <- c(1:10)

class(x)

y <- c("CS", "DS", "EC")

class(y)

x3<-c("A", 1)

class(x3)

## matrices
m <- matrix(data = c(1:15), nrow = 3)
m

m <- matrix(c(1:15), 3)
m

m <- matrix(3, c(1:15))
m

library(devtools)

## install_github("HABET/CSE270")

library(SportsAnalytics270)

nba <- nba2009_2016[c(1:100), c(2,3,5)]

nba1 <- nba2009_2016[-c(250,300,350), -c(5)]

nba2009_2016$diff = nba2009_2016$home.PTS - nba2009_2016$away.PTS

nba3 <- nba2009_2016[1:100, c(2,3,5)]

nba4 <- nba2009_2016[-c(250,300,350),-5]

mean(nba2009_2016$home.PTS)

mean(nba2009_2016$away.PTS)

table(nba2009_2016$home.WL) # home team lose/win, outputs frequences

nba2009_2016$diff <- nba2009_2016$home.PTS - nba2009_2016$away.PTS

nba4 <- nba2009_2016[nba2009_2016$SEASON_ID=='2009',]

table(nba4$SEASON_ID)

nba5 <- nba2009_2016[nba2009_2016$SEASON_ID %in% c('2009','2010'),]

table(nba5$SEASON_ID)

nba2009_2016$SEASON_ID <- as.numeric(nba2009_2016$SEASON_ID)

nba5 <- nba2009_2016[nba2009_2016$SEASON_ID < 2011,]

table(nba5$SEASON_ID)


nba6 <- nba2009_2016[nba2009_2016$SEASON_ID %in% c(2010,2011) & 
                       nba2009_2016$home.TEAM_NAME=="Detroit Pistons",]

table(nba6$home.TEAM_NAME,nba6$SEASON_ID)


nba7 <- nba2009_2016[nba2009_2016$away.TEAM_NAME=='Detroit Pistons' | 
                       nba2009_2016$home.TEAM_NAME=='Detroit Pistons',]

head(nba7)

seriea <- data.frame(Club, Points)

seriea

list1 <- list(Club, "Italy", 2017, seriea)

list1

list1[[1]]

list1[1:2]

list1[[1]][2]


## conditional statements and loops

x <- 5

if (x == 5) {
  y <- x*5
}
y


x <- 2
if (x == 5) {
  y <- x*5
} else {
  y<-x
}
y


x <- -4
if (x < 0){
  x <- x^2
} else if (x == 0) {
  x <- 1
} else {
  x <- sqrt(x)
}
x


for (i in 1:5) {
  print(i)
}


x <- c(5,6,8,9)
for (i in x){
  y <- x+2
}
y

data(mtcars)
dim(mtcars)
ncol(mtcars)


## Calculate the mean of all columns , 1st way
x <- c()
for (i in 1:ncol(mtcars)){
  x1 <- mean(mtcars[,i])
  x <- c(x,x1)
}
x

## Calculate the mean of all columns , 2nd way
x <- c()
for (i in 1:ncol(mtcars)){
  x1 <- mean(mtcars[,i])
  x[i] <- x1
}
x

## functions

formals(runif)
args(runif)

runif(min=1,n=10,max=2) # matching by name
runif(10,1,2)           # matching by position 
runif(10)               # default values are used; min=0, max=1 
runif()                 # hit tab inside the brackets to look at the args

# f <- function(a,b=1,c=2,d=NULL){
#   what needs to be done
# }
# Example

foo <- function(x,y=2){
  x^y
}
foo(4)

## function to calculate the z-score of a vector
norm <- function(x) {
  return((x-mean(x))/sd(x))
}
norm(mtcars$mpg)

## Max-Min normalization
dataframe_norm <- function(df){
  norm <- function(x){
    return((x-mean(x))/sd(x))
  }
  df1 <- c()
  for(i in 1:ncol(df)){
    x1 <- norm(df[,i])
    df1 <- cbind(df1,x1)
  }
  df1 <- as.data.frame(df1)
  colnames(df1) <- colnames(df)
  return(df1)
}

dataframe_norm(mtcars)

## Dates in R
x<- as.Date("2018-06-18")
class(x)
as.numeric(x)
as.numeric(as.Date("1970-01-01"))

oil <- read.csv("oil.csv",stringsAsFactors = FALSE)
head(oil)
str(oil)
oil$DATE <- as.Date(oil$DATE, format="%d-%b-%y")
str(oil)
oil_2014 <- oil[oil$DATE >= "2014-01-01" & oil$DATE < "2015-01-01",]
head(oil_2014)
as.Date("2018-12-31")+1
as.Date("2018-06-18")-as.Date("2018-05-18")

# Extracting weekday
oil$WEEKDAY <- weekdays(oil$DATE)
head(oil)
# Extracting month
oil$MONTH <- months(oil$DATE)
head(oil)
# Formatting
oil$DAY <- format(oil$DATE,"%d")
str(oil)
head(oil)
# Make the variable numeric
oil$DAY <- as.numeric(oil$DAY)
head(oil)


library(SportsAnalytics270)
data(nba_east)
head(nba_east) 
## wide format
#   Rank               Team  W  L   Pct   GB Season
# 1    1      Chicago Bulls 61 21 0.744    -   1991
# 2    2     Boston Celtics 56 26 0.683  5.0   1991
# 3    3    Detroit Pistons 50 32 0.610 11.0   1991
# 4    4    Milwaukee Bucks 48 34 0.585 13.0   1991
# 5    5 Philadelphia 76ers 44 38 0.537 17.0   1991

library(reshape2)
install.packages("reshape") 
nba_long <- melt(nba_east, id.vars = c("Team", "Season"))
head(nba_long)
nba15 <- read.csv("nba15.csv")
head(nba15)


nba15_wide <- reshape(nba15,timevar = "H", idvar="GAME_ID", direction = "wide")
head(nba15_long)