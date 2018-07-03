## Data Science
## Mark Hovsepyan
## Homework 1

#1) (1p)
#Create a vector "a", containing integers from 1 to 100 and 200 to 300.
#Use help (bottom right corner in RStudio) and find a function that returns
#the length of an object and print the length of the vector.

a <- c(1:100, 200:300) # creating the vector a
a
length(a) # length of vector a

#2) (2p)
#Create a vector "a" containing integers 1-5, letters A,B and C and a boolean value TRUE.
#print the type of the vector and explain why is it of that type.

a <- c(1:5, 'A', 'B', 'C', TRUE)
a
typeof(a) 

## It has attained a "character" type, since it used the default coersion rule
## which is as follows: logical -> integer -> numeric -> complex -> character


#3a) (2p) 
#Generate a vector containing even numbers from 2 to 50 inclusive (Hint! ?seq)
#Build a 5x5 matrix and populate it with the vector generated above, where matrix 
#elements are increasing by row. Finally check whether R stored it as a matrix.

## ?seq

even <- seq(2, 50, by=2) # used increment of 2 to get even numbers starting from 2
even

evenM1 <- matrix(even, nrow = 5, ncol = 5, byrow = FALSE) # by column
evenM1

evenM2 <- matrix(even, nrow = 5, ncol = 5, byrow = TRUE) # by row
evenM2

class(evenM1) # matrix
class(evenM2) # matrix

#3b) (1p)
#Print the element for which Row=3 Column=4. Make the diagonal elements of the matrix 0 (Hint! ?diag)

evenM2[3, 4] # row=3, col=4

## ?diag

diag(evenM2) <- 0
evenM2 # diagonal elements are 0

#4a) (1p)
#Load the "SportsAnalytics270" library, load the dataset "nba2009_2016" 
#from it and save it in R with the name "nba".

library(devtools)
# install_github("HABET/CSE270")
library(SportsAnalytics270)

data("nba2009_2016")
nba <- nba2009_2016
head(nba)

#4b) (2p)
#check the structure of the "nba" database and answer the following questions.
#*) What is the number of rows and columns ?
#*) What are the datatypes of columns "home.PTS", "home.WL" and "home.TEAM_NAME"

str(nba)
## there are 9600 rows and 9 columns

sapply(nba, class)

## looking at the results of str() or sapply(),
## "home.PTS" hass numerical type, 
## "home.WL" has character type 
## and "home.TEAM_NAME" is a categorical (factor) type

#5) (3p)
#Subset the dataframe "nba" into "nba_matches" which will store only home.TEAM_NAME, home.PTS,
#away.TEAM_NAME and away.PTS. Do the same by using column indexes and column names.

nba_matches <- nba[, c(4, 5, 7, 8)]
nba_matches <- nba[, c("home.TEAM_NAME", "home.PTS", "away.TEAM_NAME", "away.PTS")]
head(nba_matches) # same results for both

#6) (3p)
#Subset the "nba" dataframe into "nba_1" containing only those games, where the 
#minimum difference between points is 5. (Hint! ?abs)

## ?abs

nba_1 <- subset(nba, abs(home.PTS - away.PTS) >= 5 )
nba_1 # games with min difference of 5 points

#7) (4p)
#Write a function that takes "x" as an argument and returns vector "y" containing all
#the divisors of "x". (Hint! x%%y expression shows the remainder when x is divided by y)

divisors <- function(x){
  
  ##  vector of potential divisors
  y <- seq_len(x)
  
  ##  If the remainder is 0 => the number is a divisor of x
  y[ x%%y == 0 ]
  
}

divisors(33) # test on 33

#8) (3p)
#Use for loop statement to populate a 5x5 square matrix with random numbers from
#the interval 1-10. (Hint! ?sample).

## ?sample

randMat <- matrix(data = NA, nrow = 5, ncol = 5) # empty matrix

## with two loops
for(j in 1:5){
  for(i in 1:5){
    randMat[i, j] = sample(1:10, 1) # sample from 1 to 10 with size 1
  }
}

## or the same with one loop
for(i in 1:5){
  randMat[i, ] = sample(1:10, 5) # sample from 1 to 10 with size 5
}

randMat # randomly populated

#9) (5p)
#Write a function that takes "x" vector as an input and returns it in an increasing order.
#(Warning!!! Don't use any built in sorting function in R or from other libraries)
#(Hint! There are many sorting algorithms used to sort a vector, an array etc. One is bubblesort)
# https://en.wikipedia.org/wiki/Bubble_sort

## I am going to implement the merge sort type of algorithm into R

## This is a suplementary function that performs the merging operation
merge <- function(a, b) {
  result <- numeric(length(a) + length(b))
  ai <- 1; bi <- 1; j <- 1;
  for(j in 1:length(result)) {
    if((ai <= length(a) && a[ai] < b[bi]) || bi > length(b)) {
      result[j] <- a[ai]
      ai <- ai + 1
    } else {
      result[j] <- b[bi]
      bi <- bi + 1          
    }
  }
  result
}

## This is the main function that follows the merge sort's divide and conquer pattern
mergesort <- function(x) {
  if(length(x) > 1) {
    c <- ceiling(length(x) / 2)
    a <- mergesort(x[1:c])
    b <- mergesort(x[(c + 1):length(x)])
    merge(a, b)
  } else {
    x
  }
}

x <- sample(-200:200, 20, replace = TRUE) # just a random vector ranging from -200 to 200

mergesort(x) ## sorting the vector


#10) (3p)
# Load Exams dataset from Moodle, explain in what format (wide, long) it's represented
# and convert vice versa(If it is wide, convert to long and if long, convert to wide.)

exams <- read.csv("Exams.csv")
str(exams)

## Each subject (student) has data in multiple rows. 
## exams data is initially in long format, where their scores are in different rows

exams <- reshape(exams, idvar = "Person", timevar = "Exam", direction = "wide")
exams

## So I used reshape() to transform the dataset to wide format
## Now every student has a single row with the scores being on different columns
