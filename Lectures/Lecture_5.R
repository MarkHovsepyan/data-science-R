## String Manipulations and Regular Expressions

## characters
x <- "This is R"
x
class(x)


x <- 'This is R'
x
class(x)

x <- "Why call language \'python\' ? "
x
class(x)
  
is.character(x)

PI <- paste("Life of", pi)
PI

x <- 1:10
a <- letters[1:10]

k <- paste(x, a) # join characters
k

## use my own separator
k <- paste(x, a, sep = "-")
k

## joined with commas
k <- paste(x, a, sep = "-", collapse = ",")
k

length(k) # it is 1 character

x <- "This is R"
nchar(x) # number of characters in the string

nchar(month.name) # vector of numbers of characters in the vector

toupper(x) # make upper case
tolower(x) # make lower case

casefold(x, upper = T) # same but with an indicator

x <- "ABCDF"
substr(x, start = 3, stop = 5) # extracting a part of the string

y <- c("may", "the", "force", "be", "with", "you")
substr(y, 2, 2) <- "#" # replacing with a hashtag
y

y <- c("may", "the", "force", "be", "with", "you")
substr(y, 2, 3) <- ":)" # replacing with a smile
y


summer <- read.csv('summer.csv', stringsAsFactors = F)

discipline <- unique(summer$Discipline)
discipline[1:10]

## abbreviating the strings
disc_abbr <- abbreviate(discipline, minlength = 5)
disc_abbr[1:10]


library(stringr)

str_sub("ABCDEF", start = 1, end = 3) # subset
str_sub("ABCDEF", start = -2, end = -1) # subset from the end

genres <- c("Action, Adventure, Comedy", "Comedy", 
            "Comedy, Drama, Drama, Romance", "Crime, Drama, History")

str_detect(string = genres, pattern = "Drama") # detect the pattern

str_subset(string = genres, pattern = "Drama") # return the pattern

str_count(string = genres, pattern = "Drama") # count pattern in each element


movies <- read.csv('movies3.csv', stringsAsFactors = F)

movies$Comedy <- str_detect(string = movies$Genre, pattern = "Comedy")
movies$Romance <- str_detect(string = movies$Genre, pattern = "Romance")
table(Romance = movies$Romance, Comedy = movies$Comedy)


gen_m <- str_split(genres, ",") # split up a string into pieces
gen_m

gen_m <- str_split(genres, ",", simplify = T) # returns matrix instead of a list of vectors
gen_m

str_replace(genres, pattern = ",", replacement = " &") # replace a pattern

str_replace_all(genres, pattern = ",", replacement = " &") # replace all patterns

str <- c("123abc", "ab567cd", "abc5.00")
str_extract(str, pattern = "123") # extract a pattern



## Regular Expressions

str <- c("123abc", "ab567cd", "abc5.00")
str_subset(string = str, pattern = ".") # gets everything

str_subset(string = str, pattern = "\\.") # used escaping "\\"

str_subset(string = str, pattern = "^ab") # only starting with "ab"

str_subset(string = str, pattern = "cd$") # only ending with "cd"

str_subset(string = str, pattern = "[1-4]") # using range of letters

str_subset(string = str, pattern = "[f-z]") # once more, using a range of letters


movies$Awards[1:20]

movies$awards_num <- str_replace_all(movies$Awards, pattern = "[^0-9]",
                                     replacement = " ") # replace with a white space
movies$awards_num[1:20]

movies$awards_num <- str_replace_all(movies$awards_num, pattern = "\\s+",
                                     replacement = " ") # replace many white spaces with one
movies$awards_num[1:20]

movies$awards_num <- trimws(movies$awards_num)

x1 <- str_split(movies$awards_num, pattern = " ", simplify = T) # create a character matrix
head(x1)

x1 <- apply(x1, 2, as.numeric) # make numeric
head(x1)

x1 <- rowSums(x1, na.rm = T)
x1[1:20]

movies$awards_num <- x1

library(ggplot2)

ggplot(movies, aes(x = awards_num))+
  geom_histogram()+
  xlim(c(0, 200))+
  labs(title = "Awards histogram (wins and nominations)",
       x = "Awards", y = "Count")


m1 <- str_extract_all(movies$Awards, pattern = "[0-9]+\\swin", simplify = T)
m1[1:20]

m1 <- str_remove_all(m1, pattern = "[a-zA-Z]") # removing pattern
m1 <- as.numeric(m1) # make numeric

movies$Wins <- m1

ggplot(movies, aes(x = Wins))+
  geom_histogram()+
  labs(title = "Wins histogram",
       x = "Wins", y = "Count")+
  xlim(c(0, 100))


## extracting names and phone numbers

phones <- c("Anna 077-789663", "Hagopik 99656565",
            "Serozh2 099-65-6569 MALYAR")

names <- str_extract(phones, "^[A-Za-z]+")
names # names only

numbers <- str_extract(phones, "\\b[-0-9]+\\b") %>%
  str_remove_all("-")
numbers # numbers only

phones_data <- data.frame("Name" = names, "Phone" = numbers)
phones_data # data-frame of names and numbers
