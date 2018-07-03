## Data Visualization

library(ggplot2)

data(iris)
str(iris)

g1 <- ggplot(data = iris, aes(x=Sepal.Length, y=Sepal.Width))

## scatterplot
g1+geom_point(color="red", size=4, shape=20)
g1+geom_line()


col2rgb("gold")


library(RColorBrewer)
brewer.pal(n=5, name = "Set3")


g1+
  geom_point(aes(color = Species), size=4, shape=20)+
  scale_color_manual(values = c("red", "green", "orange"))


library(wesanderson) ## color palette library

wes_palette(name = "IsleofDogs2")

wes_palettes$Darjeeling1

g1+
  geom_point(aes(color = Species), size=4, shape=20)+
  scale_color_manual(values = c("#FF0000", "#00A08A", "#F2AD00"))

g1+
  geom_point(aes(color = Species), size=4, shape=20)+
  scale_color_manual(values = wes_palette("GrandBudapest1"))


## used color on petal length
g1+
  geom_point(aes(color = Petal.Length), size=4, shape=20)


## used shape for species
g1+
  geom_point(aes(shape = Species), size=4)

## used size for Petal.Length and color for species
g1+
  geom_point(aes(size = Petal.Length, color = Species))


## movies dataset
movies <- read.csv("movies3.csv")
colnames(movies)

## histogram of the dataset
ggplot(movies, aes(x=imdbRating))+
  geom_histogram(aes(fill = "red"), binwidth = 0.2)


## summer dataset
summer <- read.csv("summer.csv")
summer_usa <- summer[summer$Country=="USA", ]

## barchart for medal types of USA on summer Olympics
ggplot(summer_usa, aes(x = Medal, fill = Medal))+
  geom_bar()

levels(summer_usa$Medal)

## refactoring the levels
summer_usa$Medal <- factor(summer_usa$Medal, levels = c("Silver", "Gold", "Bronze"))

## new barchart
ggplot(summer_usa, aes(x = Medal))+
  geom_bar(fill = c("#E6E8FA", "#CFB53B", "#8C7853"))

usa_medals <- data.frame(Medal = c("Silver", "Gold", "Bronze"), 
                         Percentage = c(0.27, 0.49, 0.24))

ggplot(usa_medals, aes(x = Medal, y = Percentage))+
  geom_bar(stat = "identity", fill = c(4, 5, 8))

## refactoring the levels again
summer_usa$Medal <- factor(summer_usa$Medal, levels = c("Bronze", "Silver", "Gold"))

ggplot(summer_usa, aes(x = Year, fill = Medal))+
  geom_bar(position = "fill")

ggplot(summer_usa, aes(x = factor(1), fill = Medal))+
  geom_bar()

ggplot(summer_usa, aes(x = factor(1), fill = Medal))+
  geom_bar(width = 1)+
  coord_polar(theta="y")

## back to movies
ggplot(movies, aes(x=Metascore, y=imdbRating))+
  geom_point(alpha = 0.3, col = "red", size = 2.7)+
  coord_cartesian(ylim = c(5, 10), xlim = c(50, 100))


## boxplots

ggplot(iris, aes(x = Species, y = Sepal.Length))+
  geom_boxplot()

ggplot(iris, aes(x = Species, y = Petal.Length))+
  geom_boxplot()



options(scipen = 999)
ggplot(movies, aes(x = gross_adjusted))+
  geom_histogram()


ggplot(movies, aes(x = "", y = gross_adjusted))+
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 750000000))


ggplot(movies, aes(x = Metascore))+
  geom_histogram()

ggplot(movies, aes(x = "", y = Metascore))+
  geom_boxplot()+
  coord_flip()

## scale

ggplot(movies, aes(x = Metascore, y = imdbRating))+
  geom_point()+
  scale_x_log10() # log scale

ggplot(movies, aes(x=Metascore, y=imdbRating))+
  geom_point()+
  scale_y_sqrt() # sqrt scale


## using faceting
movies_sub <- movies[movies$genre_first %in% c("Action", "Comedy") & 
                       movies$Rated %in% c("PG", "PG-13", "R"), ]

ggplot(movies_sub, aes(x = Metascore, y = imdbRating))+
  geom_point()+
  facet_grid(.~Rated) # split by rating

## or same in the following way
ggplot(movies_sub, aes(x = Metascore, y = imdbRating))+
  geom_point()+
  facet_grid(Rated~.)


ggplot(movies_sub, aes(x = imdbRating))+
  geom_histogram(fill = "red")+
  facet_grid(Rated~genre_first)


## Title, labels for x and y axes

ggplot(movies, aes(x=Metascore, y=imdbRating))+
  geom_point()+
  ggtitle("IMDB ratings vs Metascore")+
  xlab("Metascore")+
  ylab("IMDB")

## same with labs()
ggplot(movies, aes(x=Metascore, y=imdbRating))+
  geom_point()+
  labs(title = "IMDB ratings vs Metascore", x = "Metascore", y = "IMDB")


## themes
ggplot(movies, aes(x=Metascore, y=imdbRating))+
  geom_point()+
  labs(title = "IMDB ratings vs Metascore", x = "Metascore", y = "IMDB")+
  theme(panel.background = element_rect(fill = "green"))


ggplot(movies, aes(x=Metascore, y=imdbRating))+
  geom_point()+
  labs(title = "IMDB ratings vs Metascore", x = "Metascore", y = "IMDB")+
  theme(panel.background = element_blank(), # remove background
        panel.grid.major = element_line(color = "green"), # grid color
        panel.grid.minor = element_line(color = "red")) # grid color


ggplot(movies, aes(x=Metascore, y=imdbRating))+
  geom_point()+
  labs(title = "IMDB ratings vs Metascore", x = "Metascore", y = "IMDB")+
  theme(axis.text.x = element_text(size = 15, color = "red", face = "italic"),
        axis.title.x = element_text(size = 16, color = "green"),
        axis.ticks.x = element_line(size = 1.5, color = "red"))


