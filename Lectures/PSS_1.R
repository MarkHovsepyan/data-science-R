## reading "autos.csv" data
autos <- read.csv("autos.csv")

## subsetting using standard functionallity
autos_new <- subset(autos, Price >= 100 & Price <= 150000 &
                      Power >= 50 & Power <= 650 &
                      Year >= 1930 & Year <= 2016)

## subsetting using "dplyr" functionallity
library("dplyr")

autos_new1 <- autos %>% 
  filter(Price >= 100 & Price <= 150000 &
  Power >= 50 & Power <= 650 &
  Year >= 1930 & Year <= 2016)

## taking one brand and one year: Mercedez-Benz and 2010
merc_2010 <- subset(autos_new, Year == 2010 & Brand == 'mercedes_benz')

library(ggplot2)

## checking power and price correlation through scatterplot
ggplot(data = merc_2010, aes(x = Power, y = Price)) +
  geom_point()

## historgam for "Brand" in our dataset
ggplot(data = autos_new, aes(x = Brand)) +
  geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate the labels on x


## subsetting a brand: Mercedez-Benz
merc <- subset(autos_new, Brand == 'mercedes_benz')

## subsetting a brand: BMW
bmw <- subset(autos_new, Brand == 'bmw')


## historgams for "Model" in our datasets: mercedes and bmw
ggplot(data = merc, aes(x = Model)) +
  geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate the labels on x

ggplot(data = bmw, aes(x = Model)) +
  geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate the labels on x

## same with faceting
merc_benz <- subset(autos_new, Brand == c('bmw', 'mercedes_benz'))

ggplot(data = merc_benz, aes(x = Model)) +
  geom_histogram(stat = "count") +
  facet_grid(.~Brand) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate the labels on x


## pie-chart for fuel types
ggplot(data = autos_new, aes(x = factor(1), fill = Fuel)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y")





