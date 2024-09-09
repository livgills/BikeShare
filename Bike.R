library(tidyverse)
library(vroom)
library(patchwork)
library(DataExplorer)
library(GGally)

#Read in Code
data <- vroom("Stat348/BikeShare/train.csv")
data_test <- vroom("Stat348/BikeShare/test.csv")

#Exploratory Data Analysis 
glimpse(data)

plot_correlation(data)
plot_bar(data)
plot_histogram(data)
plot_missing(data)


#Plots
graph1 <- ggplot(data = data) +
  geom_boxplot(aes(x=weather))
graph1

graph2 <- ggplot(data = data, aes(x = temp, y = atemp)) +
                   geom_point() +
                   geom_smooth()
graph2

graph3 <-ggplot(data = data, aes(x = humidity, y = count)) +
                  geom_point() +
                  geom_smooth()
graph3

graph4 <- ggplot(data = data, aes(x = temp, y = count)) +
                  geom_point() +
                  geom_smooth()
graph4

(graph1 + graph2)/(graph3 + graph4)
