library(tidyverse)
library(vroom)
library(patchwork)
library(DataExplorer)
library(GGally)
library(tidymodels)
library(poissonreg)

#Read in Code
data_train <- vroom("Stat348/BikeShare/train.csv")
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

#Linear Regression
# Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() %>% #Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") %>% # Regression just means quantitative response6
  fit(formula=count~temp + humidity + windspeed, data=data_train)

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_linear_model,
                            new_data=data_test) # Use fit to predict
bike_predictions ## Look at the output

#Submission to Kaggle
kaggle_submission <- bike_predictions %>%
bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")


#Poisson Regression
data_train$season <- as.factor(data_train$season)
data_test$season <- as.factor(data_test$season)
data_train$weather <- as.factor(data_train$weather)
data_test$weather <- as.factor(data_test$weather)
#Modeling
pois_model <- poisson_reg() %>% 
  set_engine("glm") %>% 
  set_mode("regression") %>% 
  fit(formula = count ~ temp + humidity + windspeed + season + weather, data = data_train)

#Predictions for poisson
bike_pred <- predict(pois_model,
                     new_data = data_test)
bike_pred

#Kaggle Submission for Poisson
kaggle_submission <- bike_predictions %>%
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write out the file
vroom_write(x=kaggle_submission, file="./PoisPreds2.csv", delim=",")
