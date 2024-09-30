library(tidyverse)
library(vroom)
library(patchwork)
library(DataExplorer)
library(GGally)
library(tidymodels)
library(poissonreg)
library(ggfortify)
library(glmnet)
library(rpart)
library(tidymodels)
library(ranger)
library(stacks)
library(dbarts)
#Read in Code
data_train <- vroom("Stat348/BikeShare/train.csv")
data_test <- vroom("Stat348/BikeShare/test.csv")

#Exploratory Data Analysis 
glimpse(data_train)

plot_correlation(data_train)
plot_bar(data_train)
plot_histogram(data_train)
plot_missing(data_train)


#Plots
graph1 <- ggplot(data = data_train) +
  geom_boxplot(aes(x=weather))
graph1

graph2 <- ggplot(data = data_train, aes(x = temp, y = atemp)) +
                   geom_point() +
                   geom_smooth()
graph2

graph3 <-ggplot(data = data_train, aes(x = humidity, y = count)) +
                  geom_point() +
                  geom_smooth()
graph3

graph4 <- ggplot(data = data_train, aes(x = temp, y = count)) +
                  geom_point() +
                  geom_smooth()
graph4

(graph1 + graph2)/(graph3 + graph4)

graph5 <- ggplot(data = data_train, aes(x = datetime, y = count)) +
  geom_point() +
  geom_smooth()
graph5

#############Homework 2
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

######## Homework 3
#Poisson Regression
data_train$season <- as.factor(data_train$season)
data_test$season <- as.factor(data_test$season)
data_train$weather <- as.factor(data_train$weather)
data_test$weather <- as.factor(data_test$weather)
#Modeling
pois_model <- poisson_reg() %>% 
  set_engine("glm") %>% 
  set_mode("regression") %>% 
  fit(formula = count ~ temp + humidity + windspeed + season + weather + holiday + datetime, data = data_train)

#Predictions for poisson
bike_pred <- predict(pois_model,
                     new_data = data_test)
bike_pred

#Kaggle Submission for Poisson
kaggle_submission <- bike_pred %>%
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write out the file
vroom_write(x=kaggle_submission, file="./PoisPreds4.csv", delim=",")


#Homework 4

#Cleaning
data_train <- vroom("Stat348/BikeShare/train.csv")
data_test <- vroom("Stat348/BikeShare/test.csv")

trainData <- data_train %>% 
  select(-casual, -registered) %>% 
  mutate( count = log(count))

glimpse(trainData)

#Baking Time

my_recipe <- recipe(count ~ ., data = trainData) %>% 
  step_mutate (weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate (weather = factor (weather, levels = 1:3, labels = c("Sunny", "Mist", "Rain"))) %>% 
  step_mutate (season = as.factor(season)) %>% 
  step_mutate (workingday = factor(workingday, levels = 0:1, labels = c("No", "Yes"))) %>% 
  step_mutate (holiday = factor(holiday, levels = 0:1, labels = c("No", "Yes"))) %>% 
  step_mutate (newtemp = ((temp + atemp)/2)) %>% 
  step_time  (datetime, features = "hour") %>% 
  step_rm (datetime) %>% 
  step_mutate(datetime_hour = as.factor(datetime_hour))
  
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, trainData)
bake(prepped_recipe, new_data = data_test)

#Workflow
lin_model <- linear_reg() %>%
set_engine("lm") %>%
set_mode("regression")

bike_workflow <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(lin_model) %>%
fit(data=trainData)

lin_preds <- predict(bike_workflow, new_data = data_test)
lin_preds <- exp(lin_preds)

#Kaggle_submission
kaggle_submission <- lin_preds %>%
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write out the file
vroom_write(x=kaggle_submission, file="./linpreds.csv", delim=",")

#################### Homework 5

#Penalized Regression
#cleaning and baking
data_train <- vroom("Stat348/BikeShare/train.csv")
data_test <- vroom("Stat348/BikeShare/test.csv")

trainData <- data_train %>% 
  select(-casual, -registered) %>% 
  mutate( count = log(count))

my_recipe2 <- recipe(count ~ ., data = trainData) %>% 
  step_mutate (weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate (weather = factor (weather, levels = 1:3, labels = c("Sunny", "Mist", "Rain"))) %>% 
  step_mutate (season = as.factor(season)) %>% 
  step_mutate (workingday = factor(workingday, levels = 0:1, labels = c("No", "Yes"))) %>% 
  step_mutate (holiday = factor(holiday, levels = 0:1, labels = c("No", "Yes"))) %>% 
  step_mutate (newtemp = ((temp + atemp)/2)) %>% 
  step_time  (datetime, features = "hour") %>% 
  step_rm (datetime) %>% 
  step_mutate(datetime_hour = as.factor(datetime_hour)) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

prepped_recipe2 <- prep(my_recipe2)
bake(prepped_recipe2, trainData)
bake(prepped_recipe2, new_data = data_test)

#Regression 
preg_model <- linear_reg(penalty=0, mixture=1) %>% #Set model and tuning
set_engine("glmnet") # Function to fit in R
preg_wf <- workflow() %>%
add_recipe(my_recipe2) %>%
add_model(preg_model) %>%
fit(data=trainData)
reg_preds <- predict(preg_wf, new_data=data_test)
reg_preds <- exp(reg_preds)

#Kaggle_submission
kaggle_submission <- reg_preds %>%
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write out the file
vroom_write(x=kaggle_submission, file="./reg_preds.csv", delim=",")


###Homework 6 Tuning and Penalized Regression
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% 
  set_engine("glmnet") 

preg_wf <- workflow() %>%
add_recipe(my_recipe2) %>%
add_model(preg_model)

grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 10) ## L^2 total tuning possibilities

folds <- vfold_cv(trainData, v = 5, repeats=5)

CV_results <- preg_wf %>%
tune_grid(resamples=folds,
          grid=grid_of_tuning_params,
          metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL

## Plot Results (example)
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=="rmse") %>%
ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
geom_line()

## Find Best Tuning Parameters
bestTune <- CV_results %>%
select_best(metric = "rmse")

final_wf <-
preg_wf %>%
finalize_workflow(bestTune) %>%
fit(data=trainData)

## Predict
pen_pred <- final_wf %>%
predict(new_data = data_test)
pen_pred <- exp(pen_pred)

kaggle_submission <- pen_pred %>%
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write out the file
vroom_write(x=kaggle_submission, file="./pen_pred.csv", delim=",")


########## Homework 7 Regression Trees
install.packages("rpart")

library(tidymodels)

my_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) %>% #Type of model
  set_engine("rpart") %>% # What R function to use
  set_mode("regression")

pred_wf <- workflow() %>%
  add_recipe(my_recipe2) %>%
  add_model(my_mod)

grid_of_tuning_params <- grid_regular(tree_depth(),
                                      cost_complexity(),
                                      min_n(),
                                      levels = 3) ## L^2 total tuning possibilities

folds <- vfold_cv(trainData, v = 5, repeats=1)

CV_results <- pred_wf %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL
bestTune <- CV_results %>%
  select_best(metric = "rmse")

final_wf <-
  pred_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=trainData)
tree_pred <- final_wf %>%
  predict(new_data = data_test)
tree_pred <- exp(tree_pred)

kaggle_submission <- tree_pred %>%
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write out the file
vroom_write(x=kaggle_submission, file="./tree_pred.csv", delim=",")

######################################################Random Forest

data_train <- vroom("Stat348/BikeShare/train.csv")
data_test <- vroom("Stat348/BikeShare/test.csv")

trainData <- data_train %>% 
  select(-casual, -registered) %>% 
  mutate( count = log(count))

ranfor_recipe <- recipe(count ~ ., data = trainData) %>% 
  step_mutate (weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate (weather = factor (weather, levels = 1:3, labels = c("Sunny", "Mist", "Rain"))) %>% 
  step_mutate (season = as.factor(season)) %>% 
  step_mutate (workingday = factor(workingday, levels = 0:1, labels = c("No", "Yes"))) %>% 
  step_mutate (holiday = factor(holiday, levels = 0:1, labels = c("No", "Yes"))) %>% 
  step_mutate (newtemp = ((temp + atemp)/2)) %>% 
  step_time  (datetime, features = "hour") %>% 
  step_rm (datetime) %>% 
  step_interact(terms = ~workingday:datetime_hour) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

ranforprepped_recipe <- prep(ranfor_recipe)
bake(ranforprepped_recipe, trainData)
bake(ranforprepped_recipe, new_data = data_test)
for_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees= 1000) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")

forest_wf <- workflow() %>%
  add_recipe(ranfor_recipe) %>%
  add_model(for_mod)

forestgrid <- grid_regular(mtry(range = c(1,10)),
                                min_n(),
                                levels = 5)
forest_folds <- vfold_cv(trainData, v = 5, repeats=1)

CV_results <- forest_wf %>%
  tune_grid(resamples=forest_folds,
            grid=forestgrid,
            metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL
bestTune <- CV_results %>%
  select_best(metric = "rmse")

finforest_wf <-
  forest_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=trainData)
forest_pred <- finforest_wf %>%
  predict(new_data = data_test)
forest_pred <- exp(forest_pred)

kaggle_submission <- forest_pred %>%
  bind_cols(., data_test) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>% 
  mutate(datetime=as.character(format(datetime))) 

vroom_write(x=kaggle_submission, file="./forest_pred4.csv", delim=",")

################Rand forest  
bike_train <- vroom("Stat348/BikeShare/train.csv")
bike_test <- vroom("Stat348/BikeShare/test.csv")

trainData <- bike_train %>% 
  select(-casual, -registered) 

ranforest_recipe <- recipe(count ~ ., data = trainData) %>% 
  step_mutate (weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate (weather = factor (weather, levels = 1:3, labels = c("Sunny", "Mist", "Rain"))) %>% 
  step_mutate (workingday = factor(workingday, levels = 0:1, labels = c("No", "Yes"))) %>% 
  step_mutate (holiday = factor(holiday, levels = 0:1, labels = c("No", "Yes"))) %>% 
  step_time  (datetime, features = "hour") %>% 
  step_mutate(hour = as.factor(datetime_hour)) %>% 
  step_rm (datetime) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

ranforprepped_recipe2 <- prep(ranforest_recipe)
bake(ranforprepped_recipe2, trainData)
bake(ranforprepped_recipe2, new_data = bike_test)
forest_mod <- rand_forest(mtry = tune(),
                       min_n=tune(),
                       trees= 500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")

forest_wf2 <- workflow() %>%
  add_recipe(ranforest_recipe) %>%
  add_model(forest_mod)

forestgrid2 <- grid_regular(mtry(range = c(1,10)),
                           min_n(),
                           levels = 3)
forest_folds2 <- vfold_cv(trainData, v = 5, repeats=1)

CV_results <- forest_wf2 %>%
  tune_grid(resamples=forest_folds2,
            grid=forestgrid2,
            metrics=metric_set(rmse, mae, rsq)) #Or leave metrics NULL
bestTune <- CV_results %>%
  select_best(metric = "rmse")

finforest_wf2 <-
  forest_wf2 %>%
  finalize_workflow(bestTune) %>%
  fit(data=trainData)
forest_pred2 <- finforest_wf2 %>%
  predict(new_data = bike_test)

kaggle_submission <- forest_pred2 %>%
  bind_cols(., bike_test) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>% 
  mutate(datetime=as.character(format(datetime))) 

vroom_write(x=kaggle_submission, file="./forest_pred3.csv", delim=",")



############################################ Stacking Models
data_train <- vroom("Stat348/BikeShare/train.csv")
data_test <- vroom("Stat348/BikeShare/test.csv")

trainData <- data_train %>% 
  select(-casual, -registered) %>% 
  mutate( count = log(count))

my_recipe2 <- recipe(count ~ ., data = trainData) %>% 
  step_mutate (weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate (weather = factor (weather, levels = 1:3, labels = c("Sunny", "Mist", "Rain"))) %>% 
  step_mutate (season = as.factor(season)) %>% 
  step_mutate (workingday = factor(workingday, levels = 0:1, labels = c("No", "Yes"))) %>% 
  step_mutate (holiday = factor(holiday, levels = 0:1, labels = c("No", "Yes"))) %>% 
  step_mutate (newtemp = ((temp + atemp)/2)) %>% 
  step_time  (datetime, features = "hour") %>% 
  step_rm (datetime) %>% 
  step_interact(terms = ~ workingday:datetime_hour) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

folds <- vfold_cv(trainData, v = 5, repeats=1)

## Create a control grid
untunedModel <- control_stack_grid() #If tuning over a grid
tunedModel <- control_stack_resamples() #If not tuning a model

preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

## Set Workflow
preg_wf <- workflow() %>%
add_recipe(my_recipe2) %>%
add_model(preg_model)

## Grid of values to tune over
preg_tuning_grid <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 5)
preg_models <- preg_wf %>%
tune_grid(resamples=folds,
          grid=preg_tuning_grid,
          metrics=metric_set(rmse, mae, rsq),
          control = untunedModel)
#####Random Forest
for_mod <- rand_forest(mtry = tune(),
                       min_n=tune(),
                       trees= 500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")

forest_wf <- workflow() %>%
  add_recipe(my_recipe2) %>%
  add_model(for_mod)

forestgrid <- grid_regular(mtry(range = c(1,10)),
                           min_n(),
                           levels = 5)

ranfor_model <- forest_wf %>%
  tune_grid(resamples=folds,
            grid=forestgrid,
            metrics=metric_set(rmse, mae, rsq),
            control = untunedModel)
###lin reg
lin_reg <-
linear_reg() %>%
set_engine("lm")
lin_reg_wf <-
workflow() %>%
add_model(lin_reg) %>%
add_recipe(my_recipe2)
lin_reg_model <-
fit_resamples(
              lin_reg_wf,
              resamples = folds,
              metrics = metric_set(rmse, mae, rsq),
              control = tunedModel)


my_stack <- stacks() %>%
add_candidates(ranfor_model) %>% 
add_candidates(lin_reg_model) %>%
add_candidates(preg_models) 


## Fit the stacked model
stack_mod <- my_stack %>%
blend_predictions() %>% # LASSO penalized regression meta-learner
  fit_members() ## Fit the members to the dataset


## Use the stacked data to get a prediction
stack_mod %>% predict(new_data=data_test)

stackpred <- predict(stack_mod, new_data= data_test)
stackpred <- exp(stackpred)

kaggle_submission <- stackpred %>%
  bind_cols(., data_test) %>% # Bind the predictions to the original test data
  select(datetime, .pred) %>% 
  rename(count = .pred) %>% 
  mutate(count = pmax(0, count)) %>% 
  mutate(datetime = as.character(format(datetime))) 

vroom_write(x = kaggle_submission, file = "./stack.csv", delim = ",")



##################Choose your own model BART
data_train <- vroom("Stat348/BikeShare/train.csv")
data_test <- vroom("Stat348/BikeShare/test.csv")

trainData <- data_train %>% 
  select(-casual, -registered) %>% 
  mutate( count = log(count))


my_recipe2 <- recipe(count~., data=trainData) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_date(datetime, features="year") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())



bart_mod <- parsnip::bart(
  mode = "regression",
  engine = "dbarts",
  trees = 50,
  prior_terminal_node_coef = .9,
  prior_terminal_node_expo = NULL,
  prior_outcome_range = NULL
)

bart_wf <- workflow() %>%
  add_recipe(my_recipe2) %>%
  add_model(bart_mod) %>% 
  fit(data=trainData)
bart_preds <- predict(bart_wf, new_data=data_test)
bart_preds <- exp(bart_preds)

#Kaggle_submission
kaggle_submission <- bart_preds %>%
  bind_cols(., data_test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle
## Write out the file
vroom_write(x=kaggle_submission, file="./bart_preds4.csv", delim=",")

