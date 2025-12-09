library(jsonlite)
library(tidymodels)
library(tidyverse)
library(tidytext)
library(textrecipes)
trainSet <- read_file("C:/Users/sophi/OneDrive/Documents/Stat_348/Cooking/WhatsCooking/train.json") %>%
fromJSON()
testSet <- read_file("C:/Users/sophi/OneDrive/Documents/Stat_348/Cooking/WhatsCooking/test.json") %>%
fromJSON()

# file.info("C:/Users/sophi/OneDrive/Documents/Stat_348/Cooking/WhatsCooking")$isdir
# list.files("C:/Users/sophi/OneDrive/Documents/Stat_348/Cooking/WhatsCooking")

# dim(trainSet)
# names(trainSet)
# class(trainSet$ingredients)
# trainSet$ingredients[[1]]

# trainSet %>%
# unnest(ingredients) %>%
#   count(ingredients)

my_recipe <- recipe(cuisine ~ ., data = trainSet) %>%
  step_mutate(ingredients = tokenlist(ingredients)) %>%
  step_tokenfilter(ingredients, max_tokens=3000) %>%
  step_tfidf(ingredients)
prepped_recipe <- prep(my_recipe)
baked <- bake(prepped_recipe, new_data=trainSet)
  

# my_model <- rand_forest(mtry=3,
#                         trees=100,
#                         min_n=5) %>%
#   set_engine("ranger") %>%
#   set_mode("classification")
# 
# my_wf <- workflow() %>%
#   add_recipe(my_recipe) %>%
#   add_model(my_model) %>%
#   fit(data = trainSet)
  
# svm_spec <- svm_linear(cost = 1) %>%
#   set_engine("LiblineaR") %>%
#   set_mode("classification")
# 
# svm_wf <- workflow() %>%
#   add_recipe(my_recipe) %>%
#   add_model(svm_spec) %>%
#   fit(data = trainSet)

model <- multinom_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet") %>%
  set_mode("classification")


my_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(model)

folds <- vfold_cv(trainSet, v = 5)

grid <- grid_regular(penalty(range = c(-4, 1)),levels = 20)

tuned <- tune_grid(my_wf,
  resamples = folds,
  grid = grid,
  metrics = metric_set(accuracy))

bestTune <- select_best(tuned, "accuracy")


preds <- predict(bestTune, new_data = testSet)

# preds <- predict(my_wf, new_data = testSet)


kaggle_submission <- preds %>%
  bind_cols(testSet %>% select(id)) %>%
  select(id, .pred_class) %>%
  rename(cuisine = .pred_class)

vroom_write(x=kaggle_submission, file="./cuisinepreds.csv", delim=",")


  