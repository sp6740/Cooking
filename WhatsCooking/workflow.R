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

dim(trainSet)
names(trainSet)
class(trainSet$ingredients)
trainSet$ingredients[[1]]

# trainSet %>%
# unnest(ingredients) %>%
#   count(ingredients)

my_recipe <- recipe(cuisine ~ ., data = trainSet) %>%
  step_mutate(ingredients = tokenlist(ingredients)) %>%
  step_tokenfilter(ingredients, max_tokens=1000) %>%
  step_tfidf(ingredients)
prepped_recipe <- prep(my_recipe)
baked <- bake(prepped_recipe, new_data=trainSet)
  

my_model <- rand_forest(mtry=3,
                        trees=100,
                        min_n=5) %>%
  set_engine("ranger") %>%
  set_mode("classification")

my_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_model) %>%
  fit(data = trainSet)
  
preds <- predict(my_wf, new_data = testSet)

preds <- preds %>% 
  mutate(.pred = exp(.pred))

kaggle_submission <- preds %>%
  bind_cols(testSet %>% select(id)) %>%
  select(id, .pred_class) %>%
  rename(cuisine = .pred_class)

vroom_write(x=kaggle_submission, file="./cuisinepreds.csv", delim=",")


  