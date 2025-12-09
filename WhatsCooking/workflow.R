library(jsonlite)
library(tidymodels)
library(tidyverse)
library(tidytext)
library(textrecipes)
library(vroom)
library(dplyr)
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


trainSet <- trainSet %>%
  as_tibble() %>%
  mutate(text_ingredients = map_chr(ingredients, ~ paste(.x, collapse = " ")),
         num_ingredients = map_int(ingredients, length))

testSet <- testSet %>%
  as_tibble() %>%
  mutate(text_ingredients = map_chr(ingredients, ~ paste(.x, collapse = " ")),
         num_ingredients = map_int(ingredients, length))

my_recipe <- recipe(cuisine ~ text_ingredients + num_ingredients, data = trainSet) %>%
  step_tokenize(text_ingredients) %>%
  step_tokenfilter(text_ingredients, max_tokens=500) %>%
  step_tfidf(text_ingredients) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors())
prepped_recipe <- prep(my_recipe)
baked <- bake(prepped_recipe, new_data=trainSet)



my_model <- rand_forest(mtry=20,
                        trees=300,
                        min_n=5) %>%
  set_engine("ranger") %>%
  set_mode("classification")

my_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_model) %>%
  fit(data = trainSet)
  


preds <- predict(my_wf, new_data = testSet)

# preds <- predict(my_wf, new_data = testSet)

testSet <- testSet %>%
  rename(id = id)


kaggle_submission <- preds %>%
  bind_cols(testSet %>% dplyr::select(id)) %>%
  rename(cuisine = .pred_class)

vroom_write(x=kaggle_submission, file="./cuisinepreds.csv", delim=",")

names(testSet)





  