library(tidyverse)
library(tidymodels)
library(patchwork)

load("data/anime_data.RData")
tidymodels_prefer()

set.seed(1)
anime_split <- initial_split(anime_data, strata = "score")
anime_train <- training(anime_split)
anime_test <- testing(anime_split)

anime_recipe <- recipe(score ~ ., data = anime_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_scale(all_numeric_predictors())

set.seed(1)
anime_folds <- vfold_cv(anime_train, v = 5)

boost_model <- boost_tree(trees = 1000,
                          min_n = tune(),
                          mtry = tune(),
                          learn_rate = tune(),
                          tree_depth = tune(),
                          sample_size = tune(),
                          loss_reduction = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

boost_workflow <- workflow() %>%
  add_model(boost_model) %>%
  add_recipe(anime_recipe)

# cover hyperparameter space as much as possible
boost_grid <- grid_latin_hypercube(min_n(),
                                   finalize(mtry(), anime_train),
                                   learn_rate(),
                                   tree_depth(),
                                   sample_size = sample_prop(),
                                   loss_reduction(),
                                   size = 50)

tune_res <- tune_grid(boost_workflow,
                      resamples = anime_folds,
                      grid = boost_grid)

autoplot(tune_res, metric = "rmse")

best_params <- select_best(tune_res, "rmse")

final_boost <- finalize_workflow(boost_workflow, best_params)

final_boost_fit <- final_boost %>% 
  fit(anime_train)

augment(final_boost_fit, new_data = anime_test) %>% 
  mutate(avg_error = .pred - score) %>% 
  ggplot(aes(x = avg_error)) +
  geom_histogram()

augment(final_boost_fit, new_data = anime_test) %>% 
  ggplot(aes(x = .pred, y = score)) +
  geom_point(alpha = 0.4) +
  geom_abline(lty = 2, color = "red")

augment(final_boost_fit, new_data = anime_test) %>%
  rmse(truth = score, estimate = .pred)

# not predicting above a certain point
predict(final_boost_fit, new_data = anime_test) %>% 
  pull(.pred) %>% 
  summary()

save(final_boost_fit, file = "models/boost_fit.rda")
