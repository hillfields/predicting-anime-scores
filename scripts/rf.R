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

rf_model <- rand_forest(mtry = tune(),
                        trees = 1000,
                        min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger")

rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(anime_recipe)

tune_res <- tune_grid(rf_workflow,
                      resamples = anime_folds,
                      grid = 20)

autoplot(tune_res)

best_params <- select_best(tune_res, "rmse")

rf_final <- finalize_workflow(rf_workflow, best_params)

rf_fit <- rf_final %>%
  fit(anime_train)

augment(rf_fit, new_data = anime_test) %>% 
  mutate(avg_error = .pred - score) %>% 
  ggplot(aes(x = avg_error)) +
  geom_histogram()

augment(rf_fit, new_data = anime_test) %>% 
  ggplot(aes(x = .pred, y = score)) +
  geom_point(alpha = 0.4) +
  geom_abline(lty = 2, color = "red")

augment(rf_fit, new_data = anime_test) %>%
  rmse(truth = score, estimate = .pred)

predict(rf_fit, new_data = anime_test) %>% 
  pull(.pred) %>% 
  summary()

save(rf_fit, file = "models/rf_fit.rda")