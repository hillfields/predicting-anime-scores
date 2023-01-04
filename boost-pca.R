library(tidyverse)
library(tidymodels)
library(patchwork)

load("data/anime_data.RData")
tidymodels_prefer()

set.seed(1)
anime_split <- initial_split(anime_data, strata = "score")
anime_train <- training(anime_split)
anime_test <- testing(anime_split)

pca_recipe <- recipe(score ~ ., data = anime_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), threshold = 0.8)

set.seed(1)
anime_folds <- vfold_cv(anime_train, v = 5)

boost_pca_model <- boost_tree(trees = 1000,
                          min_n = tune(),
                          mtry = tune(),
                          learn_rate = tune(),
                          tree_depth = tune(),
                          sample_size = tune(),
                          loss_reduction = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

pca_workflow <- workflow() %>%
  add_recipe(pca_recipe) %>%
  add_model(boost_pca_model)

# cover hyperparameter space as much as possible
boost_grid <- grid_latin_hypercube(min_n(),
                                   finalize(mtry(), anime_train),
                                   learn_rate(),
                                   tree_depth(),
                                   sample_size = sample_prop(),
                                   loss_reduction(),
                                   size = 50)

tune_res <- tune_grid(pca_workflow,
                      resamples = anime_folds,
                      grid = boost_grid)

best_params <- select_best(tune_res, "rmse")

final_pca_boost <- finalize_workflow(pca_workflow, best_params)

final_pca_boost_fit <- final_pca_boost %>% 
  fit(anime_train)

augment(final_pca_boost_fit, new_data = anime_test) %>% 
  mutate(avg_error = .pred - score) %>% 
  ggplot(aes(x = avg_error)) +
  geom_histogram()

augment(final_pca_boost_fit, new_data = anime_test) %>% 
  ggplot(aes(x = .pred, y = score)) +
  geom_point(alpha = 0.4) +
  geom_abline(lty = 2, color = "red")

augment(final_pca_boost_fit, new_data = anime_test) %>%
  rmse(truth = score, estimate = .pred)

predict(final_pca_boost_fit, new_data = anime_test) %>% 
  pull(.pred) %>% 
  summary()

save(final_pca_boost_fit, file = "models/pca_boost_fit.rda")
