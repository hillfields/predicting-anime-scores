library(tidyverse)
library(tidymodels)
library(patchwork)

load("data/anime_data.RData")
tidymodels_prefer()

set.seed(1)
anime_split <- initial_split(anime_data, strata = "score")
anime_train <- training(anime_split)
anime_test <- testing(anime_split)

set.seed(1)
anime_folds <- vfold_cv(anime_train, v = 5)

# specify recipe
pca_recipe <- recipe(score ~ ., data = anime_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), threshold = tune())

# create workflow object
pca_workflow <- workflow() %>%
  add_recipe(pca_recipe) %>%
  add_model(lm_model)

# threshold values to try
threshold_grid <- grid_regular(threshold(), levels = 10)

# fit model for each threshold
tune_res <- tune_grid(object = pca_workflow,
                      resamples = anime_folds,
                      grid = threshold_grid)

# plot metrics
autoplot(tune_res)

# select the best model based on the given metric
best_threshold <- select_best(tune_res, metric = "rmse")

# refit using the whole training data set
pca_final <- finalize_workflow(pca_workflow, best_threshold)
pca_final_fit <- fit(pca_final, data = anime_train)

augment(pca_final_fit, new_data = anime_test) %>% 
  mutate(avg_error = .pred - score) %>% 
  ggplot(aes(x = avg_error)) +
  geom_histogram()

augment(pca_final_fit, new_data = anime_test) %>% 
  ggplot(aes(x = .pred, y = score)) +
  geom_point(alpha = 0.4) +
  geom_abline(lty = 2, color = "red")

augment(pca_final_fit, new_data = anime_test) %>%
  rmse(truth = score, estimate = .pred)

save(pca_final_fit, file = "models/pca_lm_fit.rda")