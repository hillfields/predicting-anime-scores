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

knn_model <- nearest_neighbor(neighbors = tune()) %>%
  set_mode("regression") %>%
  set_engine("kknn")

knn_workflow <- workflow() %>%
  add_model(knn_model) %>%
  add_recipe(anime_recipe)

neighbors_grid <- grid_regular(neighbors(range = c(10, 100)),
                               levels = 10)

tune_res <- tune_grid(knn_workflow,
                      resamples = anime_folds,
                      grid = neighbors_grid)

autoplot(tune_res)

best_neighbors <- select_best(tune_res, metric = "rmse")

knn_final <- finalize_workflow(knn_workflow, best_neighbors)

knn_final_fit <- knn_final %>% 
  fit(anime_train)

augment(knn_final_fit, new_data = anime_test) %>% 
  mutate(avg_error = .pred - score) %>% 
  ggplot(aes(x = avg_error)) +
  geom_histogram()

augment(knn_final_fit, new_data = anime_test) %>% 
  ggplot(aes(x = .pred, y = score)) +
  geom_point(alpha = 0.4) +
  geom_abline(lty = 2, color = "red")

augment(knn_final_fit, new_data = anime_test) %>%
  rmse(truth = score, estimate = .pred)

save(knn_final_fit, file = "models/knn_fit.rda")