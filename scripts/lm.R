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

lm_model <- linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

lm_workflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(anime_recipe)

lm_fit <- lm_workflow %>%
  fit(anime_train)

augment(lm_fit, new_data = anime_test) %>% 
  mutate(avg_error = .pred - score) %>% 
  ggplot(aes(x = avg_error)) +
  geom_histogram()

augment(lm_fit, new_data = anime_test) %>% 
  ggplot(aes(x = .pred, y = score)) +
  geom_point(alpha = 0.4) +
  geom_abline(lty = 2, color = "red")

augment(lm_fit, new_data = anime_test) %>%
  rmse(truth = score, estimate = .pred)

predict(lm_fit, new_data = anime_test) %>% 
  pull(.pred) %>% 
  summary()

save(lm_fit, file = "models/lm_fit.rda")