library(tidyverse)
library(here)

anime_clean <- read_csv(here("data/anime_clean.csv"))

# calculate mean score over all anime in the data
# used in the category_mean_score() function below
total_mean_score <- anime_clean %>%
  pull(score) %>% 
  mean()

# for categories with multiple possible inputs, convert to numeric variables
category_mean_score <- function(categories, df_map, replace_empty = total_mean_score) {
  # get mapped values
  category <- df_map %>% pull(1)
  score <- df_map %>% pull(2)
  
  # use mean score over all animes if input is empty
  if (is.null(categories)) {
    return (replace_empty)
  }
  
  # otherwise, calculate the mean score over those categories
  else {
    mean_score <- categories %>%
      plyr::mapvalues(from = category, 
                      to = score,
                      warn_missing = FALSE) %>%
      as.numeric() %>%
      mean()
    
    return(mean_score)
  }
}
