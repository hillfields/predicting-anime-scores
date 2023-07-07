# Predicting Anime Scores on MyAnimeList

This interactive Shiny app allows a user to enter a URL for an anime page on [MyAnimeList](https://myanimelist.net/) and get back its predicted score using several regression models: linear regression, k-nearest neighbors, boosted trees, and random forest.

![](img/app.png)

## Results / visualization

## Model performance (test RMSE)

Anime model

- Linear regression: 0.6060566
- K-nearest neighbors: 0.5809974
- Boosted trees: 0.4378353
- Random forest: 0.4488642

## Skills

- Preprocessing data with the tidyverse
- Visualizing data with ggplot2 (static plots) and ggiraph (interactive plots)
- Building machine learning models with tidymodels
- Scraping MAL sites with rvest
- Creating an interactive app with RShiny

## Challenges

- Mapping categories to scores
- Refactor a lot of code (made functions)
- Creating the app (design, server function)
