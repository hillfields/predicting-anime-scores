# Predicting Anime Scores on MyAnimeList

This interactive Shiny app allows a user to enter a URL for an anime page on [MyAnimeList](https://myanimelist.net/) (MAL), an anime and manga database, and get back its predicted score using several regression models: linear regression, k-nearest neighbors, boosted trees, and random forest.

![](images/demo.gif)

## Skills

-   Preprocessing data with the **tidyverse**
-   Visualizing data with **ggplot2**
-   Building machine learning models with **tidymodels**
-   Scraping MAL sites with **rvest**
-   Creating an interactive app with **Shiny**

## Challenges

-   Dealing with categorical variables that could have multiple labels (mapped them to numeric scores and took the average)
-   Figuring out how to use Shiny (designing the UI, updating inputs based on the URL, organizing functions into separate scripts)

## Results

### Model performance (test RMSE)

-   **Linear regression:** 0.606
-   **K-nearest neighbors:** 0.581
-   **Boosted trees:** 0.438 (best)
-   **Random forest:** 0.449

### Visuals

#### Inference

![](images/lm_coefs.png)
![](images/boost_rf_vip.png)

#### Predictions

![](images/score_vs_numeric.png)
![](images/score_vs_source.png)
![](images/score_vs_rating.png)
![](images/score_vs_type.png)
