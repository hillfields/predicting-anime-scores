# load packages
library(shiny)
library(tidyverse)
library(tidymodels)

# load data
load("data/anime_data.RData")
load("data/genres_mean_scores.RData")
load("data/themes_mean_scores.Rdata")
load("data/studios_mean_scores.Rdata")
load("data/demographics_mean_scores.Rdata")
load("data/producers_mean_scores.Rdata")

# load models
load("models/lm_fit.rda")
load("models/boost_fit.rda")
load("models/knn_fit.rda")
load("models/pca_boost_fit.rda")
load("models/pca_lm_fit.rda")

# calculate mean score over all anime in the data
# used in the category_mean_score() function below
total_mean_score <- anime_data %>%
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

# define user interface
ui <- fluidPage(
  # title
  titlePanel("MyAnimeList Score Predictor"),
  
  sidebarLayout(
    # inputs on the left
    sidebarPanel(
      textInput("name", "Name of Anime", "Spy x Family"),
      selectInput("type", "Type", choices = levels(anime_data$type), selected = "tv"),
      numericInput("episodes", "Number of Episodes", min = 1, step = 0.5, value = 12),
      selectInput("source", "Source", choices = levels(anime_data$source), selected = "manga"),
      numericInput("members", "Number of Members", min = 0, step = 0.5, value = 1202853),
      numericInput("favorites", "Number of Favorites", min = 0, step = 0.5, value = 36217),
      selectInput("rating", "Rating", choices = levels(anime_data$rating), selected = "pg_13"),
      selectizeInput("genres", "Genres", choices = levels(factor(genres_mean_scores$genres)), 
                     multiple = TRUE, selected = c("Action", "Comedy")),
      selectizeInput("themes", "Themes", choices = levels(factor(themes_mean_scores$themes)), 
                     multiple = TRUE, selected = c("Childcare")),
      selectizeInput("studios", "Studios", choices = levels(factor(studios_mean_scores$studios)), 
                     multiple = TRUE, selected = c("Wit Studio", "CloverWorks")),
      selectizeInput("demographics", "Demographics", choices = levels(factor(demographics_mean_scores$demographics)), 
                     multiple = TRUE, selected = c("Shounen")),
      selectizeInput("producers", "Producers", choices = NULL, multiple = TRUE)
    ),
    
    # outputs on the right
    mainPanel(
      textOutput("message"),
      textOutput("lm_pred"),
      textOutput("boost_pred"),
      textOutput("knn_pred"),
      textOutput("pca_lm_pred"),
      textOutput("pca_boost_pred")
    )
  )
)

# define backend
server <- function(input, output, session) {
  # print message
  output$message <- renderText(paste0(input$name, " has the following predicted scores:"))
  
  # this is in the server side since there are 1000+ producers
  updateSelectizeInput(session, "producers", choices = levels(factor(producers_mean_scores$producers)), server = TRUE,
                       selected = c("TV Tokyo", "Shogakukan-Shueisha Productions", "TOHO animation", "Shueisha"))
  
  # turn inputs into reactive variables so that the outputs are updated when the inputs are changed
  type <- reactive(input$type)
  episodes <- reactive(input$episodes)
  anime_source <- reactive(input$source)
  members <- reactive(input$members)
  favorites <- reactive(input$favorites)
  rating <- reactive(input$rating)
  genres_score <- reactive(category_mean_score(input$genres, genres_mean_scores))
  themes_score <- reactive(category_mean_score(input$themes, themes_mean_scores))
  studios_score <- reactive(category_mean_score(input$studios, studios_mean_scores))
  demographics_score <- reactive(category_mean_score(input$demographics, demographics_mean_scores))
  producers_score <- reactive(category_mean_score(input$producers, producers_mean_scores))
  
  # make predictions
  output$lm_pred <- renderText({
    paste0("> Linear Model: ",
           predict(
             lm_fit,
             new_data = tibble(
               type = type(),
               episodes = episodes(),
               source = anime_source(),
               members = members(),
               favorites = favorites(),
               rating = rating(),
               genres_score = genres_score(),
               themes_score = themes_score(),
               studios_score = studios_score(),
               demographics_score = demographics_score(),
               producers_score = producers_score()
             ))[[1, 1]] %>% round(2))
  })
  
  output$boost_pred <- renderText({
    paste0("> Boosted Model: ",
           predict(
             final_boost_fit,
             new_data = tibble(
               type = type(),
               episodes = episodes(),
               source = anime_source(),
               members = members(),
               favorites = favorites(),
               rating = rating(),
               genres_score = genres_score(),
               themes_score = themes_score(),
               studios_score = studios_score(),
               demographics_score = demographics_score(),
               producers_score = producers_score()
             ))[[1, 1]] %>% round(2))
  })
  
  output$knn_pred <- renderText({
    paste0("> K-Nearest Neighbors Model: ",
           predict(
             knn_fit,
             new_data = tibble(
               type = type(),
               episodes = episodes(),
               source = anime_source(),
               members = members(),
               favorites = favorites(),
               rating = rating(),
               genres_score = genres_score(),
               themes_score = themes_score(),
               studios_score = studios_score(),
               demographics_score = demographics_score(),
               producers_score = producers_score()
             ))[[1, 1]] %>% round(2))
  })

  output$pca_lm_pred <- renderText({
    paste0("> PCA Linear Model: ",
           predict(
             pca_final_fit,
             new_data = tibble(
               type = type(),
               episodes = episodes(),
               source = anime_source(),
               members = members(),
               favorites = favorites(),
               rating = rating(),
               genres_score = genres_score(),
               themes_score = themes_score(),
               studios_score = studios_score(),
               demographics_score = demographics_score(),
               producers_score = producers_score()
             ))[[1, 1]] %>% round(2))
  })
  
  output$pca_boost_pred <- renderText({
    paste0("> PCA Boosted Model: ",
           predict(
             final_pca_boost_fit,
             new_data = tibble(
               type = type(),
               episodes = episodes(),
               source = anime_source(),
               members = members(),
               favorites = favorites(),
               rating = rating(),
               genres_score = genres_score(),
               themes_score = themes_score(),
               studios_score = studios_score(),
               demographics_score = demographics_score(),
               producers_score = producers_score()
             ))[[1, 1]] %>% round(2))
  })
}

# run app
shinyApp(ui, server)