# load packages
library(shiny)
library(tidyverse)
library(tidymodels)

# load data
anime_clean <- read_csv("data/anime_clean.csv")
mean_scores_genres <- read_csv("data/mean_scores_genres.csv")
mean_scores_themes <- read_csv("data/mean_scores_themes.csv")
mean_scores_studios <- read_csv("data/mean_scores_studios.csv")
mean_scores_demographics <- read_csv("data/mean_scores_demographics.csv")
mean_scores_producers <- read_csv("data/mean_scores_producers.csv")

# load models
load("models/lm_fit.rda")
load("models/knn_fit.rda")
load("models/boost_fit.rda")
load("models/rf_fit.rda")

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

# define user interface
ui <- fluidPage(
  # title
  titlePanel("MyAnimeList Score Predictor"),
  
  sidebarLayout(
    # inputs on the left
    sidebarPanel(
      textInput("name", 
                "Name of Anime", 
                "Spy x Family"),
      
      selectInput("type", 
                  "Type", 
                  choices = levels(factor(anime_clean$type)), 
                  selected = "tv"),
      
      selectInput("source", 
                  "Source", 
                  choices = levels(factor(anime_clean$source)), 
                  selected = "manga"),
      
      numericInput("members", 
                   "Number of Members", 
                   min = 0, 
                   step = 0.5, 
                   value = 1361732),
      
      numericInput("favorites", 
                   "Number of Favorites", 
                   min = 0, 
                   step = 0.5, 
                   value = 37682),
      
      selectInput("rating", 
                  "Rating", 
                  choices = levels(factor(anime_clean$rating)),
                  selected = "pg_13"),
      
      selectizeInput("genres", 
                     "Genres", 
                     choices = levels(factor(mean_scores_genres$cleaned_category)), 
                     multiple = TRUE, 
                     selected = c("Action", "Comedy")),
      
      selectizeInput("themes", 
                     "Themes", 
                     choices = levels(factor(mean_scores_themes$cleaned_category)), 
                     multiple = TRUE, 
                     selected = c("Childcare")),
      
      selectizeInput("studios", 
                     "Studios", 
                     choices = levels(factor(mean_scores_studios$cleaned_category)), 
                     multiple = TRUE, 
                     selected = c("Wit Studio", "CloverWorks")),
      
      selectizeInput("demographics", 
                     "Demographics", 
                     choices = levels(factor(mean_scores_demographics$cleaned_category)), 
                     multiple = TRUE, 
                     selected = c("Shounen")),
      
      selectizeInput("producers", 
                     "Producers", 
                     choices = NULL, 
                     multiple = TRUE)
    ),
    
    # outputs on the right
    mainPanel(
      textOutput("message"),
      textOutput("lm_pred"),
      textOutput("knn_pred"),
      textOutput("boost_pred"),
      textOutput("rf_pred")
    )
  )
)

# define backend
server <- function(input, output, session) {
  # print message
  output$message <- renderText(paste0(input$name, " has the following predicted scores:"))
  
  # this is in the server side since there are 1000+ producers
  updateSelectizeInput(session, 
                       "producers",
                       choices = levels(factor(mean_scores_producers$cleaned_category)), 
                       server = TRUE,
                       selected = c("TV Tokyo", "Shogakukan-Shueisha Productions", "TOHO animation", "Shueisha"))
  
  # turn inputs into reactive variables so that the outputs are updated when the inputs are changed
  title <- reactive(input$name)
  type <- reactive(input$type)
  anime_source <- reactive(input$source)
  members <- reactive(input$members)
  favorites <- reactive(input$favorites)
  rating <- reactive(input$rating)
  genres_score <- reactive(category_mean_score(input$genres, mean_scores_genres))
  themes_score <- reactive(category_mean_score(input$themes, mean_scores_themes))
  studios_score <- reactive(category_mean_score(input$studios, mean_scores_studios))
  demographics_score <- reactive(category_mean_score(input$demographics, mean_scores_demographics))
  producers_score <- reactive(category_mean_score(input$producers, mean_scores_producers))
  
  # make predictions
  output$lm_pred <- renderText({
    paste0("> Linear Model: ",
           predict(
             lm_final_fit,
             new_data = tibble(
               title = title(),
               type = type(),
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
    paste0("> KNN Model: ",
           predict(
             knn_final_fit,
             new_data = tibble(
               title = title(),
               type = type(),
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
    paste0("> Boosted Trees Model: ",
           predict(
             boost_final_fit,
             new_data = tibble(
               title = title(),
               type = type(),
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

  output$rf_pred <- renderText({
    paste0("> Random Forest Model: ",
           predict(
             boost_final_fit,
             new_data = tibble(
               title = title(),
               type = type(),
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
