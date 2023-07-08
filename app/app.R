# load packages
library(shiny)
library(tidyverse)
library(tidymodels)
library(here)
library(bslib)

# load functions for getting anime details and mean scores
source("scrape.R")
source("score.R")

# load data
mean_scores_genres <- read_csv(here("data/mean_scores_genres.csv"))
mean_scores_themes <- read_csv(here("data/mean_scores_themes.csv"))
mean_scores_studios <- read_csv(here("data/mean_scores_studios.csv"))
mean_scores_demographics <- read_csv(here("data/mean_scores_demographics.csv"))
mean_scores_producers <- read_csv(here("data/mean_scores_producers.csv"))

# load models
load(here("models/lm_fit.rda"))
load(here("models/knn_fit.rda"))
load(here("models/boost_fit.rda"))
load(here("models/rf_fit.rda"))

# define user interface
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  
  # title
  titlePanel(div(span("MyAnimeList Score Predictor"),
                 style = {"padding-top: 15px;"})),
  
  sidebarLayout(
    # inputs on the left
    sidebarPanel(
      tags$style(".well {background-color: #f0d0ee;}"),
      
      textInput("url",
                "MyAnimeList URL",
                "https://myanimelist.net/anime/52034/Oshi_no_Ko"),
      
      fluidRow(
        column(6,
               textInput("title", 
                         "Title of Anime", 
                         "[Oshi No Ko]")),
        column(6,
               selectInput("type", 
                           "Type", 
                           choices = levels(factor(anime_clean$type)), 
                           selected = "tv"))
      ),
      
      fluidRow(
        column(6,
               selectInput("source", 
                           "Source", 
                           choices = levels(factor(anime_clean$source)), 
                           selected = "manga")),
        column(6,
               selectInput("rating", 
                           "Rating", 
                           choices = levels(factor(anime_clean$rating)),
                           selected = "pg_13"))
      ),

      fluidRow(
        column(6,
               numericInput("members", 
                            "Number of Members", 
                            min = 0, 
                            step = 0.5, 
                            value = 570781)),
        column(6,
               numericInput("favorites", 
                            "Number of Favorites", 
                            min = 0, 
                            step = 0.5, 
                            value = 21874))
      ),

      fluidRow(
        column(6,
               selectizeInput("genres", 
                              "Genres", 
                              choices = levels(factor(mean_scores_genres$cleaned_category)), 
                              multiple = TRUE, 
                              selected = c("Drama", "Supernatural"))),
        column(6,
               selectizeInput("themes", 
                              "Themes", 
                              choices = levels(factor(mean_scores_themes$cleaned_category)), 
                              multiple = TRUE, 
                              selected = c("Reincarnation", "Showbiz")))
      ),
  
      fluidRow(
        column(6,
               selectizeInput("studios", 
                              "Studios", 
                              choices = levels(factor(mean_scores_studios$cleaned_category)), 
                              multiple = TRUE, 
                              selected = c("Doga Kobo"))),
        column(6,
               selectizeInput("demographic", 
                              "Demographic", 
                              choices = levels(factor(mean_scores_demographics$cleaned_category)), 
                              multiple = TRUE, 
                              selected = c("Seinen")))
      ),
      
      selectizeInput("producers", 
                     "Producers", 
                     choices = NULL, 
                     multiple = TRUE),
      
      # inputs take up half of the page
      width = 6
    ),
    
    # outputs on the right
    mainPanel(
      layout_column_wrap(
        # each output on its own row
        width = 1,
        
        # scores
        uiOutput("actual_score"),
        uiOutput("lm_score"),
        uiOutput("knn_score"),
        uiOutput("boost_score"),
        uiOutput("rf_score")
      ),
      
      # outputs take up half of the page
      width = 6
    )
  )
)

# define backend
server <- function(input, output, session) {
  # get inputted URL
  mal_url <- reactive(input$url)
  
  # retrieve anime details from URL
  details <- reactive(get_anime_details(mal_url()))
  
  # detect when the URL changes
  observeEvent(details(), {
    # update inputs using the details
    updateTextInput(session,
                    "title",
                    value = details()$title)
    
    updateTextInput(session,
                    "type",
                    value = details()$type)
    
    updateTextInput(session,
                    "source",
                    value = details()$source)
    
    updateNumericInput(session,
                       "members",
                       value = details()$members)
    
    updateNumericInput(session,
                       "favorites",
                       value = details()$favorites)
    
    updateTextInput(session,
                    "rating", 
                    value = details()$rating)
    
    updateSelectizeInput(session,
                         "genres",
                         selected = details()$genres)
    
    updateSelectizeInput(session,
                         "themes",
                         selected = details()$themes)
    
    updateSelectizeInput(session,
                         "studios",
                         selected = details()$studios)
    
    updateSelectizeInput(session,
                         "demographic",
                         selected = details()$demographic)
    
    updateSelectizeInput(session,
                         "producers",
                         selected = details()$producers)
  })
  
  # this is in the server side since there are 1000+ producers
  updateSelectizeInput(session, 
                       "producers",
                       choices = levels(factor(mean_scores_producers$cleaned_category)), 
                       server = TRUE,
                       selected = c("Shueisha", "CyberAgent", "Kadokawa"))
  
  # turn inputs into reactive variables so that the outputs are updated when the inputs are changed
  title <- reactive(input$title)
  type <- reactive(input$type)
  anime_source <- reactive(input$source)
  members <- reactive(input$members)
  favorites <- reactive(input$favorites)
  rating <- reactive(input$rating)
  genres_score <- reactive(category_mean_score(input$genres, mean_scores_genres))
  themes_score <- reactive(category_mean_score(input$themes, mean_scores_themes))
  studios_score <- reactive(category_mean_score(input$studios, mean_scores_studios))
  demographics_score <- reactive(category_mean_score(input$demographic, mean_scores_demographics))
  producers_score <- reactive(category_mean_score(input$producers, mean_scores_producers))
  
  # actual score
  output$actual_score <- renderUI({
    value_box(
      title = "Actual score",
      value = details()$score,
      showcase = icon("play", "fa-2x"),
      theme_color = "info"
    ) 
  })
  
  # linear regression prediction
  output$lm_score <- renderUI({
    value_box(
      title = "Linear regression",
      value = predict(
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
        ))[[1, 1]] %>% round(2),
      showcase = icon("seedling", "fa-2x"),
      theme_color = "success"
    ) 
  })
  
  # KNN prediction
  output$knn_score <- renderUI({
    value_box(
      title = "K-nearest neighbors",
      value = predict(
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
        ))[[1, 1]] %>% round(2),
      showcase = icon("house", "fa-2x"),
      theme_color = "info"
    ) 
  })
  
  # boosted trees prediction
  output$boost_score <- renderUI({
    value_box(
      title = "Boosted trees",
      value = predict(
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
        ))[[1, 1]] %>% round(2),
      showcase = icon("rocket", "fa-2x"),
      theme_color = "success"
    ) 
  })
  
  # random forest prediction
  output$rf_score <- renderUI({
    value_box(
      title = "Random forest",
      value = predict(
        rf_final_fit,
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
        ))[[1, 1]] %>% round(2),
      showcase = icon("tree", "fa-2x"),
      theme_color = "info"
    ) 
  })
}

# run app
shinyApp(ui, server)
