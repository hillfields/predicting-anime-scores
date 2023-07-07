library(shiny)
library(here)
library(tidyverse)
library(tidymodels)
library(ggiraph)

# load cleaned data
anime_clean <- read_csv(here("data/anime_clean.csv"))

# partition training and testing sets
set.seed(1)
anime_split <- initial_split(anime_clean, strata = "score")
# anime_train <- training(anime_split)
anime_test <- testing(anime_split) %>%
  mutate(title = gsub("'", "&#39;", title))

# load models
load(here("models/lm_fit.rda"))
load(here("models/knn_fit.rda"))
load(here("models/boost_fit.rda"))
load(here("models/rf_fit.rda"))

ui <- fluidPage(
  selectizeInput("type",
                 "Type", 
                 choices = levels(factor(anime_clean$type)), 
                 selected = levels(factor(anime_clean$type)),
                 multiple = TRUE),
  
  girafeOutput("boost_plot")
)

server <- function(input, output, session) {
  input_type <- reactive(input$type)
  
  anime_selected <- reactive({
    anime_test %>%
      filter(type %in% input_type())
  })
  
  output$boost_plot <- renderGirafe({
    boost_plot <- augment(boost_final_fit, new_data = anime_selected()) %>% 
      ggplot(aes(x = .pred, 
                 y = score,
                 color = type,
                 tooltip = title,
                 data_id = type)) +
      geom_point_interactive(alpha = 0.3,
                             hover_nearest = TRUE) +
      coord_cartesian(xlim = c(0, 10),
                      ylim = c(0, 10)) +
      labs(title = "Boosted trees: actual vs. predicted scores",
           x = "Predicted score",
           y = "Actual score") +
      theme_minimal()
    
    int_lm_plot <- girafe(ggobj = boost_plot,
                          options = list(opts_sizing(width = .7),
                                         opts_selection(type = "multiple"),
                                         opts_zoom(min = 1, max = 7)))
  })
}

shinyApp(ui, server)
