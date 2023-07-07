# load libraries
library(tidyverse)
library(rvest)
library(xml2)

# function to get anime details from MAL
get_anime_details <- function(url) {
  # read the HTML page of the URL for the other missing fields
  html <- read_html(url)
  
  ##### TITLE #####
  
  # get English title first
  title <- html %>%
    html_nodes(".title-english.title-inherit") %>%
    html_text2()
  
  # if missing, get Japanese title
  if (identical(title, character(0))) {
    title <- html %>%
      html_nodes(".title-name") %>%
      html_text2()
  }
  
  ##### TYPE #####
  
  type <- html %>%
    html_nodes(".information.type") %>%
    html_text2() %>%
    str_to_lower()
  
  ##### SOURCE #####
  
  source <- html %>%
    html_nodes(xpath = "//span[contains(text(), 'Source:')]") %>%
    html_nodes(xpath = "..") %>%
    html_text2() %>%
    str_split(": ") %>%
    pluck(1) %>%
    nth(2) %>%
    str_to_lower() %>%
    str_replace_all(" |-", "_")
  
  ##### MEMBERS #####
  
  members <- html %>%
    html_nodes(xpath = "//span[contains(text(), 'Members:')]") %>%
    html_nodes(xpath = "..") %>%
    html_text2() %>%
    str_split(": ") %>%
    pluck(1) %>%
    gsub(",", "", .) %>%
    nth(2) %>%
    as.numeric()
  
  ##### FAVORITES #####
  
  favorites <- html %>%
    html_nodes(xpath = "..//span[contains(text(), 'Favorites:')]") %>%
    html_nodes(xpath = "..") %>%
    html_text2() %>%
    str_split(": ") %>%
    pluck(1) %>%
    gsub(",", "", .) %>%
    nth(2) %>%
    as.numeric()
  
  ##### RATING #####
  
  rating <- html %>%
    html_nodes(xpath = "//span[contains(text(), 'Rating:')]") %>%
    html_nodes(xpath = "..") %>%
    html_text2() %>%
    str_split(": ") %>%
    pluck(1) %>%
    nth(2) %>%
    {case_when(. == "G - All Ages" ~ "g",
               . == "PG - Children" ~ "pg",
               . == "PG-13 - Teens 13 or older" ~ "pg_13",
               . == "R - 17+ (violence & profanity)" ~ "r",
               . == "R+ - Mild Nudity" ~ "r+",
               . == "Rx - Hentai" ~ "rx")}
  
  ##### GENRES #####
  
  genres <- html %>%
    html_nodes(xpath = "//span[contains(text(), 'Genre')]") %>%
    html_nodes(xpath = "..") %>%
    html_elements("a") %>%
    html_text2()
  
  if (identical(genres, character(0))) {
    genres <- "Other or NA"
  }
  
  ##### THEMES #####
  
  themes <- html %>%
    html_nodes(xpath = "//span[contains(text(), 'Theme')]") %>%
    html_nodes(xpath = "..") %>%
    html_elements("a") %>%
    html_text2()
  
  if (identical(themes, character(0))) {
    themes <- "Other or NA"
  }
  
  ##### STUDIOS #####
  
  studios <- html %>%
    html_nodes(xpath = "//span[contains(text(), 'Studios:')]") %>%
    html_nodes(xpath = "..") %>%
    html_elements("a") %>%
    html_text2()
  
  ##### DEMOGRAPHIC #####
  
  demographic <- html %>%
    html_nodes(xpath = "//span[contains(text(), 'Demographic:')]") %>%
    html_nodes(xpath = "..") %>%
    html_elements("a") %>%
    html_text2()
  
  if (identical(demographic, character(0))) {
    demographic <- "Other or NA"
  }
  
  ##### PRODUCERS #####
  
  producers <- html %>%
    html_nodes(xpath = "//span[contains(text(), 'Producers:')]") %>%
    html_nodes(xpath = "..") %>%
    html_elements("a") %>%
    html_text2()
  
  if (identical(producers, character(0))) {
    producers <- "Other or NA"
  }
  
  ##### SCORE #####
  
  score <- html %>%
    html_nodes(".score-label") %>%
    html_text2() %>%
    first()
  
  # convert to numeric if score is available
  if (score != "N/A") {
    score <- as.numeric(score)
  }
  
  ##### EVERYTHING #####
  
  list("title" = title,
       "type" = type,
       "source" = source,
       "members" = members,
       "favorites" = favorites,
       "rating" = rating,
       "genres" = genres,
       "themes" = themes,
       "studios" = studios,
       "demographic" = demographic,
       "producers" = producers,
       "score" = score) 
}

# test function
# url <- "https://myanimelist.net/anime/50265/Spy_x_Family"
# get_anime_details(url)
