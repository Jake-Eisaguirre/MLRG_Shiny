if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

# librarian downloads, if not already downloaded, and reads in needed packages

librarian::shelf(shiny, tidyverse, here, janitor, sf, lubridate, leaflet, shinyWidgets, leafem, bslib, thematic)


#Data read in
data <- read_csv(here("data", "shiny_data.csv")) %>% 
  clean_names()


# themeing
theme <- bs_theme(
  bg = "#b2d1b9", # green background
  fg = "#2d302e", # cream text foreground
  primary = "#0417c2", # hyperlinks
  secondary = "#506e57", # messages
  success = "#506e57", # operation succeeded
  info = "#506e57", # light blue from bar
  warning = "#C3512C",# yellow
  danger = "#FACE00", # orange red
  base_font = font_google("Open Sans"), 
  heading_font = font_google("Source Sans Pro"))