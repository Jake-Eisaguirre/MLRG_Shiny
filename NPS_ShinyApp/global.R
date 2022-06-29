if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

# librarian downloads, if not already downloaded, and reads in needed packages

librarian::shelf(shiny, tidyverse, here, janitor, sf, lubridate, leaflet, shinyWidgets, leafem)


#Data read in

# VES data
data <- read_csv(here("data", "shiny_data.csv")) %>% 
  clean_names()


#UI choices for VES
ui_date_ves <- unique(data$date)
ui_date_map <- unique(data$date)

#UI choices for BD
ui_date_bd <- unique(data$date)

#wilderness <- unique(ves$wilderness)
