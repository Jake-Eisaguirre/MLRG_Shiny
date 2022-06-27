if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

# librarian downloads, if not already downloaded, and reads in needed packages

librarian::shelf(shiny, tidyverse, here, janitor, sf)


#Data read in

# VES data
ves_data <- read_csv(here("data", "VES.csv")) %>% 
  clean_names()

ves <- ves_data %>% 
  dplyr::select(c(id_1, utme, utmn, wilderness, visit_date, visit_status, duration, species, visual_life_stage, 
                  visual_animal_state, count)) %>% 
  mutate(visit_date = year(visit_date)) %>% 
  rename(id = id_1,
         date = visit_date)

# bd_load data
bd_data <- read_csv(here("data", "bd_load.csv")) %>% 
  clean_names()

bd <- bd_data %>% 
  dplyr::select(id_1, utme_11, utmn_12, wilderness, visit_date, visit_status, duration, species, bd_load) %>% 
  mutate(visit_date = year(visit_date)) %>% 
  rename(id = id_1,
         utme = utme_11,
         utmn = utmn_12,
         date = visit_date)

