# if (!require(librarian)){
#   install.packages("librarian")
#   library(librarian)
# }
#  
# # librarian downloads, if not already downloaded, and reads in needed packages
# 
# librarian::shelf(shiny, tidyverse, here, shinyWidgets, leafem, bslib, thematic, shinymanager, leaflet, ggrepel, sf, stringr,fontawesome,shinycssloaders, shinydashboardPlus, lubridate, scales, cachem, htmltools, rmapshaper, DT)



library(shiny)
library(tidyverse)
library(here)
library(shinyWidgets)
library(leafem)
library(bslib)
library(thematic)
library(shinymanager)
library(leaflet)
library(ggrepel)
library(sf)
library(stringr)
library(fontawesome)
library(shinycssloaders)
library(shinydashboardPlus)
library(lubridate)
library(scales)
library(cachem)
library(htmltools)
library(rmapshaper)
library(DT)
library(terra)
library(raster)
library(shinyalert)
library(googledrive)
library(shinylogs)
library(ggh4x)


shinyOptions(cache = cachem::cache_disk("./app_cache"))
#shinyOptions(cache = cachem::cache_mem(max_size = 1000e6))

#Bd data read in
bd_data <- read_csv(here("MLRG_Shinyapp/data/bd_data.csv"))

#read in wilderness shape files
shape <- read_sf(here("MLRG_Shinyapp/data/wilderness_shapes/wilderness.shp")) 

#read in water types
water <- read_csv(here("MLRG_Shinyapp/data/water_type.csv")) %>% 
  rename(id = lake_id)

#ves data read in
ves_data <- read_csv(here("MLRG_Shinyapp/data/ves_data.csv")) %>% 
  left_join(bd_data) %>% 
  left_join(water)

# read in bd_plot data with month_year
bd_plot <- read_csv(here("MLRG_Shinyapp/data/bd_plot.csv"))

# read in ves_plot data with year_month
ves_plot <- read_csv(here("MLRG_Shinyapp/data/ves_plot.csv"))

# all visit data for map
all_visits <- read_csv(here( "MLRG_Shinyapp/data/all_visits.csv")) %>% 
  left_join(water, by = c("site_id" = "id")) %>%
  anti_join(ves_data, by = c("site_id" = "id"))


######### Database Connection and Data Download ############## 


# Site table
site <- read_csv(here("MLRG_Shinyapp/data/site.csv"))

# visit table
visit <- read_csv(here("MLRG_Shinyapp/data/visit.csv"))

# survey table
survey <- read_csv(here("MLRG_Shinyapp/data/survey.csv"))

# capture table
capture <- read_csv(here("MLRG_Shinyapp/data/capture.csv"))

# full capture data set
full_capture <- read_csv(here("MLRG_Shinyapp/data/full_capture.csv"))

# bd load table
bd_load_table <- read_csv(here("MLRG_Shinyapp/data/bd_load_table.csv"))

# VES table
ves_table <- read_csv(here("MLRG_Shinyapp/data/ves_table.csv"))

# full Ves data
full_ves <- read_csv(here("MLRG_Shinyapp/data/full_ves.csv"))

# relocate table
relocate_table <- read_csv(here("MLRG_Shinyapp/data/relocate_table.csv"), col_types = cols(collect_siteid = "c"))

# frog relocate table
relocate_frog_table <- read_csv(here("MLRG_Shinyapp/data/relocate_frog_table.csv"))

# full CMR table
full_cmr <- read_csv(here("MLRG_Shinyapp/data/full_cmr.csv"), col_types = cols(collect_siteid = "c"))


inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions
function logout() {
window.close();  //close the window
}
function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

