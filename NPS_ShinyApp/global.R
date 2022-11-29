if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

# librarian downloads, if not already downloaded, and reads in needed packages

librarian::shelf(shiny, tidyverse, here, shinyWidgets, leafem, bslib, thematic, shinymanager, leaflet, ggrepel, sf, stringr,fontawesome,
                 shinycssloaders, shinydashboardPlus, lubridate, scales, cachem,
                 rmapshaper)

shinyOptions(cache = cachem::cache_disk("./app_cache"))
#shinyOptions(cache = cachem::cache_mem(max_size = 1000e6))

#Bd and VES combined data read in
# data <- read_csv(here("data", "bd_plot.csv")) %>% 
#   select(!1)

# lakes shapefile
# lakes <- read_sf(here("data", "lakes", "lakes2021.shp")) %>% 
#   select(c(LAKEID, geometry)) %>% 
#   rename(id = LAKEID) %>%
#   filter(st_is_valid(geometry))

#Bd data read in
bd_data <- read_csv(here("data", "bd_data.csv"))



#read in wilderness shape files
shape <- read_sf(here("data", "wilderness_shapes", "wilderness.shp")) 

#read in water types
water <- read_csv(here("data", "water_type.csv")) %>% 
  rename(id = lake_id)

#ves data read in
ves_data <- read_csv(here("data", "ves_data.csv")) %>% 
  left_join(bd_data) %>% 
  left_join(water) 

# read in bd_plot data with month_year
bd_plot <- read_csv(here("data", "bd_plot.csv"))

#lakes <- read_sf(here("data", "lakes", "lakes2021.shp"))

# all visit data for map
all_visits <- read_csv(here("data", "all_visits.csv")) %>% 
  left_join(water, by = c("site_id" = "id"))
# %>% 
#   left_join(lakes, by = c("site_id" = "id"))



# # themeing
# theme <- bs_theme(
#   bg = "#b6f0c3", # green background
#   fg = "#2d302e", # cream text foreground
#   primary = "#0417c2", # hyperlinks
#   secondary = "#506e57", # messages
#   success = "#506e57", # operation succeeded
#   info = "#506e57", # light blue from bar
#   warning = "#C3512C",# yellow
#   danger = "#FACE00", # orange red
#   base_font = font_google("Open Sans"), 
#   heading_font = font_google("Source Sans Pro"))

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





