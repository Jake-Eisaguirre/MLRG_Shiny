if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

# librarian downloads, if not already downloaded, and reads in needed packages

librarian::shelf(shiny, tidyverse, here, shinyWidgets, leafem, bslib, thematic, shinymanager, leaflet, ggrepel)


#Bd and VES combined data read in
data <- read_csv(here("data", "shiny_data.csv")) %>% 
  select(!1)

#Bd data read in
bd_data <- read_csv(here("data", "bd_data.csv"))

#ves data read in
ves_data <- read_csv(here("data", "ves_data.csv")) 

#read in wilderness shape files
shape <- read_sf(here("data", "wilderness_shapes", "wilderness.shp"))


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


clean_wilderness_names <- c("Kings Canyon", "Sequoia", "John Muir", "Golden Trout", "Hoover", "None", "Yosemite", "Desolation")




