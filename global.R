if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

# librarian downloads, if not already downloaded, and reads in needed packages

librarian::shelf(shiny, tidyverse, here, shinyWidgets, leafem, bslib, thematic, shinymanager, leaflet, ggrepel, sf, stringr,fontawesome,
                 shinycssloaders, shinydashboardPlus, lubridate, scales)

captures <- data.frame(year = c(1995, 1996, 1996),
                       long = c(119.25, 119.35, 119.48),
                       lat = c(34.34, 34.56, 34.23))

visits <- data.frame(year = c(1995, 1996, 1996, 1996, 2001),
                     long = c(119.25, 119.35, 119.48, 119.44, 119.67 ),
                     lat = c(34.34, 34.56, 34.23, 34.58, 34.23))