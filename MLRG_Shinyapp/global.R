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
library(DBI)
library(RPostgres)
library(shinyalert)




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

# all visit data for map
all_visits <- read_csv(here( "MLRG_Shinyapp/data/all_visits.csv")) %>% 
  left_join(water, by = c("site_id" = "id")) %>%
  anti_join(ves_data, by = c("site_id" = "id"))


 ######### Database Connection and Data Download ############## 

# DB conection
source(here("MLRG_Shinyapp/db_creds.R"), local = T)

connection <- dbConnect(dbDriver("Postgres"),
                        dbname = sn_dbname,
                        host = sn_host,
                        port = sn_port,
                        user = sn_user,
                        password = sn_password)

dbExecute(connection, "set search_path = public")

# Site table
site <- dbGetQuery(connection, "select * from site")
site <- site %>%
  rename(site_name = name,
         site_utme = utme,
         site_utmn = utmn)

# visit table
visit <- dbGetQuery(connection, "select * from visit")
visit <- dplyr::select(visit, !c(id, site_id)) %>%
  mutate(year = year(visit_date)) %>%
  rename(visit_comment = comment)

# survey table
survey <- dbGetQuery(connection, "select * from survey")
survey <- dplyr::select(survey, !c(id, visit_id)) %>%
  rename(survey_comment = comment)

# capture table
capture <- dbGetQuery(connection, "select * from capture_survey")
capture <- dplyr::select(capture, !c(id, survey_id, surveyor_id)) %>%
  rename(capture_comment = comment,
         capture_utme = utme,
         capture_utmn = utmn)

# full capture data set
raw_capture <- dbGetQuery(connection, "select s.*, v.*, s2.*, cs.*, s3.name, bd.*
                                       from site s
                                       join visit v on s.id = v.site_id
                                       join survey s2 on v.id = s2.visit_id
                                       join capture_survey cs on s2.id = cs.survey_id
                                       join surveyor s3 on cs.surveyor_id = s3.id
                                       join bd_load bd on cs.swab_id = bd.sample_id;")

full_capture <- raw_capture %>%
  dplyr::select(!c(id..13, site_id, id..18, id..34, surveyor_id, visit_id, survey_id, swab_id)) %>%
  rename(site_name = name,
         visit_comment = comment,
         site_utme = utme,
         site_utmn = utmn,
         survey_comment = comment..32,
         capture_comment = comment..50,
         capture_utme = utme..44,
         capture_utmn = utmn..45,
         surveyor = name..51,
         swab_id = sample_id) %>%
  mutate(year = year(visit_date))

# bd load table
bd_load_table <- dbGetQuery(connection, "select * from bd_load")
bd_load_table <- bd_load_table %>% rename(swab_id = sample_id)

# VES table
ves_table <- dbGetQuery(connection, "select * from visual_survey")
ves_table <- dplyr::select(ves_table, !c(id, survey_id)) %>%
  rename(ves_comment = comment)

# full Ves data
raw_ves <- dbGetQuery(connection, "select s.*, v.*, s2.*, cs.*
                                   from site s
                                   join visit v on s.id = v.site_id
                                   join survey s2 on v.id = s2.visit_id
                                   join visual_survey cs on s2.id = cs.survey_id;")

full_ves <- raw_ves %>%
  dplyr::select(!c(id..13, site_id, id..18, id..34, visit_id, survey_id)) %>%
  rename(site_name = name,
         visit_comment = comment,
         survey_comment = comment..32,
         ves_comment = comment..41,
         site_utme = utme,
         site_utmn = utmn)%>%
  mutate(year = year(visit_date))

# relocate table
relocate_table <- dbGetQuery(connection, "select * from relocate")
relocate_table <- dplyr::select(relocate_table, !c(id)) %>% 
  mutate(year = year(collect_date)) %>% 
  rename(relocate_comment = comment)

# frog relocate table
relocate_frog_table <- dbGetQuery(connection, "select * from relocate_frog")
relocate_frog_table <- dplyr::select(relocate_frog_table, !c(id, relocate_id, surveyor_id)) %>% 
  rename(frog_comment = comment)

# full CMR table
raw_cmr <- dbGetQuery(connection, "select r.*, rf.*
                                   from relocate r 
                                   join relocate_frog rf on r.id = rf.relocate_id;")

full_cmr <- raw_cmr %>% 
  dplyr::select(!c(id, id..10, surveyor_id, relocate_id)) %>% 
  rename(relocate_comment = comment,
         frog_comment = comment..20) %>% 
  mutate(year = year(collect_date))


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





