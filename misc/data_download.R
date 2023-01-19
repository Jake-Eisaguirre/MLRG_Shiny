
if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

# librarian downloads, if not already downloaded, and reads in needed packages
librarian::shelf(tidyverse, here, janitor, sf, lubridate, RPostgres, DBI)



# DB conection
tryCatch({
  drv <- dbDriver("Postgres")
  print("Connecting to Database…")
  connection <- dbConnect(drv, 
                          dbname = Sys.getenv("sn_dbname"),
                          host = Sys.getenv("sn_host"), 
                          port = Sys.getenv("sn_port"),
                          user = Sys.getenv("sn_user"), 
                          password = Sys.getenv("sn_password"))
  print("Database Connected!")
},
error=function(cond) {
  print("Unable to connect to Database.")
})


####### VES Data #########3
ves_data <- dbGetQuery(connection, "select *
                                   from site s 
                                   join visit v on s.id = v.site_id 
                                   join survey s2 on v.id = s2.visit_id 
                                   join visual_survey vs on s2.id = vs.survey_id;")

# select desired columns and pull out month and year as new columns
clean_ves <- ves_data %>% 
  dplyr::select(c(id, utme, utmn, wilderness, visit_date, species, visual_life_stage, 
                  visual_animal_state, count)) %>% 
  mutate(date = year(visit_date),
         month = month(visit_date)) %>% 
  filter(visual_animal_state == "alive") %>% 
  mutate(month_year = paste(month, date, sep ="/"))

# pull out only desolation wilderness due to zone 10 coords
des_cords <- clean_ves %>% 
  filter(wilderness == "desolation")

des_sf <- st_as_sf(des_cords, coords = c('utme', 'utmn'), crs = "+proj=utm +zone=10")
des_ves = st_transform(des_sf, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")

# now do rest of wildernesses in zone 11 coords
clean_ves_data <- clean_ves %>% 
  filter(!wilderness %in% c("desolation", "none"))

sf_ves <- st_as_sf(clean_ves_data, coords = c('utme', 'utmn'), crs = "+proj=utm +zone=11")
leaf_ves = st_transform(sf_ves, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")

# bind desolation back to all the main df
des_bind <- rbind(leaf_ves, des_ves)



# pull out geometries as standard lat long columns
ves <- des_bind %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  as.data.frame() %>% 
  dplyr::select(!c(geometry)) %>% 
  mutate(wilderness = gsub("_", " ", wilderness),
         wilderness = str_to_title(wilderness),
         visual_life_stage = str_to_title(visual_life_stage),
         species = str_to_title(species)) %>% 
  group_by(id, date, species, visual_life_stage, visual_animal_state, wilderness, lat, long) %>% 
  summarise(count = median(count)) %>% 
  mutate(species = case_when(species == "Hyre" ~ "Pacific treefrog - Hyliola regilla",
                             species == "Thel" ~ "Western terrestrial gartersnake - Thamnophis elegans",
                             species == "Ramu" ~ "Mountain yellow-legged frog - Rana muscosa",
                             species == "Buca" ~ "Yosemite toad - Anaxyrus canorus",
                             species == "Bubo" ~ "Western toad - Anaxyrus boreas",
                             species == "Thco" ~ "Western aquatic gartersnake - Thamnophis couchii",
                             species == "Tato" ~ "California newt - Taricha torosa",
                             species == "Raca" ~ "Bullfrog - Rana catesbiana",
                             species == "Clma" ~ "Western pond turtle - Actinemys marmorata",
                             species == "Amma" ~ "Long-toed salamander - Ambystoma macrodactylum")) %>% 
  filter(!str_detect(species, "NA"))


write_csv(ves, here("MLRG_Shinyapp", "data", "ves_data.csv"))

######## END VES ###########

###### BD Data #########

bd_data <- dbGetQuery(connection, "select *
                                  from site s 
                                  join visit v on s.id = v.site_id 
                                  join survey s2 on v.id = s2.visit_id 
                                  join capture_survey cs on s2.id = cs.survey_id  
                                  join bd_load bl on cs.swab_id = bl.sample_id ")

clean_bd <- bd_data %>% 
  dplyr::select(id, utme, utmn, wilderness, visit_date, species, capture_life_stage, bd_load) %>% 
  mutate(date = year(visit_date),
         month = month(visit_date),
         bd_load = log10(bd_load + 1),
         month_year = format(visit_date, "%Y-%m")) %>% 
  mutate(species = case_when(species == "ramu" ~ "Mountain yellow-legged frog - Rana muscosa",
                             species == "hyre" ~ "Pacific treefrog - Hyliola regilla",
                             species == "tato" ~ "California newt - Taricha torosa",
                             species == "buca" ~ "Yosemite toad - Anaxyrus canorus",
                             species == "bubo" ~ "Western toad - Anaxyrus boreas"))


# pull out only desolation wilderness due to zone 10 coords
des_cords <- clean_bd %>% 
  filter(wilderness == "desolation")

des_sf <- st_as_sf(des_cords, coords = c('utme', 'utmn'), crs = "+proj=utm +zone=10")
des_bd = st_transform(des_sf, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")

# now do rest of wildernesses in zone 11 coords
clean_bd_data <- clean_bd %>% 
  filter(!wilderness %in% c("desolation", "none"))

sf_bd <- st_as_sf(clean_bd_data, coords = c('utme', 'utmn'), crs = "+proj=utm +zone=11")
leaf_bd = st_transform(sf_bd, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")

# bind desolation back to all the main df
des_bind <- rbind(leaf_bd, des_bd)

bd <- des_bind %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  as.data.frame() %>% 
  dplyr::select(!c(geometry)) %>% 
  rename(visual_life_stage = capture_life_stage) %>% 
  mutate(wilderness = gsub("_", " ", wilderness),
         wilderness = str_to_title(wilderness),
         visual_life_stage = str_to_title(visual_life_stage)) %>% 
  group_by(id, date, species, wilderness, visual_life_stage) %>% 
  summarise(bd = median(bd_load)) %>% 
  ungroup()

write_csv(bd, here("MLRG_Shinyapp", "data", "bd_data.csv"))

bd_plot <- des_bind %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>% 
  as.data.frame() %>% 
  dplyr::select(!c(geometry)) %>% 
  rename(visual_life_stage = capture_life_stage) %>% 
  mutate(wilderness = gsub("_", " ", wilderness),
         wilderness = str_to_title(wilderness),
         visual_life_stage = str_to_title(visual_life_stage)) %>% 
  group_by(id, species, visual_life_stage, month_year, lat, long, wilderness, date) %>% 
  mutate(infect = if_else(bd_load>0, 1, 0)) %>% 
  summarise(bd = median(bd_load),
            Prevalence = (sum(infect)/sum(n())),
            sample_size = n()) 

write_csv(bd_plot, here("MLRG_Shinyapp", "data", "bd_plot.csv"))

###### END BD #########


###### Raw data download ##########


# Site table
site <- dbGetQuery(connection, "select * from site")
site <- site %>%
  rename(site_name = name,
         site_utme = utme,
         site_utmn = utmn)
write_csv(site, here("MLRG_Shinyapp", "data","site.csv"))

# visit table
visit <- dbGetQuery(connection, "select * from visit")
visit <- dplyr::select(visit, !c(id, site_id)) %>%
  mutate(year = year(visit_date)) %>%
  rename(visit_comment = comment)
write_csv(visit, here("MLRG_Shinyapp", "data","visit.csv"))

# survey table
survey <- dbGetQuery(connection, "select * from survey")
survey <- dplyr::select(survey, !c(id, visit_id)) %>%
  rename(survey_comment = comment)
write_csv(survey, here("MLRG_Shinyapp", "data", "survey.csv"))

# capture table
capture <- dbGetQuery(connection, "select * from capture_survey")
capture <- dplyr::select(capture, !c(id, survey_id, surveyor_id)) %>%
  rename(capture_comment = comment,
         capture_utme = utme,
         capture_utmn = utmn)
write_csv(capture, here("MLRG_Shinyapp", "data", "capture.csv"))

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
write_csv(full_capture, here("MLRG_Shinyapp", "data", "full_capture.csv"))

# bd load table
bd_load_table <- dbGetQuery(connection, "select * from bd_load")
bd_load_table <- bd_load_table %>% rename(swab_id = sample_id)
write_csv(bd_load_table, here("MLRG_Shinyapp", "data", "bd_load_table.csv"))

# VES table
ves_table <- dbGetQuery(connection, "select * from visual_survey")
ves_table <- dplyr::select(ves_table, !c(id, survey_id)) %>%
  rename(ves_comment = comment)
write_csv(ves_table, here("MLRG_Shinyapp", "data", "ves_table.csv"))

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
write_csv(full_ves, here("MLRG_Shinyapp", "data", "full_ves.csv"))

# relocate table
relocate_table <- dbGetQuery(connection, "select * from relocate")
relocate_table <- dplyr::select(relocate_table, !c(id)) %>% 
  mutate(year = year(collect_date)) %>% 
  rename(relocate_comment = comment)
write_csv(relocate_table, here("MLRG_Shinyapp", "data", "relocate_table.csv"))

# frog relocate table
relocate_frog_table <- dbGetQuery(connection, "select * from relocate_frog")
relocate_frog_table <- dplyr::select(relocate_frog_table, !c(id, relocate_id, surveyor_id)) %>% 
  rename(frog_comment = comment)
write_csv(relocate_frog_table, here("MLRG_Shinyapp", "data", "relocate_frog_table.csv"))

# full CMR table
raw_cmr <- dbGetQuery(connection, "select r.*, rf.*
                                   from relocate r 
                                   join relocate_frog rf on r.id = rf.relocate_id;")

full_cmr <- raw_cmr %>% 
  dplyr::select(!c(id, id..10, surveyor_id, relocate_id)) %>% 
  rename(relocate_comment = comment,
         frog_comment = comment..20) %>% 
  mutate(year = year(collect_date))
write_csv(full_cmr, here("MLRG_Shinyapp", "data", "full_cmr.csv"))

