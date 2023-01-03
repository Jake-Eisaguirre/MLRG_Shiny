source(here("misc", "db_creds.R"))

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
                          dbname = sn_dbname,
                          host = sn_host, 
                          port = sn_port,
                          user = sn_user, 
                          password = sn_password)
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
                             species == "Thel" ~ "Western garter snake - Thamnophis elegans",
                             species == "Ramu" ~ "Mountain yellow-legged frog - Rana muscosa",
                             species == "Buca" ~ "Yosemite toad - Anaxyrus canorus",
                             species == "Bubo" ~ "Western toad - Anaxyrus boreas",
                             species == "Thco" ~ "Sierra garter snake - Thamnophis couchii",
                             species == "Tato" ~ "California newt - Taricha torosa",
                             species == "Raca" ~ "Bullfrog - Rana catesbiana",
                             species == "Clma" ~ "Western pond turtle - Actinemys marmorata",
                             species == "Amma" ~ "Long-toed salamander - Ambystoma macrodactylum")) %>% 
  filter(!str_detect(species, "NA"))


write_csv(ves, here("data", "ves_data.csv"))

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

write_csv(bd, here("data", "bd_data.csv"))

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

write_csv(bd_plot, here("data", "bd_plot.csv"))

###### END BD #########
            
