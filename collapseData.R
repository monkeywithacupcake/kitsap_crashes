# from collision download to simple file
# explore crash data

library(tidyverse)
library(sf)

# first, we need to get the crash data
cd1 <- read_csv("~/crashdata/data/WA_Crash_Data-1.csv") # ped
cd2 <- read_csv("~/crashdata/data/WA_Crash_Data-2.csv") # cycl
cd3 <- read_csv("~/crashdata/data/WA_Crash_Data-3.csv") # car only

cd2 <- cd2 %>%
  mutate(`MV_Unit_Veh_Cond_Typ_Cd_1` = as.character(`MV_Unit_Veh_Cond_Typ_Cd_1`),
         `Comrcl_Carr_Zip_Code` = as.character(`Comrcl_Carr_Zip_Code`)
  )
cd3 <- cd3 %>%
  mutate(`Comrcl_Carr_USDOT_Num` = as.character(`Comrcl_Carr_USDOT_Num`),
         `Comrcl_Carr_Zip_Code` = as.character(`Comrcl_Carr_Zip_Code`)
  )

cd_all <- bind_rows(cd1, cd2, cd3)
rm(cd1, cd2, cd3)

# let us get rid of columns where we just do not have data
not_all_na <- function(x) any(!is.na(x))
cd_all <- cd_all %>% select(where(not_all_na))

# each collision can have many rows - one for each table entry/car/driver/passenger/ped
# simplify to one row per collision
source("~/crashdata/getSimplifiedCitations.R") # function
coll <- cd_all %>% 
  mutate(cite_simple = getSimplifiedCitations(MV_Drvr_CitationCharge)) %>%
  group_by(Colli_Rpt_Num_Colli_Rpt_Num) %>% 
  arrange(MV_Drvr_Unit_Num) %>%
  summarise(hitrun = first(Colli_Dtl_Info_Hit_Run_Ind), 
            #veh1_d_gender  = first(MV_Drvr_Gender_Typ_Cd),
            #veh1_d_age  = first(MV_Drvr_Age),
            colli_dt = first(Colli_Dtl_Info_Colli_Date),
            obj_struck = first(toupper(Colli_Dtl_Info_ObjectStruck)),
            #p_dispatch = first(Colli_Dtl_Info_Police_Dispatch_Time),
            #p_arrive = first(Colli_Dtl_Info_Police_Arrive_Time), 
            x = first(Colli_Dtl_Info_State_Plane_X), 
            y = first(Colli_Dtl_Info_State_Plane_Y),
            ped = n_distinct(Ped_Colli_Surr_Key, na.rm=TRUE) >0,
            cyclist = n_distinct(Pedcyc_Colli_Surr_Key, na.rm=TRUE) >0,
            cite_simple = toString(na.omit(gsub("OTHER",NA,cite_simple))),
            .groups = "drop"
  ) %>%
  mutate(type = ifelse(ped | cyclist, "Includes Ped or Cyclist", "Car Only"),
         pedtype = ifelse(ped & cyclist, "Includes Both Ped & Cyclist", 
                          ifelse(ped, "Includes Ped", 
                                 ifelse(cyclist, "Includes Cyclist", NA))),
         year = lubridate::year(lubridate::mdy_hms(colli_dt)),
         hour = lubridate::hour(lubridate::mdy_hms(colli_dt)),
         tod = if_else(hour >= 6 & hour < 18, "day 0600-1759", "night 1800-0559"),
         cite_for_dui = grepl("DUI",cite_simple),
         cite_for_homicide = grepl("HOMICIDE",cite_simple),
         cite_for_speed = grepl("SPEED",cite_simple)
  )


coll2 <- st_as_sf(filter(coll,!is.na(x)), coords = c("x", "y"), crs = 2927)# 32610) 


# get the geos to add
d_dir <- '~/Documents/non-motorized/'
outline <- sf::read_sf(paste0(d_dir,"data/outline"))
flatoutline <- st_union(outline)
cities <- sf::read_sf(paste0(d_dir,"data/cities"))
county_no_cities <- st_difference(outline, st_union(cities))
uga <- sf::read_sf(paste0(d_dir,"data/uga"))
uga_no_city <- st_as_sf(uga) %>%
  filter(substring(GMA_JURISD, 1, 4) != "City") %>%
  group_by(GMA_JURISD) %>%  # ".x" is refers to the current group:
  group_modify(~ st_union(.x) %>% as_tibble()) %>%
  ungroup() %>%
  st_as_sf() %>%
  arrange(GMA_JURISD)

gkitsap <- uga_no_city %>%
  rename(PLACE = GMA_JURISD) %>%
  bind_rows(select(cities, PLACE = DESCR, geometry)) %>%
  mutate(PLACE = case_when(
    PLACE == "bi" ~ "City of Bainbridge Island",
    PLACE == "brem" ~ "City of Bremerton",
    PLACE == "poulsbo" ~ "City of Poulsbo",
    PLACE == "pt orch" ~ "City of Port Orchard",
    TRUE ~ PLACE
  ))
okit <- st_difference(outline, st_union(gkitsap)) %>%
  mutate(PLACE = "Other") %>%
  group_by(PLACE) %>% 
  group_modify(~ st_union(.x) %>% as_tibble()) %>%
  ungroup() %>%
  st_as_sf() 
gkitsap <- gkitsap %>%
  bind_rows(okit)

# add the geo to the data
coll3 <- st_join(coll2, st_buffer(st_transform(gkitsap, crs = 2927),0)) %>%
  st_set_geometry(., NULL)  # remove the point geo
write_csv(coll3, "collision_data_simple_w_CITE.csv")