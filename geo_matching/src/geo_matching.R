#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policinghomelessness/geo_matching/src/geo_matching.r


library(CGPfunctions)
library(extrafont)
library(fuzzyjoin)
library(lubridate)
library(qs)
library(rcartocolor)
library(readr)
library(readxl)
library(scales)
library(sf)
library(tidycensus)
library(tidyverse)
options(scipen=999)

stopifnot(requireNamespace("here", quietly = TRUE))
here <- here::here

########### input and output files ##############
# input files:
inputfiles <- list(
   lahsa_coords = "geo_matching/input/lahsa_coords.csv",
   lasan_coords = "geo_matching/input/lasan_coords.csv",
   lapd_coords = "geo_matching/input/lapd_coords.csv",
   lasan = "processing/output/lasan_processed.csv",
   lapd = "processing/output/processed_arrests.rds",
   lahsa = "processing/output/lahsa/lahsa_services_processed.csv",
   coords_matched = "geo_matching/frozen/all_coords_out_250m_matrix 2.csv",
   coords_groups = "geo_matching/frozen/grouped_coords.csv"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   all_coordinates = "geo_matching/output/all_coords_out.csv",
   lasan_processed_geomatched = "geo_matching/output/lasan_processed_geomatched.rds",
   lahsa_processed_geomatched = "geo_matching/output/lahsa_processed_geomatched.rds",
   lapd_processed_geomatched = "geo_matching/output/lapd_processed_geomatched.rds"
   
) %>% map(here)

#input
lahsa_coords <- read_csv(inputfiles$lahsa_coords)
lasan_coords <- read_csv(inputfiles$lasan_coords)
lapd_coords <- read_csv(inputfiles$lapd_coords)

all_coords <- bind_rows(lahsa_coords, lasan_coords, lapd_coords) %>% 
   ungroup() %>% 
   select(-dataset) %>% 
   distinct() %>% 
   mutate(unique_coordinate_id = row_number())

#placing locations in census tract
#place id into a simple features object
#get dataset of census tracts in la county
my_vars <- c(
   total_pop = "B01003_001",
   pop_poverty = "B17001_001"
)

tracts_acs <- get_acs(
   geography = "tract",
   state = "CA",
   variables = my_vars,
   geometry = T) %>% 
   select(GEOID, variable, estimate) %>%
   st_transform(6440)

#Spatial join
coords_sf <- all_coords %>% 
   filter(latitude > 0.0000) %>% 
   select(unique_coordinate_id, latitude, longitude) %>% 
   st_as_sf(coords = c("longitude", "latitude"),
            crs = 4326) %>%
   st_transform(6440)

coords_joined <- st_join(
   coords_sf,
   tracts_acs
)

temp_join <- coords_joined %>% 
   select(unique_coordinate_id, geometry, GEOID)

all_coords <- left_join(all_coords, temp_join) %>% 
   distinct()


write_csv(all_coords, outputfiles$all_coordinates)


#bring back in matched/grouped data and make some joins
#input
lasan <- read_csv(inputfiles$lasan, col_types = cols(
   latitude = col_double()))
lapd <- read_rds(inputfiles$lapd) %>% 
   mutate(GEOID = as.double(GEOID), latitude = as.double(latitude),
          longitude = as.double(longitude), geometry = as.character(geometry)) 
lahsa <- read_csv(inputfiles$lahsa)
coords_matched <- read_csv(inputfiles$coords_matched)
coords_grouped <- read_csv(inputfiles$coords_groups) %>% 
   rename(OHS_ABH = OHS_ABS) %>% 
   filter(unique_coordinate_id != 33170 | 
             (unique_coordinate_id == 33170 & OHS_ABH == "CIVIC CENTER"))

#Create several relational tables with lasan sweep as the unit of analysis. 
#were other agencies there? what did they do?

#join with coordinates grouped
lasan <- left_join(lasan, coords_grouped)
lahsa <- left_join(lahsa, coords_grouped)
lapd <- left_join(lapd, coords_grouped)

#for each lasan coordinate, get all other potential coords
lasan_to_join <- lasan %>% 
   filter(!is.na(latitude)) %>% 
   full_join(coords_matched) %>% 
   select(date, case_id, unique_coordinate_id, unique_coordinate_id_2) %>%
   distinct()

#we are doing a filtering join of lahsa services. on any date, is the coordinate ID
#a possible ID for a lasan location on that date?
lahsa_joined <- semi_join(lahsa, lasan_to_join, by = c(
   "service_date" = "date", "unique_coordinate_id" = "unique_coordinate_id_2"))

#get just the encounter ids
lahsa_id <- lahsa_joined %>% 
   select(service_date, unique_coordinate_id, lahsa_encounter_id) %>% 
   distinct()

#putting potential lahsa ids along the lasan data
lasan_matched <- left_join(lasan_to_join, lahsa_id, by =
                              c("date" = "service_date", 
                                "unique_coordinate_id_2" = "unique_coordinate_id")) %>% 
   filter(!is.na(lahsa_encounter_id) | unique_coordinate_id == unique_coordinate_id_2)

t <- lasan_matched %>% 
   group_by(case_id) %>% 
   summarise(lahsa_encounters = n_distinct(lahsa_encounter_id, na.rm = T )) %>% 
   mutate(lahsa_present = ifelse(lahsa_encounters > 0, 1, 0), 
          has_geography = 1)

#join new columns to lasan
lasan <- left_join(lasan, t)

#add to lasan whether any of the matched lahsa encounters had a housing ref
#or attainment
t <- lahsa %>% 
   select(housing_referral, housing_attained_encounter, lahsa_encounter_id)

t1 <- lasan_matched %>% 
   filter(!is.na(lahsa_encounter_id)) %>% 
   left_join(t) 

referral_encounter <- t1 %>% 
   filter(housing_referral == 1) %>% 
   select(case_id) %>% 
   distinct() %>% 
   mutate(housing_referral = 1)

lasan <- left_join(lasan, referral_encounter)
rm(referral_encounter)

attain_encounter <- t1 %>% 
   filter(housing_attained_encounter == 1) %>% 
   select(case_id) %>% 
   distinct() %>% 
   mutate(housing_attained = 1)

lasan <- left_join(lasan, attain_encounter)
rm(attain_encounter, t1)

#create and join new columns to lahsa
join_lahsa <- lasan_matched %>% 
   group_by(lahsa_encounter_id ) %>% 
   summarise(number_of_lasan_cleanings_250m = n_distinct(case_id)) %>% 
   mutate(lasan_cleaning_near = 1) %>% 
   filter(!is.na(lahsa_encounter_id))

lahsa <- left_join(lahsa, join_lahsa)

rm(join_lahsa, t, lahsa_joined, lahsa_id, lasan_matched)

#now same joins with lapd
#we are doing a filtering join of lapd arrests. on any date, is the coordinate ID
#a possible ID for a lasan location on that date?
lapd_joined <- semi_join(lapd, lasan_to_join, by = c(
   "arrest_date" = "date", "unique_coordinate_id" = "unique_coordinate_id_2"))

#get just the  ids
lapd_id <- lapd_joined %>% 
   select(arrest_date, unique_coordinate_id, report_id) %>% 
   distinct()

#putting potential lapd ids along the lasan data
lasan_matched <- left_join(lasan_to_join, lapd_id, by =
                              c("date" = "arrest_date", 
                                "unique_coordinate_id_2" = "unique_coordinate_id")) %>% 
   filter(!is.na(report_id) | unique_coordinate_id == unique_coordinate_id_2)

t <- lasan_matched %>% 
   group_by(case_id) %>% 
   summarise(lapd_arrests = n_distinct(report_id, na.rm = T )) %>% 
   mutate(lapd_arrest_madeYN = ifelse(lapd_arrests > 0, 1, 0), 
          has_geography = 1)

#join new columns to lasan
lasan <- left_join(lasan, t)

#create and join new columns to lapd
join_lapd <- lasan_matched %>% 
   group_by(report_id) %>% 
   summarise(number_of_lasan_cleanings_250m = n_distinct(case_id)) %>% 
   mutate(lasan_cleaning_near = 1) %>% 
   filter(!is.na(report_id))

lapd <- left_join(lapd, join_lapd)
rm(join_lapd, t, lapd_joined, lapd_id, lasan_matched)


#write out
write_rds(lasan, outputfiles$lasan_processed_geomatched)
write_rds(lahsa, outputfiles$lahsa_processed_geomatched)
write_rds(lapd, outputfiles$lapd_processed_geomatched)
