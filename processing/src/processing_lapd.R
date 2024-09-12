#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policingunhousedness/descriptives/src/lapd_descriptives.r
library(pacman)
p_load(lubridate, readxl, readr, rcartocolor, extrafont, scales, tidycensus,
       CGPfunctions, tidyverse, qs, fuzzyjoin, sf, janitor, tigris)
options(scipen=999)

here <- here::here

########### input and output files ##############
# input files:
inputfiles <- list(
   unhoused_arrests = "import/input/LAPD/structured data/ARRESTS AND RFCS IDENTIFIED AS HOMELESS.xlsx",
   downloaded_arrests_post2020 = "import/input/LAPD/structured data/Arrest_Data_downloaded5_9_23.csv",
   downloaded_arrests_pre2020 = "import/input/LAPD/structured data/Arrest_Data_through2019_downloaded.csv",
   addresses_marked = "processing/frozen/addresses_marked.csv",
   addresses_geocoded = "processing/frozen/lapd_addresses_geocoded.csv",
   recoded_offenses = "processing/frozen/offenses_recoded.csv",
   race_recoded = "processing/frozen/lapd_race_recoded.csv",
   crime_unhoused_victim = "import/input/LAPD/structured data/CRIMES WITH HOMELESS AS VICTIM_R.xlsx",
   crime1 = "import/input/LAPD/structured data/Crime_Data_from_2020_to_Present.csv",
   crime2 = "import/input/LAPD/structured data/Crime_Data_from_2010_to_2019_20231107.csv",
   use_of_force = "import/input/LAPD/structured data/USE OF FORCE_R.xlsx",
   meija_41_18 = "import/input/LAPD/structured data/41.18_Arrests_2021_-_2023 - Raw File.csv"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   processed_arrests = "processing/output/processed_arrests.rds",
   offenses_to_recode = "processing/frozen/offenses_to_recode.csv",
   addresses_to_mark = "processing/frozen/addresses_to_mark.csv",
   addresses_to_geocode = "processing/output/lapd_addresses_to_geocode.csv",
   race_to_recode = "processing/output/lapd_race_to_recode.csv",
   coords_out = "geo_matching/input/lapd_coords.csv",
   crimes_against_unhoused_processed = "processing/output/crimes_against_unhoused_processed.csv",
   crimes_total = "processing/output/crimes_total.csv",
   use_of_force_out = "processing/output/use_of_force_processed.csv",
   meija = "processing/output/meija.csv"
   
) %>% map(here)

#read in arrests

#full post and full_pre are the full arrests as downloded from
#unhoused arrests is data provided to HRW by LAPD in response to our PRA
#request for all arrests of unhoused people.
full_post <- read_csv(inputfiles$downloaded_arrests_post2020) %>% 
   clean_names()
full_pre <- read_csv(inputfiles$downloaded_arrests_pre2020) %>% 
   clean_names()
unhoused_arrests <- read_xlsx(inputfiles$unhoused_arrests) %>% 
   clean_names()

#Prepare unhoused arrests for join with full arrests
#id for matching to full arrests, tag for coming from unhoused data and date for filtering full arrests
unhoused_arrests <- unhoused_arrests %>% 
   mutate(id = as.character(report_no),
          unhoused = "Y",
          arrest_date = as_date(as.character(unhoused_arrests$arrest_date), format = "%Y%m%d"),
          booking_date = as_date(as.character(unhoused_arrests$booking_date), format = "%Y%m%d"))

#full arrests - bring the two time periods into one set
full_arrests <- bind_rows(full_post, full_pre) %>% 
   distinct()
rm(full_post, full_pre)

full_arrests <- full_arrests %>% 
   mutate(arrest_date = as_date(substr(arrest_date, 1, 10), format = "%m/%d/%Y"),
          booking_date = as_date(substr(booking_date, 1, 10), format = "%m/%d/%Y")) %>% 
   filter(arrest_date %in% unhoused_arrests$arrest_date) %>% 
   mutate(id = as.character(report_id)) 

#test to see how many/percentage arrest from the unhoused arrest dataset were not in the 
#open data of total arrests
 missing_in_full <- anti_join(unhoused_arrests, full_arrests, by = "id")

# #so 95.5% of unhoused arrests are in the full arrest data. 
 
#test that the charge, charge description are same in both datasets
# test_join <- left_join(unhoused_arrests, full_arrests, by = "id")
# t <- test_join %>%
#    select(Charge, `Charge Description`, CHARGE, `CHARGE DESCRIPTION`) %>%
#    mutate(charge_t = ifelse(Charge == CHARGE, T, F),
#           descript_t = ifelse(`Charge Description` == `CHARGE DESCRIPTION`, T, F))
# #yes, they are except a couple of numbers
# rm(t, test_join, missing_in_full)

#select unique columns from unhoused arrests for join for those who are in full
unhoused_join <- unhoused_arrests %>% 
   filter(!id %in% missing_in_full$id) %>% 
   select(unhoused, id, race = descent, gender = sex)

#for the 4.5% that were not in the full dataset, pull those rows and prepare to 
#bind into the data
missing_to_bind <- missing_in_full %>% 
   unite(address1, street_no, street_name, street_type, sep = " ", na.rm = T, remove = F) %>% 
   unite(address2, cross_street_name, cross_street_type, sep = " ", na.rm = T) %>% 
   mutate(address = ifelse(is.na(street_no),
                           paste(address1, "and", address2, ", Los Angeles, CA", sep = " "),
                           paste(address1, ", Los Angeles, CA", sep = " ")),
          missing = 1) %>% 
   select(-address1, -street_no, -street_name, -street_type,
          -address2, race = descent, gender = sex,
          area, area_name = areadesc, arrest_type_code = arrest_type,
          charge_group_description = description)

#full join
arrests <- full_join(full_arrests, unhoused_join, by = c("id"))

arrests <- arrests %>% 
   select(-booking_time)
missing_to_bind <- missing_to_bind %>% 
   select(-booking_time)

arrests <- bind_rows(arrests, missing_to_bind)

#recode arrest type
arrests <- arrests %>% 
   mutate(arrest_type = case_when(
      arrest_type_code == "F" ~ "Felony",
      arrest_type_code == "M" ~ "Misdemeanor",
      arrest_type_code == "O" ~ "Other",
      arrest_type_code == "I" ~ "Infraction",
      TRUE ~ "Other"
   ))

#arrest month
arrests <- arrests %>% 
   mutate(floor_arrest_date = floor_date(arrest_date, unit = "month"))

#clean addresses
arrests <- arrests %>% 
   mutate(address = str_squish(address), cross_street = str_squish(cross_street))

#output file to mark for good address or needing the cross street
arrests <- arrests %>% 
   mutate(first = word(address, start = 1L, sep = fixed(" ")))

#address out for those missing lat long
t <- arrests %>% 
   filter(lat == 0.0000) %>% 
   group_by(first) %>% 
   summarise(count = n_distinct(id))
write_csv(t, outputfiles$addresses_to_mark)

#read back in marked files
addresses_marked <- read_csv(inputfiles$addresses_marked)

addresses_to_geocode <- arrests %>% 
   filter(lat == 0.0000 & unhoused == "Y") %>% 
   left_join(addresses_marked) %>% 
   mutate(address_to_geocode = ifelse(is.na(add_cross), address, 
                                      paste(address, " and ", cross_street, ", Los Angeles", sep = ""))) %>% 
   select(id, address_to_geocode) %>% 
   distinct()

arrests <- left_join(arrests, addresses_to_geocode)

#write out addresses that need to be geocoded
write_csv(addresses_to_geocode, outputfiles$addresses_to_geocode)
rm(addresses_marked, addresses_to_geocode)

#bring back geocoded addresses
addresses_geocoded <- read_csv(inputfiles$addresses_geocoded)

addresses_geocoded <- addresses_geocoded %>% 
   filter(to_check == "NO") %>% 
   select(address_to_geocode, latitude, longitude)

#join geocoded addresses back to data
arrests <- left_join(arrests, addresses_geocoded)

#make final lat long
arrests <- arrests %>% 
   mutate(latitude = ifelse(is.na(latitude), lat, latitude),
          longitude = ifelse(is.na(longitude), lon, longitude),
          latitude = ifelse(latitude == 0, NA, latitude),
          longitude = ifelse(longitude == 0, NA, longitude))

#latitude and longitude out for geomatching with other agencies' data.
#filtering only arrests from 2019, so within LAHSA and LASAN timeframes.
coords <- arrests %>% 
   filter(arrest_date > "2019-01-01") %>% 
   filter(unhoused == "Y" & !is.na(latitude)) %>% 
   select(latitude, longitude) %>% 
   distinct() %>% 
   mutate(dataset = "LAPD")

write_csv(coords, outputfiles$coords_out)
rm(coords)

#offenses out to recode
#all offenses
#arrest groupings from downloaded data
offenses_to_recode <- arrests %>% 
   group_by(arrest_type_code, charge, charge_description, charge_group_description) %>% 
   summarise(count = n()) %>% 
   mutate(HRW_charge = NA)
write_csv(offenses_to_recode, outputfiles$offenses_to_recode)  

#read in hrw_coding
offenses_recoded <- read_csv(inputfiles$recoded_offenses) %>% 
   select(-count, -charge_group_code)

arrests <- left_join(arrests, offenses_recoded, by = c("arrest_type_code", "charge", "charge_description")) %>% 
   distinct()
rm(offenses_recoded)

#placing locations in census tract. 
#place id into a simple features object
#get dataset of census tracts in la county
#You need a census api key to use tidycensus here. See: https://walker-data.com/tidycensus/
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
arrest_sf <- arrests %>% 
   filter(lat > 0.0000) %>% 
   select(id, lat, lon, location) %>% 
   st_as_sf(coords = c("lon", "lat"),
            crs = 4326) %>%
   st_transform(6440)

arrests_joined <- st_join(
   arrest_sf,
   tracts_acs
)

temp_join <- arrests_joined %>% 
   select(id, GEOID)

arrests <- left_join(arrests, temp_join) %>% 
   distinct()

#mark if arrest has jail booking
arrests <- arrests %>% 
   mutate(booked_jail = ifelse(!is.na(booking_date), 1, 0))

#recode race
t <- arrests %>% 
   select(race) %>% 
   distinct() %>% 
   mutate(race_recoded = NA)

write_csv(t, outputfiles$race_to_recode)

race_recoded <- read_csv(inputfiles$race_recoded)

arrests <- left_join(arrests, race_recoded)

#write out processed arrests data
write_rds(arrests, outputfiles$processed_arrests)

#now process a second dataset:
#crimes against the unhoused
crimes_against <- read_xlsx(inputfiles$crime_unhoused_victim)
str(crimes_against)

#need to fill down on columns that don't have it filled, also create date and ID
crimes_against$YEAR <- ifelse(crimes_against$YEAR == 0, NA, crimes_against$YEAR)

crimes_against <- crimes_against %>% 
   fill(1:4, .direction = 'down') %>% 
   mutate(date = as_date(paste(MNTH, "/1/", YEAR, sep = ""), format = "%B/%d/%Y"),
          id = row_number())

#lat long = 1% don't have, so that's ok. Group by census tract
crimes_against_sf <- crimes_against %>% 
   filter(LAT > 0.0000) %>% 
   select(id, LAT, LON) %>% 
   st_as_sf(coords = c("LON", "LAT"),
            crs = 4326) %>%
   st_transform(6440)

crimes_against_joined <- st_join(
   crimes_against_sf,
   tracts_acs
)

temp_join <- crimes_against_joined %>% 
   select(id, GEOID)

crimes_against <- left_join(crimes_against, temp_join) %>% 
   distinct()
rm(temp_join, crimes_against_joined, crimes_against_sf)

#race
crimes_against <- crimes_against %>% 
   rename(race = DESCENT) %>% 
   left_join(race_recoded)

#change capitalized entries
crimes_against <- crimes_against %>% 
   mutate(offense = str_to_sentence(`CRIME CODE DESCRIPTION`),
          premise = str_to_sentence(PREMISE),
          weapon = str_to_sentence(WEAPON),
          case_status = str_to_sentence(`CASE STATUS DESCRIPTION`))
   
#write out
write_csv(crimes_against, outputfiles$crimes_against_unhoused_processed)
rm(crimes_against)

#Overall crime data downloaded from https://data.lacity.org/browse?q=crime&sortBy=relevance
crime1 <- read_csv(inputfiles$crime1)
crime2 <- read_csv(inputfiles$crime2)

crime <- bind_rows(crime1, crime2)
rm(crime1, crime2)

crime <- crime %>% 
   mutate(date = str_sub(`Date Rptd`, 1, 10)) %>% 
   mutate(date = as_date(date, format = "%m/%d/%Y"))

#filter just years that are same as the unhoused victim set
crime <- crime %>% 
   mutate(year = year(date)) %>% 
   filter(year > 2015 & year < 2022)

#race of victim
table(crime$`Vict Descent`)

crime <- crime %>% 
   mutate(race = case_when(
      `Vict Descent` == "B" ~ "Black",
      `Vict Descent` == "H" ~ "Latinx",
      `Vict Descent` == "W" ~ "White",
      TRUE ~ "Other"
   ))

crime <- crime %>% 
   mutate(offense = str_to_sentence(`Crm Cd Desc`), 
          status = str_to_sentence(`Status Desc`)) %>% 
   mutate(status = ifelse(status == "Invest cont", "Investigation continued",
                          ifelse(status == "Juv arrest", "Juvenile arrest",
                                 ifelse(status == "Juv other", "Juvenile other",
                                        status))))

#write out total crimes
write_csv(crime, outputfiles$crimes_total)

#process 3rd dataset:
#use_of_force
use_of_force <- read_xlsx((inputfiles$use_of_force)) %>% 
   clean_names()

use_of_force <- use_of_force %>% 
   rename(unhoused = uof_suspect_homeless_indicator, date = uof_incident_date_name) %>% 
   mutate(year = year(date), race = toupper(uof_suspect_ethnicity_desc),
          race = ifelse(race == "HISPANIC", "HISPANIC, LATIN AMER", race)) %>% 
   left_join(race_recoded)

use_of_force <- use_of_force %>% 
   mutate(id = paste(date, uof_address_of_occurence_street_number,
                     uof_address_of_occurence_street_name, 
                     uof_address_of_occurence_intersection_cross_st_1, uof_address_of_occurence_intersection_cross_st_2,
                     uof_suspect_gender_desc, race,
                     unhoused),
          force_id = paste(uof_employee_at_incd_rank_name, uof_case_type_desc, uof_non_cat_classification))

write_csv(use_of_force, outputfiles$use_of_force_out)

#meija 41.18 data
meija <- read_csv(inputfiles$meija_41_18) %>% 
   clean_names()

meija <- meija %>% 
   mutate(database = "Meija",
          arrest_date = str_sub(arrest_date, end = -5),
          arrest_date = as.Date(arrest_date, format = "%m/%d/%y")) 

write_csv(meija, outputfiles$meija)
