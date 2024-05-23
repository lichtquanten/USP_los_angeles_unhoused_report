#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policinghomelessness/descriptives/src/lapd_descriptives.r


library(pacman)
p_load(lubridate, readxl, readr, rcartocolor, extrafont, scales, tidycensus,
       CGPfunctions, tidyverse, qs, fuzzyjoin, janitor)
options(scipen=999)

here <- here::here

source("../docs/plot_themes.R")


########### input and output files ##############
# input files:
inputfiles <- list(
   lahsa_service = "import/input/LAHSA/Request 1 - Services and locations starting 2019-01-01.xlsx",
   services_recoded = "processing/frozen/lahsa_services_recoded.csv",
   roomkey_v2 = "import/input/LAHSA/PRK Clients Served - Entire Program History.xlsx",
   project_roomkey = "import/input/LAHSA/Request 15 - PRK Clients.xlsx",
   inside_safe = "import/input/LAHSA/20230721_response_to_May_PRA/29192 - HRW PRA on ISP.xlsx",
   inside_safe_recoded = "processing/frozen/inside_safe_services_recoded.csv"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   coords_out = "geo_matching/input/lahsa_coords.csv",
   lahsa_services_to_recode = "processing/output/lahsa/lahsa_services_to_recode.csv",
   lahsa_services_processed = "processing/output/lahsa/lahsa_services_processed.csv",
   roomkey_processed = "processing/output/lahsa/roomkey_processed.csv",
   inside_safe_services_recode = "processing/output/lahsa/inside_safe_services_to_recode.csv",
   inside_safe_services_processed = "processing/output/inside_safe_services_processed.csv",
   inside_safe_overview = "processing/output/inside_safe_overview.csv"

) %>% map(here)

#read LAHSA file
lahsa <- read_xlsx(inputfiles$lahsa_service, sheet = 2)

str(lahsa) 

#fix date
lahsa <- lahsa %>% 
   mutate(service_date = as.Date(service_date, format = "%m/%d/%Y"))

#add month variable 
lahsa <- lahsa %>% 
   mutate( month = floor_date(service_date, unit = "months"))

#test completeness of geodata by time - coordinate data gets good starting 7/20
lahsa <- lahsa %>% 
   mutate(coordinates = ifelse(Latitude == "NULL", 0, 1))

t <- lahsa %>% 
   group_by(month, coordinates) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(perc = count/sum(count))

lahsa <- lahsa %>% 
   rename(latitude = Latitude, longitude = Longitude) %>% 
   mutate(latitude = as.numeric(ifelse(latitude == "Null",
                                NA, latitude)),
          longitude = as.numeric(ifelse(longitude == "Null",
                                       NA, longitude)))

#write file with coordinates out
coords_out <- lahsa %>% 
   filter(!is.na(latitude)) %>% 
   select(latitude, longitude) %>% 
   distinct() %>% 
   mutate(dataset = "LAHSA")

write_csv(coords_out, outputfiles$coords_out)
rm(coords_out)

#table of services - write out for re-coding
t <- lahsa %>% 
   group_by(full_service_name) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(perc = count/sum(count))

write_csv(t, outputfiles$lahsa_services_to_recode)
rm(t)

#Bring back in new re-codes
coded_services <- read_csv(inputfiles$services_recoded)

lahsa <- left_join(lahsa, coded_services)

#ID for encounter (person-location-date)
lahsa <- lahsa %>% 
   mutate(lahsa_encounter_id = paste(Randomized_Enrollment_ID, service_date, latitude, longitude))

#yes/no housing referral
lahsa <- lahsa %>% 
   mutate(housing_referral = ifelse(service_grouped == "Housing referral", 1, 0))

#yes no encounter attained housing
t <- lahsa %>% 
   filter(housing_referral == 1 & Attained == "Attained")

lahsa <- lahsa %>% 
   mutate(housing_attained_encounter = ifelse(
      lahsa_encounter_id %in% t$lahsa_encounter_id,
      1, 
      0
   ))

#referral on that day
t <- lahsa %>% 
   filter(housing_referral == 1)

lahsa <- lahsa %>% 
   mutate(housing_referral_encounter = ifelse(
      lahsa_encounter_id %in% t$lahsa_encounter_id,
      1, 
      0
   ))

#ever received housing referral
referral <- lahsa %>% 
   filter(housing_referral == 1) 

lahsa <- lahsa %>% 
   mutate(ever_receive_referral = ifelse(
      Randomized_Enrollment_ID %in% referral$Randomized_Enrollment_ID,
      "Has received a housing referral at least once", 
      "Has never received a housing referral"
   ))

#ever attained housing
attained <- lahsa %>% 
   filter(housing_referral == 1 & Attained == "Attained")

lahsa <- lahsa %>% 
   mutate(ever_attain_housing = ifelse(
      Randomized_Enrollment_ID %in% attained$Randomized_Enrollment_ID,
      "Has attained housing at least once", 
      "Has never attained housing"
   ))

#per person, what is the latest date they attained housing
t <- lahsa %>% 
   filter(Attained == "Attained" & service_grouped == "Housing referral") %>% 
   group_by(Randomized_Enrollment_ID) %>% 
   slice_max(service_date, with_ties = F) %>% 
   mutate(latest_housing_attain_date = service_date) %>% 
   select(Randomized_Enrollment_ID, latest_housing_attain_date)

lahsa <- left_join(lahsa, t)

#write out services
write_csv(lahsa, outputfiles$lahsa_services_processed)
rm(lahsa, referral, coded_services)

#Process Project Roomkey
roomkey <- read_xlsx(inputfiles$roomkey_v2) %>% 
   clean_names()
str(roomkey)

#combined race
roomkey <- roomkey %>% 
   mutate(race_detailed =
             ifelse(clients_ethnicity == "Hispanic/Latin(a)(o)(x)", "Hispanic/Latin(a)(o)(x)",
                    ifelse(clients_race == "Black, African American, or African", "Black",
                           clients_race)),
          race = case_when(
             race_detailed == "White" ~ "White",
             race_detailed == "Black" ~ "Black",
             race_detailed == "Hispanic/Latin(a)(o)(x)" ~ "Hispanic/Latin(a)(o)(x)",
             TRUE ~ "All other"
          ))

#fix date variables
roomkey <- roomkey %>% 
   mutate(enrollments_project_start_date = 
             as.Date(enrollments_project_start_date),
          enrollments_project_exit_date = 
             as.Date(enrollments_project_exit_date))

#days in program
roomkey <- roomkey %>% 
   mutate(days_in = as.numeric(difftime(enrollments_project_exit_date,
                                        enrollments_project_start_date, units = "days")))

#days in grouping
roomkey <- roomkey %>% 
   mutate(less_48 = ifelse(days_in < 3, 1, 0),
          less_week = ifelse(days_in < 8, 1, 0),
          grouping1 = case_when(
             days_in <= 31 ~ "< 1 month",
             days_in < 181 & days_in > 31 ~ "1 month - 6 months",
             days_in < 366 & days_in > 180 ~ "6 months - 1 year",
             days_in >365 ~ "> 1 year"
          ))

#floor date
roomkey <- roomkey %>% 
   mutate(month_start = floor_date(enrollments_project_start_date, unit= "months"),
          month_end = floor_date(enrollments_project_exit_date, unit= "months"))

#fix hotel names
roomkey <- roomkey %>% 
   mutate(hotel = gsub("COVID - LA ", replacement = "", programs_name),
          hotel = gsub(" Tier 1", replacement = "", hotel))

#people still in at end of data
t <- roomkey %>% 
   group_by(enrollments_project_exit_date) %>% 
   summarise(count = n())

roomkey <- roomkey %>% 
   mutate(still_in = ifelse(enrollments_project_exit_date > "2023-06-07",
                            1, 0))
#write out
write_csv(roomkey, outputfiles$roomkey_processed)
rm(roomkey, t)


#Inside safe 
IH_enrollments <- read_xlsx(inputfiles$inside_safe, sheet = 1) %>% 
   clean_names() %>% 
   mutate(interim_housing_enrollment = "Y")
non_IH_enrollments <- read_xlsx(inputfiles$inside_safe, sheet = 2) %>% 
   clean_names() %>% 
   mutate(interim_housing_enrollment = "N")

ids <- bind_rows(IH_enrollments, non_IH_enrollments) 

ids <- ids %>% 
   mutate(project_start_date = as.Date(project_start_date, format = "%m/%d/%Y"),
          project_exit_date = as.Date(project_exit_date, format = "%m/%d/%Y"))
                                       
write_csv(ids, outputfiles$inside_safe_overview)

#inside safe services
services <- read_xlsx(inputfiles$inside_safe, sheet = 3) %>% 
   clean_names()

services <- services %>% 
   mutate(service_date = as.Date(service_date, format = "%m/%d/%Y"))

services_recoded <- read_csv(inputfiles$inside_safe_recoded)
services <- left_join(services, services_recoded)

write_csv(services, outputfiles$inside_safe_services_processed)
