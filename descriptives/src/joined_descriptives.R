#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policinghomelessness/descriptives/src/joined_descriptives.r


library(CGPfunctions)
library(extrafont)
library(fuzzyjoin)
library(lubridate)
library(qs)
library(rcartocolor)
library(readr)
library(readxl)
library(scales)
library(tidycensus)
library(tidyverse)
options(scipen=999)

here <- here::here

########### input and output files ##############
# input files:
inputfiles <- list(
   lasan = "geo_matching/output/lasan_processed_geomatched.rds",
   lapd = "geo_matching/output/lapd_processed_geomatched.rds",
   lahsa = "geo_matching/output/lahsa_processed_geomatched.rds",
   coords_matched = "geo_matching/frozen/all_coords_out_250m_matrix 2.csv",
   coords_groups = "geo_matching/frozen/grouped_coords.csv",
   roomkey = "processing/output/lahsa/roomkey_processed.csv"   
   
) %>% map(here)

#output files. 
outputfiles <- list(
   rose_hampton_lasan = "descriptives/output/rose_hampton_lasan.csv",
   rose_hampton_map = "descriptives/output/rosehampton_map.csv",
   naomi_aug_service = "descriptives/output/naomi_aug_service.csv",
   aug_18_people = "descriptives/output/aug_18_people.csv"

   ) %>% map(here)


#read files
lasan <- read_rds(inputfiles$lasan)
lapd <- read_rds(inputfiles$lapd)
lahsa <- read_rds(inputfiles$lahsa)
coords_matched <- read_csv(inputfiles$coords_matched)
coords_grouped <- read_csv(inputfiles$coords_groups) %>% 
   rename(OHS_ABH = OHS_ABS) %>% 
   filter(unique_coordinate_id != 33170 | 
             (unique_coordinate_id == 33170 & OHS_ABH == "CIVIC CENTER"))

#what were lahsa, lasan doing 2 months leading to echo park?
echo_lahsa <- lahsa %>% 
   filter(service_date >= "2020-12-18" &
             service_date <= "2021-02-18")

#individuals and total encounters
t <- echo_lahsa %>% 
   group_by(in_LA) %>% 
   summarise(total_indiv = n_distinct(Randomized_Enrollment_ID),
             total_encounters = n_distinct(lahsa_encounter_id)) %>% 
   filter(in_LA == "yes")

total_encounters <- t$total_encounters

#how many encounters had a housing referral?
t <- echo_lahsa %>% 
   filter(in_LA == "yes") %>% 
   group_by(service_grouped) %>% 
   summarise(count = n_distinct(lahsa_encounter_id)) %>% 
   mutate(perc = count/total_encounters)

#proportion of encounters with housing attained
t <- echo_lahsa %>% 
   filter(in_LA == "yes") %>% 
   group_by(housing_type, Attained) %>% 
   summarise(count = n_distinct(lahsa_encounter_id)) %>% 
   mutate(perc = count/total_encounters) %>% 
   arrange(Attained, desc(count)) %>% 
   filter(!is.na(housing_type) & Attained == "Attained") %>% 
   ungroup() %>% 
   mutate(cumperc = cumsum(perc))

#where?
t <- echo_lahsa %>% 
   filter(in_LA == "yes") %>% 
   group_by(in_DT) %>% 
   summarise(count = n_distinct(lahsa_encounter_id)) %>% 
   mutate(perc = count/sum(count))

t <- echo_lahsa %>% 
   group_by(OHS_ABH) %>% 
   summarise(count = n_distinct(lahsa_encounter_id)) %>% 
   mutate(perc = count/sum(count))

#echo park 2 mo LASAN work
echo_lasan <- lasan %>% 
   filter(date >= "2020-12-18" &
             date <= "2021-02-18" & total > 100) %>% 
   filter(is.na(in_LA) | in_LA == "yes")

t <- echo_lasan %>% 
   summarise(count = n_distinct(case_id),
             total_solid = sum(solid_waste_lbs, na.rm = T),
             total_bags = sum(bags_items_sent_to_storage, na.rm = T)) 

total_sweeps <- n_distinct(echo_lasan$case_id)

#subtract those with  no latitude?
total_sweeps <- total_sweeps - sum(is.na(echo_lasan$latitude))

#get all locations within 250m of echo park and filter on echo park sweep dates.
#in LAPD how many arrests during that time period?
#in LAHSA how many 
#in LASAN, what was taken
echo_coords <- coords_grouped %>% 
   filter(IS_Name == "Echo Park Lake") %>% 
   left_join(coords_matched)

echo_lahsa <- semi_join(lahsa, echo_coords, by = c("unique_coordinate_id" = 
                                                     "unique_coordinate_id_2")) %>% 
   filter(service_date > "2021-02-21" & service_date < "2021-03-26")

t <- echo_lahsa %>% 
   summarise(indiv = n_distinct(Randomized_Enrollment_ID),
             encounters = n_distinct(lahsa_encounter_id))
tot_indiv <- t$indiv

#how many times each person contacted?
t <- echo_lahsa %>% 
   group_by(Randomized_Enrollment_ID) %>% 
   summarise(num_contacts = n_distinct(lahsa_encounter_id)) %>% 
   group_by(num_contacts) %>% 
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count))

#what kind of services?
t <- echo_lahsa %>% 
   group_by(service_grouped, housing_type, Attained) %>% 
   summarise(indiv = n_distinct(Randomized_Enrollment_ID),
             encounters = n_distinct(lahsa_encounter_id)) 

#how many received a housing referral?
t <- echo_lahsa %>% 
   group_by(housing_referral) %>% 
   summarise(indiv = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(perc = indiv/sum(indiv))
rm(echo_lahsa)

#what did LASAN do in run-up to the Echo Park sweep?
echo_lasan <- semi_join(lasan, echo_coords, by = c("unique_coordinate_id" = 
                                                      "unique_coordinate_id_2")) %>% 
   filter(date > "2021-02-21" & date < "2021-03-26")

#remove the coronado st cleaning - not relevant
echo_lasan <- echo_lasan %>% 
   filter(case_id != 99900)

t <- echo_lasan %>% 
   summarise(count = n_distinct(case_id),
          total_solid = sum(solid_waste_lbs, na.rm = T),
          total_bags = sum(bags_items_sent_to_storage, na.rm = T)) 
rm(echo_lasan)

#now lapd?
echo_lapd <- semi_join(lapd, echo_coords, by = c("unique_coordinate_id" = 
                                                     "unique_coordinate_id_2")) %>% 
   filter(arrest_date > "2021-02-21" & arrest_date < "2021-03-26") %>% 
   filter(unhoused == "Y")

t <- echo_lapd %>% 
   group_by(arrest_date, arrest_type) %>% 
   summarise(count = n_distinct(id))

t <- echo_lapd %>% 
   filter(arrest_date == "2021-03-25") %>% 
   group_by(arrest_type, charge, charge_description) %>% 
   summarise(count = n_distinct(id))

t <- echo_lapd %>% 
   filter(arrest_date == "2021-03-25") %>% 
   group_by(booked_jail) %>% 
   summarise(count = n_distinct(id))

t <- echo_lapd %>% 
   filter(charge != "409PC")

#what were lahsa, lasan doing 2 months leading to macarthur park?
macarthur_lahsa <- lahsa %>% 
   filter(service_date >= "2021-07-27" &
             service_date <= "2021-09-27")

macarthur_lahsa <- left_join(macarthur_lahsa, coords_grouped) 

#individuals and total encounters
t <- macarthur_lahsa %>% 
   group_by(in_LA) %>% 
   summarise(total_indiv = n_distinct(Randomized_Enrollment_ID),
             total_encounters = n_distinct(lahsa_encounter_id)) %>% 
   filter(in_LA == "yes")

total_encounters <- t$total_encounters

#how many encounters had a housing referral?
t <- macarthur_lahsa %>% 
   filter(in_LA == "yes") %>% 
   group_by(service_grouped) %>% 
   summarise(count = n_distinct(lahsa_encounter_id)) %>% 
   mutate(perc = count/total_encounters)

#proportion of encounters with housing attained
t <- macarthur_lahsa %>% 
   filter(in_LA == "yes") %>% 
   group_by(housing_type, Attained) %>% 
   summarise(count = n_distinct(lahsa_encounter_id)) %>% 
   mutate(perc = count/total_encounters) %>% 
   arrange(Attained, desc(count)) %>% 
   filter(!is.na(housing_type) & Attained == "Attained") %>% 
   ungroup() %>% 
   mutate(cumperc = cumsum(perc))

#where?
t <- macarthur_lahsa %>% 
   filter(in_LA == "yes") %>% 
   group_by(in_DT) %>% 
   summarise(count = n_distinct(lahsa_encounter_id)) %>% 
   mutate(perc = count/sum(count))

t <- macarthur_lahsa %>% 
   group_by(OHS_ABH) %>% 
   summarise(count = n_distinct(lahsa_encounter_id)) %>% 
   mutate(perc = count/sum(count))

#macarthur park 2 mo LASAN work
macarthur_lasan <- lasan %>% 
   filter(date >= "2021-07-27" &
             date <= "2021-09-27" & total > 100)

macarthur_lasan <- left_join(macarthur_lasan, coords_grouped) %>% 
   filter(is.na(in_LA) | in_LA == "yes")

t <- macarthur_lasan %>% 
   summarise(count = n_distinct(case_id),
             total_solid = sum(solid_waste_lbs, na.rm = T),
             total_bags = sum(bags_items_sent_to_storage, na.rm = T)) 

total_sweeps <- n_distinct(macarthur_lasan$case_id)

#subtract those with  no latitude?
total_sweeps <- total_sweeps - sum(is.na(macarthur_lasan$latitude))

lasan_matches <- semi_join(coords_matched, macarthur_lasan)

macarthur_lasan <- macarthur_lasan %>% 
   mutate(unique_coordinate_id_2 = unique_coordinate_id)

lasan_matches2 <- semi_join(lasan_matches, macarthur_lasan,
                            by = "unique_coordinate_id_2")

t <- lasan_matches2 %>% 
   group_by(unique_coordinate_id) %>% 
   summarise(count = n_distinct(unique_coordinate_id_2))

perc <- t %>% 
   group_by(count) %>% 
   summarise(num = n_distinct(unique_coordinate_id)) %>% 
   mutate(perc = num/sum(num)) %>% 
   ungroup() %>% 
   mutate(total_ids = count*num)
sum(perc$total_ids)

#NAOMI ST

#naomi ave lasan
naomi <- lasan %>% 
   filter(case_id == "107076")

naomi_coords <- semi_join(coords_matched, naomi, by = "unique_coordinate_id")
rm(naomi)

#lasan trips there before or after
naomi_lasan <- semi_join(lasan, naomi_coords, by = c("unique_coordinate_id" = 
                                                        "unique_coordinate_id_2")) %>% 
   mutate(time_period = case_when(
      date < "2021-08-18" ~ "Before",
      date > "2021-08-18" ~ "After", 
      TRUE ~ "Day of"),
      time_period2 = case_when(
         date == "2021-08-18"  ~ "Day of",
         date < "2021-08-18" & date > "2021-07-18" ~ "Month before",
         date > "2021-08-18" & date < "2021-09-18" ~ "Month after",
         date < "2021-07-18" & date > "2020-08-18" ~ "Year before",
         date > "2021-09-18"  ~ "Year after",
      ))

t <- naomi_lasan %>% 
   group_by(time_period) %>% 
   summarise(count = n_distinct(case_id),
             total_solid = sum(solid_waste_lbs, na.rm = T),
             total_bags = sum(bags_items_sent_to_storage, na.rm = T)) 

t <- naomi_lasan %>% 
   filter(time_period == "Before" & month > "2020-08-01") %>% 
   group_by(month) %>% 
   summarise(count = n_distinct(case_id),
             total_solid = sum(solid_waste_lbs, na.rm = T),
             total_bags = sum(bags_items_sent_to_storage, na.rm = T)) 

t <- naomi_lasan %>% 
   group_by(time_period2) %>% 
   summarise(count = n_distinct(case_id),
             total_solid = sum(solid_waste_lbs, na.rm = T),
             total_bags = sum(bags_items_sent_to_storage, na.rm = T)) 

#lahsa activities at Naomi
naomi_lahsa <- semi_join(lahsa, naomi_coords, by = c("unique_coordinate_id" = 
                                                      "unique_coordinate_id_2"))  %>% 
   mutate(time_period = case_when(
      service_date == "2021-08-18"  ~ "Day of",
      service_date < "2021-08-18" ~ "Before",
      service_date > "2021-08-18" ~ "After" 
      ))

t <- naomi_lahsa %>% 
   filter(time_period == "Day of") %>% 
   group_by(service_grouped, housing_type, Attained) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))

bridge_recipient <- naomi_lahsa %>% 
   filter(service_date == "2021-08-18" & housing_type == "A Bridge Home" & Attained == "Attained")

recipient2 <- lahsa %>% 
   filter(Randomized_Enrollment_ID %in% bridge_recipient$Randomized_Enrollment_ID)
rm(bridge_recipient, recipient2)

day_of_ids <- naomi_lahsa %>% 
   filter(service_date == "2021-08-18")

day_of_hist <- semi_join(lahsa, day_of_ids, by = "Randomized_Enrollment_ID")

t <- day_of_hist %>% 
   group_by(Randomized_Enrollment_ID, service_date) %>% 
   summarise(days = n_distinct(service_date))

#previous and future lahsa visits
naomi_lahsa <- naomi_lahsa %>% 
   mutate(time_period2 = case_when(
      service_date == "2021-08-18"  ~ "Day of",
      service_date < "2021-08-18" & service_date > "2021-08-03" ~ "Two weeks before",
      service_date < "2021-08-18" & service_date > "2021-07-18" ~ "Month before",
      service_date > "2021-08-18" & service_date < "2021-09-18" ~ "Month after",
      service_date < "2021-07-18" & service_date > "2020-08-18" ~ "Year before",
      service_date > "2021-09-18"  ~ "Year after",
   )) 

t <- naomi_lahsa %>% 
   group_by(time_period2) %>% 
   summarise(dates = n_distinct(service_date),
             people = n_distinct(Randomized_Enrollment_ID),
             encounters = n_distinct(lahsa_encounter_id))

#total
t <- naomi_lahsa %>% 
   summarise(dates = n_distinct(service_date),
             people = n_distinct(Randomized_Enrollment_ID),
             encounters = n_distinct(lahsa_encounter_id))
   
t <- naomi_lahsa %>% 
   group_by(service_grouped) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))

t <- naomi_lahsa %>% 
   group_by(service_grouped, housing_type, Attained) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))
   
t <- naomi_lahsa %>% 
   filter(time_period2 == "Two weeks before") %>% 
   group_by(service_grouped, housing_type, Attained) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))

t <- naomi_lahsa %>% 
   filter(time_period2 == "Month before") %>% 
   group_by(service_grouped, housing_type, Attained) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))

t <- naomi_lahsa %>% 
   filter(time_period2 == "Year before") %>% 
   group_by(service_grouped, housing_type, Attained) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))

t <- naomi_lahsa %>% 
   filter(time_period2 == "Month after") %>% 
   group_by(service_grouped, housing_type, Attained) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))

t <- naomi_lahsa %>% 
   filter(time_period2 == "Year after") %>% 
   group_by(service_grouped, housing_type, Attained) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))

#lapd arrests (going back to beginning of 2020)
naomi_LAPD <- semi_join(lapd, naomi_coords, by = c("unique_coordinate_id" = 
                                                        "unique_coordinate_id_2"))  %>% 
   filter(arrest_date > "2019-12-31") %>% 
   mutate(time_period = case_when(
      arrest_date < "2021-08-18" ~ "Before",
      arrest_date > "2021-08-18" ~ "After", 
      TRUE ~ "Day of"))

t <- naomi_LAPD %>% 
   filter(unhoused == "Y") %>% 
   group_by(arrest_date, arrest_type, HRW_charge) %>% 
   summarise(count = n_distinct(report_id))
rm(naomi_LAPD)

#Naomi street LAHSA activities
join <- naomi_lahsa %>% 
   filter(service_date > "2021-08-04" & service_date < "2021-08-19") 

t1 <- join %>% 
   group_by(service_date, Exit_Destination) %>% 
   summarise(People_exited_to = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(Exit_Destination = paste("Exited to ", Exit_Destination)) %>% 
   pivot_wider(names_from = Exit_Destination, values_from = People_exited_to)

services <- join %>% 
   group_by(service_date, full_service_name) %>% 
   summarise(num_received = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(full_service_name = paste("Service: ", full_service_name)) %>% 
   pivot_wider(names_from = full_service_name, values_from = num_received)

temp <- join %>% 
   group_by(service_date) %>% 
   summarise(total_people_encountered = n_distinct(Randomized_Enrollment_ID))

table <- left_join(temp, services)
table <- left_join(table, t1)

write_csv(table, outputfiles$naomi_aug_service)

#people on the 18th
people_filter <- join %>% 
   filter(service_date < "2021-08-18" & service_date > "2021-08-17") 

Aug_18_people <- lahsa %>% 
   filter(Randomized_Enrollment_ID %in% people_filter$Randomized_Enrollment_ID) %>% 
   arrange(Randomized_Enrollment_ID, service_date)
write_csv(Aug_18_people, outputfiles$aug_18_people)

#number of unique days they encountered each person
days_encountered <- join %>% 
   group_by(Randomized_Enrollment_ID) %>% 
   summarise(number_of_times_encountered = n_distinct(service_date))
#4 people (19%) received outreach twice during this time period. the rest just once.

rm(naomi_coords, naomi_lahsa, naomi_lasan)

#which match? to this case?  HAMPTON
hampton_coords <- lasan %>% 
   filter( date == "2021-09-16" & case_id == 108459)

matching_cases <- semi_join(coords_matched, hampton_coords )

#lasan - how many times have they been there?
lasan_hampton <- semi_join(lasan, matching_cases,
                           by = c("unique_coordinate_id" =
                                     "unique_coordinate_id_2"))
#lahsa at hampton
#day of
lahsa_hampton <- semi_join(lahsa, matching_cases,
                           by = c("unique_coordinate_id" =
                                     "unique_coordinate_id_2"))
day_of_lahsa <- lahsa_hampton %>% 
   filter(service_date == "2021-09-16")

#id of people from that day
indiv <- semi_join(lahsa, day_of_lahsa, 
                   by = "Randomized_Enrollment_ID") 
t <- indiv %>% 
   filter(Randomized_Enrollment_ID == 286796353) %>% 
   group_by(service_date, unique_coordinate_id) %>% 
   summarise(summarise = n())

#all of this person's locations were in the same hampton areas?
test <- anti_join(t, matching_cases, 
                  by = c("unique_coordinate_id" = 
                            "unique_coordinate_id_2"))

t <- indiv %>% 
   filter(Randomized_Enrollment_ID == 286796353) %>% 
   group_by(service_date, service_grouped) %>% 
   summarise(summarise = n())

#how many times LASAN been to the area?
t <- lasan_hampton %>% 
   group_by(date) %>%
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T),
             lbs_taken = sum(solid_waste_lbs, na.rm = T),
             bags = sum(bags_items_sent_to_storage, na.rm = T)) %>% 
   mutate(pounds_per_sweep = lbs_taken/number_of_sweeps,
          bags_per_sweep = bags/number_of_sweeps,
          sweeps_per_bag = number_of_sweeps/bags,
          pounds_per_bag = lbs_taken/bags)
write_csv(t, outputfiles$rose_hampton_lasan)

t <-  lasan_hampton %>% 
   group_by(nature_of_call) %>%
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T)) %>% 
   mutate(number_of_sweeps/sum(number_of_sweeps))

sum(t$lbs_taken)
sum(t$bags)

per_year <- lasan_hampton %>% 
   group_by(year(date)) %>%
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T))
rm(t, per_year)

#lahsa total time at area
t <- lahsa_hampton %>% 
   group_by(service_date) %>% 
   summarise(number_of_people = n_distinct(Randomized_Enrollment_ID))

   
t <- lahsa_hampton %>% 
   group_by(Randomized_Enrollment_ID) %>% 
   summarise(number_of_days = n_distinct(service_date))

#find these folks throughout LAHSA
lahsa_hampton2 <- semi_join(lahsa, lahsa_hampton,
                            by = "Randomized_Enrollment_ID") %>% 
   mutate(at_hampton = ifelse(lahsa_encounter_id %in%
                                 lahsa_hampton$lahsa_encounter_id,
                              1, 0))
t <- lahsa_hampton2 %>% 
   group_by(at_hampton, service_date) %>% 
   summarise(number_of_people = n_distinct(Randomized_Enrollment_ID))

t <- lahsa_hampton2 %>% 
   group_by(Randomized_Enrollment_ID) %>% 
   summarise(number_of_days = n_distinct(lahsa_encounter_id),
             number_of_locations = n_distinct(unique_coordinate_id)) %>% 
   mutate(cat = case_when(
      number_of_days == 1 & number_of_locations == 1 ~ "One encounter, only at Hampton/Rose",
      number_of_days != 1 & number_of_locations == 1 ~ "Multiple encounters, only at Hampton/Rose",
      number_of_days != 1 & number_of_locations != 1 ~ "Multiple encounters, at multiple locations",
      TRUE ~ "Other"
        ))

#only at hampton/once, only at hampton more than once, total times
table <- t %>% 
   group_by(cat) %>% 
   summarise(people = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(perc = people/sum(people))

t <- lahsa_hampton2 %>% 
   group_by(ever_receive_referral) %>% 
   summarise(people = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(perc = people/sum(people))

t <- lahsa_hampton2 %>% 
   group_by(ever_attain_housing) %>% 
   summarise(people = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(perc = people/sum(people))

t <- lahsa_hampton2 %>% 
   filter(in_LA == "yes") %>% 
   group_by(latitude, longitude) %>% 
   summarise(number_of_people = n_distinct(Randomized_Enrollment_ID)) %>% 
   filter(!is.na(latitude))
write_csv(t, outputfiles$rose_hampton_map)

#lapd
lapd_hampton <- semi_join(lapd, matching_cases,
                          by = c("unique_coordinate_id" =
                                    "unique_coordinate_id_2")) %>% 
   filter(unhoused == "Y")

t <- lapd_hampton %>% 
   filter(arrest_date < "2019-01-01") %>% 
   group_by( arrest_type, booked_jail) %>% 
   summarise(num = n_distinct(report_id))

t <- lapd_hampton %>% 
   filter(arrest_date < "2019-01-01") %>% 
   group_by( arrest_type, HRW_charge) %>% 
   summarise(num = n_distinct(report_id)) %>% 
   ungroup() %>% 
   mutate(perc = num/sum(num))

t <- lapd_hampton %>% 
   filter(arrest_date > "2019-01-01") %>% 
   group_by(year(arrest_date), arrest_type) %>% 
   summarise(num = n_distinct(report_id))

t <- lapd_hampton %>% 
   filter(arrest_date > "2019-01-01") %>% 
   group_by( arrest_type, HRW_charge) %>% 
   summarise(num = n_distinct(report_id)) %>% 
   ungroup() %>% 
   mutate(perc = num/sum(num))

t <- lapd_hampton %>% 
   filter(arrest_date > "2021-09-16") %>% 
   group_by( arrest_type) %>% 
   summarise(num = n_distinct(report_id)) 

t <- lapd_hampton %>% 
   filter(arrest_date > "2021-09-16") %>% 
   group_by(HRW_charge) %>% 
   summarise(num = n_distinct(report_id)) 

rm(hampton_coords, hampton_sweep, lahsa_hampton, lahsa_hampton2,
   lasan_hampton, lapd_hampton)

#main between 5th and 7th, "2021-10-12"
main_coords <- lasan %>% 
   filter(case_id == 109834 | case_id == 109780 | case_id == 109779)

matching_cases <- semi_join(coords_matched, main_coords )

#lasan - how many times have they been there?
lasan_main <- semi_join(lasan, matching_cases,
                        by = c("unique_coordinate_id" =
                                  "unique_coordinate_id_2"))
#lahsa at main
#day of
lahsa_main <- semi_join(lahsa, matching_cases,
                        by = c("unique_coordinate_id" =
                                  "unique_coordinate_id_2"))

#what did lasan do?
lasan_main <- lasan_main %>% 
   mutate(
   time_period = case_when(
      date == "2021-10-12"  ~ "Day of",
      date < "2021-10-12" & date > "2021-09-20" ~ "Three weeks before",
      date > "2021-10-12" & date < "2021-10-20" ~ "Week after"
   ),
   time_period2 = case_when(
      date == "2021-10-12"  ~ "Day of",
      date < "2021-10-12" & date > "2020-09-12" ~ "Year before",
      date > "2021-10-12" & date < "2022-10-12" ~ "Year after"
   ))

t <- lasan_main %>% 
   group_by(time_period) %>% 
   summarise(count = n_distinct(case_id),
             total_solid = sum(solid_waste_lbs, na.rm = T),
             total_bags = sum(bags_items_sent_to_storage, na.rm = T)) 

lahsa_main <-  lahsa_main %>% 
   mutate(
      time_period = case_when(
         service_date == "2021-10-12"  ~ "Day of",
         service_date < "2021-10-12" & service_date > "2021-09-20" ~ "Three weeks before",
         service_date > "2021-10-12" & service_date < "2021-10-20" ~ "Week after"
         ),
      time_period2 = case_when(
         service_date == "2021-10-12"  ~ "Day of",
         service_date < "2021-10-12" & service_date > "2020-09-12" ~ "Year before",
         service_date > "2021-10-12" & service_date < "2022-10-12" ~ "Year after"
      ))


t <- lahsa_main %>% 
   filter(time_period == "Day of") %>% 
   group_by(Randomized_Enrollment_ID, housing_referral,
            housing_attained_encounter) %>% 
   summarise(people = n_distinct(lahsa_encounter_id))

t <- lahsa_main %>% 
   group_by(time_period) %>% 
   summarise(dates = n_distinct(service_date),
             people = n_distinct(Randomized_Enrollment_ID),
             encounters = n_distinct(lahsa_encounter_id))

t <- lahsa_main %>% 
   filter(!is.na(time_period)) 

t1 <- t %>% 
   filter(housing_referral_encounter == 1) %>% 
   group_by(ever_attain_housing) %>% 
   summarise(people = n_distinct(Randomized_Enrollment_ID))



lapd_main <- semi_join(lapd, matching_cases,
                                  by = c("unique_coordinate_id" =
                                            "unique_coordinate_id_2")) %>%
   filter(unhoused == "Y") 
rm(lapd_main)

#31st and Main, oct 19th
main_coords <- lasan %>% 
   filter( date == "2021-10-19" & case_id == 131334)

matching_cases <- semi_join(coords_matched, main_coords )

#lasan - how many times have they been there?
lasan_main <- semi_join(lasan, matching_cases,
                           by = c("unique_coordinate_id" =
                                     "unique_coordinate_id_2"))
#lahsa at main
#day of
lahsa_main <- semi_join(lahsa, matching_cases,
                           by = c("unique_coordinate_id" =
                                     "unique_coordinate_id_2"))
day_of_lahsa <- lahsa_main %>% 
   filter(service_date == "2021-10-19")

#how many times LASAN been to the area?
t <- lasan_main %>% 
   group_by(date) %>%
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T),
             lbs_taken = sum(solid_waste_lbs, na.rm = T),
             bags = sum(bags_items_sent_to_storage, na.rm = T)) %>% 
   mutate(pounds_per_sweep = lbs_taken/number_of_sweeps,
          bags_per_sweep = bags/number_of_sweeps,
          sweeps_per_bag = number_of_sweeps/bags,
          pounds_per_bag = lbs_taken/bags)

t <- lahsa_main %>% 
   group_by(service_date) %>%
   summarise(people = n_distinct(Randomized_Enrollment_ID),
             encounters = n_distinct(lahsa_encounter_id))

#lahsa housing referrals before and after
lahsa_main <- lahsa_main %>% 
   mutate(time_period2 = case_when(
      service_date == "2021-10-19"  ~ "Day of",
      service_date < "2021-10-19" & service_date > "2021-09-18" ~ "Month before",
      service_date > "2021-10-19" & service_date < "2021-11-20" ~ "Month after"
   )) 

t <- lahsa_main %>% 
   group_by(time_period2) %>% 
   summarise(dates = n_distinct(service_date),
             people = n_distinct(Randomized_Enrollment_ID),
             encounters = n_distinct(lahsa_encounter_id))

t <- lahsa_main %>% 
   filter(time_period2 == "Month before") %>% 
   group_by(ever_receive_referral, ever_attain_housing) %>% 
   summarise(people = n_distinct(Randomized_Enrollment_ID))

lahsa_main <- lahsa_main %>% 
   mutate(time_period2 = case_when(
      service_date == "2021-10-19"  ~ "Day of",
      service_date < "2021-10-19" & service_date > "2020-10-19" ~ "Year before",
      service_date > "2021-10-19" & service_date < "2022-10-19" ~ "Year after"
   )) 

t <- lahsa_main %>% 
   group_by(time_period2) %>% 
   summarise(dates = n_distinct(service_date),
             people = n_distinct(Randomized_Enrollment_ID),
             encounters = n_distinct(lahsa_encounter_id))

t <- lahsa_main %>% 
   filter(time_period2 == "Year before" | time_period2 == "Year after") %>% 
   group_by(ever_receive_referral, ever_attain_housing) %>% 
   summarise(people = n_distinct(Randomized_Enrollment_ID))

# #lahsa total time at area
t <- lahsa_main %>% 
   group_by(service_date) %>% 
   summarise(number_of_people = n_distinct(Randomized_Enrollment_ID))

#ceres st "2021-11-04"
main_coords <- lasan %>% 
   filter( date == "2021-11-04" & (case_id == 111155 | case_id == 111113 ))

# 2022-03-01 Selma/Schrader
#which match? to this case?
selma_coords <- lasan %>% 
   filter( date == "2022-03-01" & case_id == 116917)

matching_cases <- semi_join(coords_matched, selma_coords )

#lasan - how many times have they been there?
lasan_selma <- semi_join(lasan, matching_cases,
                           by = c("unique_coordinate_id" =
                                     "unique_coordinate_id_2"))
#lahsa at selma
#day of
lahsa_selma <- semi_join(lahsa, matching_cases,
                           by = c("unique_coordinate_id" =
                                     "unique_coordinate_id_2"))

lahsa_selma <- lahsa_selma %>% 
   mutate(time_period2 = case_when(
      service_date == "2022-03-01"  ~ "Day of",
      service_date < "2022-03-01" & service_date > "2022-02-01" ~ "Month before",
      service_date > "2022-03-01" & service_date < "2022-04-01" ~ "Month after"
   )) 

t <- lahsa_selma %>% 
   group_by(time_period2) %>% 
   summarise(dates = n_distinct(service_date),
             people = n_distinct(Randomized_Enrollment_ID),
             encounters = n_distinct(lahsa_encounter_id))

t <- lahsa_selma %>% 
   filter(time_period2 == "Day of") %>% 
   group_by(ever_receive_referral, ever_attain_housing) %>% 
   summarise(people = n_distinct(Randomized_Enrollment_ID))

selma_two_months <- lahsa_selma %>% 
   filter(!is.na(time_period2)) 

t <- lahsa_selma %>% 
   filter(Randomized_Enrollment_ID %in% 
             selma_two_months$Randomized_Enrollment_ID) %>% 
   group_by(ever_receive_referral, ever_attain_housing) %>% 
   summarise(people = n_distinct(Randomized_Enrollment_ID))

t1 <- lahsa %>% 
   filter(Randomized_Enrollment_ID %in% 
             selma_two_months$Randomized_Enrollment_ID) %>% 
   filter(ever_attain_housing == "Has attained housing at least once") %>% 
   group_by(latest_housing_attain_date) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))

#how many times LASAN been to the area?
t <- lasan_selma %>% 
   group_by(date) %>%
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T),
             lbs_taken = sum(solid_waste_lbs, na.rm = T),
             bags = sum(bags_items_sent_to_storage, na.rm = T)) %>% 
   mutate(pounds_per_sweep = lbs_taken/number_of_sweeps,
          bags_per_sweep = bags/number_of_sweeps,
          sweeps_per_bag = number_of_sweeps/bags,
          pounds_per_bag = lbs_taken/bags)

rm(selma_coords, selma_two_months, lahsa_selma, lasan_selma)

#macarthur park specific to area (other macarthur in this script was
#about citywide activities during that timeperiod)
macarthur_coords1 <- lasan %>% 
   filter( date < "2021-10-25" & date > "2021-10-01")

macarthur_coords <- lasan %>% 
              filter(case_id == 109605 | case_id == 109906 | 
                        case_id == 109425 | case_id == 110020)

matching_cases <- semi_join(coords_matched, macarthur_coords)

macarthur_lasan <- semi_join(lasan, matching_cases,
                         by = c("unique_coordinate_id" =
                                   "unique_coordinate_id_2")) 

macarthur_lahsa <- semi_join(lahsa, matching_cases,
                         by = c("unique_coordinate_id" =
                                   "unique_coordinate_id_2"))

macarthur_lapd <- semi_join(lapd, matching_cases,
                             by = c("unique_coordinate_id" =
                                       "unique_coordinate_id_2"))

#people encountered in park in sept/oct
mac_park_indivs <- macarthur_lahsa %>% 
   filter(service_date < "2021-10-29" & service_date > "2021-08-30")

n_distinct(mac_park_indivs$Randomized_Enrollment_ID)

t <- mac_park_indivs %>% 
   group_by(Randomized_Enrollment_ID) %>% 
   summarise(times = n_distinct(lahsa_encounter_id)) %>% 
   group_by(times) %>% 
   summarise(total_inds = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(perc = total_inds/sum(total_inds))

t <- mac_park_indivs %>% 
   group_by(service_grouped, Attained) %>% 
   summarise(times = n_distinct(lahsa_encounter_id)) 
   
t <- mac_park_indivs %>% 
   group_by(service_grouped, Attained, housing_type) %>% 
   summarise(times = n_distinct(lahsa_encounter_id)) 

t <- mac_park_indivs %>% 
   mutate(attained_within_time = ifelse(
      service_grouped == "Housing referral" & Attained == "Attained",
      1, 0
   ))  %>% 
   filter(attained_within_time == 1)

mac_park_indivs <- mac_park_indivs %>% 
   mutate(attained_within_time = 
             ifelse(Randomized_Enrollment_ID %in% t$Randomized_Enrollment_ID,
                    1, 0))

t <- mac_park_indivs %>% 
   group_by(attained_within_time) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))  %>% 
   mutate(perc = count/sum(count))

remove <- mac_park_indivs %>% 
   filter(attained_within_time == 0)

t <- macarthur_lahsa %>% 
   filter(Randomized_Enrollment_ID 
          %in% remove$Randomized_Enrollment_ID) %>% 
   mutate(after_macpark = ifelse(service_date > "2021-10-31", 1, 0)) %>% 
   group_by(Randomized_Enrollment_ID, after_macpark) %>% 
   summarise(count = n_distinct(lahsa_encounter_id))

# macarthur lasan
t <- macarthur_lasan %>% 
   filter(date < "2021-10-29" & date > "2021-08-30") %>% 
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T),
             lbs_taken = sum(solid_waste_lbs, na.rm = T),
             bags = sum(bags_items_sent_to_storage, na.rm = T)) %>% 
   mutate(pounds_per_sweep = lbs_taken/number_of_sweeps,
          bags_per_sweep = bags/number_of_sweeps,
          sweeps_per_bag = number_of_sweeps/bags)

#macarthur lapd
macarthur_lapd <- macarthur_lapd %>% 
   filter(arrest_date < "2021-10-29" & 
             arrest_date > "2021-08-30" & unhoused == "Y")

t <- macarthur_lapd %>% 
   group_by(HRW_charge, booked_jail) %>% 
   summarise(count = n_distinct(report_id)) %>% 
   ungroup() %>% 
   mutate(perc = count/sum(count))

t <- macarthur_lapd %>% 
   group_by(race_recoded) %>% 
   summarise(count = n_distinct(report_id)) %>% 
   ungroup() %>% 
   mutate(perc = count/sum(count))

rm(macarthur_coords, macarthur_coords1, mac_park_indivs, macarthur_lahsa,
   macarthur_lapd, macarthur_lasan)



#venice
venice_coords <- lasan %>% 
   filter( date < "2021-07-30" & date > "2021-06-01") %>% 
   filter(str_detect(address_geo, "Ocean Front"))

matching_cases <- semi_join(coords_matched, venice_coords)

venice_lasan <- semi_join(lasan, matching_cases,
                             by = c("unique_coordinate_id" =
                                       "unique_coordinate_id_2")) 

venice_lasan <- venice_lasan %>% 
   mutate(
      time_period = case_when(
         date < "2021-07-31" & date > "2021-06-01" ~ "June/July 2021",
         date < "2021-06-01" & date > "2021-03-01" ~ "Three months before",
         date < "2021-11-01" & date > "2021-08-01"~ "Three months after"
      ))


t <- venice_lasan %>% 
   filter(solid_waste_lbs > 99) %>% 
   group_by(time_period) %>% 
   summarise(count = n_distinct(case_id),
             total_solid = sum(solid_waste_lbs, na.rm = T),
             total_bags = sum(bags_items_sent_to_storage, na.rm = T)) 

t <- venice_lasan %>% 
   filter(time_period == "June/July 2021") %>% 
   group_by(bags_taken) %>% 
   summarise(count = n_distinct(case_id)) %>% 
   mutate(perc = count/sum(count))

t <- venice_lasan %>% 
   filter(time_period == "June/July 2021") %>% 
   group_by(lahsa_present) %>% 
   summarise(count = n_distinct(case_id)) %>% 
   mutate(perc = count/sum(count))

t <- venice_lasan %>% 
   filter(time_period == "June/July 2021") %>% 
   filter(lahsa_present == 0)

t1 <- lahsa %>% 
   filter(service_date == "2021-07-30")

venice_lahsa <- semi_join(lahsa, matching_cases,
                             by = c("unique_coordinate_id" =
                                       "unique_coordinate_id_2"))

venice_lahsa<- venice_lahsa %>% 
   mutate(
   time_period = case_when(
      service_date < "2021-07-31" & service_date > "2021-06-01" ~ "June/July 2021",
      service_date < "2021-06-01" & service_date > "2021-03-01" ~ "Three months before",
      service_date < "2021-11-01" & service_date > "2021-08-01"~ "Three months after"
   ))

t <- venice_lahsa %>% 
   filter(time_period == "June/July 2021") %>% 
   group_by(Randomized_Enrollment_ID) %>% 
   summarise(encounters = n_distinct(lahsa_encounter_id))

t1 <- t %>% 
   group_by(encounters) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(perc = count/sum(count))

t <- venice_lahsa %>% 
   filter(time_period == "June/July 2021") %>% 
   group_by(Randomized_Enrollment_ID) %>% 
   summarise(ever_housing = n_distinct(housing_referral_encounter))

#48 definitely did. Remove them and then count encounters with group by
t <- t %>% 
   filter(ever_housing == 1)

t1 <- venice_lahsa %>% 
   filter(Randomized_Enrollment_ID %in% t$Randomized_Enrollment_ID) %>% 
   group_by(housing_referral_encounter) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))

t <- venice_lahsa %>% 
   filter(housing_referral_encounter == 1 & time_period == "June/July 2021") %>% 
   group_by(latest_housing_attain_date) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID))

### did lahsa come back in three months after
t <- venice_lahsa %>% 
   filter(time_period == "Three months after" | 
             time_period == "June/July 2021") %>% 
   group_by(Randomized_Enrollment_ID, time_period) %>% 
   summarise(count = n_distinct(lahsa_encounter_id))

times_per <- t %>% 
   group_by(Randomized_Enrollment_ID) %>% 
   summarise(times = n())

after_july <- t %>% 
   filter(time_period == "Three months after")

times_per <- times_per %>% 
   filter(Randomized_Enrollment_ID %in% after_july$Randomized_Enrollment_ID)

t <- times_per %>% 
   group_by(times) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(perc = count/sum(count))

t <- lahsa_main %>% 
   filter(time_period2 == "Month before") %>% 
   group_by(ever_receive_referral, ever_attain_housing) %>% 
   summarise(people = n_distinct(Randomized_Enrollment_ID))


#ABH zones
#presence of lahsa at lasan cleanings
t <- lasan %>% 
   filter(!is.na(lahsa_present)) %>% 
   mutate(abh_yn = ifelse(!is.na(OHS_ABH), 1, 0)) %>% 
   group_by(abh_yn, lahsa_present) %>% 
   summarise(count = n_distinct(case_id)) %>% 
   mutate(perc = count/sum(count))

lahsa_abh <- lahsa %>% 
   mutate(abh_yn = ifelse(!is.na(OHS_ABH), 1, 0))

t <- lahsa_abh %>% 
   group_by(abh_yn, housing_referral_encounter) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(perc = count/sum(count))

t <- lahsa_abh %>% 
   group_by(abh_yn) %>% 
   summarise(count = n_distinct(lahsa_encounter_id)) %>% 
   mutate(perc = count/sum(count))

t <- lahsa_abh %>% 
   group_by(abh_yn, ever_attain_housing) %>% 
   summarise(count = n_distinct(Randomized_Enrollment_ID)) %>% 
   mutate(perc = count/sum(count))

lapd_abh <- lapd %>% 
   filter(arrest_date > "2017-12-31") %>% 
   mutate(abh_yn = ifelse(!is.na(OHS_ABH), 1, 0)) %>% 
   filter(unhoused == "Y")

t <- lapd_abh %>% 
   group_by(abh_yn) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count))

t <- lapd_abh %>% 
   group_by(abh_yn, booked_jail) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count))

t1 <- lapd_abh %>% 
   group_by(abh_yn, HRW_charge) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count))

t <- lapd_abh %>% 
   group_by(abh_yn, perc_change_group) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count))



