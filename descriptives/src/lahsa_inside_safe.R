#
# Authors:     BR
# Maintainers: BR
# Copyright:   2023
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policinghomelessness/descriptives/src/lahsa_inside_safe.r
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
   is_overview = "processing/output/inside_safe_overview.csv",
   is_services = "processing/output/inside_safe_services_processed.csv"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   is_encampment_table = "descriptives/output/lahsa/IS_encampment_table.csv",
   IS_enrollments_week = "descriptives/output/lahsa/IS_enrollments_week.csv",
   perc_encampment_perm = "descriptives/output/lahsa/IS_perc_encampment_perm.csv"

   
) %>% map(here)


#read in 
is_overview <- read_csv(inputfiles$is_overview)
is_services <- read_csv(inputfiles$is_services)

min(is_overview$project_start_date)
min(is_services$service_date)
max(is_overview$project_start_date)
max(is_services$service_date)

#date of services 
t <- is_services %>% 
   group_by(service_date) %>% 
   summarise(people = n_distinct(random_id))

#counts
n_distinct(is_overview$random_id)
n_distinct(is_services$random_id)

#who is in overview but not services?
#some ids in one or other or both

#ids
t1 <- is_overview %>% 
   select(random_id) %>% 
   distinct() %>% 
   mutate(in_overview = 1)

t2 <- is_services %>% 
   select(random_id) %>% 
   distinct() %>% 
   mutate(in_services = 1)

table <- full_join(t1, t2) %>% 
   mutate(database = case_when(
      in_services == 1 & in_overview == 1 ~"Both",
      in_services == 1 & is.na(in_overview) ~ "Services only", 
      TRUE ~"Overview only"))

table(table$database)
rm(t1, t2)

#total start enrollments
t <- is_overview %>% 
   group_by(random_id) %>% 
   slice_min(project_start_date) %>% 
   ungroup() %>% 
   mutate(floor_week = floor_date(project_start_date, unit = "weeks")) %>% 
   group_by(floor_week) %>% 
   summarise(count = n_distinct(random_id))
write_csv(t, outputfiles$IS_enrollments_week)

t <- t %>% 
   filter(floor_week < "2023-03-26") %>% 
   summarise(avg = mean(count))
   


#start date by site
t <- is_overview %>% 
   group_by(random_id) %>% 
   slice_min(project_start_date) %>% 
   ungroup() %>% 
   group_by(encampment_site, project_start_date) %>% 
   summarise(count = n_distinct(random_id)) 

t1 <- t %>% 
   group_by(encampment_site) %>% 
   slice_min(project_start_date) %>% 
   select(encampment_site, earliest_start_date = project_start_date)

t2 <- t %>% 
   group_by(encampment_site) %>% 
   slice_max(count, with_ties = F) %>% 
   select(encampment_site, highest_outreach_date = project_start_date, people_reached_on_max_date = count)

t3 <- is_overview %>% 
   group_by(encampment_site) %>% 
   summarise(total_people = n_distinct(random_id))

table <- left_join(t3, t1)
table <- left_join(table, t2) %>% 
   arrange(desc(total_people))
write_csv(table, outputfiles$is_encampment_table)

#count of days of outreach per site
days_outreach <- t %>% 
   group_by(encampment_site) %>% 
   summarise(num_days = n_distinct(project_start_date))

rm(t, t1, t2, t3)

#total enrollments

#What kind of programs
t <- is_overview %>% 
   group_by(random_id) %>% 
   summarise(progs = n_distinct(program_type_name_group))

#was everyone enrolled in interim housing?
t <- is_overview %>% 
   group_by(random_id) %>% 
   summarise(count = n_distinct(interim_housing_enrollment))

t1 <- t %>% 
   filter(count == 1)

no_interim_group <- semi_join(is_overview, t1) %>% 
   filter(interim_housing_enrollment == "N")

#mark them in services
is_services <- is_services %>% 
   mutate(no_interim = ifelse(random_id %in% no_interim_group$random_id, 1, 0))

#perm housing attained in the services
ids <- is_services %>% 
   filter(perm_housing == "Referred and attained permanent housing")

t1 <- semi_join(is_services, ids, by = "random_id")
t2 <- semi_join(is_overview, t1)

test <- is_overview %>% 
   filter(destination_group == "Permanent Situation")

test2 <- semi_join(is_overview, test, by = "random_id")
n_distinct(test2$random_id)

#look at permanent
perm_overview <- is_overview %>% 
   filter(destination_group == "Permanent Situation")

perm2 <- semi_join(is_overview, perm_overview, by = "random_id")

n_distinct(perm2$random_id)  #82 went to permanent situation 
last_row_perm <- perm2 %>% 
   group_by(random_id) %>% 
   slice_max(project_exit_date)

temp <- last_row_perm %>% 
   filter(destination_group != "Permanent Situation")

perm_last <- perm2 %>% 
   filter(destination_group == "Permanent Situation") %>% 
   group_by(random_id) %>% 
   slice_max(project_exit_date) %>% 
   select(random_id, date_of_perm_exit = project_exit_date) %>% 
   distinct() %>% 
   mutate(permanent_exit = 1,
          permanent_last_date = 1)

#add to overview and services
is_overview <- left_join(is_overview, perm_last)
is_services <- left_join(is_services, perm_last)

#in services, last date of a bed or hotel voucher and ever had a bed or voucher
t <- is_services %>% 
   filter(bed_yn == 1) %>% 
   group_by(random_id) %>% 
   slice_max(service_date, n = 1) %>% 
   select(last_bed_hotel_date = service_date, random_id) %>% 
   distinct() %>% 
   mutate(ever_had_bed = 1)

is_services <- left_join(is_services, t)

#last date in services for referral permament housing
t <- is_services %>% 
   filter(housing_type == "Permanent housing") %>% 
   group_by(random_id) %>% 
   slice_max(service_date) %>% 
   select(random_id, last_date_perm_housing_referral = service_date) %>% 
   distinct()  %>% 
   mutate(referred_to_perm_housing = 1)
   
is_services <- left_join(is_services, t)

#mark people with bed dates later than permanent housing referral dates
is_services <- is_services %>% 
   mutate(interim_later = ifelse(last_bed_hotel_date > last_date_perm_housing_referral, 1, 0))

#attained referral to permanent
t <- is_services %>% 
   filter(perm_housing == "Referred and attained permanent housing") %>% 
   select(random_id) %>% 
   distinct() %>% 
   mutate(attained_perm = 1)

is_services <- left_join(is_services, t)

#total who exited or attained perm housing
is_services <- is_services %>% 
   mutate(exit_attain_perm = ifelse(permanent_exit == 1 | attained_perm == 1, 1, 0))



#so how many left to permanent housing and had been referred?
t <- is_services %>% 
   group_by(permanent_exit, referred_to_perm_housing) %>% 
   summarise(count = n_distinct(random_id)) %>% 
   ungroup() %>% 
   mutate(perc = count/sum(count))

t <- is_services %>% 
   group_by(exit_attain_perm, referred_to_perm_housing) %>% 
   summarise(count = n_distinct(random_id)) %>% 
   ungroup() %>% 
   mutate(perc = count/sum(count))

#people who attained/exited - mark in overview
t <- is_services %>% 
   filter(attained_perm == 1) %>% 
   select(random_id, attained_perm) %>% 
   distinct()

is_overview <- left_join(is_overview, t)

t <- is_services %>% 
   filter(exit_attain_perm == 1) %>% 
   select(random_id, exit_attain_perm) %>% 
   distinct()

is_overview <- left_join(is_overview, t)

#for those who haven't left but were referred, how many attained
t <- is_services %>% 
   filter(is.na(permanent_exit) & referred_to_perm_housing == 1) %>% 
   group_by(attained_perm) %>% 
   summarise(count = n_distinct(random_id)) %>% 
   mutate(perc = count/sum(count))


#of those who attained, is last date of attainment after last date of interim bed?
t <- is_services %>% 
   filter(attained_perm == 1) %>% 
   group_by(interim_later) %>% 
   summarise(count = n_distinct(random_id)) %>% 
   mutate(perc = count/sum(count))

#how many exited permanently?
total <- is_services %>% 
   group_by(exit_attain_perm) %>% 
   summarise(count = n_distinct(random_id)) %>% 
   mutate(perc = count/sum(count)) 


#for permanent exits, what type of exits
t <- is_overview %>% 
   filter(permanent_exit == 1 & destination_group == "Permanent Situation") %>% 
   group_by(random_id) %>% 
   slice_max(project_exit_date) %>% 
   group_by(destination) %>% 
   summarise(count = n_distinct(random_id)) 

104 - sum(t$count)

unknown_data <-data.frame(destination = "Unknown/No data", count = 21)

t <- bind_rows(t, unknown_data) %>% 
   mutate(perc = count/sum(count)) 
rm(unknown_data)

#people without referral but got subsidy>?
t <- is_services %>% 
   select(random_id, referred_to_perm_housing) %>% 
   distinct()
is_overview <- left_join(is_overview, t)

t <- is_overview %>% 
   filter(permanent_exit == 1 & destination_group == "Permanent Situation") %>% 
   group_by(random_id) %>% 
   slice_max(project_exit_date) %>% 
   group_by(destination, referred_to_perm_housing) %>% 
   summarise(count = n_distinct(random_id)) %>% 
   mutate(perc = count/sum(count))

#% by encampment
t <- is_overview %>% 
   group_by(encampment_site, exit_attain_perm) %>% 
   summarise(count = n_distinct(random_id)) %>% 
   mutate(perc = count/sum(count)) %>% 
   filter(exit_attain_perm == 1) %>% 
   ungroup() %>% 
   arrange(desc(perc)) %>% 
   slice(1:10)
write_csv(t, outputfiles$perc_encampment_perm)

t1 <- is_overview %>% 
   group_by( exit_attain_perm, encampment_site) %>% 
   summarise(count = n_distinct(random_id)) %>% 
   mutate(perc = count/sum(count))

t1 <- is_overview %>% 
   filter(exit_attain_perm == 1 & destination_group == "Permanent Situation") %>% 
   select(random_id) %>% 
   distinct()
   group_by(random_id) %>% 
   slice_max(project_exit_date) %>% 
   group_by(destination) %>% 
   summarise(count = n_distinct(random_id)) %>% 
   mutate(perc = count/sum(count))

& destination_group == "Permanent Situation"
   
   
#for interim housing, what kinds of beds?
t <- is_services %>% 
   group_by(random_id) %>% 
   summarise(bed_types = n_distinct(bed))

#people who have definitely left the program what is their exit
t <- is_overview







beds <- is_services %>% 
   filter(!is.na(bed)) %>% 
   group_by(bed) %>% 
   summarise(count = n_distinct(random_id)) %>% 
   mutate(perc= count/sum(count))

test <- is_services %>% 
   group_by(ever_had_bed) %>% 
   summarise(count = n_distinct(random_id)) %>% 
   mutate(perc = count/sum(count))

t <- is_services %>% 
   filter(is.na(ever_had_bed))

test <- t %>% 
   filter(!is.na(housing_attained))

#did the person ever receive or attain permament housing referral

#how many in services?
t <- is_services %>% 
   group_by(housing_referral_yn, housing_type, housing_attained) %>% 
   summarise(count = n_distinct(random_id))

#find perm2 in services
test <- semi_join(is_services, perm2, by = "random_id")

#mark people who were referred and did/did not attain permanent housing
t <- is_services %>% 
   group_by(perm_housing) %>% 
   summarise(count = n_distinct(random_id))


