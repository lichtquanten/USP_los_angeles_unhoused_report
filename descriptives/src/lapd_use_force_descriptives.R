#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policinghomelessness/descriptives/src/lapd_use_force_descriptives.r
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

stopifnot(requireNamespace("here", quietly = TRUE))
here <- here::here


########### input and output files ##############
# input files:
inputfiles <- list(
   use_force = "processing/output/use_of_force_processed.csv",
   pit_population = "processing/output/unhoused_count.csv",
   arrests = "processing/output/processed_arrests.rds"
   
) %>% map(here)

#output files. 
outputfiles <- list(
 use_of_force_rate = "descriptives/output/lapd/use_of_force.csv",
 race_force1 = "descriptives/output/lapd/race_force1.csv"
   
) %>% map(here)

#read in
use_force <- read_csv(inputfiles$use_force)

#percentage of use of force incidents (no matter how much force was used) that are unhoused
t <- use_force %>% 
   group_by(year, unhoused) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count))


#use of force per arrest total and per year
force_per <- t %>% 
   filter( year < 2021) %>% 
   mutate(Year = as.character(year))

total <- force_per %>% 
   ungroup() %>% 
   group_by(unhoused) %>% 
   summarise(count = sum(count)) %>% 
   mutate(Year = "Total")

force_per <- bind_rows(force_per, total) %>% 
   select(Year, unhoused, count)

#rates of use of force per arrests per year and total
arrests <- read_rds(inputfiles$arrests)

relevant_arrests <- arrests %>% 
   filter(!is.na(HRW_charge)) %>%
   mutate(Year = year(arrest_date)) %>% 
   filter(Year > 2015 & Year < 2021)

arrests_per <- relevant_arrests %>% 
   group_by(Year, unhoused, booked_jail) %>% 
   summarise(total_arrests = n_distinct(id))

total <- arrests_per %>% 
   ungroup() %>% 
   group_by(unhoused, booked_jail) %>% 
   summarise(total_arrests = sum(total_arrests)) %>% 
   mutate(Year = "Total")

arrests_per <- arrests_per %>% 
   mutate(year = Year, Year = as.character(year)) %>% 
   bind_rows(total)

force_per <- left_join(force_per, arrests_per)

force_per <- force_per %>% 
   mutate(rate = (count/total_arrests) * 1000)

force_per_out <- force_per %>% 
   filter(Year != "Total" & unhoused == "Y") %>% 
   select(Year, rate, booked_jail) %>% 
   pivot_wider(names_from = booked_jail, values_from = rate)
write_csv(force_per_out, outputfiles$use_of_force_rate)

rm(force_per, total, arrests_per)   

#use of force by race
race <- use_force %>% 
   filter(year < 2021) %>% 
   group_by(unhoused, race_recoded) %>% 
   summarise(count = n_distinct(id))

relevant_arrests <- relevant_arrests %>% 
   mutate(race_recoded = case_when(
      is.na(race_recoded) & descent_code == "B" ~ "Black",
      is.na(race_recoded) & descent_code == "W" ~ "White",
      is.na(race_recoded) & descent_code == "H" ~ "Latinx",
      TRUE ~ race_recoded
   )) 

arrests1 <- relevant_arrests %>% 
   group_by(unhoused, race_recoded) %>% 
   summarise(total_arrests = n_distinct(id))

arrests2 <- relevant_arrests %>% 
   filter(booked_jail == 1) %>% 
   group_by(unhoused, race_recoded) %>% 
   summarise(booked_arrests = n_distinct(id))

race <- left_join(race, arrests1)
race <- left_join(race, arrests2)

race <- race %>% 
   mutate(rate_per_1K_arrests_any = (count/total_arrests)* 1000,
          rate_per_1k_arrests_booked = (count/booked_arrests)* 1000)

race_out <- race %>% 
   filter(race_recoded == "Black" | race_recoded == "White" | race_recoded == "Latinx") %>% 
   select(unhoused, race_recoded, rate_per_1k_arrests_booked) %>% 
   pivot_wider(values_from = rate_per_1k_arrests_booked, names_from = unhoused)

write_csv(race_out, outputfiles$race_force1)

#rates per unhoused pop
pop <- read_csv(inputfiles$pit_population)

pop <- pop %>% 
   filter(year < 2021 & Geography == "Los Angeles") %>% 
   group_by(race) %>% 
   summarise(pop = sum(num))

race <- race %>% 
   mutate(race = race_recoded)  %>% 
   left_join( pop) %>% 
   filter(unhoused == "Y" ) %>% 
   mutate(rate_per_pop = (count/pop) * 1000)

#types of force
t <- use_force %>% 
   group_by(unhoused, uof_case_type_desc, uof_non_cat_classification) %>% 
   summarise(count = n_distinct(id)) %>% 
   group_by(unhoused) %>% 
   mutate(perc = count/sum(count))
