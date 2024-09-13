#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policinghomelessness/descriptives/src/lapd_descriptives.r
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
   arrests = "processing/output/processed_arrests.rds",
   pit_population = "processing/output/unhoused_count.csv",
   city_census = "processing/output/city_census_data.rds",
   victim_rate = "descriptives/output/lapd/victim_rates_per_year.csv",
   meija = "processing/output/meija.csv"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   total_arrests_time = "descriptives/output/lapd/total_arrests_over_time.csv",
   all_naomi_ave = "descriptives/output/lapd/all_naomi_ave_LAHSA.csv",
   naomi_aug_service = "descriptives/output/lapd/naomi_Aug_services.csv",
   aug_18_people = "descriptives/output/lapd/aug_18_people.csv",
   overall_charges = "descriptives/output/lapd/overall_charges.csv",
   full_arrest_table = "descriptives/output/lapd/full_arrest_table.csv",
   proportion_charge_unhoused = "descriptives/output/lapd/proportion_charge_unhoused.csv",
   offenses_over_time = "descriptives/output/lapd/offenses_over_time.csv",
   change_by_charge = "descriptives/output/lapd/change_by_charge.csv",
   change_by_charge_rate = "descriptives/output/lapd/change_by_charge_rate.csv",
   jails = "descriptives/output/lapd/jails.csv",
   race_table_lapd = "descriptives/output/lapd/race_table_lapd.csv",
   unhoused_specific_charges = "descriptives/output/lapd/unhoused_specific_charges.csv", 
   arrest_victim_rate = "descriptives/output/lapd/arrest_victim_rate.csv",
   updated_4118_rates = "descriptives/output/lapd/41.18_updated_rates.csv",
   arrests_by_housing_status = "descriptives/output/lapd/arrests_by_housing_status.csv"
   
) %>% map(here)

#read in
arrests <- read_rds(inputfiles$arrests)

#unhoused
unhoused <- arrests %>% 
   filter(unhoused == "Y" &!is.na(HRW_charge))

#dates
min(arrests$arrest_date)
max(arrests$arrest_date)

#proportion of arrests
total_props <- arrests %>% 
   group_by(unhoused) %>% 
   summarise(count = n_distinct(id))  %>% 
   mutate(perc = count/sum(count),
          arrest_type = "Total") %>% 
   filter(unhoused == "Y")

#same including arrest type
total_props2 <- arrests %>% 
   group_by(arrest_type, unhoused) %>% 
   summarise(count = n_distinct(id))  %>% 
   mutate(perc = count/sum(count)) %>% 
   filter(unhoused == "Y")

total_props2 <- bind_rows(total_props2, total_props)
rm(total_props, total_props2)

#now same but by year
total_props <- arrests %>% 
   group_by(year(floor_arrest_date), unhoused) %>% 
   summarise(count = n_distinct(id))  %>% 
   mutate(perc = count/sum(count),
          arrest_type = "Total") 

#same including arrest type
total_props2 <- arrests %>% 
   group_by(year(floor_arrest_date), arrest_type, unhoused) %>% 
   summarise(count = n_distinct(id))  %>% 
   mutate(perc = count/sum(count)) 

total_props2 <- bind_rows(total_props2, total_props)
rm(total_props, total_props2)

#simple monthly totals for both unhoused and housed arrests
total_arrests <- arrests %>% 
   group_by(floor_date(floor_arrest_date, unit = "season"), unhoused) %>% 
   summarise(count = n_distinct(id))  %>% 
   pivot_wider(names_from = unhoused, values_from = count)

total_arrests <- arrests %>% 
   group_by(floor_date(floor_arrest_date, unit = "quarter"), unhoused) %>% 
   summarise(count = n_distinct(id))  %>% 
   pivot_wider(names_from = unhoused, values_from = count)

total_arrests <- arrests %>% 
   group_by(floor_arrest_date, unhoused) %>% 
   summarise(count = n_distinct(id))  %>% 
   pivot_wider(names_from = unhoused, values_from = count)
cor.test(total_arrests$Y, total_arrests$`NA`)

#how many infractions ended with a booking?
t <- arrests %>% 
   filter(arrest_type_code == "I") %>% 
   group_by(booking_location) %>% 
   summarise(count = n_distinct(id))  %>% 
   mutate(perc = count/sum(count))

#proportion of unhoused arrests by type
t <- unhoused %>% 
   group_by(arrest_type) %>% 
   summarise(count = n_distinct(id))  %>% 
   mutate(perc = count/sum(count))

#unhoused arrests over time
t <- unhoused %>% 
   group_by(floor_arrest_date) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(arrest_type = "Total")

t1 <- unhoused %>% 
   group_by(arrest_type, floor_arrest_date) %>% 
   summarise(count = n_distinct(id)) %>% 
   bind_rows(t) %>% 
   filter(arrest_type != "Other") %>% 
   pivot_wider(names_from = arrest_type, values_from = count)
write_csv(t1, outputfiles$total_arrests_time)

#percentage change by year
t1 <- unhoused %>% 
   group_by(year = year(floor_arrest_date)) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(arrest_type = "Total",
          percent_change_2016 = ((count - count[year == 2016])/count[year == 2016])) 

absolute_reduction <- t1$count[t1$year == 2016] - t1$count[t1$year == 2022]

t <- unhoused %>% 
   filter(arrest_type_code != "O") %>% 
   group_by(year = year(floor_arrest_date), arrest_type) %>% 
   summarise(count = n_distinct(id)) %>% 
   group_by( arrest_type)  %>% 
   mutate(percent_change_2016 = ((count - count[year == 2016])/count[year == 2016])) 

#arrests per 1,000 unhoused people
unhoused_count <- read_csv(inputfiles$pit_population)

count_total <- unhoused_count %>% 
   filter(race == "Total" & Geography == "Los Angeles") 

t1 <- left_join(t1, count_total) %>% 
   mutate(arrests_per_count = count/num * 1000)

#what charges?
#distinct charge combos
t <- arrests %>% 
   group_by(charge, charge_description) %>% 
   summarise(count = n_distinct(id)) 

n_distinct(arrests$HRW_charge)

#Top charges
arrest_charges_total <- unhoused %>% 
   group_by(HRW_charge) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = round(count/sum(count), 3) * 100) %>% 
   arrange(desc(count)) %>% 
   mutate( cumulative_percent = cumsum(perc)) 
write_csv(arrest_charges_total, outputfiles$overall_charges)

#charges by type of arrest
total_arrests <- unhoused %>% 
   filter(arrest_type_code != "D" ) %>% 
   group_by(HRW_charge, arrest_type) %>% 
   summarise(total_arrests = n_distinct(id)) %>% 
   ungroup() %>% 
   mutate(arrest_type = paste(arrest_type, " arrests", sep = "")) %>% 
   pivot_wider(names_from = arrest_type, values_from = total_arrests) %>% 
   mutate(`Total arrests` = rowSums(across(where(is.numeric)), na.rm = T),
          `Percent infraction` = round(`Infraction arrests`/`Total arrests`, 4),
          `Percent misdemeanor` = round(`Misdemeanor arrests`/`Total arrests`, 4) ,
          `Percent felony` = round(`Felony arrests`/`Total arrests`, 4) ,
          `Percent "other"` = round(`Other arrests`/`Total arrests`, 4) ) %>% 
   arrange(desc(`Total arrests`)) %>% 
   select(Charge = HRW_charge, `Total arrests`, `Percent infraction`, `Percent misdemeanor`,
          `Percent felony`, `Percent "other"`)

#replace NAs
total_arrests[is.na(total_arrests)] = 0

write_csv(total_arrests, outputfiles$full_arrest_table)

#proportion of arrests that are of unhoused
t <- arrests %>% 
   filter(!is.na(HRW_charge)) %>% 
   group_by(HRW_charge) %>% 
   summarise(total_arrests = n_distinct(id))

t1 <- arrests %>% 
   filter(!is.na(HRW_charge)) %>% 
   group_by(HRW_charge, unhoused) %>% 
   summarise(unhoused_arrests = n_distinct(id)) %>% 
   group_by(HRW_charge) %>% 
   mutate(percentage_arrests_unhoused = round(unhoused_arrests/sum(unhoused_arrests), 3) ) %>% 
   filter(unhoused == "Y" ) %>% 
   arrange(desc(percentage_arrests_unhoused), desc(unhoused_arrests)) 

t <- left_join(t, t1) %>% 
   filter(total_arrests > 1000) %>% 
   arrange(desc(percentage_arrests_unhoused), desc(unhoused_arrests)) 

write_csv(t, outputfiles$proportion_charge_unhoused)

#104.15
t <- unhoused %>% 
   filter(HRW_charge == "Cannabis regulation (e.g. 104.15 LAMC)") %>% 
   group_by(arrest_date) %>% 
   summarise(count = n_distinct(id)) %>% 
   arrange(desc(count)) %>% 
   mutate(perc = round(count/sum(count), 3) ) 

t1 <- t %>% 
   group_by(count) %>% 
   summarise(num = n_distinct(arrest_date)) %>% 
   mutate(perc = num/sum(num))
rm(t1)

t <- unhoused %>% 
   filter(HRW_charge == "Cannabis regulation (e.g. 104.15 LAMC)") %>% 
   group_by(address) %>% 
   summarise(count = n_distinct(id)) %>% 
   arrange(desc(count)) %>% 
   mutate(perc = round(count/sum(count), 3) ) 

#Change arrests over time
total_change <- unhoused %>% 
   filter(arrest_type_code != "O") %>% 
   group_by(year = year(floor_arrest_date)) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(diff = count - count[year == 2016],
          percent_change_2016 = ((count - count[year == 2016])/count[year == 2016]),
          HRW_charge = "Total")

change_by_charge <- unhoused %>% 
   filter(arrest_type_code != "O") %>% 
   group_by(year = year(floor_arrest_date), HRW_charge) %>% 
   summarise(count = n_distinct(id)) %>% 
   group_by(HRW_charge)  %>% 
   mutate(diff = count - count[year == 2016],
          percent_change_2016 = ((count - count[year == 2016])/count[year == 2016])) 

change <- bind_rows(change_by_charge, total_change)

change <- change %>%
   group_by(year) %>% 
   mutate(perc_of_diff = ifelse(diff == 0, NA,
                                diff/diff[HRW_charge == "Total"])) %>% 
   arrange(year, desc(perc_of_diff)) %>% 
   group_by(year) %>% 
   mutate(cumperc = 1 - cumsum(perc_of_diff)) %>% 
   filter(year == 2022)

t <- change %>% 
   group_by(HRW_charge) %>% 
   summarise(yrs = n_distinct(year))

rm(total_change, change_by_charge)

#change from 2016 arrest totals per charge
t <- unhoused %>% 
   filter(arrest_type_code != "O") %>% 
   group_by(year = year(floor_arrest_date), HRW_charge) %>% 
   summarise(count = n_distinct(id)) %>% 
   group_by(HRW_charge)  %>% 
   mutate(percent_change_2016 = ((count - count[year == 2016])/count[year == 2016])) 

#wide data for output and filter top ten offenses for output
#filter top 10 charges
filter <- total_arrests %>% 
   filter(`Total arrests` > 5800)

t1 <- t %>% 
   select(-percent_change_2016) %>% 
   semi_join(filter, by = c("HRW_charge" = "Charge")) %>% 
   pivot_wider(values_from = count, names_from = HRW_charge)

write_csv(t1, outputfiles$offenses_over_time)
rm(filter)

#broader groupings
t <- unhoused %>% 
   group_by(perc_change_group, HRW_charge) %>% 
   summarise(count = n_distinct(id)) %>% 
   arrange(perc_change_group, desc(count))

#change from 2016 by charge groupings
charge_groupings_change <- unhoused %>% 
   filter(arrest_type_code != "O") %>% 
   group_by(year = year(floor_arrest_date), perc_change_group) %>% 
   summarise(count = n_distinct(id))  %>% 
   group_by(perc_change_group) %>% 
   mutate( percent_change_2016 = ((count - count[year == 2016])/count[year == 2016])) 
   
change_by_charge <- charge_groupings_change %>% 
   select(-count) %>% 
   pivot_wider(names_from = perc_change_group, values_from = percent_change_2016)
write_csv(change_by_charge, outputfiles$change_by_charge)
rm(t1, change_by_charge)

#percent change as a rate per 1,000 unhoused
change_by_charge <- charge_groupings_change %>% 
   select(-percent_change_2016) %>% 
   left_join(count_total) %>% 
   group_by(perc_change_group) %>% 
   mutate(rate = count/num * 1000,
      percent_change_2016 = ((rate - rate[year == 2016])/rate[year == 2016])) 

change_by_charge_out <- change_by_charge %>% 
   select(percent_change_2016, perc_change_group, year) %>% 
   pivot_wider(names_from = perc_change_group, values_from = percent_change_2016)

write_csv(change_by_charge_out, outputfiles$change_by_charge_rate)
rm(change_by_charge_out)

#overall arrest rate and by charge grouping
change_charge_join <- change_by_charge %>% 
   select(perc_change_group, year, percent_change_2016) %>% 
   pivot_wider(names_from = perc_change_group, values_from = percent_change_2016)

overall_rate_change <- unhoused %>% 
   filter(arrest_type_code != "O") %>% 
   group_by(year = year(floor_arrest_date)) %>% 
   summarise(arrests = n_distinct(id)) %>% 
   left_join(count_total) %>% 
   mutate(overall_arrest_rate = (arrests/num) * 1000,
          percent_change_2016 = ((overall_arrest_rate
                                  - overall_arrest_rate[year == 2016])/overall_arrest_rate[year == 2016])) %>% 
   select(year, change_overall_arrest_rate = percent_change_2016)

change_charge_join <- left_join(change_charge_join, overall_rate_change)

victim_rates <- read_csv(inputfiles$victim_rate) %>% 
   mutate(victim_perc_change = ((`Unhoused victim rate`
                                 - `Unhoused victim rate`[year == 2016])/`Unhoused victim rate`[year == 2016])) %>% 
   select(year, victim_perc_change)

change_charge_join <- left_join(change_charge_join, victim_rates)

write_csv(change_charge_join, outputfiles$arrest_victim_rate)
rm(victim_rates, change_charge_join, overall_rate_change)

#compare arrest rates v. unhoused victim rates for specific offense types
victim_rates <- read_csv(inputfiles$victim_rate) %>% 
   select(year, `Unhoused victim rate`)

overall_rate_change <- unhoused %>% 
   filter(arrest_type_code != "O") %>% 
   group_by(year = year(floor_arrest_date)) %>% 
   summarise(arrests = n_distinct(id)) %>% 
   left_join(count_total) %>% 
   mutate(overall_arrest_rate = (arrests/num) * 1000) %>% 
   select(year, overall_arrest_rate)

victim_rates <- left_join(victim_rates, overall_rate_change)

cor.test(victim_rates$`Unhoused victim rate`, victim_rates$overall_arrest_rate)

#do the unhoused and housed have similar changes in violent charges?
change_by_charge2 <- arrests %>% 
   filter(arrest_type_code != "O" & is.na(unhoused)) %>% 
   filter(!is.na(perc_change_group)) %>% 
   group_by(year  = year(floor_arrest_date), perc_change_group) %>% 
   summarise(count = n_distinct(id))  %>% 
   group_by(perc_change_group) %>% 
   mutate( percent_change_2016 = ((count - count[year == 2016])/count[year == 2016])) %>% 
   select(-count)

#Change in rates for unhoused-specific charges
total_unhoused_specific_rate <- unhoused %>% 
   filter(arrest_type_code != "O") %>% 
   filter(!is.na(perc_change_group)) %>% 
   group_by(year = year(floor_arrest_date), perc_change_group) %>% 
   summarise(count = n_distinct(id))  %>% 
   left_join(count_total) %>% 
   group_by(perc_change_group) %>% 
   mutate(rate = count/num * 1000) %>% 
   select(rate, perc_change_group, year) %>% 
   pivot_wider(names_from = perc_change_group, values_from = rate) %>% 
   select(year, `Unhoused-specific charge`)

unhoused_specific_rate <- unhoused %>% 
   filter(arrest_type_code != "O") %>% 
   filter(perc_change_group == "Unhoused-specific charge") %>% 
   group_by(year = year(floor_arrest_date), HRW_charge) %>% 
   summarise(count = n_distinct(id)) %>% 
   left_join(count_total) %>% 
   group_by(HRW_charge) %>% 
   mutate(rate = count/num * 1000) %>% 
   select(rate, HRW_charge, year) %>% 
   pivot_wider(names_from = HRW_charge, values_from = rate) 

unhoused_specific_rate <- bind_cols(unhoused_specific_rate, total_unhoused_specific_rate)

write_csv(unhoused_specific_rate, outputfiles$unhoused_specific_charges)
rm(unhoused_specific_rate, total_unhoused_specific_rate)

#Jail bookings
t <- arrests %>% 
   group_by(arrest_type, booked_jail) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count))

t <- arrests %>% 
   group_by(booked_jail, arrest_type, unhoused) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count))

t <- arrests %>% 
   group_by(booked_jail,  unhoused) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count))

#total unhoused booked
t <- unhoused %>% 
   group_by(booked_jail) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count))

#which jails
t1 <- unhoused %>% 
   filter(booked_jail == 1) %>% 
   group_by(booking_location) %>% 
   summarise(count = n_distinct(id)) %>% 
   arrange(desc(count)) %>% 
   mutate(perc = count/sum(count),
          cumperc = cumsum(perc)) %>% 
   select(-perc)

t2 <- arrests %>% 
   filter(booked_jail == 1) %>% 
   group_by(unhoused, booking_location) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count)) %>% 
   mutate(type = ifelse(is.na(unhoused), "Housed", "Unhoused")) %>% 
   ungroup() %>% 
   select(-unhoused, -count) %>% 
   pivot_wider(names_from = type, values_from = perc)

t <- left_join(t1, t2)
write_csv(t, outputfiles$jails)
rm(t1, t2, t)

#dispositions
t <- unhoused %>% 
   group_by(arrest_type, disposition_description) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count))

#race
race_table <- unhoused %>% 
   group_by(race = race_recoded) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count)) %>% 
   select(race, `Percentage of Unhoused Arrests` = perc)

#LA pop
city_pop <- read_rds(inputfiles$city_census)

#total race percentages during time period 2016-2022
tot_pop <- as.numeric(city_pop %>% 
   filter(year > 2015 & year < 2023) %>% 
   summarise(pop = sum(total_pop)))

city_race_pop <- city_pop %>% 
   filter(year > 2015 & year < 2023) %>% 
   select(year, Black = black, White =white_non_hisp, 
          Latinx = hisp) %>% 
   pivot_longer(names_to = "race", cols = c(Black, White, Latinx)) %>% 
   group_by(race) %>% 
   summarise(pop = sum(value)) %>% 
   mutate(perc = pop/tot_pop) %>% 
   select(race, `Percentage of Los Angeles Population` = perc)

race_table <- left_join(race_table, city_race_pop)
   
#unhoused pop
pit_population <- read_csv(inputfiles$pit_population)

pit_race <- pit_population %>% 
   filter(Year > 2015 & Year < 2024 & race != "Total"
          & Geography == "Los Angeles") %>% 
   group_by(race) %>% 
   summarise(pop = sum(num, na.rm = T)) %>% 
   mutate(`Percentage of Unhoused Population` = 
             pop/sum(pop)) %>% 
   select(-pop)

race_table <- left_join(race_table, pit_race)

#odds ratio for housed v unhoused arrests for city total pop for years
tot_pop
tot_unhoused_pop <-  as.numeric(pit_population %>% 
   filter(Year > 2015 & Year < 2023 & race == "Total"
          & Geography == "Los Angeles") %>% 
      summarise(count = sum(num)))
housed_pop <- tot_pop - tot_unhoused_pop

arrests_by_housing_status <- arrests %>% 
   group_by(unhoused) %>% 
   summarise(arrests = n_distinct(report_id, na.rm = T)) %>% 
   mutate(arrest_type = "Total citations and arrests")

t <- arrests %>% 
   filter(arrest_type != "Other") %>% 
   group_by(unhoused, arrest_type) %>% 
   summarise(arrests = n_distinct(report_id))

arrests_by_housing_status <- bind_rows(arrests_by_housing_status, t)

#odds of jail booking
t <- arrests %>% 
   group_by(unhoused, booked_jail) %>% 
   summarise(arrests = n_distinct(report_id)) %>% 
   filter(booked_jail == 1) %>% 
   mutate(arrest_type = "Booked into jail") %>% 
   select(-booked_jail)

arrests_by_housing_status <- bind_rows(arrests_by_housing_status, t)

#calculate percentage
arrests_by_housing_status <- arrests_by_housing_status %>% 
   mutate(pop = ifelse(is.na(unhoused), housed_pop, tot_unhoused_pop)) %>% 
   mutate(perc = arrests/pop,
          rate_per_1000 = perc * 1000,
          odds = paste("1 in ", 1/round(perc, 3), sep = "")) %>% 
   group_by(arrest_type) %>% 
   mutate(diff = (rate_per_1000/rate_per_1000[is.na(unhoused)]))
write_csv(arrests_by_housing_status, outputfiles$arrests_by_housing_status)        

#housed/unhoused rates by year - total arrests citations
t <- arrests %>% 
   mutate(year = year(arrest_date)) %>% 
   group_by(year, unhoused) %>% 
   summarise(arrests = n_distinct(report_id)) 

city_join <- city_pop %>% 
   select(year, pop = total_pop) %>% 
   mutate(year = as.double(year))

pit_join <- unhoused_count %>% 
   filter(Geography == "Los Angeles" & race == "Total") %>% 
   select(year, pop = num) %>% 
   mutate(unhoused = "Y")

 join <- bind_rows(city_join, pit_join)  %>% 
   pivot_wider(values_from = pop, names_from = unhoused) %>% 
   mutate(housed_pop = `NA` - Y) %>% 
   select(-`NA`) %>% 
   pivot_longer(cols = -year) %>% 
   rename(unhoused = name, pop = value) %>% 
   mutate(unhoused = ifelse(unhoused != "Y", NA, unhoused))

t <- left_join(t, join) 
rm(city_join, pit_join, join)

t <- t %>% 
   mutate(perc = arrests/pop,
          rate_per_1000 = perc * 1000) %>% 
   mutate(diff = (rate_per_1000/rate_per_1000[is.na(unhoused)]))

rm(pit_race, tot_pop, city_race_pop, arrests_by_housing_status)

#race just unhoused-specific offenses total then specific
race1 <- unhoused %>% 
   filter(HRW_charge == "Sit/lie/sleep on sidewalk or street (41.18)") %>% 
   group_by(race = race_recoded) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count)) %>% 
   select(race, `Percentage of 41.18 arrests` = perc)

race2 <- unhoused %>% 
   filter(HRW_charge == "Open alcohol/drinking in public") %>% 
   group_by(race = race_recoded) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count)) %>% 
   select(race, `Percentage of open container/drinking arrests` = perc)

race3 <- unhoused %>% 
   filter(HRW_charge == "Park regulations") %>% 
   group_by(race = race_recoded) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count)) %>% 
   select(race, `Percentage of park regulations arrests` = perc)

race4 <- unhoused %>% 
   filter(HRW_charge == "Illegal possession of shopping cart") %>% 
   group_by(race = race_recoded) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count)) %>% 
   select(race, `Percentage of possession of shopping cart arrests` = perc)

race5 <- unhoused %>% 
   filter(HRW_charge == "Leaving personal property (56.11)") %>% 
   group_by(race = race_recoded) %>% 
   summarise(count = n_distinct(id)) %>% 
   mutate(perc = count/sum(count)) %>% 
   select(race, `Percentage of 56.11 arrests` = perc)

race_table <- race_table %>% 
   left_join(race1) %>% 
   left_join(race2) %>% 
   left_join(race3) %>% 
   left_join(race5) 
write_csv(race_table, outputfiles$race_table_lapd)

#number of pop per 1 unhoused person by race.
#latest numbers only.
city_join <- city_pop %>% 
   filter(year == 2022) %>% 
   rename("Black" = "black", 
          "Latinx" = "hisp", 
          "White" = "white_non_hisp") %>% 
   pivot_longer(names_to = "race",
                values_to = "pop",
                cols = c("Black",
                         "Latinx",
                         "White"))

pit_join <- pit_population %>% 
   filter(Year == 2022 & Geography == "Los Angeles") %>% 
   select(race, num)
   
city_join <- left_join(city_join, pit_join) %>% 
   mutate(pop_per_unhoused = pop/num)

rm(city_join, pit_join)

#areas by race
t <- unhoused %>%
   group_by(race_recoded, area_name) %>% 
   summarise(count = n_distinct(report_id)) %>% 
   mutate(perc = count/sum(count)) %>% 
   arrange(race_recoded, desc(perc))

#Try to de-duplicate 41.18 from Controller Meija's data and extend arrests for 41.18
meija <- read_csv(inputfiles$meija)

arrest_41 <- arrests %>% 
   filter(HRW_charge == "Sit/lie/sleep on sidewalk or street (41.18)")

meija_time_overlap <- meija %>% 
   filter(arrest_date %in% arrest_41$arrest_date)

arresttest <- arrest_41 %>% 
   filter(arrest_date %in% meija_time_overlap$arrest_date)

#almost complete overlap
rm(arresttest)

meija_non_overlap <- meija %>% 
   filter(!arrest_date %in% arrest_41$arrest_date) %>% 
   mutate(floor_arrest_date = floor_date(arrest_date, unit = "months"))

#floor dates to the month, group by arrest type
arrest_41_table <- arrest_41 %>% 
   group_by(floor_arrest_date, arrest_type) %>% 
   summarise(count = n_distinct(report_id))  %>% 
   pivot_wider(names_from = arrest_type, values_from = count) %>% 
   select(-Other)

additional_months <- meija_non_overlap %>% 
   group_by(floor_arrest_date, arrest_type) %>% 
   summarise(count = n_distinct(report_id))  %>% 
   pivot_wider(names_from = arrest_type, values_from = count)

arrest_41_table <- bind_rows( arrest_41_table, additional_months)

t1 <- arrest_41 %>% 
   group_by(floor_arrest_date) %>% 
   summarise(Total = n_distinct(report_id)) 

t2 <- meija_non_overlap %>% 
   group_by(floor_arrest_date) %>% 
   summarise(Total = n_distinct(report_id)) 

t1 <- bind_rows(t1, t2)

arrest_41_table <- left_join(arrest_41_table, t1) %>% 
   mutate(year = year(floor_arrest_date)) %>% 
   left_join(count_total) %>% 
   mutate(monthly_rate = Total/num * 10000) 

write_csv(arrest_41_table, outputfiles$updated_4118_rates)
rm(additional_months, meija, meija_non_overlap, meija_time_overlap, t1, t2,
   arrest_41, arrest_41_table)

   