#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policinghomelessness/descriptives/src/lapd_descriptives.r
library(pacman)
p_load(lubridate, readxl, readr, rcartocolor, extrafont, scales, tidycensus,
       CGPfunctions, tidyverse, qs, fuzzyjoin)
options(scipen=999)

here <- here::here

########### input and output files ##############
# input files:
inputfiles <- list(
   unhoused_victim = "processing/output/crimes_against_unhoused_processed.csv",
   pit_population = "processing/output/unhoused_count.csv",
   total_crimes = "processing/output/crimes_total.csv",
   arrests_by_charge_rate = "descriptives/output/lapd/change_by_charge_rate.csv"
   
   
) %>% map(here)

#output files. 
outputfiles <- list(
   per_year_rate = "descriptives/output/lapd/victim_rates_per_year.csv",
   specific_offenses = "descriptives/output/lapd/crimes_against_unhoused_offenses.csv",
   investigation_status = "descriptives/output/lapd/investigation_status.csv"
   
) %>% map(here)

#read in
unhoused_victim <- read_csv(inputfiles$unhoused_victim)
pit_pop <- read_csv(inputfiles$pit_population)

t <- unhoused_victim %>% 
   group_by(date) %>% 
   summarise(count = n())

#round full years (so no 1/2022)
unhoused_victim <- unhoused_victim %>% 
   filter(date < "2022-01-01")

#pit total per year in la
pit_total <- pit_pop %>% 
   filter(Geography == "Los Angeles" & race == "Total")

#how many victims per year and rate per unhoused pop
per_year <- unhoused_victim %>% 
   group_by(year = year(date)) %>% 
   summarise(count = n()) %>% 
   left_join(pit_total) %>% 
   mutate(rate = (count/num) * 1000)

#what is the total rate of crime?
#read total crime in
total_crime <- read_csv(inputfiles$total_crimes)

#read in total pop
total_pop <- read_rds(inputfiles$city_census)

total_per_year <- total_crime %>% 
   mutate(year = as.character(year)) %>% 
   group_by(year) %>% 
   summarise(count = n_distinct(DR_NO)) %>% 
   left_join(total_pop) %>% 
   mutate(rate = (count/total_pop) * 1000) %>% 
   select(year, total_crime_count = count, total_crime_rate = rate)
   
#table
per_year <- per_year %>% 
   mutate(year = as.character(year)) %>% 
   left_join(total_per_year) 

sum(per_year$count)

per_year_out <- per_year %>% 
   select(year, "Unhoused victim rate" = rate,
          "Overall victim rate" = total_crime_rate)
write_csv(per_year_out, outputfiles$per_year_rate)

#total difference in rates over the full years?
total_crime2 <- total_crime %>% 
   mutate(year = as.character(year)) %>% 
   group_by(year) %>% 
   summarise(count = n_distinct(DR_NO)) %>% 
   left_join(total_pop) %>% 
   summarise(total_crime = sum(count), total_population = sum(total_pop)) %>% 
   mutate(rate = (total_crime/total_population) * 1000) 
 
total_unhoused_crime <- per_year %>% 
   summarise(total_victims = sum(count),
             total_unhoused_pop = sum(num)) %>% 
   mutate(total_rate = (total_victims/total_unhoused_pop) * 1000)

total_unhoused_crime$total_rate/total_crime2$rate
total_unhoused_crime$total_unhoused_pop/total_crime2$total_population


#specific offenses
unhoused_specific <- unhoused_victim %>% 
   group_by(offense) %>% 
   summarise(unhoused_victims = n()) %>% 
   arrange(desc(unhoused_victims)) %>% 
   mutate(perc_unhoused_victims = unhoused_victims/sum(unhoused_victims),
          rank_unhoused = row_number(),
          unhoused_rate_per_1000 = 
             (unhoused_victims/total_unhoused_crime$total_unhoused_pop) * 1000)
   

total_offenses <- total_crime %>% 
   group_by(offense) %>% 
   summarise(total_offenses = n_distinct(DR_NO)) %>% 
   arrange(desc(total_offenses)) %>% 
   mutate(perc_total_victims = total_offenses/sum(total_offenses),
          rank_total = row_number(),
          total_rate_per_1000 = (total_offenses/total_crime2$total_population) * 1000)

specific_offenses <- left_join(unhoused_specific, total_offenses)

specific_offenses_out <- specific_offenses %>% 
   mutate(rate_ratio = unhoused_rate_per_1000/total_rate_per_1000,
          unhoused_percentage_of_total_offenses = unhoused_victims/total_offenses) %>% 
   filter(rank_unhoused < 16)

write_csv(specific_offenses_out, outputfiles$specific_offenses)

#where
where <- unhoused_victim %>% 
   group_by(premise) %>% 
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count))
rm(where)

#status per crime for unhoused and total
unhoused_status <- unhoused_victim %>% 
   group_by(offense, status = case_status) %>% 
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count))

total_status <- total_crime %>% 
   filter(status != "Unk") %>% 
   group_by(offense, status) %>% 
   summarise(total_offenses = n_distinct(DR_NO)) %>% 
   mutate(perc_total_victims = total_offenses/sum(total_offenses)) %>% 
   select(offense, status, perc_total_victims)


joined_out <- left_join(unhoused_status, total_status) %>% 
   filter(status == "Investigation continued" & count > 100) %>% 
   mutate(`Arrest rate (Unhoused victim)` = 1 - perc,
          `Arrest rate (Total victims)` = 1 - perc_total_victims) %>% 
   mutate(diff = `Arrest rate (Total victims)` -
             `Arrest rate (Unhoused victim)`,
          `Total arrest rate percent higher` = (`Arrest rate (Unhoused victim)` - 
                                     `Arrest rate (Total victims)`) /
             `Arrest rate (Unhoused victim)`,
          `Rate ratio` = `Arrest rate (Total victims)`/
             `Arrest rate (Unhoused victim)`) %>% 
   filter(offense %in% specific_offenses_out$offense) %>% 
   select(offense, `Arrest rate (Unhoused victim)`,
          `Arrest rate (Total victims)`, 
          `Rate ratio`,
          `Percentage point difference` = diff,
          )

write_csv(joined_out, outputfiles$investigation_status)

#overall arrest rates from lapd_descriptives
arrest_rates <- read_csv(inputfiles$arrests_by_charge_rate)


#race of victims
race1 <- unhoused_victim %>% 
   filter(YEAR < 2022) %>% 
   group_by(race = race_recoded) %>% 
   summarise(count = n_distinct(id))

race2 <- pit_pop %>% 
   filter(Geography == "Los Angeles" & race != "Total" & year < 2022) %>% 
   group_by(race) %>% 
   summarise(total_pop = sum(num))

race1 <- left_join(race1, race2)

race1 <- race1 %>% 
   mutate(Rate = (count/total_pop) * 1000, 
          perc = count/sum(count))
rm(race1, race2)

