#
# Authors:     BR
# Maintainers: BR
# Copyright:   2023
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policinghomelessness/descriptives/src/lasan_descriptives.r


library(pacman)
p_load(lubridate, readxl, readr, rcartocolor, extrafont, scales, tidycensus,
       CGPfunctions, tidyverse, qs, fuzzyjoin, ggmap)
options(scipen=999)

here <- here::here


########### input and output files ##############
# input files:
inputfiles <- list(
   lasan = "geo_matching/output/lasan_processed_geomatched.rds"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   lasan_totals = "descriptives/output/lasan/lasan_totals.csv",
   lasan_lahsa_present = "descriptives/output/lasan/lasan_lahsa_present.csv",
   lasan_lahsa_present_time = "descriptives/output/lasan/lasan_lahsa_present_time.csv",
   abh_table = "descriptives/output/lasan/abh_table.csv",
   sweeps_to_map = "descriptives/output/lasan/sweeps_to_map.csv",
   property = "descriptives/output/lasan/property.csv"

) %>% map(here)

#input
lasan <- read_rds(inputfiles$lasan)

#timeframe
min(lasan$date)
max(lasan$date)

#remove any with no solid waste
lasan <- lasan %>% 
   filter(no_solid_waste == 0) %>% 
   filter(solid_waste_lbs > 100 & in_LA == "yes") 

#types of cleanings
t <- lasan %>% 
   group_by(nature_of_call) %>% 
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T))

#summary by month for graphs
t <- lasan %>% 
   group_by(month) %>% 
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T),
             lbs_taken = sum(solid_waste_lbs, na.rm = T),
             bags = sum(bags_items_sent_to_storage, na.rm = T)) %>% 
   mutate(pounds_per_sweep = lbs_taken/number_of_sweeps,
          bags_per_sweep = bags/number_of_sweeps,
          sweeps_per_bag = number_of_sweeps/bags,
          pounds_per_bag = lbs_taken/bags)
write_csv(t, outputfiles$lasan_totals)

sum(t$number_of_sweeps)
n_distinct(lasan$case_id)
sum(t$lbs_taken)
n_distinct(t$month)
median(t$sweeps_per_bag)
sum(t$bags)

#average monthly pounds taken
avg <- t %>% 
   filter(month < "2023-02-01") %>% 
   summarise(avg_month = mean(lbs_taken))

#avg bags in 2021 and 2022
avg <- t %>% 
   filter(month < "2023-01-01" & month > "2020-12-01") %>% 
   summarise(avg_month = mean(bags))

#total numbers for entire time period for LA
t <- lasan %>% 
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T),
             lbs_taken = sum(solid_waste_lbs, na.rm = T),
             bags = sum(bags_items_sent_to_storage, na.rm = T)) %>% 
   mutate(pounds_per_sweep = lbs_taken/number_of_sweeps,
          bags_per_sweep = bags/number_of_sweeps,
          sweeps_per_bag = number_of_sweeps/bags,
          pounds_per_bag = lbs_taken/bags)

#numbers for just 2021 and 2022
t <- lasan %>% 
   filter(month < "2023-01-01" & month > "2020-12-01") %>% 
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T),
             lbs_taken = sum(solid_waste_lbs, na.rm = T),
             bags = sum(bags_items_sent_to_storage, na.rm = T)) %>% 
   mutate(pounds_per_sweep = lbs_taken/number_of_sweeps,
          bags_per_sweep = bags/number_of_sweeps,
          sweeps_per_bag = number_of_sweeps/bags,
          pounds_per_bag = lbs_taken/bags)

#per year
t <- lasan %>% 
   filter(month < "2023-01-01" & month > "2019-12-01") %>% 
   group_by(year(date)) %>%
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T),
             lbs_taken = sum(solid_waste_lbs, na.rm = T),
             bags = sum(bags_items_sent_to_storage, na.rm = T)) %>% 
   mutate(pounds_per_sweep = lbs_taken/number_of_sweeps,
          bags_per_sweep = bags/number_of_sweeps,
          sweeps_per_bag = number_of_sweeps/bags,
          pounds_per_bag = lbs_taken/bags)

#% of cleanings over 100 pounds with no bag taken.
t <- lasan %>% 
   group_by(bags_items_sent_to_storage) %>% 
   summarise(count = n_distinct(case_id, na.rm = T)) %>% 
   mutate(perc = count/sum(count))

#total rate of lahsa present over time
t1 <- lasan %>% 
   filter(has_geography == 1) %>% 
   group_by(month, lahsa_present) %>% 
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count)) %>% 
   filter(lahsa_present == 1) %>% 
   filter(month > "2020-06-01" & month < "2022-04-01")
write_csv(t1, outputfiles$lasan_lahsa_present_time)

#percentage where Lahsa was present - only data after june 2020
# percent where present by nature
 t <- lasan %>% 
    filter(has_geography == 1) %>% 
   filter(month > "2020-06-01" & month < "2022-04-01") %>% 
   group_by(nature_of_call, lahsa_present) %>% 
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count)) %>% 
   filter(lahsa_present == 1) %>% 
   select(-count)

#% present total
 t1 <- lasan %>% 
    filter(has_geography == 1) %>% 
    filter(month > "2020-06-01" & month < "2022-04-01") %>% 
    group_by(lahsa_present) %>% 
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count), nature_of_call = "Total") %>% 
   filter(lahsa_present == 1) %>% 
   select(-count)

table <- bind_rows(t, t1)

#percent where lahsa referred someone to housing
t <- lasan %>% 
   filter(has_geography == 1) %>% 
   filter(month > "2020-06-01" & month < "2022-04-01") %>% 
   group_by(nature_of_call, housing_referral) %>% 
   summarise(count = n()) %>% 
   mutate(percent_housing_referral = count/sum(count)) %>% 
   filter(housing_referral == 1) %>% 
   select(-count)

#% present total
t1 <- lasan %>% 
   filter(has_geography == 1) %>% 
   filter(month > "2020-06-01" & month < "2022-04-01") %>% 
   group_by(housing_referral) %>% 
   summarise(count = n()) %>% 
   mutate(percent_housing_referral = count/sum(count), nature_of_call = "Total") %>% 
   filter(housing_referral == 1) %>% 
   select(-count)

table2 <- bind_rows(t, t1)

#percent were someone attained housing
t <- lasan %>% 
   filter(has_geography == 1) %>% 
   filter(month > "2020-06-01" & month < "2022-04-01") %>% 
   group_by(nature_of_call, housing_attained) %>% 
   summarise(count = n()) %>% 
   mutate(percent_housing_attained = count/sum(count)) %>% 
   filter(housing_attained == 1) %>% 
   select(-count)

#% present total
t1 <- lasan %>% 
   filter(has_geography == 1) %>% 
   filter(month > "2020-06-01" & month < "2022-04-01") %>% 
   group_by(housing_attained) %>% 
   summarise(count = n()) %>% 
   mutate(percent_housing_attained = count/sum(count), nature_of_call = "Total") %>% 
   filter(housing_attained == 1) %>% 
   select(-count)

table3 <- bind_rows(t, t1)
table <- left_join(table, table2)
table <- left_join(table, table3)
table <- table %>% 
   select(-housing_referral, -housing_attained)

#total count by nature if call
t2 <-  lasan %>% 
   filter(has_geography == 1) %>% 
   filter(month > "2020-06-01" & month < "2022-04-01") %>% 
   group_by(nature_of_call) %>% 
   summarise(count = n())  

#total count
t3 <-  lasan %>% 
   filter(has_geography == 1) %>% 
   filter(month > "2020-06-01" & month < "2022-04-01") %>% 
   summarise(count = n()) %>% 
   mutate(nature_of_call = "Total")

t <- bind_rows(t2, t3)
table <- left_join(table, t)

table <- table %>% 
   select(nature_of_call, count, perc, percent_housing_referral,
          percent_housing_attained) 


write_csv(table, outputfiles$lasan_lahsa_present)
rm(t, t1, t2, t3, table)

#when LAHSA wasn't present, were there property bags taken?
t <- lasan %>% 
   filter(month > "2020-06-01" & month < "2022-04-01") %>% 
   filter(bags_items_sent_to_storage != 0) %>% 
   group_by(lahsa_present) %>% 
   summarise(count = n()) %>% 
   mutate(perc = count/sum(count))


#lasan in A Bridge Home zones
#overall %
lasan <- lasan %>% 
   mutate(abh_yn = ifelse(!is.na(OHS_ABH), 1, 0))

t <- lasan %>% 
   filter(total > 99 & in_LA == "yes") %>% 
   group_by(abh_yn) %>% 
   summarise(count = n_distinct(case_id)) %>% 
   mutate(perc = count/sum(count))

t <- lasan %>% 
   filter(total > 99 & in_LA == "yes") %>% 
   group_by(abh_yn, bags_items_sent_to_storage) %>% 
   summarise(count = n_distinct(case_id)) %>% 
   mutate(perc = count/sum(count))

t <- lasan %>% 
   filter(total > 99 & in_LA == "yes") %>% 
   group_by(abh_yn) %>% 
   summarise(median_taken = median(solid_waste_lbs, na.rm = T),
             mean = mean(solid_waste_lbs, na.rm = T))

# are ABH happening on a schedule?
t <- lasan %>% 
   filter(total > 99 & in_LA == "yes" & abh_yn == 1) %>% 
   group_by(date) %>% 
   summarise(count = n_distinct(case_id)) 


#ABH_OHS table - #num of cleanings, avg property thrown away, avg bags
t <- lasan %>% 
   filter(total > 99 & in_LA == "yes" & abh_yn == 1) %>% 
   group_by(OHS_ABH) %>% 
   summarise(count = n_distinct(case_id),
             avg_taken = mean(solid_waste_lbs, na.rm = T),
             avg_bags = mean(bags_items_sent_to_storage, na.rm = T)) %>% 
   arrange(desc(count)) %>% 
   mutate(site = str_to_title(OHS_ABH)) %>% 
   ungroup() %>% 
   select(site, count, avg_taken, avg_bags) %>% 
   filter(count > 200)
write_csv(t, outputfiles$abh_table)


#write out coords with # of sweeps and # of pounds
t <- lasan %>% 
   group_by(latitude, longitude, geometry) %>% 
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T),
             lbs_taken = sum(solid_waste_lbs, na.rm = T),
             bags = sum(bags_items_sent_to_storage, na.rm = T))
write_csv(t, outputfiles$sweeps_to_map)


property_bags <- t %>% 
   filter(bags > 0)
write_csv(property_bags, outputfiles$property)

#percent taking a bag by cleaning type
t <- lasan %>% 
   group_by(nature_of_call, bags_taken) %>% 
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T)) %>% 
   mutate(perc = number_of_sweeps/sum(number_of_sweeps))

t <- lasan %>% 
   filter(nature_of_call != "OHS Spot Cleaning" & nature_of_call != 
             "Public Right-Of-Enforcement") %>% 
   group_by( bags_taken) %>% 
   summarise(number_of_sweeps = n_distinct(case_id, na.rm = T)) %>% 
   mutate(perc = number_of_sweeps/sum(number_of_sweeps))
