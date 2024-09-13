#
# Authors:     BR
# Maintainers: BR
# Copyright:   2023
# =========================================
# OneDrive-HumanRightsWatchInc/20220101_USA_USP_LosAngelesUnhoused/descriptives/src/rent_burden.r"
library(CGPfunctions)
library(extrafont)
library(fuzzyjoin)
library(janitor)
library(leaflet)
library(lubridate)
library(mapview)
library(qs)
library(rcartocolor)
library(readr)
library(readxl)
library(scales)
library(sf)
library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
options(scipen=999)
options(tigris_use_cache = TRUE)

here <- here::here

########### input and output files ##############
# input files:
inputfiles <- list(
   
) %>% map(here)

#output files. 
outputfiles <- list(
   vars = "descriptives/output/rent_burden/vars.csv",
   rent_perc_tracts = "descriptives/output/rent_burden/rent_perc_tracts.csv",
   rent_burden_tracts = "descriptives/output/rent_burden/tracts_rent_burden.csv",
   burden_time = "descriptives/output/rent_burden/burden_over_time.csv",
   burden_time_viz = "descriptives/output/rent_burden/burden_viz.csv"
   
) %>% map(here)


#get census data at tract level
vars <- load_variables(2012, dataset = "acs1")
write_csv(vars, outputfiles$vars)


my_vars <- c(
   non_cit_tot = "B05001_006",
   total_pop = "B05001_001"
)


#for Los Angeles place, try to download multiple years of 1 year
#table for Table b_25
#get population data - 2020 is experimental due to covid. 
#So we'll do 2005-19 and 21 here and get 2020 separate.
years <- c(2012:2019, 2021, 2022) 
names(years) <- years
years

vars <- load_variables(2005, "acs1")


# loop over list of years and get 1 year acs estimates
multi_year <- map_dfr(
   years,
   ~ {get_acs(
      geography = "place",
      table = "B25003", 
      year = .x,
      survey = "acs1",
      geometry = FALSE
   )  
   },
   .id = "year"  # when combining results, add id var (name of list item)
) 

multi_year <- multi_year %>%
   filter(GEOID == "0644000") %>% 
   select(-moe) %>%  
   arrange(variable, NAME) %>% 
   print()

multi_year <- multi_year %>% 
   pivot_wider(names_from = variable, values_from = estimate)

multi_year <- multi_year %>% 
   mutate(own_percent = B25003_002/B25003_001,
          rent_percent = B25003_003/B25003_001)
str(multi_year)

total_households <- multi_year$B25003_001[multi_year$year == 2022]


#total rent as a percentage in 2022 in LA
t <- get_acs(geography = "place",
             table = "B25070",
             state = "CA",
             geometry = F,
             year = 2022,
             survey = "acs1"
            
) %>% 
   filter(GEOID == "0644000")

t <- t %>% 
   mutate(perc = estimate/cur_data()$estimate[1])

t1 <- t %>% 
   filter(variable == "B25070_007" | variable == "B25070_008" |
             variable == "B25070_009" |
             variable == "B25070_010" )
total_rent_burden_perc  <- sum(t1$perc)       
total_rent_burden_households <- sum(t1$estimate) 
severe_burden_perc <- t1$perc[t1$variable == "B25070_010"]
severe_burden_households <- t1$estimate[t1$variable == "B25070_010"]
total_rent_burden_households/total_households
severe_burden_households/total_households


#percentage of renters by tract
t22 <- get_acs(geography = "tract",
             table = "B25003", 
             state = "CA",
             county = "Los Angeles", 
             geometry = F,
             year = 2022,
             survey = "acs5"
)

t21 <- get_acs(geography = "tract",
              table = "B25003", 
              state = "CA",
              county = "Los Angeles", 
              geometry = F,
              year = 2021,
              survey = "acs5"
)

tracts21 <- t21 %>% 
   select(-moe) %>% 
   pivot_wider(names_from = variable, values_from = estimate)

tracts22 <- t22 %>% 
   select(-moe) %>% 
   pivot_wider(names_from = variable, values_from = estimate)

tracts21 <- tracts21 %>% 
   mutate(rent_percent_21 = B25003_003/B25003_001 * 100) %>% 
   select(GEOID, rent_percent_21)

tracts22 <- tracts22 %>% 
   mutate(rent_percent = B25003_003/B25003_001 * 100) %>% 
   left_join(tracts21)


write_csv(tracts, outputfiles$rent_perc_tracts)


#rent burden by tract
t <- get_acs(geography = "tract",
             table = "B25070",
             state = "CA",
             county = "Los Angeles", 
             geometry = F,
             year = 2022,
             survey = "acs5"
)

t1 <- t %>% 
   mutate(name = case_when(
      variable == "B25070_007" ~ "30",
      variable == "B25070_008" ~ "35",
      variable == "B25070_009" ~ "40",
      variable == "B25070_010" ~ "50",
      variable == "B25070_001" ~ "Tot",
      TRUE ~ NA)) %>% 
   filter(!is.na(name)) %>% 
   select(name, estimate, GEOID) %>% 
   pivot_wider(names_from = name, values_from = estimate) 

t1 <- t1 %>% 
   mutate(total_rent_burden =  `30` + `35` + `40` + `50`,
          rent_burden_perc = 100 * total_rent_burden/Tot) %>% 
   mutate(GEOID = str_sub(GEOID, start = -6))

write_csv(t1, outputfiles$rent_burden_tracts)

#rent burden over time
years <- c(2012:2019, 2021, 2022) 
names(years) <- years
years

vars <- load_variables(2021, "acs1")

my_vars <- c(
   total_renters = "B25070_001",
   rent_30 = "B25070_007",
   rent_35 = "B25070_008",
   rent_40 = "B25070_009",
   rent_50 = "B25070_010"
   )

# loop over list of years and get 1 year acs estimates
multi_year <- map_dfr(
   years,
   ~ {get_acs(
      geography = "place",
      variables = my_vars, 
      year = .x,
      survey = "acs1",
      geometry = FALSE
   )  
   },
   .id = "year"  # when combining results, add id var (name of list item)
) 

multi_year <- multi_year %>%
   filter(GEOID == "0644000") %>% 
   select(-moe) %>%  
   arrange(variable, NAME) %>% 
   print()

multi_year <- multi_year %>% 
   pivot_wider(names_from = variable, values_from = estimate)

multi_year_rent <- multi_year %>% 
   mutate(burden_rent = rent_30 + rent_35 + rent_40,
          severe_burden_rent = rent_50) %>% 
   select(year, burden_rent, severe_burden_rent, total_renters)
      
#burden (owners)
my_vars <- c(
   owner_households = "B25091_001",
   own_30a = "B25091_008",
   own_35a = "B25091_009",
   own_40a = "B25091_010",
   own_50a = "B25091_011",
   own_30b = "B25091_019",
   own_35b = "B25091_020",
   own_40b = "B25091_021",
   own_50b = "B25091_022"
)


multi_year <- map_dfr(
   years,
   ~ {get_acs(
      geography = "place",
      variables = my_vars, 
      cache_table = TRUE,
      year = .x,
      survey = "acs1",
      geometry = FALSE
   )  
   },
   .id = "year"  # when combining results, add id var (name of list item)
) 

multi_year <- multi_year %>%
   filter(GEOID == "0644000") %>% 
   select(-moe) %>%  
   arrange(variable, NAME) %>% 
   print()

multi_year <- multi_year %>% 
   pivot_wider(names_from = variable, values_from = estimate)

multi_own <- multi_year %>% 
   mutate(burden_own = own_30a + own_30b + own_35a + own_35b + own_40a + own_40b,
          severe_burden_own = own_50a+own_50b) %>% 
   select(year, owner_households, burden_own, severe_burden_own)

burden <- left_join(multi_own, multi_year_rent)

burden <- burden %>% 
   mutate(total_households = total_renters + owner_households,
          total_30 = burden_own + burden_rent,
          total_severe = severe_burden_own + severe_burden_rent,
          total_rent_burden = burden_rent + severe_burden_rent,
          total_own_burden = burden_own + severe_burden_own,
          total_burden = total_30 + total_severe,
          perc_burden = total_burden/total_households, 
          perc_burden_own = (burden_own + severe_burden_own)/owner_households,
          perc_burden_rent = (burden_rent + severe_burden_rent)/total_renters)

write_csv(burden, outputfiles$burden_time)

burden2 <- burden %>% 
   select(year, total_30, total_severe) %>% 
   pivot_longer(cols = total_30:total_severe) %>% 
   pivot_wider(names_from = year, values_from = value)

write_csv(burden2, outputfiles$burden_time_viz)

rm(multi_year, multi_year_rent, multi_own)


#proportion of city that is Black over time

my_vars <- c(
   total_pop = "B01003_001",
   Black = "B02009_001"
)

years <- c(2005:2019, 2021, 2022) 
names(years) <- years
years

# loop over list of years and get 1 year acs estimates
multi_year <- map_dfr(
   years,
   ~ {get_acs(
      geography = "place",
      variables = my_vars, 
      year = .x,
      survey = "acs1",
      geometry = FALSE
   )  
   },
   .id = "year"  # when combining results, add id var (name of list item)
) 

multi_year <- multi_year %>%
   filter(GEOID == "0644000") %>% 
   select(-moe) %>%  
   arrange(variable, NAME) %>% 
   print()

multi_year2 <- multi_year %>% 
   pivot_wider(names_from = variable, values_from = estimate) %>% 
   mutate(perc = Black/total_pop)


#Gini
my_vars <- c(
   gini = "B19083"
)

# loop over list of years and get 1 year acs estimates
multi_year <- map_dfr(
   years,
   ~ {get_acs(
      geography = "place",
      variables = my_vars, 
      year = .x,
      survey = "acs1",
      geometry = FALSE
   )  
   },
   .id = "year"  # when combining results, add id var (name of list item)
) 

multi_year <- multi_year %>%
   filter(GEOID == "0644000") %>% 
   select(-moe) %>%  
   arrange(variable, NAME) %>% 
   print()