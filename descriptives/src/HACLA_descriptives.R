# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policinghomelessness/descriptives/src/HACLA_descriptives.r
library(pacman)
p_load(lubridate, readxl, readr, extrafont, scales, tidyverse, tidycensus,
       purrr, quantmod)

options(scipen=999)

here <- here::here

########### input and output files ##############
# input files:

inputfiles <- list(
HACLA_budget = "import/input/HACLA/CPRA - Funding.xlsx",
housing_units = "import/input/HACLA/UNit History 1940 - 2015.xlsx",
yearly_cpi = "processing/output/yearly_cpi.csv",
census = "processing/output/city_census_data.rds"
     
      ) %>% map(here)

#output files. 
outputfiles <- list(
        housing_per = "descriptives/output/HACLA_units_per.csv",
        budget_housing = "descriptives/output/HACLA_funding_inflation.csv",
        annual_pops = "descriptives/output/city_census_pops.csv"

         ) %>% map(here)


#read in
budget <- read_xlsx(inputfiles$HACLA_budget, sheet = 2)
         
units <- read_xlsx(inputfiles$housing_units, sheet = 3)
      

#bring in inflation, set index year and multiply against budget values
yearly_cpi <- read_csv(inputfiles$yearly_cpi) %>% 
   mutate(adj_factor = cpi/cpi[cpi_year == 2022]) %>% 
   rename(year = cpi_year)
   
budget <- left_join(budget, yearly_cpi)

budget <- budget %>% 
   mutate(fed_grants_adjusted = `Subtotal HUD PHA Grants` * adj_factor,
          local_grants_adjusted = `Subtotal State and Local Grants` * adj_factor,
          total_adjusted = Total * adj_factor,
          fed_perc = fed_grants_adjusted/total_adjusted)

budget_out <- budget %>% 
   select(year, total_adjusted)

write_csv(budget_out, outputfiles$budget_housing)

#units per budget
units <- left_join(units, budget_out)

units <- units %>% 
   mutate(dollars_per_unit = total_adjusted/total_units,
          year = as.character(year))

#units per pop and poverty
census <- read_rds(inputfiles$census) 

census <- census %>% 
   select(year, total_pop, pop_poverty)

units <- left_join(units, census)

units <- units %>% 
   mutate(units_per_population = total_pop/total_units,
          units_per_pov = pop_poverty/total_units)

units_out <- units %>% 
   select(year, units_per_population, units_per_pov) %>% 
   filter(!is.na(units_per_population))

write_csv(units_out, outputfiles$housing_per)

#simple percentage of pop in poverty
census <- census %>% 
   mutate(perc_below_poverty = pop_poverty/total_pop)
