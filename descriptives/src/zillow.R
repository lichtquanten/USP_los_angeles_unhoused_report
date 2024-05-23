#
# Authors:     BR
# Maintainers: BR
# Copyright:   2023
# =========================================
# OneDrive-HumanRightsWatchInc/20220101_USA_USP_LosAngelesUnhoused/descriptives/src/zillow.r"
library(pacman)
p_load(lubridate, readxl, readr, rcartocolor, extrafont, scales, tidycensus,
       CGPfunctions,  qs, fuzzyjoin, sf, janitor, tigris,
       mapview, leaflet, tmap, tidyverse)
options(scipen=999)
options(tigris_use_cache = TRUE)

here <- here::here

source("../docs/plot_themes.R")


########### input and output files ##############
# input files:
inputfiles <- list(
   house_prices_neighborhood = "import/input/zillow/Neighborhood_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
   house_prices = "import/input/zillow/City_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
   house_zip = "import/input/zillow/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv",
   rent_prices = "import/input/zillow/City_zori_uc_sfrcondomfr_sm_month.csv",
   rent_zip = "import/input/zillow/Zip_zori_uc_sfrcondomfr_sm_month.csv"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   house_prices = "descriptives/output/zillow/house_prices.csv",
   house_perc_change = "descriptives/output/zillow/house_perc_change.csv",
   house_neighborhood = "descriptives/output/zillow/house_neighborhood.csv",
   rent_prices = "descriptives/output/zillow/rent_prices.csv",
   rent_change = "descriptives/output/zillow/rent_change.csv"

) %>% map(here)


#read_in
house <- read_csv(inputfiles$house_prices)
rent <- read_csv(inputfiles$rent_prices)
rent_zip <- read_csv(inputfiles$rent_zip)
house_neighborhood <- read_csv(inputfiles$house_prices_neighborhood)
house_zip <- read_csv(inputfiles$house_zip)


#house prices top 50 cities
house <- house %>% 
   filter(SizeRank < 50)

perc_change <- house %>% 
   select(RegionName, State, `2000-01-31`:`2024-01-31`) %>% 
   pivot_longer(cols = `2000-01-31`:`2024-01-31`, values_to = "price", names_to = "month")

perc_change <- perc_change %>% 
   group_by(RegionName) %>% 
   mutate(perc_change = ((price/price[month == "2000-01-31"] - 1)*100)) 

t <- perc_change %>% 
   select(RegionName, month, perc_change) %>% 
   pivot_wider(names_from = RegionName, values_from = perc_change) %>% 
   mutate(avg_50 = rowMeans(select(., `New York`:`Bakersfield`), na.rm = T)) %>% 
   select(-Rochester)

write_csv(t, outputfiles$house_perc_change)

t <- perc_change %>% 
   filter(month == "2024-01-31") %>% 
   select(RegionName, price) %>% 
   arrange(desc(price)) %>% 
   ungroup() %>% 
   slice_head(n=15)
write_csv(t, outputfiles$house_prices)

#house prices neighborhood
house_neighborhood <- house_neighborhood %>% 
   filter(City == "Los Angeles") %>% 
   select(latest = "2024-01-31", RegionName) %>% 
   arrange(desc(latest))

house_zip <- house_zip %>% 
   filter(City == "Los Angeles")

#rent perc change
rent <- rent %>% 
   filter(SizeRank < 50)

perc_change <- rent %>% 
   select(RegionName, State, `2015-01-31`:`2024-01-31`) %>% 
   pivot_longer(cols = `2015-01-31`:`2024-01-31`, values_to = "price", names_to = "month")

perc_change <- perc_change %>% 
   group_by(RegionName) %>% 
   mutate(perc_change = ((price/price[month == "2015-01-31"] - 1)*100)) 

t <- perc_change %>% 
   select(RegionName, month, perc_change) %>% 
   pivot_wider(names_from = RegionName, values_from = perc_change) %>% 
   mutate(avg_50 = rowMeans(select(., `New York`:`Bakersfield`), na.rm = T)) %>% 
   select(-Rochester)

write_csv(t, outputfiles$rent_change)
