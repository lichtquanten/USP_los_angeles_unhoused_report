#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policingunhousedness/descriptives/src/lapd_descriptives.r
library(pacman)
p_load(lubridate, readxl, readr, rcartocolor, extrafont, scales, tidycensus,
       CGPfunctions, tidyverse, qs, fuzzyjoin, sf, janitor, tigris, quantmod)
options(scipen=999)

here <- here::here

########### input and output files ##############
# input files:
inputfiles <- list(
   unhoused_count = "processing/frozen/unhoused_count.csv"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   city_census = "processing/output/city_census_data.rds",
   tract_census = "processing/output/tract_census_data.rds",
   inflation = "processing/output/yearly_cpi.csv",
   unhoused_count = "processing/output/unhoused_count.csv"
   
) %>% map(here)

# View variables

#get population data - 2020 is experimental due to covid. 
#So we'll do 2005-19 and 21 here and get 2020 separate.
years <- c(2005:2019, 2021) 
names(years) <- years
years

vars <- load_variables(2005, "acs1")

# which census variables?
my_vars <- c(
   total_pop = "B01003_001",
   pop_poverty = "B17001_002",
   white_non_hisp = "B03002_003",
   black = "B03002_004",
   hisp = "B03001_003"
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

str(multi_year)


#create 2020 avg between two years.
t <- multi_year %>%
   filter(year == "2019" | year == "2021") %>% 
   summarise_if(is.numeric, mean) %>%
   mutate(year = "2020", NAME = "Los Angeles city, California", GEOID = "0644000")

multi_year <- bind_rows(multi_year, t)

#duplicate 2021 into 2022
t <- multi_year %>% 
   filter(year == "2021") %>% 
   mutate(year = "2022")
multi_year <- bind_rows(multi_year, t)

write_rds(multi_year, outputfiles$city_census)


#get same for census tracts but only for recent years
years <- c(2016:2019, 2021) 
names(years) <- years
years

multi_year <- map_dfr(
   years,
   ~ {get_acs(
      geography = "tract",
      state = "CA",
      variables = my_vars,
      year = .x,
      survey = "acs5",
      geometry = FALSE
   )  
   },
   .id = "year"  # when combining results, add id var (name of list item)
) 


#create 2020 avg between two years.

multi_year <- multi_year %>% 
   select(-moe) %>% 
   pivot_wider(names_from = variable, values_from = estimate)

t <- multi_year %>%
   group_by(GEOID) %>% 
   filter(year == "2019" | year == "2021") %>% 
   summarise_if(is.numeric, mean) %>%
   mutate(year = "2020")

multi_year <- bind_rows(multi_year, t)

#duplicate 2021 into 2022
t <- multi_year %>% 
   filter(year == "2021") %>% 
   mutate(year = "2022")
multi_year <- bind_rows(multi_year, t)

write_rds(multi_year, outputfiles$tract_census)


#inflation.  Get the csv download link from here
#https://fred.stlouisfed.org/series/CPIAUCSL

monthly_cpi <-
   read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1138&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2023-05-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-06-21&revision_date=2023-06-21&nd=1947-01-01", 
            header = TRUE)
monthly_cpi$cpi_year <- year(monthly_cpi$DATE)

#get annual avg
yearly_cpi <- monthly_cpi %>% 
   group_by(cpi_year) %>%
   summarize(cpi = mean(CPIAUCSL))

write_csv(yearly_cpi, outputfiles$inflation)

#process unhoused count
unhoused_count <- read_csv(inputfiles$unhoused_count)
str(unhoused_count)
unhoused_count$year <- as.character(unhoused_count$Year)

t <- unhoused_count %>%
   group_by(race, Geography) %>% 
   filter(year == "2020" | year == "2022") %>% 
   summarise_if(is.numeric, mean) %>%
   mutate(num = round(num, 0)) %>% 
   mutate(year = "2021") %>% 
   select(-Year)

unhoused_count <- bind_rows(unhoused_count, t)

write_csv(unhoused_count, outputfiles$unhoused_count)
