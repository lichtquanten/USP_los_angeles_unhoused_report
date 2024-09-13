#
# Authors:     BR
# Maintainers: BR
# Copyright:   2023
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policinghomelessness/processing/src/processing_lasan.r
library(CGPfunctions)
library(extrafont)
library(fuzzyjoin)
library(janitor)
library(lubridate)
library(qs)
library(readr)
library(readxl)
library(rcartocolor)
library(scales)
library(tidycensus)
library(tidyverse)
options(scipen=999)

stopifnot(requireNamespace("here", quietly = TRUE))
here <- here::here

########### input and output files ##############
# input files:
inputfiles <- list(
   lasan = "import/input/LASAN/WPIMS Data - 201910010_20230217_ver2.xls",
   bridge_coded = "processing/frozen/bridge_coded.csv",
   first_words_coded = "processing/frozen/first_words_coded.csv",
   last_words_coded = "processing/frozen/last_words_coded.csv",
   geocoded_phase1 = "processing/frozen/lasan_addresses_gecoded_phase1.csv",
   geocoded_phase2 = "processing/frozen/lasan_addresses_manually_geocoded_phase2.csv"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   bridge_to_code = "processing/output/bridge_to_code.csv",
   first_words_to_code = "processing/output/first_words_to_code.csv",
   last_words_to_code = "processing/output/last_word_to_code.csv",
   lasan_addresses_to_geocode = "processing/output/lasan_addresses_to_geocode.csv",
   manual_geocode = "processing/output/manual_to_geocode.csv",
   coordinates_out = "geo_matching/input/lasan_coords.csv",
   lasan_processed = "processing/output/lasan_processed.csv"
   
) %>% map(here)

lasan <- read_xls(inputfiles$lasan) %>% 
   clean_names()

#date
lasan$date <- as_date(lasan$case_date, format = "%d-%b-%y %H%M%p")


#floor date
lasan <- lasan %>% 
   mutate(month = floor_date(date, unit = "months"))

#mark ones with no waste
lasan <- lasan %>% 
   mutate(no_solid_waste = ifelse(is.na(solid_waste_lbs) | solid_waste_lbs < 0.5,
                            1, 0)) %>% 
   mutate(total = rowSums(across(c(solid_waste_lbs:bags_items_sent_to_storage)), na.rm = T))
table(lasan$total)

#filter out anything where LASAN didn't take anything. Not of interest.
lasan  <- lasan %>% 
   filter(total > 0)

#address cleaning to go out for geocoding
#remove * -- and --- but not -
lasan_addresses <- lasan %>% 
   select(case_id, address, cross_streets) %>% 
   mutate(address2 = str_replace(address, '\\*', ''),
          cross_street = str_replace(cross_streets, '\\*', '')) %>% 
   mutate(address2 = gsub("--", "", address2),
          cross_street = gsub("--", "", cross_street))

#separate out a bridge home - get addresses manually
bridge_to_code <- lasan_addresses %>% 
   filter(str_detect(address, 'ABH') | str_detect(cross_street, 'ABH')) 
write_csv(bridge_to_code, outputfiles$bridge_to_code)

#remove those ones from lasan
lasan_addresses <- lasan_addresses %>% 
   filter(!case_id %in% bridge_to_code$case_id)

#bring back in fixed ABH addresses
bridge_coded <- read_csv(inputfiles$bridge_coded) %>% 
   clean_names() %>% 
   mutate(case_id = as.character(case_id))

lasan_addresses <- bind_rows(lasan_addresses, bridge_coded)
rm(bridge_coded, bridge_to_code)

#now separate out first and last words
lasan_addresses <- lasan_addresses %>% 
   mutate(first = word(address2, start = 1L, sep = fixed(" ")),
          last = word(address2, start = -1L)) %>% 
   rename(address_orig = address, address = address2)

#for 0 street number, remove 0 and paste in cross street
lasan_addresses <- lasan_addresses %>% 
   mutate(address = ifelse(first == "0", str_remove(address, '(\\w+\\s+){1}'), address)) %>% 
   mutate(address = ifelse(first == "0", paste(cross_street, " and ", address, sep = ""), address))

#write out other first words to mark those where we can just paste in the cross street
t <- lasan_addresses %>% 
   group_by(first) %>% 
   summarise(count = n()) %>% 
   distinct()

write_csv(t, outputfiles$first_words_to_code)
first_coded <- read_csv(inputfiles$first_words_coded) 

lasan_addresses <- left_join(lasan_addresses, first_coded)
rm(first_coded)

#paste cross street for those we marked
lasan_addresses <- lasan_addresses %>% 
   mutate(address = ifelse(is.na(paste_cross), address, paste(cross_street, " and ", address, sep = "")))

#write out last word to mark bad zips to delete
t <- lasan_addresses %>% 
   group_by(last) %>% 
   summarise(count = n()) %>% 
   distinct()
write_csv(t, outputfiles$last_words_to_code)

#read_back last word
last <- read_csv(inputfiles$last_words_coded) %>% 
   distinct()

lasan_addresses <- left_join(lasan_addresses, last)

#for those that are marked, delete last word in address (cuz it confuses geocoder) FIX LAST WORD
lasan_addresses <- lasan_addresses %>% 
   mutate(address = ifelse(is.na(delete_last), address, word(address, 1, -2)))

#fix some that added "NA and" with NA cross street
lasan_addresses <- lasan_addresses %>% 
   mutate(address = gsub("NA and", "", address))

#write out
write_csv(lasan_addresses, outputfiles$lasan_addresses_to_geocode)

#bring back in addresses that had been sent to Google api
addresses_geocoded <- read_csv(inputfiles$geocoded_phase1) %>% 
   clean_names()
n_distinct(lasan_addresses$address)
n_distinct(addresses_geocoded$address_geo)

#select ones that we need to check and prioritize those that appear often
t <- addresses_geocoded %>% 
   group_by(to_check) %>% 
   summarise(count = n_distinct(case_id)) %>% 
   mutate(perc = count/sum(count))

t <- addresses_geocoded %>% 
   filter(to_check == T) %>% 
   group_by(address_geo) %>% 
   summarise(count = n_distinct(case_id)) %>% 
   arrange(desc(count)) %>% 
   mutate(perc = count/sum(count),
          cumperc = cumsum(perc)) 

write_csv(t, outputfiles$manual_geocode)

#bring new geocodes back in
geocoded2 <- read_csv(inputfiles$geocoded_phase2) %>% 
   select(address_geo, lat = latitude, long = longitude)

addresses_geocoded <- left_join(addresses_geocoded, geocoded2)

addresses_geocoded <- addresses_geocoded %>% 
   mutate(latitude = ifelse(!is.na(lat), lat, latitude),
          longitude = ifelse(!is.na(long), long, longitude), 
          latitude = ifelse(to_check == T & is.na(lat), NA,
                            latitude),
          longitude = ifelse(to_check == T & is.na(long), NA,
                            longitude)) 

join <- addresses_geocoded %>% 
   mutate(case_id = as.character(case_id)) %>% 
   select(case_id, latitude, longitude, formatted_address, address_geo)

lasan <- left_join(lasan, join)

#categorical variable on any bags taken
lasan <- lasan %>% 
   mutate(bags_taken = ifelse(bags_items_sent_to_storage > 0, 1, 0))

#round the coordinates
lasan <- lasan %>%
   mutate(latitude2 = as.numeric(latitude), longitude2 = longitude) %>%
   mutate(latitude = round(latitude2, 4),
          longitude = round(longitude2, 4))

#output coordinates to geo-matching
coordinates_out <- lasan %>% 
   select(latitude, longitude) %>% 
   distinct()

write_csv(coordinates_out, outputfiles$coordinates_out)

#write out processed LASAN sweeps dataset
write_csv(lasan, outputfiles$lasan_processed)

