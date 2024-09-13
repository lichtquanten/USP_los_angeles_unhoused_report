#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# OneDrive-HumanRightsWatch/HRW/HRW-us-losangeles-policinghomelessness/descriptives/src/joined_descriptives.r


library(CGPfunctions)
library(extrafont)
library(fuzzyjoin)
library(janitor)
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
 roomkey = "processing/output/lahsa/roomkey_processed.csv"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   daily_count = "descriptives/output/lahsa/daily_count.csv",
   flow = "descriptives/output/lahsa/flow.csv",
   facilities_perc_of_max = "descriptives/output/lahsa/facs_perc_of_max.csv",
   perc_exit_perm = "descriptives/output/lahsa/perc_exit_perm.csv",
   daily_capacity = "descriptives/output/lahsa/daily_capacity.csv",
   length_stay = "descriptives/output/lahsa/length_of_stay.csv",
   facilities_table = "descriptives/output/lahsa/facilities_table.csv",
   echo_figure = "descriptives/output/lahsa/echo_figure.csv",
   macarthur_figure = "descriptives/output/lahsa/macarthur_figure.csv",
   length_plot = "descriptives/output/lahsa/length_stay_plot.pdf",
   exits_by_fac = "descriptives/output/lahsa/exits_by_fac.csv"


) %>% map(here)


#read in 
roomkey <- read_csv(inputfiles$roomkey)

#how many people in on each day?

daily_count <- roomkey %>% 
   filter(!is.na(enrollments_project_exit_date)) %>% 
   mutate(index_client_id, day = map2(as.Date(enrollments_project_start_date), 
                                      as.Date(enrollments_project_exit_date), 
                            ~ seq(.x, .y, by = 'day'))) %>%
   unnest(c(day)) %>% 
   group_by(day) %>%
   summarise(number_of_ids = n_distinct(index_client_id)) %>% 
   filter(day < "2023-06-13")

write_csv(daily_count, outputfiles$daily_count)

#number of days to 1000
daily_count <- daily_count  %>% 
   mutate(cumulative_days = row_number())

#average age
mean(roomkey$clients_current_age, na.rm = T)
median(roomkey$clients_current_age, na.rm = T)

t <- roomkey %>% 
   group_by(destination_is_permanent) %>% 
   summarise(median_age = median(clients_current_age, na.rm = T))


t <- roomkey %>% 
   group_by(destination_is_permanent, race) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   mutate(perc = count/sum(count))


#race
race <- roomkey %>% 
   group_by(race) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   mutate(perc = count/sum(count))

#number starting and leaving on any day
days1 <- roomkey %>% 
   group_by(day = enrollments_project_start_date) %>% 
   summarise(num_entering = n_distinct(index_client_id))

days2 <- roomkey %>% 
   group_by(day = enrollments_project_exit_date) %>% 
   summarise(num_leaving = n_distinct(index_client_id))

days1 <- left_join(days1, days2)

days_out <- days1 %>% 
   mutate(net_flow = num_entering - num_leaving) %>% 
   filter(day > "2020-10-01" & day < "2021-09-30") %>% 
   mutate(rolling_7day_avg = zoo::rollmean(net_flow, k = 7, fill = NA))

write_csv(days_out, outputfiles$flow)

#daily count per facility
daily_count <- roomkey %>% 
   filter(!is.na(enrollments_project_exit_date)) %>% 
   group_by(hotel) %>% 
   mutate(index_client_id, day = map2(as.Date(enrollments_project_start_date), as.Date(enrollments_project_exit_date), 
                               ~ seq(.x, .y, by = 'day'))) %>%
   unnest(c(day)) %>% 
   group_by(hotel, day) %>%
   summarise(count = n_distinct(index_client_id))

#which facilities lost people during spike days?
spike_day <- daily_count %>% 
   filter(day == "2020-11-29" | day == "2020-11-30" | day == "2020-12-01" ) %>% 
   pivot_wider(values_from = count, names_from = day) %>% 
   adorn_totals("row")

#clients that left that day?
clients <- roomkey %>% 
   mutate(date = as_date(enrollments_project_exit_date)) %>% 
   filter(date == "2020-11-30") %>% 
   group_by(destination_is_permanent) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   mutate(perc = count/sum(count))

spike_day <- daily_count %>% 
   filter(day == "2020-12-31" | day == "2021-01-01" | day == "2021-01-02") %>% 
   pivot_wider(values_from = count, names_from = day) %>% 
   adorn_totals("row")

clients <- roomkey %>% 
   mutate(date = as_date(enrollments_project_exit_date)) %>% 
   filter(date == "2021-01-01") %>% 
   group_by(destination_is_permanent) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   mutate(perc = count/sum(count))

spike_day <- daily_count %>% 
   filter(day == "2021-03-31" | day == "2021-04-01" | day == "2021-04-02") %>% 
   pivot_wider(values_from = count, names_from = day) %>% 
   adorn_totals("row")

clients <- roomkey %>% 
   mutate(date = as_date(enrollments_project_exit_date)) %>% 
   filter(date == "2021-04-01") %>% 
   group_by(destination_is_permanent) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   mutate(perc = count/sum(count))


#Facilities 
facs <- daily_count %>% 
   group_by(hotel) %>% 
   filter(count == max(count)) %>% 
   slice(1) %>% 
   select(hotel, max_capacity = count)

facilities_table <- daily_count %>% 
   group_by(hotel) %>% 
   summarise(start_date = min(day),
             end_date = max(day),
             median_count = median(count))

facilities_table <- left_join(facilities_table, facs)

facilities_table <- facilities_table %>% 
   mutate(weeks_in_roomkey = round(difftime(start_date, end_date, units = "weeks") * -1, 0),
          median_percent_of_capacity = median_count/max_capacity )

write_csv(facilities_table, outputfiles$facilities_table)


#facility level % of max capacity
daily_count <- left_join(daily_count, facs)


daily_count <- daily_count %>% 
   mutate(perc_of_max = count/max_capacity)

top_5 <- facs %>% 
   ungroup() %>% 
   slice_max(n = 5, order_by = max_capacity)
rm(facs)

daily_out <- semi_join(daily_count, top_5) 

daily_out <- daily_out %>% 
   select(hotel, day, perc_of_max) %>% 
   mutate(perc_of_max = perc_of_max * 100) %>% 
   pivot_wider(values_from = perc_of_max, names_from = hotel) 

write_csv(daily_out, outputfiles$facilities_perc_of_max)

grand <- daily_count %>% 
   filter(hotel == "Grand Hotel")

#total percentage of capacity
daily_capacity <- daily_count %>% 
   group_by(day) %>% 
   summarise(total_capacity = sum(max_capacity),
             daily_in = sum(count)) %>% 
   mutate(perc_of_max = daily_in/total_capacity) %>% 
   mutate(hotel = "Total")

write_csv(daily_capacity, outputfiles$daily_capacity)

#average in 2021
avg <- daily_capacity %>% 
   filter(day > "2020-12-31" & day < "2022-01-01") %>% 
   summarise(avg_count = mean(daily_in),
             avg_cap = mean(perc_of_max))

#macarthur sweep
macarthur <- daily_count %>% 
   filter(hotel == "Mayfair Hotel" )

macarthur <- bind_rows(macarthur, daily_capacity)
macarthur <- bind_rows(macarthur, grand)

macarthur <- macarthur %>% 
   select(day, hotel, perc_of_max) %>% 
   pivot_wider(names_from = "hotel", values_from = "perc_of_max")
write_csv(macarthur, outputfiles$macarthur_figure)

#echo park
echo <- bind_rows(daily_capacity, grand) %>% 
   select(day, hotel, perc_of_max) %>% 
   pivot_wider(names_from = "hotel", values_from = "perc_of_max")

write_csv(echo, outputfiles$echo_figure)

t <- roomkey %>% 
   group_by(hotel) %>% 
   summarise(count = n_distinct(index_client_id))

t <- roomkey %>% 
   filter(still_in == 0) %>% 
   group_by(destination_is_permanent) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   mutate(perc = count/sum(count))


#perc by month
t <- roomkey %>% 
   filter(still_in == 0) %>% 
   group_by(month_end, destination_is_permanent) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   mutate(perc = count/sum(count)) %>% 
   filter(destination_is_permanent == "No") %>% 
   mutate(percent_exit_permanent = 1 - perc)
write_csv(t, outputfiles$perc_exit_perm)

#numbers by day - what is happening in jan 23
t <- roomkey %>% 
   filter(still_in == 0) %>% 
   group_by(enrollments_project_exit_date) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   mutate(perc = count/sum(count))

t <- roomkey %>% 
   filter(still_in == 0) %>% 
   group_by(month_end) %>% 
   summarise(count = n_distinct(index_client_id))

#overall length of stay
t <- roomkey %>% 
   group_by(grouping1) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   ungroup() %>% 
   mutate(perc = count/sum(count))

t1 <- roomkey %>% 
   group_by(less_48) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   mutate(perc = count/sum(count))


t2 <- roomkey %>% 
   group_by(less_week) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   mutate(perc = count/sum(count))

t <- bind_rows(t, t1, t2)
write_csv(t, outputfiles$length_stay)

#length stay and housing
t <- roomkey %>% 
   filter(still_in == 0) %>% 
   group_by(destination_is_permanent, days_in) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   pivot_wider(names_from = destination_is_permanent, values_from = count)


#plot
p <- roomkey %>%
   filter(still_in == 0) %>% 
   ggplot( aes(x = days_in, fill=destination_is_permanent)) +
   geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
   scale_fill_manual(values=c("#69b3a2", "#404080")) +
   xlim(NA, 600) +
   theme(legend.position = c(0.6, 0.7)) +
     theme(panel.grid = element_blank()) + 
   labs(fill = "Exited to Permanent Housing",
        x = "Days in PRK hotels",
        y = "Count of people",
        title = "Length of Project Roomkey Stay by Exit Type")
p

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)
ggsave(p, file = outputfiles$length_plot, 
       units="in", width = 6, height = 6, dpi = 600)
embed_fonts(outputfiles$length_plot) 

#top 10 facs
top_10 <- facilities_table %>% 
   arrange(desc(max_capacity)) %>% 
   slice(1:10)

t <- roomkey %>% 
   filter(still_in == 0) %>% 
   filter(hotel %in% top_10$hotel) %>% 
   group_by(hotel, destination_is_permanent) %>% 
   summarise(count = n_distinct(index_client_id)) %>% 
   mutate(perc = count/sum(count)) %>% 
   filter(destination_is_permanent == "Yes") %>% 
   arrange(desc(perc)) %>% 
   select(hotel, perc)

write_csv(t, outputfiles$exits_by_fac)
rm(top_10, t)

#average days in
t <- roomkey %>% 
   filter(still_in == 0) %>% 
   group_by(destination_is_permanent) %>% 
   summarise(mean = mean(days_in),
             median = median(days_in))

#check 10/12/21 for people from Main between 5th and 7th
t <- roomkey %>% 
   filter(enrollments_project_start_date == "2021-10-12")

t <- roomkey %>% 
   group_by(enrollments_project_start_date) %>% 
   summarise(count = n_distinct(index_client_id))

   
