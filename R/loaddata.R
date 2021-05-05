library(tidyverse)
library(auk)

if (is.na(auk_get_ebd_path())) {
   stop("please set ebd path with auk_set_ebd_path(YOUR_EBD_PATH)")
}

input_file <- file.path('ebd_US-VT_201001_202011_relSep-2020.txt')
# input_file <- file.path('ebd_US-VT-001_201103_202103_relMar-2021.txt') # this is the most recent data
f_out <- 'data/ebd_filtered.txt'

# columns to keep for filtering
cols <- c('group_identifier', 'sampling_event_identifier', 'observer_id',
          'scientific_name', 'observation_count', 'category', 'common_name',
          'county_code', 'locality', 'locality_id', 'locality_type',
          'latitude', 'longitude', 'observation_date', 'protocol_code', 
          'time_observations_started', 'duration_minutes', 'effort_distance_km', 
          'all_species_reported', 'approved', 'reviewed', 'state', 'county')

# auk filtering
ebird_data <- input_file %>%
   auk_ebd() %>%
   auk_complete() %>%
   auk_date(date = c('*-05-01', '*-05-15')) %>%
   auk_protocol(c("Stationary", "Traveling")) %>% 
   auk_filter(file = f_out, overwrite = TRUE, keep = cols) %>%
   read_ebd()

# filtering that auk cannot do 
ebird_data_filtered <- ebird_data %>%
   filter(locality_type == "H") %>%
   write_csv('data/filtered.csv')

# county list 
countycodes <- ebird_data_filtered %>% 
   group_by(state, county) %>% 
   summarise(county_code = first(county_code)) %>% 
   write_csv('data/counties.csv')

# hotspot list 
loc_list <- ebird_data_filtered %>% 
   group_by(county_code, locality) %>% 
   summarise(latitude = first(latitude),
             longitude = first(longitude)) %>%
   write_csv('data/hotspots.csv')

# clear global variables 
rm(list = ls())
