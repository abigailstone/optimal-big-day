library(tidyverse)
library(auk)

if (is.na(auk_get_ebd_path())) {
   stop("please set ebd path with auk_set_ebd_path(YOUR_EBD_PATH)")
}

input_file <- file.path('ebd_US-VT_201103_202103_relMar-2021.txt')
# input_file <- file.path('ebd_US-VT-001_201103_202103_relMar-2021.txt') # this is the most recent data
f_out <- 'data/ebd_filtered.txt'

# columns to keep for filtering
cols <- c('group_identifier', 'sampling_event_identifier', 'observer_id',
          'scientific_name', 'observation_count', 'common_name',
          'county_code', 'locality', 'locality_id', 'locality_type',
          'latitude', 'longitude', 'observation_date',
          'time_observations_started', 'duration_minutes', 'effort_distance_km', 
          'all_species_reported', 'state', 'county')

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
get_county_list <- function(){
   countycodes <- ebird_data_filtered %>% 
      group_by(state, county) %>% 
      summarise(county_code = first(county_code)) %>% 
      write_csv('data/counties.csv') 
}

# hotspot list 
get_hotspot_list <- function(){
   loc_list <- ebird_data_filtered %>% 
      group_by(county_code, locality) %>% 
      summarise(latitude = first(latitude),
                longitude = first(longitude)) %>%
      write_csv('data/hotspots.csv')
   
}


if(FALSE){
   get_county_list()
   get_hotspot_list()
}

# clear global variables 
rm(list = ls())
