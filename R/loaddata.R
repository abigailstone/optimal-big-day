library(tidyverse)
library(auk)

# assumes that ebd path has been set: 
# auk_set_ebd_path(YOUR_EBD_PATH, overwrite=TRUE)
input_file <- file.path('ebd_US-VT_201001_202011_relSep-2020.txt')
# input_file <- file.path('ebd_US-VT-001_201103_202103_relMar-2021.txt') # this is the most recent data
f_out <- 'data/ebd_filtered.txt'

# auk filtering
ebird_data <- input_file %>%
   auk_ebd() %>%
   auk_complete() %>%
   auk_date(date = c('*-05-01', '*-05-15')) %>%
   auk_filter(file = f_out, overwrite = TRUE) %>%
   read_ebd()

cols <- c('category', 'common_name', 'observation_count',
         'county_code', 'locality', 'locality_id', 'locality_type',
         'latitude', 'longitude', 'observation_date', 'time_observations_started',
         'protocol_code', 'duration_minutes', 'effort_distance_km', 'all_species_reported',
         'approved', 'reviewed', 'checklist_id')

ebird_data_filtered <- ebird_data %>%
   
   filter(all_species_reported==TRUE, # complete lists
          protocol_code %in% c('P21', 'P22'), # travelling or stationary
          category == "species", # omit sp. %>%
          observation_count != "0",  # "X" is fine, we just want to know presence
          locality_type == "H",
          
          ) %>% 
          
   select(all_of(cols))

write_csv(ebird_data_filtered, 'data/filtered.csv')




