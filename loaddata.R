library(tidyverse)
library(auk)

# assumes that ebd path has been set: 
# auk_set_ebd_path(YOUR_EBD_PATH, overwrite=TRUE)
input_file <- file.path('ebd_US-VT_201001_202011_relSep-2020.txt')
f_out <- 'ebd_filtered.txt'

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
         'approved', 'reviewed')

ebird_data_filtered <- ebird_data %>%
   # complete lists
   filter(all_species_reported==TRUE) %>% 
   # traveling or stationary
   filter(protocol_code %in% c('P21', 'P22')) %>%
   select(all_of(cols))

write.csv(ebird_data_filtered, file='filtered.csv')
