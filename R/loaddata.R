library(tidyverse)
library(auk)

# load all of the data in the auk_ebd path 
write_data_main <- function(files){
   
   ebd_data <- NULL
   f_out <- 'data/ebd_filtered.txt'
   
   # columns to keep for filtering
   cols <- c('group_identifier', 'sampling_event_identifier', 'observer_id',
             'scientific_name', 'observation_count', 'common_name',
             'county_code', 'locality', 'locality_id', 'locality_type',
             'latitude', 'longitude', 'observation_date',
             'time_observations_started', 'duration_minutes', 'effort_distance_km', 
             'all_species_reported', 'state', 'county')
   
   for (f in files){
      
      # auk filtering
      data <- f %>%
         auk_ebd() %>%
         auk_complete() %>%
         auk_date(date = c('*-05-01', '*-05-15')) %>%
         auk_protocol(c("Stationary", "Traveling")) %>% 
         auk_filter(file = f_out, overwrite = TRUE, keep = cols) %>%
         read_ebd()
      
      # filtering that auk cannot do 
      filtered <- data %>%
         filter(locality_type == "H")
      
      # join to existing data 
      if (is.null(ebd_data)){
         ebd_data <- filtered
      } else {
         ebd_data <- rbind(ebd_data, filtered)
      }
      
   }
   
   # save the filtered data
   write_csv(ebd_data, 'data/filtered.csv')
   
}

# county list 
write_county_list <- function(data) {
   data %>% 
      group_by(state, county) %>% 
      summarise(county_code = first(county_code),
                .groups = 'drop') %>% 
      write_csv('data/counties.csv') 
}

# hotspot list 
write_hotspot_list <- function(data) {
   data %>% 
      group_by(county_code, locality) %>% 
      summarise(latitude = first(latitude),
                longitude = first(longitude),
                .groups = 'drop') %>%
      write_csv('data/hotspots.csv')
}


if(FALSE){
   
   if (is.na(auk_get_ebd_path())) {
      stop("please set ebd path with auk_set_ebd_path(YOUR_EBD_PATH)")
   }
   
   files <- list.files(auk_get_ebd_path(), pattern='*.txt')
   write_data_main(files)
   
   data <- read_csv('data/filtered.csv')
   write_county_list(data)
   write_hotspot_list(data)
   
   # clear global variables 
   rm(list = ls())
   
}

