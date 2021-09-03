# new methods for processing entire US ebd, written by Myles on 9/2/21
# lots of redundancy with existing methods in fct_loaddata.R, but I didn't want
# to remove all that code

library(tidyverse)
library(auk)

# use auk commands to trim down big ebd.txt file
# 
# in: ebd_path (e.g. 212gb file)
# out: writes a filtered txt file to ebd_filtered_path
# month: pick 'may' or 'oct'
filter_with_auk <- function(ebd_path, ebd_filtered_path, month) {
  
  # columns to keep for filtering
  cols <- c('group_identifier', 'sampling_event_identifier', 'observer_id',
            'scientific_name', 'observation_count', 'common_name',
            'county_code', 'locality', 'locality_id', 'locality_type',
            'latitude', 'longitude', 'observation_date',
            'time_observations_started', 'duration_minutes', 'effort_distance_km', 
            'state', 'county')
  
  
  # get data from first half of May or October, when big day usually? is
  if (month == 'may') {
    dates <- c('*-05-01', '*-05-15')
  } else if (month == 'oct') {
    dates <- c('*-10-01', '*-10-15')
  } else {
    stop('Month ', month, ' not supported')
  }
  
  ebd_path %>%
    auk::auk_ebd() %>%
    auk::auk_complete() %>%
    auk::auk_date(dates) %>%
    auk::auk_protocol(c("Stationary", "Traveling")) %>%
    auk::auk_filter(file = ebd_filtered_path, overwrite = FALSE, keep = cols)
}

# do additional filtering in memory
# ebd_filtered_path: this should be the same path as the output of filter_with_auk
# out: returns (does not write) the filtered ebd tibble
filter_in_memory <- function(ebd_filtered_path) {
  
  # read auk output into memory
  ebd <- readr::read_tsv(ebd_filtered_path)
  
  # NOTE: read_ebd seems to be slow, so we don't use it. Usually, it would handle:
  # (1) fixing column names 
  # (2) getting only species level taxa (with auk_rollup)
  # (3) duplicated observations from group checklists 
  
  
  # fix column names
  colnames(ebd) <- colnames(ebd) %>% 
    stringr::str_replace_all(' ', '_') %>%
    stringr::str_to_lower()
  
  # list of scientific names for species level taxon
  ebird_species <- auk::ebird_taxonomy %>% 
    filter(category == 'species') %>%
    pull(common_name)
  
  # only keep checklists recorded at hotspots
  # and observations at species level
  ebd <- ebd %>% 
    filter(locality_type == 'H',
           common_name %in% ebird_species)
  
  # remove duplicate rows originating from group checklists
  ebd <- ebd %>% mutate(temp = stringr::str_c(sampling_event_identifier, 
                                              common_name))
  ebd <- ebd[!duplicated(ebd$temp),]
  ebd <- ebd %>% select(-c(temp))
  
  ebd
}

# do all filtering on the ebird dataset and return a tibble
all_filtering <- function(ebd_path, ebd_filtered_path, month) {
  
  filter_with_auk(ebd_path, ebd_filtered_path, month)
  
  filter_in_memory(ebd_filtered_path)
}

if (FALSE) {
  
  print(paste('start time:', Sys.time()))
  
  devtools::load_all()

  ebd_filtered <- all_filtering('../ebd/ebd_US_relApr-2021.txt', 
                                'data_local/ebd_filtered.txt',
                                'oct')

  # optionally, save progress to disk with one of these
  # save(ebd_filtered, 'data_local/ebd_filtered.rda')
  # readr::write_csv(ebd_filtered, 'data_local/ebd_filtered.csv')

  print(paste('filtering finished at:', Sys.time()))

  print(paste('starting writing probs at:', Sys.time()))
  
  write_probs_all_counties(ebd_filtered)
  
  write_county_list(ebd_filtered)
  
  write_hotspot_list(ebd_filtered)
  
  print(paste('all processing finished at:', Sys.time()))
  
}
  


