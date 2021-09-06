# load all of the data in the auk_ebd path 
#' @importFrom rlang .data
#' @importFrom magrittr %>%
write_data_main <- function(files, write = TRUE) {
   
   ebd_data <- NULL
   f_out <- 'data_local/ebd_filtered.txt'
   
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
         auk::auk_ebd() %>%
         auk::auk_complete() %>%
         auk::auk_date(date = c('*-05-01', '*-05-15')) %>%
         auk::auk_protocol(c("Stationary", "Traveling")) %>% 
         auk::auk_filter(file = f_out, overwrite = TRUE, keep = cols) %>%
         auk::read_ebd()
      
      # filtering that auk cannot do 
      filtered <- data %>%
         dplyr::filter(.data$locality_type == "H")
      
      # join to existing data 
      if (is.null(ebd_data)){
         ebd_data <- filtered
      } else {
         ebd_data <- rbind(ebd_data, filtered)
      }
      
   }
   
   if (write) {
      # save the filtered data
      readr::write_csv(ebd_data, 'data_local/filtered.csv')
   } else {
      ebd_data
   }
}


# county list 
#' @importFrom rlang .data
write_county_list <- function(data, write = TRUE) {
   
   # we only want county codes for which there is a prob_per_loc
   countycodes <- 
      stringr::str_subset(list.files('data_local/'), 
                          '_prob_per_loc.csv') %>%
      stringr::str_sub(1, 9)
   
   counties <- data %>% 
      dplyr::group_by(.data$state, .data$county) %>% 
      dplyr::summarise(county_code = dplyr::first(.data$county_code),
                .groups = 'drop') %>% 
      dplyr::filter(county_code %in% countycodes)
   
   if (write) {
      readr::write_csv(counties, 'data_local/counties.csv') 
   } else {
      counties
   }
      
}

# hotspot list 
#' @importFrom rlang .data
write_hotspot_list <- function(data, write = TRUE) {
   
   # get all the hotspots in the data - inluding locality ID and coordinates
   hotspots <- data %>% 
      dplyr::group_by(.data$county_code, .data$locality) %>% 
      dplyr::summarise(
         locality_id = dplyr::first(.data$locality_id),
         latitude = dplyr::first(.data$latitude),
         longitude = dplyr::first(.data$longitude),
         .groups = 'drop')
   
   # get a list of valid hotspots based on the prob_per_locs
   valid_hotspots <-  
      stringr::str_subset(list.files('data_local/'), 
                                     '_prob_per_loc.csv') %>% 
      lapply(function(x) {
         
         county_code <- stringr::str_sub(x, 1, 9)
            
         readr::read_csv(paste0('data_local/', x),
                         show_col_types = FALSE) %>%
            dplyr::transmute(locality,
                      county_code) }) %>%
      dplyr::bind_rows()
   
   # join the two to get a list of all valid hotspots, complete with
   # locality ID and coordinates
   hotspots <- dplyr:::inner_join(valid_hotspots, 
                                  hotspots, 
                                  by = c('county_code', 'locality'))
      
   if (write) {
      readr::write_csv(hotspots, 'data_local/hotspots.csv')
   } else {
      hotspots
   }
      
}


if(FALSE){
   
   if (is.na(auk::auk_get_ebd_path())) {
      stop("please set ebd path with auk::auk_set_ebd_path(YOUR_EBD_PATH)")
   }
   
   files <- list.files(auk::auk_get_ebd_path(), pattern='ebd.*.txt')
   write_data_main(files)
   
   data <- readr::read_csv('data_local/filtered.csv')
   write_county_list(data)
   write_hotspot_list(data)
   
   # clear global variables 
   rm(list = ls())
   
}

