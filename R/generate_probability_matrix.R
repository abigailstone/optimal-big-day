# filtered data to species/hotspot probability matrix

#' Calculate effort information for each hotspot
#' 
#' @param observations A tibble of eBird data
#' @return A tibble with statistics on the sampling effort at each hotspot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
effort_per_hotspot <- function(observations) {
   observations %>% 
      # filter(county_code == 'US-VT-001') %>%
      dplyr::mutate(effort_distance_km = ifelse(is.na(.data$effort_distance_km), 0, .data$effort_distance_km)) %>% # handle stationary lists
      dplyr::distinct(.data$checklist_id, .keep_all = TRUE) %>%
      dplyr::group_by(.data$locality) %>% 
      dplyr::summarize(
         n_checklists = dplyr::n(),
         total_time = sum(.data$duration_minutes),
         med_time = stats::median(.data$duration_minutes),
         iqr_time = stats::IQR(.data$duration_minutes),
         total_distance = sum(.data$effort_distance_km)
      ) %>% 
      dplyr::mutate(time_per_checklist = .data$total_time / .data$n_checklists) %>% 
      dplyr::filter(.data$n_checklists > 1)
}

#' Calculate how many times each species was reported at each hotspot
#' 
#' @param observations A tibble of eBird data
#' @return A tibble with number of checklists reporting each species at each hotspot
#' @importFrom rlang .data
#' @importFrom magrittr %>%
n_observations_per_hotspot <- function(observations) {
   observations %>%
      dplyr::group_by(.data$locality, .data$common_name) %>%
      dplyr::summarize(n_sp_loc = dplyr::n(), .groups = "drop") %>%
      tidyr::spread(key = .data$common_name, value = .data$n_sp_loc, fill = 0)
}

#' Calculate probability of seeing each species at each hotspot
#' 
#' @param observations A tibble of eBird data
#' @return A matrix with probability of seeing each species at each hotspot
probability_matrix <- function(observations) {
   
   effort_per_loc <- effort_per_hotspot(observations)
   counts_per_loc <- n_observations_per_hotspot(observations)
   
   # join species frequency and effort information
   counts_and_effort_per_loc <- effort_per_loc %>%
      dplyr::left_join(counts_per_loc, by = "locality")   
   
   counts_and_effort_per_loc %>% 
      dplyr::mutate_at(
         
         # for only species columns (exclude locality and effort columns)
         .vars = setdiff(colnames(counts_and_effort_per_loc), 
                                  colnames(effort_per_loc)),
                
          # divide by the number of checklists at that location
         .funs = list(~ . / n_checklists))
}


#' Save a single prob_per_loc matrix for a specified county
#' 
#' @param observations A tibble of eBird data
#' @param loc A string denoting the county code
#' @return invisibly returns the prob_per_loc matrix
#' @importFrom rlang .data
write_prob_per_loc <- function(observations, loc){
   
   # filter to this county
   observations <- dplyr::filter(observations, .data$county_code == loc)
   
   # compute probability matrix
   prob_per_loc <- probability_matrix(observations)
   
   # assumes that a data folder has been created in current working directory
   readr::write_csv(prob_per_loc, paste('data/', loc, '_prob_per_loc.csv', sep=''))
   
}

#' Save a prob_per_loc matrix for every county in the dataset
#' 
#' @param observations A tibble of eBird data
#' @return invisibly returns the last prob_per_loc matrix written
#' TODO this return behavior is kind of funky...
write_probs_all_counties <- function(observations){
   
   counties <- unique(observations$county_code)
   
   for(i in counties){ 
      write_prob_per_loc(observations, i)
   }
   
}

# turning this off so that sourcing the file just loads the functions
if (FALSE) {
   observations <- read_csv('data/filtered.csv')
   write_probs_all_counties(observations)
}


