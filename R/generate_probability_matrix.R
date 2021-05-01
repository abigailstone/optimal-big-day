# filtered data to species/hotspot probability matrix
library(tidyverse)
library(reshape2)

# return tibble with effort information for each hotspot
effort_per_hotspot <- function(observations) {
   observations %>% 
      # filter(county_code == 'US-VT-001') %>%
      mutate(effort_distance_km = ifelse(is.na(effort_distance_km), 0, effort_distance_km)) %>% # handle stationary lists
      distinct(checklist_id, .keep_all = TRUE) %>%
      group_by(locality) %>% 
      summarize(
         n_checklists = n(),
         total_time = sum(duration_minutes),
         med_time = median(duration_minutes),
         iqr_time = IQR(duration_minutes),
         total_distance = sum(effort_distance_km)
      ) %>% 
      mutate(time_per_checklist = total_time / n_checklists) %>% 
      filter(n_checklists > 1)
}

# graphing effort variables
# effort_tall <- melt(effort_per_loc, id.vars="locality")
# effort_tall %>% ggplot(aes(value)) + 
#    geom_density() + 
#    facet_wrap(~variable, scales = "free")

# does total time correlate with number of checklists for each hotspot?
# effort_per_loc %>% 
#    ggplot(aes(n_checklists, total_time)) + 
#    geom_point() + 
#    stat_smooth()



# return a tibble with number of checklists reporting each species at each hotspot
n_observations_per_hotspot <- function(observations) {
   observations %>%
      group_by(locality, common_name) %>%
      summarize(n_sp_loc = n(), .groups = "drop") %>%
      spread(key = common_name, value = n_sp_loc, fill = 0)
}

# return a matrix with the probability of seeing 
# each species at each hotspot
probability_matrix <- function(observations) {
   
   effort_per_loc <- effort_per_hotspot(observations)
   counts_per_loc <- n_observations_per_hotspot(observations)
   
   # join species frequency and effort information
   counts_and_effort_per_loc <- effort_per_loc %>%
      left_join(counts_per_loc, by = "locality")   
   
   counts_and_effort_per_loc %>% 
      mutate_at(
         
         # for only species columns (exclude locality and effort columns)
         .vars = setdiff(colnames(counts_and_effort_per_loc), 
                                  colnames(effort_per_loc)),
                
          # divide by the number of checklists at that location
         .funs = list(~ . / n_checklists))
}


# save a single prob_per_loc matrix
write_prob_per_loc <- function(observations, loc){
   
   prob_per_loc <- probability_matrix(observations)
   
   # assumes that a data folder has been created in current working directory
   write_csv(prob_per_loc, paste('data/', loc, '_prob_per_loc.csv', sep=''))
   
}

# save a prob_per_loc matrix for every county in the dataset
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


