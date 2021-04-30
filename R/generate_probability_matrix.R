# filtered data to species/hotspot probability matrix
library(tidyverse)
library(reshape2)

observations <- read_csv('data/filtered.csv')

# for each location, calculate number of checklists and number of times each species has been observed
effort_per_loc <- observations %>% 
   filter(county_code == 'US-VT-001') %>%
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

# graphing effort variables
effort_tall <- melt(effort_per_loc, id.vars="locality")
effort_tall %>% ggplot(aes(value)) + 
   geom_density() + 
   facet_wrap(~variable, scales = "free")

# does total time correlate with number of checklists for each hotspot?
effort_per_loc %>% 
   ggplot(aes(n_checklists, total_time)) + 
   geom_point() + 
   stat_smooth()

# counts of checklists per hotspot
counts_per_loc <- observations %>%
   group_by(locality, common_name) %>%
   summarize(n_sp_loc = n()) %>%
   spread(key = common_name, value = n_sp_loc, fill = 0) %>%
   right_join(effort_per_loc, by = 'locality') %>%
   # reorder the columns by moving effort vars before inidividual speices counts
   select(colnames(effort_per_loc), length(colnames(effort_per_loc)):ncol(.))

# counts to probabilities
prob_per_loc <- counts_per_loc %>%
   mutate_at(.vars = vars(length(colnames(effort_per_loc)):ncol(.)),
             .funs = list(~ . / n_checklists)) %>%
   ungroup

write_csv(prob_per_loc, 'data/prob_per_loc.csv')
