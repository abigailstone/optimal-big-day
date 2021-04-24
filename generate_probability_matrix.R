# filtered data to species/hotspot probability matrix
library(tidyverse)

observations <- read_csv('filtered.csv')

# for each location, calculate number of checklists and number of times each species has been observed
checklists_per_loc <- observations %>% 
   filter(county_code == 'US-VT-001') %>%
   group_by(locality) %>%
   summarize(n_checklists = n_distinct(checklist_id)) %>% 
   # filter locations with 1 checklist since all species will be 100% probability
   filter(n_checklists > 1)

counts_per_loc <- observations %>%
   group_by(locality, common_name) %>%
   summarize(n_sp_loc = n()) %>%
   spread(key = common_name, value = n_sp_loc, fill = 0) %>%
   right_join(checklists_per_loc, by = 'locality') %>%
   #reorder the columns by moving n_checklists before individual species counts
   select('locality', 'n_checklists', 3:ncol(.))

# counts to probabilities
prob_per_loc <- counts_per_loc %>%
   mutate_at(.vars = vars( 3:ncol(.)),
             .funs = list(~ . / n_checklists)) %>%
   ungroup

write_csv(prob_per_loc, 'prob_per_loc.csv')
