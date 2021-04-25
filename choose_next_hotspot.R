library(tidyverse)
library(bench)

prob_per_loc <- read_csv("prob_per_loc.csv")

# remove effort columns
prob_per_loc <- prob_per_loc %>% select(-c(n_checklists, total_time, med_time, iqr_time, total_distance, time_per_checklist))

# probability of seeing each species if visiting two hotspots
# h1 <- prob_per_loc[1, ] %>% as.numeric() %>% unlist()
# h2 <- prob_per_loc[2, ] %>% unlist()

# we're interested in the probability of seeing a species under 
# different scenarios (combinations of locations)
# this probability is the same as 1 - P(we dont see the sp. at any of the locs), 
# for species A, for locations 1, 2, ... n, we would calculate:
# 1 - (1 - p_a_loc1) * (1 - p_a_loc2) * ... * (1 - p_a_locn)

# method 1 
# species is string, prob_per_loc is tibble
prob_sp <- function(species, prob_per_loc) { 
   
   p_list <- prob_per_loc %>% 
      select_at(species) %>%
      sapply(function(x) 1 - x)
   
   # probability of observing the species given the combination of locations 
   1 - prod(p_list)
}

# method 2 
prob_sp2 <- function(species, prob_per_loc) {
   
   p <- prob_per_loc[[species]]
   
   1 - prod(dbinom(x = 0, size = 1, prob = p))
}

# method 1 and 2 are equal
result1 <- prob_sp('Yellow-throated Vireo', prob_per_loc) 
result2 <- prob_sp2('Yellow-throated Vireo', prob_per_loc)
unique(result1 == result2)

# method 2 is (a lot) faster
mark(prob_sp('Yellow-throated Vireo', prob_per_loc), 
     prob_sp2('Yellow-throated Vireo', prob_per_loc))

# we actually want to do this for all species, but select hotpots

# return the probability of seeing each species for a given set of hotspots
# hotspots is a vector of hotspot strings, prob_per_loc is tibble
# return type is a named vector
prob_hotspots <- function(hotspots, prob_per_loc) {
   
   H <- prob_per_loc %>% 
      filter(locality %in% hotspots) %>% 
      select(-c(locality))
   
   f <- function(sp_probs) (1 - prod(dbinom(0, 1, sp_probs)))
   
   apply(H, 2, f)
   
}

# quick manual check for correctness, not comprehensive
hotspots <- c('Battell Woods', 'Bittersweet Falls')
species <- c("Yellow-rumped Warbler", "American Robin")
prob_per_loc %>% filter(locality %in% hotspots) %>% select(species)
result <- prob_hotspots(hotspots, prob_per_loc)
result[names(result) %in% species]





