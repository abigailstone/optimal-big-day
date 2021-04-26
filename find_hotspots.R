library(tidyverse)
source('choose_next_hotspot.R')

# get just probabilities in species x hotspot matrix
prob_per_loc <- read_csv('prob_per_loc.csv')
prob_per_loc <- prob_per_loc %>% 
   select(-c(n_checklists, total_time, med_time, 
             iqr_time, total_distance, time_per_checklist))

# vector of strings for hotspot names 
hotspots <- prob_per_loc$locality

# horribly hacky
# select the location with the highest probability sum 
# probs is a tibble, locations by species
get_first_best <- function(probs){
   probs[which.max(rowSums(probs[2:ncol(probs)])),]
}

# Select the k optimal hotspots to visit - greedy selection
# probs is a tibble of probabilities for each location, species 
# k is an integer
select_hotspots <- function(probs, k){
   
   # select the initial best hotspot 
   H <- get_first_best(prob_per_loc)$locality
   remaining_loc <- hotspots[!(hotspots %in% H)]
   
   # iterate for the number of hotspots we want to select
   for (i in 1:k-1){
      
      remaining_probs <- probs %>% 
         filter(locality %in% remaining_loc) 
      
      # initialize variables
      current_h <- NULL 
      current_best <- 0
      
      # pick the next best hotspot using prob_hotspots 
      for(h in remaining_probs$locality){
         s <- sum(prob_hotspots(c(H, h), remaining_probs))
         
         # update the current best result for this comparison
         if (s > current_best){
            current_best <- s 
            current_h <- h
         }

      }

      # add the best to our collection of optimal hotspots 
      # and remove it from the list of remaining possibilities
      H <- c(H, current_h)
      remaining_loc <- remaining_loc[!(remaining_loc %in% current_best)]
   }
   
   return(H)
}

# mark(select_hotspots(prob_per_loc, 5), iterations=1)

best_H <- select_hotspots(prob_per_loc, 5)
print(best_H)

