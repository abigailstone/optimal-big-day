library(tidyverse)

# return a new copy of prob_per_loc without the effort columns
drop_effort_cols <- function(prob_per_loc) {
   prob_per_loc %>% 
      select(-c(n_checklists, total_time, med_time, 
                iqr_time, total_distance, time_per_checklist))
}

# horribly hacky
# select the location with the highest probability sum 
# probs is a tibble, locations by species
get_first_best <- function(probs){
   probs[which.max(rowSums(probs[2:ncol(probs)])),]
}

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

# Select the k optimal hotspots to visit - greedy selection
# probs is a tibble of probabilities for each location, species 
# k is an integer
select_hotspots <- function(probs, k){
   
   # vector of strings for hotspot names 
   hotspots <- prob_per_loc$locality
   
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

if (FALSE) {
   # get just probabilities in species x hotspot matrix
   prob_per_loc <- read_csv('data/prob_per_loc.csv')
   prob_per_loc <- drop_effort_cols(prob_per_loc)
   best_H <- select_hotspots(prob_per_loc, 5)
   print(best_H)
}