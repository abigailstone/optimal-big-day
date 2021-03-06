#' Drop effort columns from prob_per_loc
#' 
#' @param prob_per_loc A tibble with the probability of observing each species at each locality
#' @return A new copy of prob_per_loc without the effort columns
#' @export
#' @examples
#' drop_effort_cols(sample_prob_per_loc)
drop_effort_cols <- function(prob_per_loc) {
   
   which_cols <- 
      !is.na(auk::ebird_species(colnames(prob_per_loc))) | 
      colnames(prob_per_loc) == "locality"
   
   prob_per_loc[, which_cols]
}

#' Select the location with the highest probability sum 
#' 
#' @param probs A tibble with the probability of observing each species at each locality
#' @return The name of the location with the highest expected number of species
#' @export
#' @examples 
#' get_first_best(drop_effort_cols(sample_prob_per_loc))
get_first_best <- function(probs) {
   probs[[which.max(rowSums(probs[2:ncol(probs)])), "locality"]]
}


#' Calculate the probability of seeing each species for a given set of hotspots
#' 
#' @param hotspots A character vector of hotspot names
#' @param prob_per_loc A tibble with the probability of observing each species at each locality
#' @return A named numerical vector of the probability of observing each species at the given combination of hotspots
#' @export
#' @examples
#' hotspots <- c('Otter View Park', 'Button Bay State Park')
#' prob_hotspots(hotspots, drop_effort_cols(sample_prob_per_loc))
prob_hotspots <- function(hotspots, prob_per_loc) {
   
   H <- prob_per_loc[prob_per_loc$locality %in% hotspots, 
                     names(prob_per_loc) != "locality"]
   
   f <- function(sp_probs) (1 - prod(stats::dbinom(0, 1, sp_probs)))
   
   apply(H, 2, f)
   
}

#' Greedy selection algorithm to select the k optimal hotspots to visit
#' 
#' @param probs A tibble with the probability of observing each species at each locality
#' @param k The number of hotspots you want to visit
#' @param H A character vector of the hotspots you definitely want to visit
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @examples 
#' # Not run because of long runtime
#' # hotspots <- c('Otter View Park', 'Button Bay State Park')
#' # select_hotspots(drop_effort_cols(prob_per_loc), 5, hotspots)
select_hotspots <- function(probs, k, H=NULL){
   
   # vector of strings for hotspot names 
   hotspots <- probs[['locality']]
   
   if (is.null(H)){
      # select the initial best hotspot 
      H <- get_first_best(probs)
   } 
   
   remaining_loc <- hotspots[!(hotspots %in% H)]
   
   # iterate for the number of hotspots we want to select
   for (i in seq_len(k-length(H))){
   
      remaining_probs <- probs %>% 
         dplyr::filter(.data$locality %in% remaining_loc) 
      
      # initialize variables
      current_h <- NULL 
      current_best <- 0
      
      # pick the next best hotspot using prob_hotspots 
      for(h in remaining_probs[['locality']]){
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

#' Return the expected number of species at each of the selected hotspots
#' 
#' @param hotspots a character vector of hotspots of interest 
#' @param probs A tibble with the probability of observing each species at each locality
#' @return A numerical vector with the expected number of species at each hotspot
#' @importFrom magrittr %>%
#' @importFrom rlang .data 
#' @importFrom utils head tail
#' @importFrom dplyr select
#' @export
#' @examples 
#' hotspots <- c('Otter View Park', 'Button Bay State Park')
#' pred_hotspot_totals(hotspots, drop_effort_cols(sample_prob_per_loc))
pred_hotspot_totals <- function(hotspots, probs){
   
   locality_totals <- NULL

   for (i in 1:(length(hotspots))){
      h <- probs %>% 
         dplyr::filter(.data$locality %in% hotspots[i])
      
      pred  <- rowSums(select(h, -c('locality')))
      locality_totals <- c(locality_totals, pred)
   }
   
   total_sum <- sum(prob_hotspots(hotspots, probs))

   return(locality_totals)
}

#' Return the total expected number of species if visiting all suggested hotspots
#'
#' @param hotspots a character vector of hotspots of interest
#' @param probs A tibble with the probability of observing each species at each locality 
#' @return A numerical value - the predicted total of species at this set of hotspots 
#' @importFrom magrittr %>% 
#' @importFrom rlang .data 
#' @export
#' @examples 
#' hotspots <- c('Otter View Park', 'Hurd Grassland')
#' pred_total(hotspots, drop_effort_cols(sample_prob_per_loc))
pred_total <- function(hotspots, probs){
   
   h <- probs %>% 
      dplyr::filter(.data$locality %in% hotspots)
   
   total <- sum(prob_hotspots(hotspots, probs))
   
   return(total)
}


if (FALSE) {
   # get just probabilities in species x hotspot matrix
   prob_per_loc <- read_csv('data_local/prob_per_loc.csv')
   prob_per_loc <- drop_effort_cols(prob_per_loc)
   best_H <- select_hotspots(prob_per_loc, 5)
   print(best_H)
}