library(tidyverse)
library(ggplot2)
library(gridExtra)
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

graph_prob_hotspots <- function(hotspots, prob_per_loc) {
   
   result <- prob_hotspots(hotspots, prob_per_loc)
   
   result_tbl <- tibble(
      sp = names(result),
      p = unname(result)
   )

   # p1 <- result_tbl %>% 
   #    ggplot(aes(p)) + 
   #    geom_bar()

   p2 <- result_tbl %>%
      ggplot(aes(p)) +
      geom_density(bw = 0.005)

   # grid.arrange(p1, p2)
   
   p2
}


# hotspots_list is a list of vectors of hotspots
# e.g. list(c('Battell Woods', 'Bittersweet Falls'), c('Otter View Park', 'Hurd Grassland'))
graph_prob_hotspots_comparison <- function(hotspots_list, prob_per_loc) {
   
   plots <- lapply(hotspots_list, function (x) graph_prob_hotspots(x, prob_per_loc) )
   
   do.call(grid.arrange, plots)
   
}

l <- list(
   c('Otter View Park'),
   c('Otter View Park', 'Turkey Lane', 'Snake Mountain WMA', 'Dead Creek WMA IBA')
)

graph_prob_hotspots_comparison(l, prob_per_loc)
# the second combination of hotspots in l should result in more species seen


# would be nice to get these graphs layered on top of each other,
# I think the first step would be to get them into a tbl together...
# this very much does not work
# prob_hotspots_comparison <- function(hotspots_list, prob_per_loc) {
#    
#    l <- lapply(hotspots_list, function(x) prob_hotspots(x, prob_per_loc))
#    
#    tbl <- tibble(
#       sp = names(l[[1]])
#    )
#    
#    for (i in l) {
#       tbl <- mutate(tbl, unname(i))
#    }
#    
#    return(tbl)
# }


# freq poly plot (not normalized) for one hotspot vs several 
# this could probably go in the function above but it works a
# bit differently 
hist1 <- prob_hotspots(c('Otter View Park'), prob_per_loc)
hist2 <- prob_hotspots(c('Otter View Park', 'Turkey Lane', 'Snake Mountain WMA', 'Dead Creek WMA IBA'), prob_per_loc)
tbl <- tibble(hist1, hist2)

tbl %>% 
   ggplot() + 
   geom_freqpoly(mapping = aes(x = hist1,
                               color = "one")) + 
   geom_freqpoly(mapping = aes(x = hist2,
                               color = "many")) + 
   labs(x = 'Probability by species', 
        y = 'number of species') + 
   scale_color_manual(name = "",
                      values = c("one" = "#56B4E9", "many" = "#E68F00"),
                      labels = c("|H| = 1", "|H| = 4")) + 
   theme_bw()



