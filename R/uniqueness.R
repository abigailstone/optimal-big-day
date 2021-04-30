library(tidyverse)

prob_per_loc <- read_csv('prob_per_loc.csv')

numMax <- function(x){length(which(x == max(x)))}

stats <- prob_per_loc %>% 
   # number of species with > 50% probability at each hotspot
   # problematic for locations with only a few checklists!
   mutate(n_likely = rowSums(. > 0.5)) %>% 
   select('locality', 'n_checklists', 'n_likely',  3:ncol(.))



# trying to find the number of values in each row that 
# are equal to the max of their respective column 
# none of these are quite getting there
test <- max.col(prob_per_loc[, 3:ncol(prob_per_loc)]) %>% View()
which.max(prob_per_loc$`American Bittern`)