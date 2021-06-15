# assumes that sample_observations has already been loaded
sample_prob_per_loc <- probability_matrix(sample_observations)
usethis::use_data(sample_prob_per_loc, overwrite = TRUE)
