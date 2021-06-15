# assumes that sample_observations has already been loaded
sample_counties <- write_county_list(sample_observations, write = FALSE)
usethis::use_data(sample_counties, overwrite = TRUE)
