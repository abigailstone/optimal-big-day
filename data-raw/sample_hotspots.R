# assumes that sample_observations has already been loaded
sample_hotspots <- write_hotspot_list(sample_observations, write = FALSE)
usethis::use_data(sample_hotspots, overwrite = TRUE)
