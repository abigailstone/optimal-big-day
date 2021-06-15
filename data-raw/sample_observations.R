# download EBD from https://ebird.org/data/download/ebd
# this data was a custom download with the options:
# (1) Species: All species
# (2) Region: Addison County, VT, US
# (3) Date Range: April 2020
# (4) Include unvetted data: unchecked (default)

# set ebd path to the directory containing the .txt dataset
auk::auk_set_ebd_path("YOUR_EBD_PATH")
file <- "ebd_US-VT-001_201103_202103_relMar-2021.txt"
sample_observations <- write_data_main(file, write = FALSE)
usethis::use_data(sample_observations, overwrite = TRUE)
