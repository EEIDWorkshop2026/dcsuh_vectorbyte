#playing with vecdyn and environmental variables

library(here)

library(sf)
library(tidyverse)

#vecdyn helper functions
source(here("VecDyn_Dataset_Access.R"))

#pull data from vecdyn
getDataset(220)

#dataset locs
locs <-
  dataset %>%
  distinct(sample_location, sample_lat_dd, sample_long_dd) %>%
  mutate(sample_location = paste(sample_location, row_number(), sep = "_")) %>%
  st_as_sf(coords = c("sample_lat_dd", "sample_long_dd"))



clim <- read_csv(file = here("clim_dat_florida.csv"), skip = 6)

abund_sf <-
  dataset %>%
  st_as_sf()





