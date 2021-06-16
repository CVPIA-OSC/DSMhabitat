library(tidyverse)

species_lookup <- c('fr', 'sr', 'st', 'wr', 'lfr')
names(species_lookup) <- c("Fall Run Chinook", "Spring Run Chinook", "Steelhead", "Winter Run Chinook", "Late-Fall Run Chinook")

watershed_lengths <- read_csv('data-raw/salmonid_habitat_extents.csv') %>%
  arrange(Species, Id) %>%
  mutate(Species = species_lookup[Species]) %>%
  rename(order = Id, watershed = River, lifestage = Habitat, species = Species, source = Source)

usethis::use_data(watershed_lengths, overwrite = TRUE)
