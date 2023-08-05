library(tidyverse)
library(readxl)
library(janitor)

# read in data
record_data <- clean_names(read_excel("raw_data/seabirds.xls", sheet = 1))
species_data <- clean_names(read_excel("raw_data/seabirds.xls", sheet = 2))
ship_code <- clean_names(read_excel("raw_data/seabirds.xls", sheet = 3))
birds_code <- clean_names(read_excel("raw_data/seabirds.xls", sheet = 4))



#head(seabirds)
# lots of variables, quite complicated dataset

# hints
# Do we need all the variables for this data?
# You’ll need to join the ship data to the bird record data



birds_joined <- left_join(species_data, record_data, by = "record_id")

# lose the abbreviations and sensu lato at the end of bird species
birds_clean <- birds_joined %>% 
  rename(species = species_common_name_taxon_age_sex_plumage_phase) %>%
  mutate(species = str_remove(species, " [A-Z0-9]+$"),
         species = str_remove(species, " AD$"|" IMM$"),
         species = str_remove(species, " sensu lato")) %>% 
  select(record_id, species, count, lat, long) # only need these cols for analysis


write_csv(birds_clean, "clean_data/birds_cleaned.csv")
