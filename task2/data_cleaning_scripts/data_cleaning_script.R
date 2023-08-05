library(tidyverse)

# read in data
cake <- read_csv("raw_data/cake-ingredients-1961.csv")
cake_code <- read_csv("raw_data/cake_ingredient_code.csv")

# examine data
head(cake)
# seems to be a column for cake and then lots of codes with numbers for entries
head(cake_code)
# the code csv provides the information on what ingredient each code refers to
# and the measure it comes in

# 1.2.1 Some cleaning hints
# This data isn’t in tidy format. You’ll need to figure out how to get
# it into our classic data format.
# We want the actual ingredient names, not the abbreviations. 
# You’ll find these in cake_ingredient_code.csv

# going to pivot the cake data so that ingredient is a column
cake_longer <- cake %>% 
  pivot_longer(cols = AE:ZH, names_to = "code", values_to = "measure_amount")

# going to join the datasets
cake_tidy <- left_join(cake_longer, cake_code, by = "code") %>% 
  janitor::clean_names() %>% 
  select(-code, cake, ingredient, measure_amount, measure)

# save cleaned data
write_csv(cake_tidy, "clean_data/cake_clean.csv")