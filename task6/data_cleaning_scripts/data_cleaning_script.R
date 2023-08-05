# The data is available in the file dog_survey.csv 
# It is the results of a survey filled in by dog owners 
# about their dogs.
# 
# 2.2.1 Some cleaning hints
# Ensure there are no duplicates in the data. 
# Write code to check for duplicates.
# Ensure information for a single dog per row
# Be mindful of the data issues we see in this data,
# they might be the type of things we see from
# ‘free hand’ survey responses!

library(tidyverse)
dogs <- read_csv("raw_data/dog_survey.csv")

dogs %>% 
  distinct(dog_size)

# tidy up gender column
dogs_tidy <- dogs %>% 
  filter(!id == 174) %>%  # remove row with multiple pets
  mutate(dog_gender = case_when( # make sure genders are clean
    str_detect(dog_gender, "^[Mm]") ~ "Male",
    str_detect(dog_gender, "^[Ff]") ~ "Female",
    .default = "Unknown"
  )) %>% 
  mutate(dog_size = case_when( # clean sizes
    str_detect(dog_size, "XS") ~ "Extra small",
    str_detect(dog_size, "^[Ss]") ~ "Small",
    str_detect(dog_size, "^[Mm]") ~ "Medium",
    str_detect(dog_size, "^[Ll]") ~ "Large",
    str_detect(dog_size, "XL") ~ "Extra large",
    .default = "Unknown"
  )) %>% 
  distinct() %>% # remove duplicates
  mutate(
    dog_age = as.numeric(str_extract(dog_age, "[0-9]+")), # extract ages
    human_age = dog_age / 6) %>% # calculate human ages
  mutate(amount_spent_on_dog_food = str_extract(amount_spent_on_dog_food, "£[0-9]+"),
         amount_spent_on_dog_food = abs(as.numeric(str_remove(amount_spent_on_dog_food, "£"))))


write_csv(dogs_tidy, "clean_data/dogs_clean.csv")
