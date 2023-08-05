library(tidyverse)
library(readxl)
library(janitor)


# this data comes in 3 xlsx spreadsheets
candy_2015 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")


# Rename cols before binding rows

candy_2015 <- candy_2015 %>% 
  rename(age_years = `How old are you?`) %>% 
  clean_names()

candy_2016 <- candy_2016 %>% 
  rename(age_years = `How old are you?`) %>% 
  rename(gender = `Your gender:`) %>% 
  clean_names() %>% 
  rename(country = which_country_do_you_live_in)

candy_2017 <- candy_2017 %>% 
  rename(Age_years = `Q3: AGE`) %>% 
  rename(gender = `Q2: GENDER`) %>% 
  clean_names()

# remove q's in column headers
#names(candy_2017) <- str_remove(names(candy_2017), "q[0-9]+_")
# can also use the tidyverse way:
#candy_2017 %>% 
 # rename_with(.cols = starts_with("q"), .fn = ~ str_remove(., "q[0-9]+_")) # starts_with doesn't take regex
# or..
candy_2017 <- candy_2017 %>% 
  rename_with(.cols = matches("^q[0-9]+_"), .fn = ~ str_remove(., "q[0-9]+_"))

# Add an id number to keep track entries when make long format
# convert to desired long format and adding columns 'gender' and 'country' so can bind to 2017
# also add a year column so that know which year the data comes from when bind all sets together
# 1) 2015
candy_2015_clean <- 
  candy_2015 %>%
  mutate(person_id = row_number()) %>%
  pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
  mutate(
    year = 2015,
    gender = NA,
    country = NA
  ) %>%
  select(
    year,
    person_id,
    going_out = are_you_going_actually_going_trick_or_treating_yourself,
    age_years,
    gender,
    country,
    candy,
    rating
  ) 

# 2) 2016
candy_2016_clean <- 
  candy_2016 %>%
  mutate(person_id = max(candy_2015_clean$person_id) + row_number()) %>%
  pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
  mutate(
    year = 2016,
  ) %>%
  select(
    year,
    person_id,
    going_out = are_you_going_actually_going_trick_or_treating_yourself,
    age_years,
    gender,
    country,
    candy,
    rating
  ) 

# 3) 2017
candy_2017_clean <- 
  candy_2017 %>%
  mutate(person_id = max(candy_2016_clean$person_id) + row_number()) %>%
  rename("x100_grand_bar" = `100_grand_bar`) %>% 
  pivot_longer(x100_grand_bar:york_peppermint_patties, names_to = "candy", values_to = "rating") %>%
  mutate(
    year = 2017,
  ) %>%
  select(
    year,
    person_id,
    going_out,
    age_years,
    gender,
    country,
    candy,
    rating
  ) 

# Bind rows
candy_combined <- bind_rows(candy_2015_clean, candy_2016_clean, candy_2017_clean)

# clean data
candy_combined <- candy_combined %>% 
  mutate(
    age_years = as.numeric(age_years),
    rating = str_to_lower(rating),
    country = str_to_lower(country),
    rating = as.factor(rating),
    going_out = as.factor(going_out),
    gender = as.factor(gender)
    )
  

# Cleaning country column -------------------------------------------------

# Can see from country column many different ways can say the same country 
# (e.g. us vs united states) but there are also many different mispellings/incorrect names
# to have accurate counts by country need to clean this up - and most will be manual
#  hard coding these mispellings from manually looking at the column entries

#from review possible misspellings


us_mispellings <- 
  c("usa", "US", "uSA", "United States of America",
    "united states", "United States", "us", "USSA",
    "U.S.A.", "Murica", "USA!", "USA (I think but it's an election year so who can really tell)",
    "U.S.", "Usa", "Us", "America", "Units States", "United states", 
    "USA USA USA", "USA! USA! USA!", "the best one - usa",
    "u.s.", "united states of america", "The Yoo Ess of Aaayyyyyy",
    "united states of america", "USA!!!!!!", "USA! USA!",
    "United Sates", "Sub-Canadian North America... 'Merica",
    "U.s.", "Merica", "UNited States", "united stetes", "north carolina",
    "'merica", "new jersey", "unhinged states", "california",
    "united statea", "united state", "united ststes", "unied states",
    "pittsburgh", "u s", "unites states", "unite states", "new york",
    "ahem....amerca", "murrika", "alaska", "united staes", "united stated",
    "united statss")

us_mispellings <- 
  str_to_lower(us_mispellings)


uk_mispellings <- 
  str_to_lower(c("United kingdom", "England", "uk", "scotland",
                 "endland", "u.k.", "endland", "united kindom" ))


unknown <- 
  str_to_lower(c("A tropical island south of the equator", "this one",
    "51.0", "47.0", "there isn't one for old men", "Neverland", 
    "54.0", "god's country", "Trumpistan", "30.0", "Not the USA or Canada",
    "See above", "Denial", "Earth", "46", "insanity lately",
    "somewhere", "one of the best ones", "cascadia", "somewhere", "eua",
    "the republic of cascadia", "a", "atlantis", "soviet canuckistan", 
    "narnia", "i don't know anymore", "fear and loathing", "ud", "europe"))

# for loop

candy_combined_clean <- candy_combined %>% 
  mutate(
    # if the country matches the pattern, name it e.g. united states, otherwise leave as country
    country = if_else(str_detect(country, "usa"), "united states", country), 
    country = if_else(str_detect(country, "us"), "united states", country),
    country = if_else(str_detect(country, "united states"), "united states", country),
    country = if_else(str_detect(country, "u.s."), "united states", country),
    country = if_else(str_detect(country, "america"), "united states", country),
    country = if_else(country %in% us_mispellings, "united states", country),
    # Tidy other countries
    country = if_else(country %in% uk_mispellings, "united kingdom", country),
    country = if_else(country %in% c("canada`", "can", "canae"), "canada", country),
    country = if_else(country == "españa", "spain", country),
    country = if_else(country == "korea", "south korea", country),
    country = if_else(country == "the netherlands", "netherlands", country),
    country = if_else(country == "brasil", "brazil", country),
    # Change remaining into missing values
    country = if_else(str_detect(country, "[0-9]"), NA_character_, country), 
    country = if_else(country %in% unknown, NA_character_, country)
  )

unique(candy_combined_clean$country)

# Remove outliers ---------------------------------------------------------

# seems un likely any ages over 99 so have assumed errors (to avoid skewing the average ages) 
candy_combined_clean <- 
  candy_combined_clean %>%
  mutate(age_years = if_else(age_years > 99, NA_real_, age_years))


# Write clean data to csv -------------------------------------------------

write_csv(candy_combined_clean , "clean_data/candy_clean.csv")

