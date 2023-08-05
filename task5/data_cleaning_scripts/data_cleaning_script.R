library(tidyverse)

# read in data
rwa <- read_csv("raw_data/rwa.csv")

# hints
# 
# You’ll need to calculate people’s overall RWA score. 
# This is found from the mean of questions 3 to 22. Questions 1 and 2 are warm up questions. Note that the following questions are reverse scored: 4, 6, 8, 9, 11, 13, 15, 18, 20, 21.
# Read the file rwa_codebook.txt to understand how to clean this data. [Hint: you may want to recode some of the variables to give their values meaning].

# examine data
head(rwa)
names(rwa)
# just Qs and numbers
# Qs are question responses, Es are times elapsed

# reverse score function 
reverse_score <- function(x) {
  return(10 - x)
}

# create dataset with certain question scores reversed
rwa_reverse <- purrr::map_at(rwa, c(4, 6, 8, 9, 11, 13, 15, 18, 20, 21), reverse_score) %>%
  as_tibble()

# going to calculate mean score and recode other variables
rwa_tidy <- rwa_reverse %>% 
  rowid_to_column("subject_ID") %>% 
  select(subject_ID, starts_with("Q"), age, testelapse, gender, hand, education, familysize, urban) %>% 
  mutate(rwa = rowMeans(select(., Q3:Q22))) %>% 
  mutate(gender = recode(gender,
                         "1" = "male",
                         "2" = "female",
                         "3" = "other",
                         .default = "unknown"),
         hand = recode(hand,
                       "1" = "right",
                       "2" = "left",
                       "3" = "both",
                       .default = "unknown"),
         education = recode(education,
                            "1" = "less than high school",
                            "2" = "high school",
                            "3" = "university degree",
                            "4" = "graduate degree",
                            .default = "unknown"),
         urban = recode(urban,
                        "1" = "rural",
                        "2" = "suburban",
                        "3" = "urban",
                        .default = "unknown"),
         age_bracket = case_when(
                        age < 18 ~ "under 18",
                        age <= 25 ~ "18 to 25",
                        age <= 40 ~ "26 to 40",
                        age <= 60 ~ "41 to 60",
                        .default = "over 60"))


write_csv(rwa_tidy, "clean_data/rwa_clean.csv")
  
  

