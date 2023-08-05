library(tidyverse)
library(janitor)

decathlon <- read_rds("raw_data/decathlon.rds")

# let's take a look at the data
#head(decathlon)
#glimpse(decathlon)
# 13 cols, 41 rows, data on different decathlon events, ranks, and points

# turn row names into a column
decathlon_clean <- rownames_to_column(decathlon, var = "surname") %>% 
 clean_names() %>%  # clean column names
 mutate(surname = str_to_title(surname)) # capitalise surnames


# table is currently wide, let's pivot to make it long
decathlon_tidy <- decathlon_clean %>% 
  pivot_longer(
    cols = x100m:x1500m,
    names_to = "event",
    values_to = "distance_m_or_time_s"
  )

# save the cleaned data
write_csv(decathlon_tidy, "clean_data/decathlon_clean.csv")

