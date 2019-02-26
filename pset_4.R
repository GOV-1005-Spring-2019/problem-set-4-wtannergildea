library(dplyr)
library(janitor)
library(gt)
library(tidyverse)
library(readxl)

polling_Data <- read_csv("ps_4_elections-poll-nc09-3.csv")

# View(polling_data)


# 1. There were X respondents who supported the Democratic candidate.
# dem_candidate_supporter <- polling_data %>%  
#  filter(response == "Dem") %>% 
# nrow()

# 2. There were X more respondents who favored the Republican candidate than who were Undecided.
#favor_rep <- polling_data %>% 
# filter(hrep_fav == "Favorable") %>% 
# nrow() 

#favor_undecided <- polling_data %>% 
#  filter(hrep_fav == "Don't know") %>% 
#  nrow()
  
#rep_over_undecided <- (favor_rep - favor_undecided) %>% 
# View()

# answer = 104

# 3. There are two gender variables (gender and gender_combined). 
# There are X individuals for whom these variables have different values.

#different_gender <- polling_data %>% 
#  select(gender, gender_combined) %>% 
#  filter(gender != gender_combined) %>% 
#  nrow() %>% 
#  View()

# answer = 10

# 4. There are X respondents listed as “White” under race_eth who are not listed as “White” under file_race_black.
# polling_data %>% 
#  select(race_eth, file_race_black) %>% 
#  filter(race_eth == "White", file_race_black != "White") %>% 
#  nrow() %>% 
#  View()

#answer = 28

# 5.  The first response of Dem came X minutes (rounded to the nearest minute)
# before the first response of Rep.
# first Dem was at 2018-10-26 22:06:37
# first Rep was at 2018-10-26 22:17:39

