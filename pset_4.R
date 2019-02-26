library(dplyr)
library(janitor)
library(gt)
library(tidyverse)
library(readxl)
library(lubridate)

polling_data <- read_csv("ps_4_elections-poll-nc09-3.csv")

# View(polling_data)


# 1. There were X respondents who supported the Democratic candidate.
# dem_candidate_supporter <- polling_data %>%  
#  filter(response == "Dem") %>% 
# nrow()
#answer = 219

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

# dem_time <- polling_data %>% 
#   filter(response == "Dem") %>% 
#   arrange(timestamp) %>% 
#   slice(1) %>% 
#   pull(timestamp)
# 
# 
# rep_time <- polling_data %>% 
#   filter(response == "Rep") %>% 
#   arrange(timestamp) %>% 
#   slice(1) %>% 
#   pull(timestamp)
# 
# time_difference <-  (minute(rep_time) - minute(dem_time))
# 
# answer = 11

# Question 2 box 

table <- polling_data %>%
  select(response, race_eth, final_weight) %>%
  group_by(race_eth, response) %>%

  # All you need to know for this class is: Use sum(weight_var) in place of n().

  summarize(total = sum(final_weight)) %>%
  filter(race_eth != "[DO NOT READ] Don't know/Refused") %>%
  spread(key =  response, value = total, fill = 0) %>%
  mutate(all = Dem + Rep + Und + `3`) %>%
  mutate(Dem = Dem / all) %>%
  mutate(Rep = Rep / all) %>%
  mutate(Und = Und / all) %>%
  select(-all, - `3`) %>%

  # One of the biggest pieces of black magic incantation in R is ungroup(). (I
  # did not mention this in class.) Summary: Whenever you group a tibble (as we
  # do above) the grouping stays with an resulting object, until you explicitly
  # ungroup() it. That can't ever hurt things (right? TFs?) and it often helps,
  # as in this case.

  ungroup() %>%

  # You will have a chance to explore many other gt commands in problem set #4.
  # I added two extras that we did not get to in class.

gt() %>%
  tab_header(
    title = "Polling Results in NC 9th") %>%

  cols_label(
    race_eth = NULL,
    Dem = "DEM.",
    Rep = "REP.",
    Und = "UND."
  ) %>%

  fmt_percent(columns = vars(Dem, Rep, Und),
              decimals = 0)

View(table)
