library(dplyr)
library(janitor)
library(gt)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)

polling_data <- read_csv("ps_4_elections-poll-nc09-3.csv")

# Madlibs

# 1. There were X respondents who supported the Democratic candidate.
dem_candidate_supporter <- polling_data %>%
 filter(response == "Dem") %>%
nrow()

# 2. There were X more respondents who favored the Republican candidate than who were Undecided.
favor_rep <- polling_data %>%
filter(response == "Rep") %>%
nrow()

favor_undecided <- polling_data %>%
 filter(response == "Und") %>%
 nrow()

rep_over_undecided <- (favor_rep - favor_undecided)

# 3. There are two gender variables (gender and gender_combined).
# There are X individuals for whom these variables have different values.

different_gender <- polling_data %>%
 select(gender, gender_combined) %>%
 filter(gender != gender_combined) %>%
 nrow()

# 4. There are X respondents listed as “White” under race_eth who are not listed as “White” under file_race_black.
polling_data %>%
 select(race_eth, file_race_black) %>%
 filter(race_eth == "White", file_race_black != "White") %>%
 nrow()

# 5.  The first response of Dem came X minutes (rounded to the nearest minute)
# before the first response of Rep.
# first Dem was at 2018-10-26 22:06:37
# first Rep was at 2018-10-26 22:17:39

dem_time <- polling_data %>%
  filter(response == "Dem") %>%
  arrange(timestamp) %>%
  slice(1) %>%
  pull(timestamp)

rep_time <- polling_data %>%
  filter(response == "Rep") %>%
  arrange(timestamp) %>%
  slice(1) %>%
  pull(timestamp)

time_difference <- (minute(rep_time) - minute(dem_time))


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
  arrange(factor(race_eth, levels = c("White", "Black", "Hispanic", "Asian", "Other"))) %>%


  ungroup() %>%



gt() %>%
  tab_header(
    title = "Polling Results in North Carolina 9th Congressional District") %>%

  cols_label(
    race_eth = "Race",
    Dem = "DEM.",
    Rep = "REP.",
    Und = "UND."   ) %>%

  fmt_percent(columns = vars(Dem, Rep, Und),
              decimals = 0)




# Question 3 voter education level
voter_education_violin <- polling_data %>%
  select(educ, final_weight) %>%
  filter(educ != "[DO NOT READ] Refused") %>%
  group_by(educ)

  # trying to change order
voter_education_violin$educ <-
  fct_rev(factor(voter_education_violin$educ, levels = c("Graduate or Professional Degree",
                                  "Bachelors' degree",
                                  "Some college or trade school",
                                  "High school",
                                  "Grade school")))

voter_education_violin %>%
  ggplot(aes(x = educ, y = final_weight)) +

  geom_violin() +

  coord_flip() +

  geom_jitter(alpha = .5, width = .2, size = 1) +

  labs(title = "More Educated Matter Less in North Carolina 9th",
       subtitle = "Poll gives more weight to people who are less likely to participate in polls",
       caption = "New York Times Upshot/Siena College 2018 live polls") +

  ylab("Weight Given to Respondent in Calculating Poll Results") + xlab(NULL)



# Question 4

# Republican response based on educ and age
categorical_data <- polling_data %>% 
  select(ager, educ, response)  %>% 
  filter(response == "Rep") %>% 
  filter(educ != "[DO NOT READ] Refused") %>% 
  filter(ager != "[DO NOT READ] Refused") %>% 
  group_by(educ, ager) %>% 
  mutate(count = n())

categorical_data$educ <- factor(categorical_data$educ, levels = c("Graduate or Professional Degree",                                    "Bachelors' degree", 
                                  "Some college or trade school", 
                                   "High school", 
                                   "Grade school"))
  
 ggplot(categorical_data, aes(x = ager, y = count, fill = educ)) + 
   geom_col(position = "dodge", width = .5) +
 
   labs(title = "Republican Voters by Education Level and Age",  
        subtitle = "Per Response to Poll",
        caption = "New York Times Upshot/Siena College 2018 live polls",
        fill = "Education Level") +

   xlab("Age") + ylab("Count") 


