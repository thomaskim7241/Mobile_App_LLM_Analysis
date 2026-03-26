setwd("~/Documents/IDSC_4521/LLM_App_Analytics")
library(tidyverse)
#User_df <- read_csv('user_info.csv')
#Usage_df <- read_csv('weekly_usage.csv')
## sum(is.na(User_df)) = 351 (298 rows with missing values, 295 unique PanelistIDs)
## sum(is.na(Usage_df)) = 0

sum(is.na(User_df$Night))

unique(Usage_df$WECITYunique(Usage_df$WEGENDERunique(Usage_df$WEEK)s
                             
User_df %>% 
  count(PANELISTID) %>% 
  filter(n > 1)

# How many total duplicate rows exist
User_df %>% count(PANELISTID) %>% filter(n > 1) %>% summarise(sum(n))
# How many total duplicate rows exist
User_df %>% count(PANELISTID) %>% filter(n > 1) %>% summarise(sum(n))
# Look at a specific duplicate to understand the pattern
User_df %>% filter(PANELISTID == "GWSUSA-cb618ad1-4054-448a-9058-8b57b3cd62de")
# Missing df for na values
missing_df <- User_df %>% 
  filter(if_any(everything(), is.na))
view(missing_df)
unique(missing_df$PANELISTID)

summary(Usage_df$FOREGROUNDDURATION)

353107.54/60/60

Usage_df %>%
  filter(FOREGROUNDDURATION == max(FOREGROUNDDURATION)) %>%
  select(PANELISTID, APPDESCRIPTION, WEEK, FOREGROUNDDURATION)
