setwd("~/Documents/IDSC_4521/LLM_App_Analytics")
library(tidyverse)
User_df <- read_csv('user_info.csv')
Usage_df <- read_csv('weekly_usage.csv')
AI_App_Names_DF <- read_csv('AI_App_Names.csv')
## sum(is.na(User_df)) = 351 (298 rows with missing values, 295 unique PanelistIDs)
## sum(is.na(Usage_df)) = 0

sum(is.na(User_df$Night))

unique(Usage_df$WECITYunique(Usage_df$WEGENDERunique(Usage_df$WEEK)
                             
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

Usage_df %>%
  arrange(desc(FOREGROUNDDURATION)) %>%
  select(PANELISTID, APPDESCRIPTION, WEEK, FOREGROUNDDURATION) %>%
  head(20)


# # Age missingness correlations --------------------------------------------
# User_df <- User_df %>%
#   mutate(AGE_MISSING = is.na(AGE))
# User_df %>%
#   group_by(AGE_MISSING) %>%
#   summarise(
#     n = n(),
#     pct_treated = mean(TREATED),
#     avg_weekday = mean(Weekday, na.rm = TRUE),
#     avg_weekend = mean(Weekend, na.rm = TRUE),
#     avg_morning = mean(Morning, na.rm = TRUE),
#     avg_afternoon = mean(Afternoon, na.rm = TRUE),
#     avg_night = mean(Night, na.rm = TRUE)
#   )
# User_df %>%
#   group_by(AGE_MISSING, GENDER) %>%
#   summarise(n = n()) %>%
#   mutate(pct = n / sum(n))
# table(User_df$AGE_MISSING, User_df$TREATED)

# # Gender Missingness Correlations -----------------------------------------
# 
# User_df <- User_df %>%
#   mutate(Gender_missing = is.na(GENDER))
# User_df %>%
#   group_by(Gender_missing) %>%
#   summarise(
#     n = n(),
#     pct_treated = mean(TREATED),
#     avg_weekday = mean(Weekday, na.rm = TRUE),
#     avg_weekend = mean(Weekend, na.rm = TRUE),
#     avg_morning = mean(Morning, na.rm = TRUE),
#     avg_afternoon = mean(Afternoon, na.rm = TRUE),
#     avg_night = mean(Night, na.rm = TRUE)
#   )
# User_df %>%
#   group_by(Gender_missing, AGE) %>%
#   summarise(n = n()) %>%
#   mutate(pct = n / sum(n))
# table(User_df$Gender_missing, User_df$TREATED)


# Unknown Gender Correlation Analysis -------------------------------------

# User_df <- User_df %>%
#   mutate(AGE_MISSING = is.na(AGE))
# 
# # Filter all rows where GENDER is Unknown
# unknown_gender <- User_df %>%
#   filter(GENDER == "Unknown")
# 
# # Check how many there are
# nrow(unknown_gender)
# 
# # View them
# View(unknown_gender)
# # Summary statistics for Unknown gender group
# User_df %>%
#   filter(GENDER == "Unknown") %>%
#   summarise(
#     n = n(),
#     pct_treated = mean(TREATED),
#     avg_weekday = mean(Weekday, na.rm = TRUE),
#     avg_weekend = mean(Weekend, na.rm = TRUE),
#     avg_morning = mean(Morning, na.rm = TRUE),
#     avg_afternoon = mean(Afternoon, na.rm = TRUE),
#     avg_night = mean(Night, na.rm = TRUE),
#     avg_age = mean(AGE, na.rm = TRUE)
#   )
# 
# # Compare Unknown vs Male vs Female across all behavioral variables
# User_df %>%
#   group_by(GENDER) %>%
#   summarise(
#     n = n(),
#     pct_treated = mean(TREATED),
#     avg_weekday = mean(Weekday, na.rm = TRUE),
#     avg_weekend = mean(Weekend, na.rm = TRUE),
#     avg_morning = mean(Morning, na.rm = TRUE),
#     avg_afternoon = mean(Afternoon, na.rm = TRUE),
#     avg_night = mean(Night, na.rm = TRUE),
#     avg_age = mean(AGE, na.rm = TRUE)
#   )
# 
# # Check if Unknown gender users are the same as missing AGE users
# User_df %>%
#   filter(GENDER == "Unknown") %>%
#   count(AGE_MISSING)
# 
# # Check treatment breakdown for Unknown gender
# table(User_df$GENDER, User_df$TREATED)


# # Export Excel of App names -----------------------------------------------
# # Load writexl package (install if needed)
# install.packages("writexl")
# library(writexl)
# 
# # Extract all unique app names from APPDESCRIPTION
# unique_apps <- Usage_df %>%
#   distinct(APPDESCRIPTION) %>%
#   arrange(APPDESCRIPTION)
# 
# # Write to Excel file
# write_xlsx(unique_apps, "~/Documents/IDSC_4521/LLM_App_Analytics/unique_apps.xlsx")
