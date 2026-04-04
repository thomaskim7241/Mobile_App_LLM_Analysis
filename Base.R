# =============================================================================
# LLM App Analytics - Base Exploration
# =============================================================================

# Setup -----------------------------------------------------------------------
setwd("~/Documents/IDSC_4521/LLM_App_Analytics")
library(tidyverse)

User_df          <- read_csv('user_info.csv')
Usage_df         <- read_csv('weekly_usage.csv')
AI_App_Names_DF  <- read_csv('AI_App_Names.csv')


# Data Quality Checks ---------------------------------------------------------

# NA counts
sum(is.na(User_df))   # 351 NAs (298 rows with missing values, 295 unique PanelistIDs)
sum(is.na(Usage_df))  # 0

# Outlier detection: observations above 10080 minutes (more than a full week)
above_week <- Usage_df %>%
  filter(FOREGROUNDDURATION > 10080) %>%
  arrange(desc(FOREGROUNDDURATION))

nrow(above_week)
View(above_week)

range(Usage_df$FOREGROUNDDURATION)


# App Name Search -------------------------------------------------------------

# Unique app names containing "bot"
Usage_df %>%
  filter(grepl("bot", APPDESCRIPTION, ignore.case = TRUE)) %>%
  distinct(APPDESCRIPTION) %>%
  arrange(APPDESCRIPTION) %>%
  View()

# Unique app names containing "ai"
Usage_df %>%
  filter(grepl("ai", APPDESCRIPTION, ignore.case = TRUE)) %>%
  distinct(APPDESCRIPTION) %>%
  arrange(APPDESCRIPTION) %>%
  View()


# Top Usage Inspection --------------------------------------------------------

# Top 1000 largest FOREGROUNDDURATION observations
top_1000 <- Usage_df %>%
  arrange(desc(FOREGROUNDDURATION)) %>%
  head(1000)

View(top_1000)
view(Usage_df$FOREGROUNDDURATION)


# Duplicate Detection ---------------------------------------------------------

# # Check if any exact duplicate rows exist (all 4 columns identical)
# Usage_df %>%
#   group_by(PANELISTID, WEEK, APPDESCRIPTION, FOREGROUNDDURATION) %>%
#   filter(n() > 1) %>%
#   nrow()

# # How many total duplicate rows exist
# Usage_df %>% count(PANELISTID) %>% filter(n > 1) %>% summarise(sum(n))

# # Check if ALL duplicates follow the exact 2x2 pattern
# User_df %>%
#   group_by(PANELISTID) %>%
#   filter(n() > 1) %>%
#   count(PANELISTID) %>%
#   count(n)

# # Check how many distinct Weekday values each duplicate panelist has
# User_df %>%
#   group_by(PANELISTID) %>%
#   filter(n() > 1) %>%
#   summarise(distinct_weekday  = n_distinct(Weekday),
#             distinct_afternoon = n_distinct(Afternoon)) %>%
#   count(distinct_weekday, distinct_afternoon)

# # Get all PANELISTIDs that appear more than once
# duplicate_ids <- User_df %>%
#   count(PANELISTID) %>%
#   filter(n > 1) %>%
#   pull(PANELISTID)
# duplicate_ids

# # Look at a specific duplicate panelist
# User_df %>% filter(PANELISTID == "GWSUSA-cb618ad1-4054-448a-9058-8b57b3cd62de")
# Usage_df %>%
#   filter(PANELISTID == "GWSUSA-7c508bc1-3e8d-4600-908b-5ec694d3b10d") %>%
#   View()

# # Check panelist counts in User_df
# User_df %>%
#   count(PANELISTID) %>%
#   filter(n > 1)


# Age Missingness Analysis ----------------------------------------------------

# User_df <- User_df %>%
#   mutate(AGE_MISSING = is.na(AGE))

# User_df %>%
#   group_by(AGE_MISSING) %>%
#   summarise(
#     n            = n(),
#     pct_treated  = mean(TREATED),
#     avg_weekday  = mean(Weekday,    na.rm = TRUE),
#     avg_weekend  = mean(Weekend,    na.rm = TRUE),
#     avg_morning  = mean(Morning,    na.rm = TRUE),
#     avg_afternoon = mean(Afternoon, na.rm = TRUE),
#     avg_night    = mean(Night,      na.rm = TRUE)
#   )

# User_df %>%
#   group_by(AGE_MISSING, GENDER) %>%
#   summarise(n = n()) %>%
#   mutate(pct = n / sum(n))

# table(User_df$AGE_MISSING, User_df$TREATED)


# Gender Missingness Analysis --------------------------------------------------

# User_df <- User_df %>%
#   mutate(Gender_missing = is.na(GENDER))

# User_df %>%
#   group_by(Gender_missing) %>%
#   summarise(
#     n            = n(),
#     pct_treated  = mean(TREATED),
#     avg_weekday  = mean(Weekday,    na.rm = TRUE),
#     avg_weekend  = mean(Weekend,    na.rm = TRUE),
#     avg_morning  = mean(Morning,    na.rm = TRUE),
#     avg_afternoon = mean(Afternoon, na.rm = TRUE),
#     avg_night    = mean(Night,      na.rm = TRUE)
#   )

# User_df %>%
#   group_by(Gender_missing, AGE) %>%
#   summarise(n = n()) %>%
#   mutate(pct = n / sum(n))

# table(User_df$Gender_missing, User_df$TREATED)


# Unknown Gender Analysis -----------------------------------------------------

# User_df <- User_df %>%
#   mutate(AGE_MISSING = is.na(AGE))

# # Filter all rows where GENDER is Unknown
# unknown_gender <- User_df %>%
#   filter(GENDER == "Unknown")
# nrow(unknown_gender)
# View(unknown_gender)

# # Summary statistics for Unknown gender group
# User_df %>%
#   filter(GENDER == "Unknown") %>%
#   summarise(
#     n            = n(),
#     pct_treated  = mean(TREATED),
#     avg_weekday  = mean(Weekday,    na.rm = TRUE),
#     avg_weekend  = mean(Weekend,    na.rm = TRUE),
#     avg_morning  = mean(Morning,    na.rm = TRUE),
#     avg_afternoon = mean(Afternoon, na.rm = TRUE),
#     avg_night    = mean(Night,      na.rm = TRUE),
#     avg_age      = mean(AGE,        na.rm = TRUE)
#   )

# # Compare Unknown vs Male vs Female across all behavioral variables
# User_df %>%
#   group_by(GENDER) %>%
#   summarise(
#     n            = n(),
#     pct_treated  = mean(TREATED),
#     avg_weekday  = mean(Weekday,    na.rm = TRUE),
#     avg_weekend  = mean(Weekend,    na.rm = TRUE),
#     avg_morning  = mean(Morning,    na.rm = TRUE),
#     avg_afternoon = mean(Afternoon, na.rm = TRUE),
#     avg_night    = mean(Night,      na.rm = TRUE),
#     avg_age      = mean(AGE,        na.rm = TRUE)
#   )

# # Check if Unknown gender users overlap with missing AGE users
# User_df %>%
#   filter(GENDER == "Unknown") %>%
#   count(AGE_MISSING)

# # Check treatment breakdown for Unknown gender
# table(User_df$GENDER, User_df$TREATED)


# Export ----------------------------------------------------------------------

# # Export all unique app names to Excel
# install.packages("writexl")
# library(writexl)

# unique_apps <- Usage_df %>%
#   distinct(APPDESCRIPTION) %>%
#   arrange(APPDESCRIPTION)

# write_xlsx(unique_apps, "~/Documents/IDSC_4521/LLM_App_Analytics/unique_apps.xlsx")
