# =============================================================================
# LLM App Analytics — Cleaning dataset
# =============================================================================

## TASK 1 — FINISH DATA CLEANING
# Step 1 Load libraries and data
library(tidyverse)
library(caret)
library(randomForest)
library(writexl)
setwd("~/Documents/IDSC_4521/LLM_App_Analytics")
Usage_df <- read_csv("weekly_usage.csv")
User_df <- read_csv("user_info.csv")
AI_App_Names_df  <- read_csv('AI_App_Names.csv')

# Step 2 Remove exact duplicates from Usage_df
Usage_df <- Usage_df %>% distinct()

# Step 3 — Fill numeric NAs with 0 before aggregation
# Rationale: NAs in usage columns represent true zero activity,
# not missing data. Replacing with 0 before averaging ensures
# zero-usage periods are correctly weighted in the aggregated mean
# rather than being silently excluded, which would inflate averages.
User_df <- User_df %>%
  mutate(across(c(Weekday, Weekend, Morning, Afternoon, Night),
                ~replace_na(.x, 0)))

# Step 4 Aggregate numerical numbers and merge panelistIDS (I believe this method
                                                           # is best for analytical purposes)
User_df <- User_df %>%
  group_by(PANELISTID) %>%
  summarise(
    AGE      = first(AGE),
    GENDER   = first(GENDER),
    CITY     = first(CITY),
    TREATED  = first(TREATED),
    Weekday   = mean(Weekday),
    Weekend   = mean(Weekend),
    Morning   = mean(Morning),
    Afternoon = mean(Afternoon),
    Night     = mean(Night),
    .groups = "drop"
  )

# Step 5 Flag missing AGE and Unknown gender, Give NA age unknwon
# Create AGE buckets per professor guidance
User_df <- User_df %>%
  mutate(
    AGE_MISSING = is.na(AGE),
    AGE_GROUP = case_when(
      is.na(AGE)       ~ "Unknown",
      AGE < 23         ~ "15-22 (Student)",
      AGE < 38         ~ "23-37 (Young Professional)",
      AGE < 58         ~ "38-57 (Established Career)",
      AGE >= 58        ~ "58+ (Pre-Retirement)",
      TRUE             ~ "Unknown"
    )
  )

# Step 6: FOREGROUNDDURATION outlier decision and flagging
# Extreme values (> 10,080 minutes) were investigated and found to be
# proportionally equal across treated (0.041%) and control (0.041%) groups.
# Per client guidance, symmetric bias does not affect group comparisons.
# Values retained to avoid excluding heavy users who may carry analytical signal.
# IS_EXTREME flag available for robustness checks if needed.
# # Allows robustness check later if needed
Usage_df <- Usage_df %>%
  mutate(IS_EXTREME = FOREGROUNDDURATION > 10080)

# # Check if extreme values are balanced across treatment groups
# Usage_df %>%
#   left_join(User_df %>% select(PANELISTID, TREATED), by = "PANELISTID") %>%
#   group_by(TREATED, IS_EXTREME) %>%
#   summarise(n = n(), .groups = "drop")

# Step 7 Flag AI apps in Usage_df
Usage_df <- Usage_df %>%
  mutate(IS_AI = APPDESCRIPTION %in% AI_App_Names_df$APPDESCRIPTION)

# Step 8: Build panel-level feature matrix
# Aggregate Usage_df to one row per panelist then join User_df demographics
panel_df <- Usage_df %>%
  group_by(PANELISTID) %>%
  summarise(
    total_duration      = sum(FOREGROUNDDURATION),
    avg_weekly_duration = mean(FOREGROUNDDURATION),
    n_unique_apps       = n_distinct(APPDESCRIPTION),
    n_weeks_observed    = n_distinct(WEEK),
    ai_app_duration     = sum(FOREGROUNDDURATION[IS_AI == TRUE]),
    non_ai_duration     = sum(FOREGROUNDDURATION[IS_AI == FALSE]),
    pct_ai_usage        = ai_app_duration / total_duration,
    n_ai_apps_used      = n_distinct(APPDESCRIPTION[IS_AI == TRUE]),
    .groups = "drop"
  ) %>%
  left_join(
    User_df %>% select(PANELISTID, AGE, GENDER, CITY, TREATED,
                       Weekday, Weekend, Morning, Afternoon, Night,
                       AGE_MISSING, AGE_GROUP),
    by = "PANELISTID"
  )