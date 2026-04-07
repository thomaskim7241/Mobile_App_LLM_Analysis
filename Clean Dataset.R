# =============================================================================
# LLM App Analytics — Cleaning and Normalization
# =============================================================================

## TASK 1 — FINISH DATA CLEANING
Step 1: Load libraries and data
library(tidyverse)
library(caret)
library(randomForest)
library(writexl)
setwd("~/Documents/IDSC_4521/LLM_App_Analytics")
Usage_df <- read_csv("weekly_usage.csv")
User_df <- read_csv("user_info.csv")
AI_App_Names_df  <- read_csv('AI_App_Names.csv')

Step 2: Remove exact duplicates from Usage_df
Usage_df <- Usage_df %>% distinct()

Step 3: Aggregate numerical numbers and merge panelistIDS (I believe this method
                                                           is best for analytical purposes)
User_df <- User_df %>%
  group_by(PANELISTID) %>%
  summarise(
    AGE      = first(AGE),
    GENDER   = first(GENDER),
    CITY     = first(CITY),
    TREATED  = first(TREATED),
    Weekday   = mean(Weekday, na.rm = TRUE),
    Weekend   = mean(Weekend, na.rm = TRUE),
    Morning   = mean(Morning, na.rm = TRUE),
    Afternoon = mean(Afternoon, na.rm = TRUE),
    Night     = mean(Night, na.rm = TRUE),
    .groups = "drop"
  )

Step 4: Flag missing AGE and Unknown gender
User_df <- User_df %>%
  mutate(AGE_MISSING = is.na(AGE))

# Step 5: Foregroundduration has been determined as systematically distributed 
# between both groups
# # Allows robustness check later if needed
# Usage_df <- Usage_df %>%
#   mutate(IS_EXTREME = FOREGROUNDDURATION > 10080)
# 
# # Check if extreme values are balanced across treatment groups
# Usage_df %>%
#   left_join(User_df %>% select(PANELISTID, TREATED), by = "PANELISTID") %>%
#   group_by(TREATED, IS_EXTREME) %>%
#   summarise(n = n(), .groups = "drop")

Step 6: Flag AI apps in Usage_df
Usage_df <- Usage_df %>%
  mutate(IS_AI = APPDESCRIPTION %in% AI_App_Names_df$APPDESCRIPTION)

Step 7: Filling in numerical NA values
User_df <- User_df %>%
  mutate(across(c(Weekday, Weekend, Morning, Afternoon, Night),
                ~replace_na(.x, 0)))