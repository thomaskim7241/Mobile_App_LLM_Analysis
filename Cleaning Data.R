# =============================================================================
# LLM App Analytics — Data Cleaning & AI Reconciliation
# =============================================================================
# Purpose: Clean Usage_df and User_df, reconcile keyword-based AI classification
# against the client TREATED flag, consolidate AI app names into canonical
# brand buckets, identify adopters, and save checkpoints for downstream scripts.
# =============================================================================

# Step 1 Load libraries and data
library(tidyverse)
setwd("~/Documents/IDSC_4521/LLM_App_Analytics")
Usage_df        <- read_csv("weekly_usage.csv")
User_df         <- read_csv("user_info.csv")
AI_App_Names_df <- read_csv("AI_App_Names.csv")

# Step 2 Remove exact duplicates from Usage_df (4 rows confirmed)
Usage_df <- Usage_df %>% distinct()

# Step 3 Fill numeric NAs with 0 in User_df behavioral columns
# 2.05% of panelists (242) had NA in at least one time-of-day column. Client
# confirmed zero-usage is a legitimate state, so NAs are treated as true zeros
# before aggregation to avoid inflating averages by silently excluding them.
User_df <- User_df %>%
  mutate(across(c(Weekday, Weekend, Morning, Afternoon, Night),
                ~ replace_na(.x, 0)))

# Step 4 Aggregate duplicate PANELISTID rows in User_df
# 85 panelists had 4 rows each from a suspected cartesian join artifact.
# first() preserves demographics, mean() collapses behavioral duplicates.
User_df <- User_df %>%
  group_by(PANELISTID) %>%
  summarise(
    AGE       = first(AGE),
    GENDER    = first(GENDER),
    CITY      = first(CITY),
    TREATED   = first(TREATED),
    Weekday   = mean(Weekday),
    Weekend   = mean(Weekend),
    Morning   = mean(Morning),
    Afternoon = mean(Afternoon),
    Night     = mean(Night),
    .groups   = "drop"
  )

# Step 5 AGE bucketing with Unknown placeholder for NA
User_df <- User_df %>%
  mutate(
    AGE_MISSING = is.na(AGE),
    AGE_GROUP = case_when(
      is.na(AGE) ~ "Unknown",
      AGE < 23   ~ "15-22 (Student)",
      AGE < 38   ~ "23-37 (Young Professional)",
      AGE < 58   ~ "38-57 (Established Career)",
      AGE >= 58  ~ "58+ (Pre-Retirement)",
      TRUE       ~ "Unknown"
    )
  )

# Step 6 Flag extreme FOREGROUNDDURATION values
# 10,080 minutes = 168 hours = 1 full week of continuous foreground use,
# the physical maximum. Extreme values are proportionally equal across
# treated (0.041%) and control (0.041%) groups, so symmetric bias does not
# affect group comparisons. Retained with flag for optional robustness checks.
Usage_df <- Usage_df %>%
  mutate(IS_EXTREME = FOREGROUNDDURATION > 10080)

# Step 7 Reconcile AI classification against client TREATED flag
# IS_AI_RAW is the initial 148-app keyword match. Client owns TREATED as
# ground truth. Final IS_AI = IS_AI_RAW AND TREATED == 1, which drops 3,035
# rows (27.8%) from 616 control panelists across 102 apps that the keyword
# classifier flagged but the client did not. Results reflect the client-defined
# adopter cohort, not all panelists who ever touched a mobile LLM app.
Usage_df <- Usage_df %>%
  mutate(IS_AI_RAW = APPDESCRIPTION %in% AI_App_Names_df$APPDESCRIPTION) %>%
  left_join(User_df %>% select(PANELISTID, TREATED), by = "PANELISTID") %>%
  mutate(IS_AI = IS_AI_RAW & (TREATED == 1))

# Step 8 Set usage threshold
# FOREGROUNDDURATION units confirmed as minutes (median 3.95, p95 434).
# 1-minute threshold approximates the 60-second minimum for filtering
# accidental opens out of the adopter analysis.
USAGE_THRESHOLD <- 1

# Step 9 Consolidate AI app names into canonical brand buckets
# Five buckets: ChatGPT, Gemini, Character.AI, AI Chatbot Nova, Other AI.
# Microsoft Copilot folded into Other AI (~25 users, below support threshold).
# Claude absent from reconciled data. Manual regex mapping is safer than
# fuzzy matching on 107K unique app names.
source("ai_app_searcher.R")
inv <- build_ai_inventory()

ai_canonical_map <- tribble(
  ~pattern,                                                ~canonical,
  "(?i)character.?ai|ncharacter\\.ai",                     "Character.AI",
  "(?i)ai chatbot.*nova|nova chatgpt",                     "AI Chatbot Nova",
  "(?i)\\bgemini\\b|\\bbard\\b|pocket bard|bionic\\.gemini|gemini9", "Gemini",
  "(?i)chatgpt|chat ?gpt|\\bgpt\\b|openai|openaibot|chatgdt|genie.*chatgpt|aico.*chat gpt|lisa gpt|gai chat|voicegpt|gpt notes|phat gpt|smart gpt|trade gpt|math ?gpt|chef ?gpt|lawyer ?gpt|alissu gpt|zinny gpt|run gpt", "ChatGPT"
)

classify_ai <- function(app_name) {
  if (is.na(app_name)) return(NA_character_)
  for (i in seq_len(nrow(ai_canonical_map))) {
    if (str_detect(app_name, ai_canonical_map$pattern[i])) {
      return(ai_canonical_map$canonical[i])
    }
  }
  return("Other AI")
}

ai_lookup <- inv %>%
  mutate(AI_CANONICAL = map_chr(APPDESCRIPTION, classify_ai))

Usage_df <- Usage_df %>%
  left_join(ai_lookup %>% select(APPDESCRIPTION, AI_CANONICAL),
            by = "APPDESCRIPTION") %>%
  mutate(AI_CANONICAL = if_else(IS_AI, AI_CANONICAL, NA_character_))

# Step 10 Identify AI adopters and compute first AI week
# For each adopter, find the earliest week they crossed the usage threshold.
# weeks_since_adoption enables cohort-time analysis (aligning all adopters
# at their personal week 0) alongside calendar-time analysis.
first_ai_week <- Usage_df %>%
  filter(IS_AI == TRUE, FOREGROUNDDURATION >= USAGE_THRESHOLD) %>%
  group_by(PANELISTID) %>%
  summarise(first_ai_week = min(WEEK), .groups = "drop")

Usage_df <- Usage_df %>%
  left_join(first_ai_week, by = "PANELISTID") %>%
  mutate(weeks_since_adoption = as.numeric(WEEK - first_ai_week) / 7)

# Step 11 Rank canonical AI buckets by user reach and duration
ai_rankings <- Usage_df %>%
  filter(IS_AI == TRUE, FOREGROUNDDURATION >= USAGE_THRESHOLD) %>%
  group_by(AI_CANONICAL) %>%
  summarise(
    total_duration_min = sum(FOREGROUNDDURATION, na.rm = TRUE),
    n_unique_users     = n_distinct(PANELISTID),
    n_user_weeks       = n(),
    avg_session_min    = mean(FOREGROUNDDURATION, na.rm = TRUE),
    .groups            = "drop"
  ) %>%
  arrange(desc(n_unique_users))

# Step 12 Save checkpoints for downstream scripts
if (!dir.exists("data")) dir.create("data")
saveRDS(Usage_df,      "data/Usage_df_cleaned.rds")
saveRDS(User_df,       "data/User_df_cleaned.rds")
saveRDS(ai_lookup,     "data/ai_canonical_lookup.rds")
saveRDS(ai_rankings,   "data/ai_rankings.rds")
saveRDS(first_ai_week, "data/first_ai_week.rds")