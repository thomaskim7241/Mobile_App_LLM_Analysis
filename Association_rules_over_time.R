# =============================================================================
# Association_rules_over_time.R
# Project: IDSC 4521 Capstone — Mobile LLM App Landscape
# Purpose: Mine monthly association rules with AI canonical buckets on the LHS
#          and categorized non-AI app types on the RHS, across the 2023 adopter
#          cohort. Track how co-usage patterns evolve over the year ChatGPT
#          went mainstream.
# =============================================================================

library(tidyverse)
library(arules)
library(lubridate)

setwd("~/Documents/IDSC_4521/LLM_App_Analytics")
dir.create("data",   showWarnings = FALSE, recursive = TRUE)
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# PART A — Build reduced working dataset
# =============================================================================

Usage_df <- readRDS("data/Usage_df_cleaned.rds")
User_df  <- readRDS("data/User_df_cleaned.rds")

USAGE_THRESHOLD     <- 1   # 1 minute (FOREGROUNDDURATION is in minutes)
NON_AI_MIN_ADOPTERS <- 10  # drop non-AI apps used by fewer than 10 adopters

# A1: Reclassify missed AI apps that slipped past the keyword list
# These were identified during the manual non-AI app review.
late_ai_apps <- c("Ask AI - Chat with Chatbot",
                  "Imagine : AI Art Generator",
                  "Wonder - AI Art Generator",
                  "starryai - Create Art with AI",
                  "ChatOn",
                  "Chat Smith",
                  "DaVinci",
                  "Nerd AI",
                  "ParagraphAI",
                  "Writecream",
                  "Talkie: Soulful Character AI",
                  "Replika: My AI Friend",
                  "iFriend - Virtual AI Friend",
                  "Chai - Chat with AI Friends")

Usage_df <- Usage_df %>%
  mutate(
    IS_AI        = if_else(APPDESCRIPTION %in% late_ai_apps & TREATED == 1,
                           TRUE, IS_AI),
    AI_CANONICAL = if_else(APPDESCRIPTION %in% late_ai_apps & TREATED == 1,
                           "Other AI", AI_CANONICAL)
  )

# A2: Restrict to TREATED panelists, apply usage threshold
adopter_ids <- User_df %>%
  filter(TREATED == 1) %>%
  pull(PANELISTID)

usage_adopters <- Usage_df %>%
  filter(PANELISTID %in% adopter_ids,
         FOREGROUNDDURATION >= USAGE_THRESHOLD)

# A3: Define AI buckets and build unified ITEM column
ai_buckets <- c("ChatGPT", "Gemini", "Character.AI",
                "AI Chatbot Nova", "Other AI")

usage_adopters <- usage_adopters %>%
  mutate(ITEM = if_else(IS_AI, AI_CANONICAL, APPDESCRIPTION))

# A4: Load app category mapping and join
app_categories <- read_csv("data/app_category_map.csv")

usage_adopters <- usage_adopters %>%
  left_join(app_categories %>% rename(ITEM_CAT = category),
            by = "ITEM") %>%
  mutate(
    ITEM_CATEGORIZED = case_when(
      ITEM %in% ai_buckets       ~ ITEM,
      ITEM_CAT == "DROP"          ~ NA_character_,
      !is.na(ITEM_CAT)           ~ ITEM_CAT,
      TRUE                        ~ "Other"
    )
  ) %>%
  filter(!is.na(ITEM_CATEGORIZED))

# A5: Pre-filter categories with fewer than NON_AI_MIN_ADOPTERS unique users
category_keepers <- usage_adopters %>%
  filter(!ITEM_CATEGORIZED %in% ai_buckets) %>%
  group_by(ITEM_CATEGORIZED) %>%
  summarise(n_adopters = n_distinct(PANELISTID), .groups = "drop") %>%
  filter(n_adopters >= NON_AI_MIN_ADOPTERS) %>%
  pull(ITEM_CATEGORIZED)

usage_adopters <- usage_adopters %>%
  filter(ITEM_CATEGORIZED %in% c(ai_buckets, category_keepers))

# A6: Add MONTH column for temporal grouping (12 monthly periods)
usage_adopters <- usage_adopters %>%
  mutate(MONTH = floor_date(WEEK, "month"))

# A7: Keep only columns the rules script needs
usage_adopters <- usage_adopters %>%
  select(PANELISTID, MONTH, ITEM_CATEGORIZED, FOREGROUNDDURATION)

# A8: Save reduced working dataset
saveRDS(usage_adopters, "data/usage_adopters.rds")

cat("Reduced working dataset built\n")
cat("  Rows:                ", nrow(usage_adopters), "\n")
cat("  Unique items:        ", n_distinct(usage_adopters$ITEM_CATEGORIZED), "\n")
cat("  Unique panelists:    ", n_distinct(usage_adopters$PANELISTID), "\n")
cat("  Months:              ", n_distinct(usage_adopters$MONTH), "\n\n")

# =============================================================================
# PART B — Association rules over time
# =============================================================================

# Apriori parameters
APRIORI_PARAMS <- list(
  supp   = 0.001,
  conf   = 0.5,
  minlen = 2,
  maxlen = 2
)

# Categories to exclude from RHS (too ubiquitous or too heterogeneous)
EXCLUDE_RHS <- c("System & Utilities", "Other")

# B1: Helper function
mine_month_rules <- function(month_data, month_label, ai_buckets, params) {
  
  baskets <- month_data %>%
    distinct(PANELISTID, ITEM_CATEGORIZED) %>%
    group_by(PANELISTID) %>%
    summarise(items = list(ITEM_CATEGORIZED), .groups = "drop") %>%
    pull(items)
  
  if (length(baskets) == 0) return(NULL)
  
  transactions <- as(baskets, "transactions")
  
  # Only constrain LHS to AI buckets actually present in this month
  available_items    <- itemLabels(transactions)
  ai_buckets_present <- intersect(ai_buckets, available_items)
  
  if (length(ai_buckets_present) == 0) {
    cat("  No AI buckets present in", as.character(month_label), "skipping\n")
    return(NULL)
  }
  
  rules <- apriori(
    transactions,
    parameter  = list(supp   = params$supp,
                      conf   = params$conf,
                      minlen = params$minlen,
                      maxlen = params$maxlen),
    appearance = list(lhs     = ai_buckets_present,
                      default = "rhs"),
    control    = list(verbose = FALSE)
  )
  
  if (length(rules) == 0) return(NULL)
  
  rules_df <- as(rules, "data.frame") %>%
    as_tibble() %>%
    mutate(
      month     = month_label,
      n_baskets = length(transactions),
      lhs       = str_extract(rules, "(?<=\\{)[^}]+(?=\\} =>)"),
      rhs       = str_extract(rules, "(?<==> \\{)[^}]+(?=\\})")
    ) %>%
    select(month, lhs, rhs, support, confidence, lift, count, n_baskets)
  
  # Filter out junk RHS categories
  rules_df <- rules_df %>%
    filter(!rhs %in% EXCLUDE_RHS)
  
  return(rules_df)
}

# B2: Loop over 12 months
months_in_panel <- usage_adopters %>%
  distinct(MONTH) %>%
  arrange(MONTH) %>%
  pull(MONTH)

rules_over_time <- map_dfr(months_in_panel, function(m) {
  cat("Mining rules for", as.character(m), "...\n")
  month_data <- usage_adopters %>% filter(MONTH == m)
  mine_month_rules(month_data, m, ai_buckets, APRIORI_PARAMS)
})

cat("\nTotal rules mined across all months:", nrow(rules_over_time), "\n")
cat("Unique rules:",
    n_distinct(paste(rules_over_time$lhs, rules_over_time$rhs)), "\n\n")

# B3: Persistence summary (rules appearing in 2+ months are more reliable)
rule_persistence <- rules_over_time %>%
  group_by(lhs, rhs) %>%
  summarise(
    n_months_present = n(),
    avg_lift         = mean(lift),
    avg_confidence   = mean(confidence),
    avg_support      = mean(support),
    .groups          = "drop"
  ) %>%
  arrange(desc(n_months_present), desc(avg_lift))

rules_over_time <- rules_over_time %>%
  left_join(
    rule_persistence %>% select(lhs, rhs, n_months_present),
    by = c("lhs", "rhs")
  )

# B4: Save rule outputs
saveRDS(rules_over_time,  "data/rules_over_time.rds")
saveRDS(rule_persistence, "data/rule_persistence.rds")

# =============================================================================
# PART C — Visualizations
# =============================================================================

# C1: Line chart — lift of top 10 most persistent rules over time
top_rules <- rule_persistence %>%
  filter(n_months_present >= 6) %>%
  slice_max(avg_lift, n = 10) %>%
  mutate(rule_label = paste0(lhs, " => ", rhs))

p1_data <- rules_over_time %>%
  mutate(rule_label = paste0(lhs, " => ", rhs)) %>%
  filter(rule_label %in% top_rules$rule_label)

p1 <- ggplot(p1_data, aes(x = month, y = lift,
                          color = rule_label, group = rule_label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    title    = "Top 10 Persistent AI Co-Usage Rules Over 2023",
    subtitle = "Lift across the year for rules present in 6+ months",
    x        = NULL, y = "Lift",
    color    = "Rule"
  ) +
  theme_bw() +
  theme(
    legend.position       = "right",
    legend.text           = element_text(size = 8, color = "black"),
    legend.title          = element_text(color = "black"),
    axis.text.x           = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y           = element_text(color = "black"),
    axis.title            = element_text(color = "black"),
    plot.title            = element_text(color = "black", face = "bold"),
    plot.subtitle         = element_text(color = "black"),
    panel.background      = element_rect(fill = "white", color = NA),
    plot.background       = element_rect(fill = "white", color = NA),
    panel.grid.major      = element_line(color = "grey85"),
    panel.grid.minor      = element_line(color = "grey92")
  )

ggsave("output/01_top_rules_lift_over_time.png", p1,
       width = 12, height = 6, dpi = 300)

# C2: Heatmap — companion category x month, faceted by AI bucket
p2_data <- rules_over_time %>%
  filter(lhs %in% c("ChatGPT", "Gemini")) %>%
  group_by(lhs, rhs) %>%
  filter(n() >= 3) %>%
  ungroup()

p2 <- ggplot(p2_data, aes(x = month, y = reorder(rhs, lift), fill = lift)) +
  geom_tile(color = "white") +
  facet_wrap(~ lhs, scales = "free_y") +
  scale_fill_viridis_c(option = "viridis") +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  labs(
    title    = "Companion App Category Lift by Month: ChatGPT vs Gemini",
    subtitle = "Higher lift = stronger co-usage relative to chance",
    x        = NULL, y = NULL,
    fill     = "Lift"
  ) +
  theme_bw() +
  theme(
    axis.text.y           = element_text(size = 8, color = "black"),
    axis.text.x           = element_text(color = "black"),
    strip.text            = element_text(color = "black", face = "bold"),
    strip.background      = element_rect(fill = "grey95", color = "grey80"),
    legend.text           = element_text(color = "black"),
    legend.title          = element_text(color = "black"),
    plot.title            = element_text(color = "black", face = "bold"),
    plot.subtitle         = element_text(color = "black"),
    panel.background      = element_rect(fill = "white", color = NA),
    plot.background       = element_rect(fill = "white", color = NA),
    panel.grid            = element_blank()
  )

ggsave("output/02_companion_heatmap.png", p2,
       width = 12, height = 8, dpi = 300)

# C3: Emergence chart — count of new rules first appearing each month
emergence <- rules_over_time %>%
  group_by(lhs, rhs) %>%
  summarise(first_month = min(month), .groups = "drop") %>%
  count(first_month, name = "new_rules") %>%
  arrange(first_month)

p3 <- ggplot(emergence, aes(x = first_month, y = new_rules)) +
  geom_col(fill = "#2C6BAC", color = "white") +
  geom_text(aes(label = new_rules), vjust = -0.5, size = 3, color = "black") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    title    = "New AI Co-Usage Rules Emerging Each Month in 2023",
    subtitle = "Count of association rules appearing for the first time",
    x        = NULL, y = "New rules"
  ) +
  theme_bw() +
  theme(
    axis.text.x           = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y           = element_text(color = "black"),
    axis.title            = element_text(color = "black"),
    plot.title            = element_text(color = "black", face = "bold"),
    plot.subtitle         = element_text(color = "black"),
    panel.background      = element_rect(fill = "white", color = NA),
    plot.background       = element_rect(fill = "white", color = NA),
    panel.grid.major.x    = element_blank(),
    panel.grid.minor      = element_blank(),
    panel.grid.major.y    = element_line(color = "grey85")
  )

ggsave("output/03_rule_emergence.png", p3,
       width = 10, height = 5, dpi = 300)

# C4: All rules summary table — sorted by persistence then lift
p4_data <- rule_persistence %>%
  filter(n_months_present >= 3) %>%
  arrange(desc(n_months_present), desc(avg_lift))

cat("\n=== Rules present in 3+ months ===\n")
print(p4_data, n = Inf)

# =============================================================================
# PART D — Export
# =============================================================================

write_csv(rules_over_time,  "output/rules_over_time.csv")
write_csv(rule_persistence, "output/rule_persistence.csv")

cat("\n=== Association rules analysis complete ===\n")
cat("Charts saved to output/\n")
cat("Data saved to data/ and output/\n")

rules_over_time %>%
  arrange(lhs, rhs, month) %>%
  print(n = Inf)




### make model to see banking models vs AI usage with treated vs untreated users
# Define treated_ids from User_df
treated_ids <- User_df %>%
  filter(TREATED == 1) %>%
  pull(PANELISTID)

# What banking apps are Gemini users actually using
gemini_users <- Usage_df %>%
  filter(PANELISTID %in% treated_ids, AI_CANONICAL == "Gemini") %>%
  pull(PANELISTID) %>% unique()

Usage_df %>%
  filter(PANELISTID %in% gemini_users,
         APPDESCRIPTION %in% (app_categories %>% 
                                filter(category == "Banking & Finance") %>% 
                                pull(ITEM))) %>%
  group_by(APPDESCRIPTION) %>%
  summarise(n_users = n_distinct(PANELISTID),
            total_duration = sum(FOREGROUNDDURATION, na.rm = TRUE)) %>%
  arrange(desc(n_users)) %>%
  head(15)


audio_apps <- app_categories %>% filter(category == "Streaming Audio") %>% pull(ITEM)

# Compare audio app usage: treated vs hypothetical baseline
Usage_df %>%
  filter(APPDESCRIPTION %in% audio_apps) %>%
  mutate(treated = PANELISTID %in% treated_ids) %>%
  group_by(APPDESCRIPTION, treated) %>%
  summarise(n_users = n_distinct(PANELISTID), .groups = "drop") %>%
  pivot_wider(names_from = treated, values_from = n_users, 
              names_prefix = "treated_", values_fill = 0) %>%
  mutate(treated_rate = treated_TRUE / 1623,
         control_rate = treated_FALSE / 9915,
         gap = treated_rate - control_rate) %>%
  arrange(gap)



# Average monthly AI app duration per treated user
ai_duration_per_user <- Usage_df %>%
  filter(PANELISTID %in% treated_ids, IS_AI == TRUE) %>%
  group_by(PANELISTID) %>%
  summarise(total_minutes_year = sum(FOREGROUNDDURATION, na.rm = TRUE)) %>%
  summarise(avg_monthly_minutes = mean(total_minutes_year) / 12) %>%
  pull(avg_monthly_minutes)

cat("Average monthly AI minutes per treated user:", round(ai_duration_per_user, 1), "\n")



rules_over_time %>%
  mutate(period = if_else(month < as.Date("2023-07-01"), "H1 2023", "H2 2023")) %>%
  group_by(period, lhs, rhs) %>%
  summarise(avg_lift = mean(lift), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = avg_lift) %>%
  mutate(change = `H2 2023` - `H1 2023`) %>%
  arrange(desc(abs(change))) %>%
  head(15)



gemini_telecom <- Usage_df %>%
  filter(PANELISTID %in% gemini_users,
         month(WEEK) %in% c(7, 9),
         APPDESCRIPTION %in% (app_categories %>% 
                                filter(category == "Telecom") %>% pull(ITEM))) %>%
  group_by(APPDESCRIPTION, MONTH = floor_date(WEEK, "month")) %>%
  summarise(n_users = n_distinct(PANELISTID), .groups = "drop") %>%
  arrange(desc(n_users))
gemini_telecom



# =============================================================================
# Pressure Test: Five questions before finalizing the paper's framing
# Run after Cleaning.R and Association_rules_over_time.R
# =============================================================================

library(tidyverse)
library(lubridate)

setwd("~/Documents/IDSC_4521/LLM_App_Analytics")

# Reload the objects the questions rely on
Usage_df        <- readRDS("data/Usage_df_cleaned.rds")
User_df         <- readRDS("data/User_df_cleaned.rds")
usage_adopters  <- readRDS("data/usage_adopters.rds")
rules_over_time <- readRDS("data/rules_over_time.rds")

treated_ids <- User_df %>% filter(TREATED == 1) %>% pull(PANELISTID)
control_ids <- User_df %>% filter(TREATED == 0) %>% pull(PANELISTID)

# =============================================================================
# QUESTION 1: Before/after treatment design
# Can we actually rule out causation for Gemini banking association?
# =============================================================================

cat("\n========== QUESTION 1: BEFORE/AFTER TREATMENT ==========\n\n")

# Step 1: Derive each treated user's first AI usage week as their treatment date
treatment_dates <- Usage_df %>%
  filter(PANELISTID %in% treated_ids, IS_AI == TRUE) %>%
  group_by(PANELISTID) %>%
  summarise(treatment_week = min(WEEK), .groups = "drop")

cat("Treatment date distribution:\n")
treatment_dates %>%
  mutate(treatment_month = floor_date(treatment_week, "month")) %>%
  count(treatment_month) %>%
  arrange(treatment_month) %>%
  print(n = Inf)

# Step 2: Identify banking apps
banking_apps <- c("PayPal", "Cash App", "Coinbase", "Venmo", "Google Pay",
                  "Robinhood", "Coinbase Wallet", "Chime Banking", "Samsung Pay",
                  "Credit Karma", "Crypto.com", "SoFi", "COIN", "Webull",
                  "Capital One Mobile")

# Step 3: Compare banking usage in the 4 weeks before vs after treatment
before_after <- Usage_df %>%
  filter(PANELISTID %in% treated_ids,
         APPDESCRIPTION %in% banking_apps) %>%
  inner_join(treatment_dates, by = "PANELISTID") %>%
  mutate(
    weeks_from_treatment = as.numeric(WEEK - treatment_week) / 7,
    period = case_when(
      weeks_from_treatment >= -4 & weeks_from_treatment < 0  ~ "Before (4wks)",
      weeks_from_treatment >= 0  & weeks_from_treatment <= 4 ~ "After (4wks)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

banking_shift <- before_after %>%
  group_by(PANELISTID, period) %>%
  summarise(
    banking_minutes = sum(FOREGROUNDDURATION, na.rm = TRUE),
    banking_apps_used = n_distinct(APPDESCRIPTION),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = c(banking_minutes, banking_apps_used),
    values_fill = 0
  )

cat("\nMean banking minutes before vs after AI adoption:\n")
banking_shift %>%
  summarise(
    mean_before = mean(`banking_minutes_Before (4wks)`),
    mean_after  = mean(`banking_minutes_After (4wks)`),
    change_min  = mean_after - mean_before,
    pct_change  = round((mean_after - mean_before) / mean_before * 100, 1)
  ) %>% print()

# Paired t-test
t_test_result <- t.test(
  banking_shift$`banking_minutes_After (4wks)`,
  banking_shift$`banking_minutes_Before (4wks)`,
  paired = TRUE
)
cat("\nPaired t-test (after vs before):\n")
print(t_test_result)

# =============================================================================
# QUESTION 2: Clean the control group of contamination
# =============================================================================

cat("\n========== QUESTION 2: CONTROL GROUP CONTAMINATION ==========\n\n")

contaminated_controls <- Usage_df %>%
  filter(PANELISTID %in% control_ids, IS_AI == TRUE) %>%
  pull(PANELISTID) %>%
  unique()

cat("Contaminated control users:", length(contaminated_controls), "\n")
cat("Percentage of control group:", 
    round(length(contaminated_controls) / length(control_ids) * 100, 2), "%\n")

clean_control_ids <- setdiff(control_ids, contaminated_controls)
cat("Clean control users:", length(clean_control_ids), "\n\n")

# Check how much AI usage contaminated controls have
contamination_intensity <- Usage_df %>%
  filter(PANELISTID %in% contaminated_controls, IS_AI == TRUE) %>%
  group_by(PANELISTID) %>%
  summarise(
    total_ai_minutes = sum(FOREGROUNDDURATION, na.rm = TRUE),
    n_ai_weeks = n_distinct(WEEK),
    .groups = "drop"
  )

cat("Intensity of contamination (AI usage among contaminated controls):\n")
summary(contamination_intensity)

# =============================================================================
# QUESTION 3: Is Gemini actually an Android effect?
# Check whether Gemini users have systematically different device signals
# =============================================================================

cat("\n========== QUESTION 3: ANDROID CONFOUND ==========\n\n")

# Proxy for Android users: Samsung apps, Google Pay, My T-Mobile, etc.
android_indicator_apps <- c("Samsung Pay", "Samsung Health", "Google Pay",
                            "Google Photos", "Android Auto")

gemini_users <- Usage_df %>%
  filter(PANELISTID %in% treated_ids, AI_CANONICAL == "Gemini") %>%
  pull(PANELISTID) %>% unique()

chatgpt_users <- Usage_df %>%
  filter(PANELISTID %in% treated_ids, AI_CANONICAL == "ChatGPT") %>%
  pull(PANELISTID) %>% unique()

android_check <- Usage_df %>%
  filter(APPDESCRIPTION %in% android_indicator_apps) %>%
  mutate(
    user_group = case_when(
      PANELISTID %in% gemini_users  ~ "Gemini",
      PANELISTID %in% chatgpt_users ~ "ChatGPT",
      PANELISTID %in% control_ids   ~ "Control",
      TRUE ~ "Other Treated"
    )
  ) %>%
  group_by(user_group, APPDESCRIPTION) %>%
  summarise(n_users = n_distinct(PANELISTID), .groups = "drop") %>%
  left_join(
    tibble(
      user_group = c("Gemini", "ChatGPT", "Control", "Other Treated"),
      total_users = c(length(gemini_users), length(chatgpt_users),
                      length(control_ids),
                      length(treated_ids) - length(gemini_users) - length(chatgpt_users))
    ),
    by = "user_group"
  ) %>%
  mutate(adoption_rate = round(n_users / total_users * 100, 1))

cat("Android indicator app adoption rates by group:\n")
android_check %>%
  select(user_group, APPDESCRIPTION, adoption_rate) %>%
  pivot_wider(names_from = user_group, values_from = adoption_rate,
              values_fill = 0) %>%
  print()

# =============================================================================
# QUESTION 4: Do treated users actually look different from controls?
# Compare banking app rates between treated Gemini users and clean controls
# =============================================================================

cat("\n========== QUESTION 4: TREATED vs CONTROL COMPARISON ==========\n\n")

comparison_df <- Usage_df %>%
  filter(APPDESCRIPTION %in% banking_apps) %>%
  mutate(
    user_group = case_when(
      PANELISTID %in% gemini_users          ~ "Gemini",
      PANELISTID %in% chatgpt_users         ~ "ChatGPT",
      PANELISTID %in% clean_control_ids     ~ "Clean Control",
      TRUE ~ "Other"
    )
  ) %>%
  filter(user_group != "Other") %>%
  group_by(user_group, APPDESCRIPTION) %>%
  summarise(n_users = n_distinct(PANELISTID), .groups = "drop") %>%
  left_join(
    tibble(
      user_group = c("Gemini", "ChatGPT", "Clean Control"),
      total_users = c(length(gemini_users), length(chatgpt_users),
                      length(clean_control_ids))
    ),
    by = "user_group"
  ) %>%
  mutate(adoption_rate = round(n_users / total_users * 100, 1))

rate_comparison <- comparison_df %>%
  select(user_group, APPDESCRIPTION, adoption_rate) %>%
  pivot_wider(names_from = user_group, values_from = adoption_rate,
              values_fill = 0) %>%
  mutate(
    gemini_gap  = Gemini - `Clean Control`,
    chatgpt_gap = ChatGPT - `Clean Control`
  ) %>%
  arrange(desc(gemini_gap))

cat("Banking app adoption rates (%) by user group:\n")
print(rate_comparison)

cat("\nInterpretation guide:\n")
cat("  gemini_gap > 5   = Gemini users meaningfully more likely to use this app\n")
cat("  gemini_gap < -5  = Gemini users meaningfully less likely to use this app\n")
cat("  |gap| < 5        = Signal is weak, finding might not survive scrutiny\n")

# =============================================================================
# QUESTION 5: Did the treated user pool grow through the year?
# Test whether saturation story holds
# =============================================================================

cat("\n========== QUESTION 5: TREATED POOL GROWTH ==========\n\n")

cumulative_treated <- treatment_dates %>%
  mutate(treatment_month = floor_date(treatment_week, "month")) %>%
  count(treatment_month, name = "new_adopters") %>%
  arrange(treatment_month) %>%
  mutate(cumulative = cumsum(new_adopters))

cat("Cumulative treated users by month:\n")
print(cumulative_treated)

cat("\nGrowth ratio (Dec vs Jan cumulative):\n")
growth_ratio <- tail(cumulative_treated$cumulative, 1) / 
  head(cumulative_treated$cumulative, 1)
cat("  ", round(growth_ratio, 2), "x\n")

cat("\nActive treated users per month (people actually using AI that month):\n")
active_by_month <- Usage_df %>%
  filter(PANELISTID %in% treated_ids, IS_AI == TRUE) %>%
  mutate(month = floor_date(WEEK, "month")) %>%
  group_by(month) %>%
  summarise(active_users = n_distinct(PANELISTID), .groups = "drop") %>%
  arrange(month)
print(active_by_month)

# Visual
ggplot(cumulative_treated, aes(x = treatment_month, y = cumulative)) +
  geom_line(linewidth = 1, color = "#2F5496") +
  geom_point(size = 2, color = "#2F5496") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    title = "Cumulative Treated Users by Month (2023)",
    subtitle = "Testing the saturation hypothesis",
    x = NULL, y = "Cumulative treated users"
  ) +
  theme_bw()

ggsave("output/04_treated_growth.png", width = 10, height = 5, dpi = 300)

cat("\n========== ALL FIVE QUESTIONS COMPLETE ==========\n")


# =============================================================================
# Logistic regression: Does AI adoption predict financial app usage?
# Outcome: Binary indicator for whether the user uses any finance app
# Predictors: AI adoption, controls
# Visualization: User segment coloring for interpretability
# =============================================================================

library(tidyverse)
library(broom)

# Define the finance app universe from the Banking & Finance category
# Pulled from your actual app_categories mapping
finance_apps <- app_categories %>%
  filter(category == "Banking & Finance") %>%
  pull(ITEM)

cat("Number of finance apps in scope:", length(finance_apps), "\n")

# =============================================================================
# Build user level dataset
# =============================================================================

reg_df <- User_df %>%
  select(PANELISTID, AGE, GENDER, TREATED, Weekday, Weekend,
         Morning, Afternoon, Night) %>%
  # AI tool flags
  left_join(
    Usage_df %>%
      filter(IS_AI == TRUE) %>%
      group_by(PANELISTID) %>%
      summarise(
        used_chatgpt   = any(AI_CANONICAL == "ChatGPT"),
        used_gemini    = any(AI_CANONICAL == "Gemini"),
        used_character = any(AI_CANONICAL == "Character.AI"),
        used_nova      = any(AI_CANONICAL == "AI Chatbot Nova"),
        used_other_ai  = any(AI_CANONICAL == "Other AI"),
        total_ai_minutes = sum(FOREGROUNDDURATION, na.rm = TRUE),
        n_ai_apps        = n_distinct(APPDESCRIPTION),
        .groups = "drop"
      ),
    by = "PANELISTID"
  ) %>%
  # Finance outcome: binary indicator and intensity
  left_join(
    Usage_df %>%
      filter(APPDESCRIPTION %in% finance_apps, FOREGROUNDDURATION > 0) %>%
      group_by(PANELISTID) %>%
      summarise(
        uses_finance        = TRUE,
        total_finance_min   = sum(FOREGROUNDDURATION, na.rm = TRUE),
        n_finance_apps      = n_distinct(APPDESCRIPTION),
        .groups = "drop"
      ),
    by = "PANELISTID"
  ) %>%
  # Overall engagement controls
  left_join(
    Usage_df %>%
      group_by(PANELISTID) %>%
      summarise(
        total_minutes = sum(FOREGROUNDDURATION, na.rm = TRUE),
        n_apps_used   = n_distinct(APPDESCRIPTION),
        .groups = "drop"
      ),
    by = "PANELISTID"
  ) %>%
  mutate(
    across(c(used_chatgpt, used_gemini, used_character, used_nova,
             used_other_ai, uses_finance),
           ~replace_na(.x, FALSE)),
    across(c(used_chatgpt, used_gemini, used_character, used_nova,
             used_other_ai, uses_finance),
           as.integer),
    total_ai_minutes = replace_na(total_ai_minutes, 0),
    n_ai_apps        = replace_na(n_ai_apps, 0),
    total_finance_min = replace_na(total_finance_min, 0),
    n_finance_apps    = replace_na(n_finance_apps, 0),
    log_total_minutes = log(total_minutes + 1),
    log_n_apps        = log(n_apps_used + 1),
    age_group = case_when(
      is.na(AGE)  ~ "Unknown",
      AGE < 23    ~ "15-22 Student",
      AGE < 38    ~ "23-37 Young Prof",
      AGE < 58    ~ "38-57 Established",
      AGE >= 58   ~ "58+ Pre-Retirement"
    ),
    age_group = factor(age_group, levels = c("38-57 Established", "15-22 Student",
                                             "23-37 Young Prof", "58+ Pre-Retirement",
                                             "Unknown"))
  ) %>%
  filter(!is.na(GENDER))

cat("Dataset size:", nrow(reg_df), "\n")
cat("Finance app users:", sum(reg_df$uses_finance), 
    "(", round(mean(reg_df$uses_finance) * 100, 1), "%)\n\n")

# =============================================================================
# MAIN MODEL: AI tools + demographics + engagement controls
# =============================================================================

main_model <- glm(
  uses_finance ~ used_chatgpt + used_gemini + used_character + used_nova +
    used_other_ai + age_group + GENDER +
    log_total_minutes + log_n_apps,
  data   = reg_df,
  family = binomial(link = "logit")
)

cat("=== MAIN MODEL ===\n")
summary(main_model)

# =============================================================================
# Forest plot with user type coloring
# =============================================================================

coef_df <- tidy(main_model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # Clean up term names
    clean_term = case_when(
      term == "used_chatgpt"      ~ "ChatGPT",
      term == "used_gemini"       ~ "Gemini",
      term == "used_character"    ~ "Character.AI",
      term == "used_nova"         ~ "AI Chatbot Nova",
      term == "used_other_ai"     ~ "Other AI",
      str_detect(term, "age_group") ~ str_replace(term, "age_group", ""),
      term == "GENDERMale"        ~ "Male",
      term == "log_total_minutes" ~ "Total usage (log)",
      term == "log_n_apps"        ~ "App diversity (log)",
      TRUE ~ term
    ),
    # Categorize each predictor for color
    predictor_type = case_when(
      term %in% c("used_chatgpt", "used_gemini", "used_character",
                  "used_nova", "used_other_ai")    ~ "AI Tool",
      str_detect(term, "age_group")                ~ "Age Group",
      term == "GENDERMale"                         ~ "Gender",
      term %in% c("log_total_minutes", "log_n_apps") ~ "Engagement",
      TRUE ~ "Other"
    ),
    predictor_type = factor(predictor_type,
                            levels = c("AI Tool", "Engagement", "Age Group", "Gender")),
    significant = p.value < 0.05
  ) %>%
  arrange(predictor_type, desc(estimate))

# Forest plot
forest_plot <- ggplot(coef_df,
                      aes(x = estimate,
                          y = reorder(clean_term, as.numeric(predictor_type) * -1000 + estimate),
                          color = predictor_type)) +
  geom_point(size = 3.5, aes(shape = significant)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_x_log10(breaks = c(0.5, 0.75, 1, 1.5, 2, 3, 5, 10)) +
  scale_color_manual(values = c(
    "AI Tool"    = "#2F5496",
    "Engagement" = "#C0504D",
    "Age Group"  = "#9BBB59",
    "Gender"     = "#8064A2"
  )) +
  scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 1), guide = "none") +
  labs(
    title    = "What Predicts Finance App Usage?",
    subtitle = "Odds ratios from logistic regression (log scale). Reference age: 38-57 Established.",
    x        = "Odds Ratio (95% CI)",
    y        = NULL,
    color    = "Predictor Type",
    caption  = "Solid points indicate p < 0.05. Values above 1 increase odds of finance app usage."
  ) +
  theme_bw() +
  theme(
    legend.position  = "right",
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11, color = "grey30"),
    axis.text.y      = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

print(forest_plot)
ggsave("output/06_finance_odds_ratios.png", forest_plot,
       width = 11, height = 7, dpi = 300)

why did you# =============================================================================
# Summary output for the paper
# =============================================================================

cat("\n=== Key odds ratios for finance app usage ===\n")
coef_df %>%
  filter(predictor_type == "AI Tool") %>%
  select(clean_term, estimate, conf.low, conf.high, p.value) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(.x, 2)),
         p.value = format.pval(p.value, digits = 3)) %>%
  print()

library(pscl)
cat("\nMcFadden Pseudo R-squared:", round(pR2(main_model)["McFadden"], 3), "\n")

# =============================================================================
# Diagnostic: Observed vs predicted finance usage rate by AI adoption status
# =============================================================================

cat("\n=== Observed finance usage rate by AI tool adoption ===\n")
reg_df %>%
  pivot_longer(c(used_chatgpt, used_gemini, used_character, used_nova, used_other_ai),
               names_to = "ai_tool", values_to = "used") %>%
  group_by(ai_tool, used) %>%
  summarise(finance_rate = mean(uses_finance), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = used, values_from = c(finance_rate, n)) %>%
  mutate(
    ai_tool = str_remove(ai_tool, "used_"),
    gap_pct = round((finance_rate_1 - finance_rate_0) * 100, 1)
  ) %>%
  print()


reg_df %>%
  count(age_group) %>%
  mutate(finance_rate = NA)

# With finance rates
reg_df %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    finance_rate = round(mean(uses_finance) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n))