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
  theme_minimal() +
  theme(legend.position  = "right",
        legend.text      = element_text(size = 8),
        axis.text.x      = element_text(angle = 45, hjust = 1))

ggsave("output/01_top_rules_lift_over_time.png", p1,
       width = 12, height = 6, dpi = 300)

# C2: Heatmap — companion category x month, faceted by AI bucket
p2_data <- rules_over_time %>%
  filter(lhs %in% c("ChatGPT", "Gemini")) %>%
  group_by(lhs, rhs) %>%
  filter(n() >= 3) %>%
  ungroup()

p2 <- ggplot(p2_data, aes(x = month, y = reorder(rhs, lift), fill = lift)) +
  geom_tile() +
  facet_wrap(~ lhs, scales = "free_y") +
  scale_fill_viridis_c(option = "magma") +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  labs(
    title    = "Companion App Category Lift by Month: ChatGPT vs Gemini",
    subtitle = "Higher lift = stronger co-usage relative to chance",
    x        = NULL, y = NULL,
    fill     = "Lift"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

ggsave("output/02_companion_heatmap.png", p2,
       width = 12, height = 8, dpi = 300)

# C3: Emergence chart — count of new rules first appearing each month
emergence <- rules_over_time %>%
  group_by(lhs, rhs) %>%
  summarise(first_month = min(month), .groups = "drop") %>%
  count(first_month, name = "new_rules") %>%
  arrange(first_month)

p3 <- ggplot(emergence, aes(x = first_month, y = new_rules)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = new_rules), vjust = -0.5, size = 3) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    title    = "New AI Co-Usage Rules Emerging Each Month in 2023",
    subtitle = "Count of association rules appearing for the first time",
    x        = NULL, y = "New rules"
  ) +
  theme_minimal()

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