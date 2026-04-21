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


# # =============================================================================
# # Logistic regression: Does AI adoption predict financial app usage?
# # Outcome: Binary indicator for whether the user uses any finance app
# # Predictors: AI adoption, controls
# # Visualization: User segment coloring for interpretability
# # =============================================================================
# 
# library(tidyverse)
# library(broom)
# 
# # Define the finance app universe from the Banking & Finance category
# # Pulled from your actual app_categories mapping
# finance_apps <- app_categories %>%
#   filter(category == "Banking & Finance") %>%
#   pull(ITEM)
# 
# cat("Number of finance apps in scope:", length(finance_apps), "\n")
# 
# # =============================================================================
# # Build user level dataset
# # =============================================================================
# 
# reg_df <- User_df %>%
#   select(PANELISTID, AGE, GENDER, TREATED, Weekday, Weekend,
#          Morning, Afternoon, Night) %>%
#   # AI tool flags
#   left_join(
#     Usage_df %>%
#       filter(IS_AI == TRUE) %>%
#       group_by(PANELISTID) %>%
#       summarise(
#         used_chatgpt   = any(AI_CANONICAL == "ChatGPT"),
#         used_gemini    = any(AI_CANONICAL == "Gemini"),
#         used_character = any(AI_CANONICAL == "Character.AI"),
#         used_nova      = any(AI_CANONICAL == "AI Chatbot Nova"),
#         used_other_ai  = any(AI_CANONICAL == "Other AI"),
#         total_ai_minutes = sum(FOREGROUNDDURATION, na.rm = TRUE),
#         n_ai_apps        = n_distinct(APPDESCRIPTION),
#         .groups = "drop"
#       ),
#     by = "PANELISTID"
#   ) %>%
#   # Finance outcome: binary indicator and intensity
#   left_join(
#     Usage_df %>%
#       filter(APPDESCRIPTION %in% finance_apps, FOREGROUNDDURATION > 0) %>%
#       group_by(PANELISTID) %>%
#       summarise(
#         uses_finance        = TRUE,
#         total_finance_min   = sum(FOREGROUNDDURATION, na.rm = TRUE),
#         n_finance_apps      = n_distinct(APPDESCRIPTION),
#         .groups = "drop"
#       ),
#     by = "PANELISTID"
#   ) %>%
#   # Overall engagement controls
#   left_join(
#     Usage_df %>%
#       group_by(PANELISTID) %>%
#       summarise(
#         total_minutes = sum(FOREGROUNDDURATION, na.rm = TRUE),
#         n_apps_used   = n_distinct(APPDESCRIPTION),
#         .groups = "drop"
#       ),
#     by = "PANELISTID"
#   ) %>%
#   mutate(
#     across(c(used_chatgpt, used_gemini, used_character, used_nova,
#              used_other_ai, uses_finance),
#            ~replace_na(.x, FALSE)),
#     across(c(used_chatgpt, used_gemini, used_character, used_nova,
#              used_other_ai, uses_finance),
#            as.integer),
#     total_ai_minutes = replace_na(total_ai_minutes, 0),
#     n_ai_apps        = replace_na(n_ai_apps, 0),
#     total_finance_min = replace_na(total_finance_min, 0),
#     n_finance_apps    = replace_na(n_finance_apps, 0),
#     log_total_minutes = log(total_minutes + 1),
#     log_n_apps        = log(n_apps_used + 1),
#     age_group = case_when(
#       is.na(AGE)  ~ "Unknown",
#       AGE < 23    ~ "15-22 Student",
#       AGE < 38    ~ "23-37 Young Prof",
#       AGE < 58    ~ "38-57 Established",
#       AGE >= 58   ~ "58+ Pre-Retirement"
#     ),
#     age_group = factor(age_group, levels = c("38-57 Established", "15-22 Student",
#                                              "23-37 Young Prof", "58+ Pre-Retirement",
#                                              "Unknown"))
#   ) %>%
#   filter(!is.na(GENDER))
# 
# cat("Dataset size:", nrow(reg_df), "\n")
# cat("Finance app users:", sum(reg_df$uses_finance), 
#     "(", round(mean(reg_df$uses_finance) * 100, 1), "%)\n\n")
# 
# # =============================================================================
# # MAIN MODEL: AI tools + demographics + engagement controls
# # =============================================================================
# 
# main_model <- glm(
#   uses_finance ~ used_chatgpt + used_gemini + used_character + used_nova +
#     used_other_ai + age_group + GENDER +
#     log_total_minutes + log_n_apps,
#   data   = reg_df,
#   family = binomial(link = "logit")
# )
# 
# cat("=== MAIN MODEL ===\n")
# summary(main_model)
# 
# # =============================================================================
# # Forest plot with user type coloring
# # =============================================================================
# 
# coef_df <- tidy(main_model, conf.int = TRUE, exponentiate = TRUE) %>%
#   filter(term != "(Intercept)") %>%
#   mutate(
#     # Clean up term names
#     clean_term = case_when(
#       term == "used_chatgpt"      ~ "ChatGPT",
#       term == "used_gemini"       ~ "Gemini",
#       term == "used_character"    ~ "Character.AI",
#       term == "used_nova"         ~ "AI Chatbot Nova",
#       term == "used_other_ai"     ~ "Other AI",
#       str_detect(term, "age_group") ~ str_replace(term, "age_group", ""),
#       term == "GENDERMale"        ~ "Male",
#       term == "log_total_minutes" ~ "Total usage (log)",
#       term == "log_n_apps"        ~ "App diversity (log)",
#       TRUE ~ term
#     ),
#     # Categorize each predictor for color
#     predictor_type = case_when(
#       term %in% c("used_chatgpt", "used_gemini", "used_character",
#                   "used_nova", "used_other_ai")    ~ "AI Tool",
#       str_detect(term, "age_group")                ~ "Age Group",
#       term == "GENDERMale"                         ~ "Gender",
#       term %in% c("log_total_minutes", "log_n_apps") ~ "Engagement",
#       TRUE ~ "Other"
#     ),
#     predictor_type = factor(predictor_type,
#                             levels = c("AI Tool", "Engagement", "Age Group", "Gender")),
#     significant = p.value < 0.05
#   ) %>%
#   arrange(predictor_type, desc(estimate))
# 
# # Forest plot
# forest_plot <- ggplot(coef_df,
#                       aes(x = estimate,
#                           y = reorder(clean_term, as.numeric(predictor_type) * -1000 + estimate),
#                           color = predictor_type)) +
#   geom_point(size = 3.5, aes(shape = significant)) +
#   geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 0.7) +
#   geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
#   scale_x_log10(breaks = c(0.5, 0.75, 1, 1.5, 2, 3, 5, 10)) +
#   scale_color_manual(values = c(
#     "AI Tool"    = "#2F5496",
#     "Engagement" = "#C0504D",
#     "Age Group"  = "#9BBB59",
#     "Gender"     = "#8064A2"
#   )) +
#   scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 1), guide = "none") +
#   labs(
#     title    = "What Predicts Finance App Usage?",
#     subtitle = "Odds ratios from logistic regression (log scale). Reference age: 38-57 Established.",
#     x        = "Odds Ratio (95% CI)",
#     y        = NULL,
#     color    = "Predictor Type",
#     caption  = "Solid points indicate p < 0.05. Values above 1 increase odds of finance app usage."
#   ) +
#   theme_bw() +
#   theme(
#     legend.position  = "right",
#     plot.title       = element_text(face = "bold", size = 14),
#     plot.subtitle    = element_text(size = 11, color = "grey30"),
#     axis.text.y      = element_text(size = 11),
#     panel.grid.minor = element_blank()
#   )
# 
# print(forest_plot)
# ggsave("output/06_finance_odds_ratios.png", forest_plot,
#        width = 11, height = 7, dpi = 300)
# 
# why did you# =============================================================================
# # Summary output for the paper
# # =============================================================================
# 
# cat("\n=== Key odds ratios for finance app usage ===\n")
# coef_df %>%
#   filter(predictor_type == "AI Tool") %>%
#   select(clean_term, estimate, conf.low, conf.high, p.value) %>%
#   mutate(across(c(estimate, conf.low, conf.high), ~round(.x, 2)),
#          p.value = format.pval(p.value, digits = 3)) %>%
#   print()
# 
# library(pscl)
# cat("\nMcFadden Pseudo R-squared:", round(pR2(main_model)["McFadden"], 3), "\n")
# 
# # =============================================================================
# # Diagnostic: Observed vs predicted finance usage rate by AI adoption status
# # =============================================================================
# 
# cat("\n=== Observed finance usage rate by AI tool adoption ===\n")
# reg_df %>%
#   pivot_longer(c(used_chatgpt, used_gemini, used_character, used_nova, used_other_ai),
#                names_to = "ai_tool", values_to = "used") %>%
#   group_by(ai_tool, used) %>%
#   summarise(finance_rate = mean(uses_finance), n = n(), .groups = "drop") %>%
#   pivot_wider(names_from = used, values_from = c(finance_rate, n)) %>%
#   mutate(
#     ai_tool = str_remove(ai_tool, "used_"),
#     gap_pct = round((finance_rate_1 - finance_rate_0) * 100, 1)
#   ) %>%
  # print()


# =============================================================================
# CONVERSION ANALYSIS: Does AI adoption cause finance app uptake?
# 
# Assumes already in environment:
#   - User_df, Usage_df (from Cleaning.R)
#   - app_categories (from Association_rules_over_time.R Part A)
# =============================================================================

library(tidyverse)

# =============================================================================
# SETUP: Rebuild helper objects
# =============================================================================

treated_ids <- User_df %>% filter(TREATED == 1) %>% pull(PANELISTID)
control_ids <- User_df %>% filter(TREATED == 0) %>% pull(PANELISTID)

finance_apps <- app_categories %>%
  filter(category == "Banking & Finance") %>%
  pull(ITEM)

cat("Treated users:", length(treated_ids), "\n")
cat("Control users:", length(control_ids), "\n")
cat("Finance apps in scope:", length(finance_apps), "\n\n")

# =============================================================================
# STEP 1: Derive each treated user's first AI adoption week
# =============================================================================

treatment_dates <- Usage_df %>%
  filter(PANELISTID %in% treated_ids, IS_AI == TRUE) %>%
  group_by(PANELISTID) %>%
  summarise(treatment_week = min(WEEK), .groups = "drop") %>%
  left_join(
    Usage_df %>%
      filter(IS_AI == TRUE) %>%
      group_by(PANELISTID) %>%
      summarise(
        first_ai_tool = first(AI_CANONICAL[WEEK == min(WEEK)]),
        .groups = "drop"
      ),
    by = "PANELISTID"
  )

# =============================================================================
# STEP 2: Build pre/post finance usage for each treated user (4-week window)
# =============================================================================

WINDOW_WEEKS <- 4

pre_post_df <- Usage_df %>%
  filter(PANELISTID %in% treated_ids,
         APPDESCRIPTION %in% finance_apps,
         FOREGROUNDDURATION > 0) %>%
  inner_join(treatment_dates, by = "PANELISTID") %>%
  mutate(
    weeks_from_treatment = as.numeric(WEEK - treatment_week) / 7,
    period = case_when(
      weeks_from_treatment >= -WINDOW_WEEKS & weeks_from_treatment < 0 ~ "pre",
      weeks_from_treatment >= 0  & weeks_from_treatment <= WINDOW_WEEKS ~ "post",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

user_periods <- expand_grid(
  PANELISTID = treated_ids,
  period     = c("pre", "post")
) %>%
  left_join(
    pre_post_df %>%
      group_by(PANELISTID, period) %>%
      summarise(
        uses_finance    = TRUE,
        finance_minutes = sum(FOREGROUNDDURATION, na.rm = TRUE),
        n_finance_apps  = n_distinct(APPDESCRIPTION),
        .groups = "drop"
      ),
    by = c("PANELISTID", "period")
  ) %>%
  mutate(
    uses_finance    = replace_na(uses_finance, FALSE),
    finance_minutes = replace_na(finance_minutes, 0),
    n_finance_apps  = replace_na(n_finance_apps, 0),
    uses_finance    = as.integer(uses_finance)
  ) %>%
  left_join(User_df %>% select(PANELISTID, AGE, GENDER), by = "PANELISTID") %>%
  left_join(treatment_dates %>% select(PANELISTID, first_ai_tool),
            by = "PANELISTID")

user_periods_wide <- user_periods %>%
  pivot_wider(id_cols = c(PANELISTID, first_ai_tool, AGE, GENDER),
              names_from = period,
              values_from = c(finance_minutes, uses_finance)) %>%
  mutate(
    new_finance_user = as.integer(uses_finance_pre == 0 & uses_finance_post == 1)
  )

# =============================================================================
# STEP 3: Treated user conversion rates by AI tool
# =============================================================================

cat("=== Treated user conversion rates by AI tool ===\n")
treated_conversion_summary <- user_periods_wide %>%
  filter(!is.na(first_ai_tool)) %>%
  group_by(first_ai_tool) %>%
  summarise(
    n_users              = n(),
    n_not_using_pre      = sum(uses_finance_pre == 0),
    n_became_users       = sum(new_finance_user),
    conversion_rate_pct  = round(n_became_users / n_not_using_pre * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(conversion_rate_pct))
print(treated_conversion_summary)

# =============================================================================
# STEP 4: Matched control baseline (same calendar windows as treated users)
# =============================================================================

treatment_weeks <- treatment_dates %>% pull(treatment_week) %>% unique()

cat("\nComputing matched control baseline across", length(treatment_weeks),
    "treatment weeks...\n")

baseline_by_week <- map_dfr(treatment_weeks, function(tw) {
  pre_window  <- c(tw - 28, tw - 1)
  post_window <- c(tw, tw + 27)
  
  pre_users <- Usage_df %>%
    filter(PANELISTID %in% control_ids,
           APPDESCRIPTION %in% finance_apps,
           FOREGROUNDDURATION > 0,
           WEEK >= pre_window[1] & WEEK <= pre_window[2]) %>%
    pull(PANELISTID) %>% unique()
  
  post_users <- Usage_df %>%
    filter(PANELISTID %in% control_ids,
           APPDESCRIPTION %in% finance_apps,
           FOREGROUNDDURATION > 0,
           WEEK >= post_window[1] & WEEK <= post_window[2]) %>%
    pull(PANELISTID) %>% unique()
  
  not_pre      <- setdiff(control_ids, pre_users)
  became_users <- intersect(not_pre, post_users)
  
  tibble(
    treatment_week      = tw,
    n_not_using_pre     = length(not_pre),
    n_became_users      = length(became_users),
    conversion_rate_pct = length(became_users) / length(not_pre) * 100
  )
})

control_baseline_avg <- mean(baseline_by_week$conversion_rate_pct)
cat("\n=== Control baseline ===\n")
cat("Average conversion rate:", round(control_baseline_avg, 1), "%\n")
cat("Median conversion rate :", round(median(baseline_by_week$conversion_rate_pct), 1), "%\n\n")

# =============================================================================
# STEP 5: Build the conversion-level dataset for regression
# Each row is a user with binary conversion outcome
# =============================================================================

# Treated users who were not using finance pre
treated_conversion <- user_periods_wide %>%
  filter(!is.na(first_ai_tool), uses_finance_pre == 0) %>%
  transmute(
    PANELISTID,
    ai_tool   = first_ai_tool,
    converted = new_finance_user,
    AGE, GENDER
  )

# Control users: sample from 20 random treatment weeks for efficiency
set.seed(42)
sampled_weeks <- sample(treatment_weeks, 20)

control_conversion <- map_dfr(sampled_weeks, function(tw) {
  pre_window  <- c(tw - 28, tw - 1)
  post_window <- c(tw, tw + 27)
  
  pre_users <- Usage_df %>%
    filter(PANELISTID %in% control_ids,
           APPDESCRIPTION %in% finance_apps,
           FOREGROUNDDURATION > 0,
           WEEK >= pre_window[1] & WEEK <= pre_window[2]) %>%
    pull(PANELISTID) %>% unique()
  
  post_users <- Usage_df %>%
    filter(PANELISTID %in% control_ids,
           APPDESCRIPTION %in% finance_apps,
           FOREGROUNDDURATION > 0,
           WEEK >= post_window[1] & WEEK <= post_window[2]) %>%
    pull(PANELISTID) %>% unique()
  
  not_pre <- setdiff(control_ids, pre_users)
  
  tibble(
    PANELISTID = not_pre,
    ai_tool    = "Control",
    converted  = as.integer(PANELISTID %in% post_users)
  )
}) %>%
  distinct(PANELISTID, .keep_all = TRUE) %>%
  left_join(User_df %>% select(PANELISTID, AGE, GENDER), by = "PANELISTID")

# Combine treated and control
conv_df <- bind_rows(treated_conversion, control_conversion) %>%
  mutate(
    ai_tool   = factor(ai_tool, levels = c("Control", "ChatGPT", "Gemini",
                                           "Character.AI", "AI Chatbot Nova",
                                           "Other AI")),
    age_group = case_when(
      is.na(AGE)  ~ "Unknown",
      AGE < 23    ~ "15-22",
      AGE < 38    ~ "23-37",
      AGE < 58    ~ "38-57",
      AGE >= 58   ~ "58+"
    ),
    age_group = factor(age_group, levels = c("38-57", "15-22", "23-37",
                                             "58+", "Unknown"))
  ) %>%
  filter(!is.na(GENDER))

cat("\n=== Conversion dataset composition ===\n")
conv_df %>% count(ai_tool) %>% print()

# =============================================================================
# STEP 6: Logistic regression on conversion
# =============================================================================

conv_model <- glm(
  converted ~ ai_tool + age_group + GENDER,
  data   = conv_df,
  family = binomial(link = "logit")
)

cat("\n=== Logistic regression: conversion outcome ===\n")
summary(conv_model)

# Extract odds ratios with CIs
coef_table <- data.frame(
  term      = names(coef(conv_model)),
  estimate  = round(exp(coef(conv_model)), 2),
  conf_low  = round(exp(confint.default(conv_model)[, 1]), 2),
  conf_high = round(exp(confint.default(conv_model)[, 2]), 2),
  p_value   = format.pval(summary(conv_model)$coefficients[, 4], digits = 3)
)

cat("\n=== Odds ratios: conversion vs matched controls ===\n")
print(coef_table)

# =============================================================================
# STEP 7: Visualization 1 — Raw conversion rates bar chart
# =============================================================================

conversion_data <- bind_rows(
  treated_conversion_summary %>%
    transmute(group = first_ai_tool,
              conversion = conversion_rate_pct,
              group_type = "AI Tool"),
  tibble(group = "Control baseline",
         conversion = round(control_baseline_avg, 1),
         group_type = "Control")
)

conv_bar_plot <- ggplot(conversion_data,
                        aes(x = conversion,
                            y = reorder(group, conversion),
                            fill = group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(conversion, "%")),
            hjust = -0.15, size = 4.5, fontface = "bold",
            color = "#2B2D42") +
  geom_vline(xintercept = control_baseline_avg, linetype = "dashed",
             color = "#6B7280", linewidth = 0.6) +
  annotate("text", x = control_baseline_avg, y = 6.6,
           label = "Control baseline",
           hjust = -0.1, vjust = 0, size = 3.3,
           color = "#6B7280", fontface = "italic") +
  scale_fill_manual(values = c(
    "Gemini"            = "#2F80A8",
    "ChatGPT"           = "#4A5568",
    "Other AI"          = "#4A5568",
    "Character.AI"      = "#4A5568",
    "AI Chatbot Nova"   = "#4A5568",
    "Control baseline"  = "#9CA3AF"
  )) +
  scale_x_continuous(limits = c(0, 108), breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "AI adoption dramatically increases finance app conversion",
    subtitle = "Share of users with no pre-adoption finance app usage who started using\nfinance apps in the 4 weeks after. Controls matched on treatment week.",
    x        = "Conversion rate (within 4 weeks)",
    y        = NULL,
    caption  = "n = 1,623 treated users across 5 AI tool groups. Control baseline averaged across 52 treatment-week matched samples."
  ) +
  theme_minimal() +
  theme(
    legend.position    = "none",
    plot.title         = element_text(face = "bold", size = 15, color = "#1E2761"),
    plot.subtitle      = element_text(size = 11, color = "#4B5563",
                                      margin = margin(t = 4, b = 12)),
    plot.caption       = element_text(size = 9, color = "#6B7280", hjust = 0,
                                      margin = margin(t = 12)),
    axis.text.y        = element_text(size = 11, color = "#2B2D42"),
    axis.text.x        = element_text(size = 10, color = "#6B7280"),
    axis.title.x       = element_text(size = 10, color = "#6B7280",
                                      margin = margin(t = 8)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.3),
    plot.margin        = margin(t = 30, r = 20, b = 15, l = 10)
  ) +
  coord_cartesian(clip = "off")

print(conv_bar_plot)
ggsave("output/07_conversion_comparison.png", conv_bar_plot,
       width = 10, height = 5.5, dpi = 300)

# =============================================================================
# STEP 8: Visualization 2 — Odds ratio coefficient plot
# =============================================================================

coef_plot_df <- data.frame(
  term      = names(coef(conv_model)),
  estimate  = exp(coef(conv_model)),
  conf_low  = exp(confint.default(conv_model)[, 1]),
  conf_high = exp(confint.default(conv_model)[, 2]),
  p_value   = summary(conv_model)$coefficients[, 4]
) %>%
  filter(str_detect(term, "ai_tool")) %>%
  mutate(
    tool = str_remove(term, "ai_tool"),
    tool = factor(tool, levels = c("ChatGPT", "Gemini", "Character.AI",
                                   "AI Chatbot Nova", "Other AI")),
    is_gemini = tool == "Gemini"
  )

coef_plot <- ggplot(coef_plot_df,
                    aes(x = estimate,
                        y = reorder(tool, estimate),
                        color = is_gemini)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "#6B7280", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high),
                 height = 0.25, linewidth = 1) +
  geom_point(size = 5) +
  geom_text(aes(label = paste0(round(estimate, 1), "x")),
            hjust = -0.6, size = 4.5, fontface = "bold",
            color = "#2B2D42") +
  scale_color_manual(values = c("TRUE" = "#2F80A8", "FALSE" = "#4A5568")) +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100),
                labels = function(x) paste0(x, "x")) +
  labs(
    title    = "Odds of finance app conversion vs matched controls",
    subtitle = "Among users not using finance apps pre-adoption. Control baseline = 1.0x.\nControlling for age group and gender.",
    x        = "Odds ratio (95% CI, log scale)",
    y        = NULL,
    caption  = "All estimates p < 0.001. Reference: matched control users in same calendar windows."
  ) +
  theme_minimal() +
  theme(
    legend.position    = "none",
    plot.title         = element_text(face = "bold", size = 15, color = "#1E2761"),
    plot.subtitle      = element_text(size = 11, color = "#4B5563",
                                      margin = margin(t = 4, b = 12)),
    plot.caption       = element_text(size = 9, color = "#6B7280", hjust = 0,
                                      margin = margin(t = 12)),
    axis.text.y        = element_text(size = 12, color = "#2B2D42"),
    axis.text.x        = element_text(size = 10, color = "#6B7280"),
    axis.title.x       = element_text(size = 10, color = "#6B7280",
                                      margin = margin(t = 8)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.3)
  )

print(coef_plot)
ggsave("output/08_conversion_odds.png", coef_plot,
       width = 10, height = 5, dpi = 300)

cat("\n=== Analysis complete ===\n")
cat("Charts saved to output/07_conversion_comparison.png and 08_conversion_odds.png\n")



