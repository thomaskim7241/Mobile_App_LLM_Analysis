# =============================================================================
# ai_app_searcher.R
# Purpose: Interactive utility to explore the reconciled AI app universe.
#          Use this BEFORE building the canonical mapping in the cleaning
#          script. Source the file, then call the functions below.
# Assumes: Usage_df exists in memory with IS_AI and APPDESCRIPTION columns.
# =============================================================================

library(tidyverse)

# ---- Core: unique AI app inventory ------------------------------------------
# Returns a tibble of every distinct AI app name with row counts and user
# counts. This is your master reference for bucketing decisions.
build_ai_inventory <- function(usage_df = Usage_df) {
  usage_df %>%
    filter(IS_AI == TRUE) %>%
    group_by(APPDESCRIPTION) %>%
    summarise(
      n_rows         = n(),
      n_users        = n_distinct(PANELISTID),
      total_duration = sum(FOREGROUNDDURATION, na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    arrange(desc(n_users))
}

# ---- Searcher: find apps matching a regex pattern ---------------------------
# Case-insensitive by default. Returns matching rows with counts so you can
# see what a candidate pattern would capture BEFORE committing it to the map.
search_ai <- function(pattern,
                      inventory = NULL,
                      usage_df = Usage_df,
                      case_insensitive = TRUE) {
  
  if (is.null(inventory)) inventory <- build_ai_inventory(usage_df)
  
  regex_pattern <- if (case_insensitive) paste0("(?i)", pattern) else pattern
  
  matches <- inventory %>%
    filter(str_detect(APPDESCRIPTION, regex_pattern))
  
  cat("Pattern:", regex_pattern, "\n")
  cat("Matched apps:", nrow(matches), "\n")
  cat("Total rows:  ", sum(matches$n_rows), "\n")
  cat("Total users: ", sum(matches$n_users), "\n\n")
  
  print(matches, n = Inf)
  invisible(matches)
}

# ---- Anti-searcher: find apps NOT yet covered by a set of patterns ----------
# Pass a named vector or list of patterns. Returns AI apps that no pattern
# matches. Use this to find the long tail for "Other AI".
find_uncovered_ai <- function(patterns,
                              inventory = NULL,
                              usage_df = Usage_df) {
  
  if (is.null(inventory)) inventory <- build_ai_inventory(usage_df)
  
  combined <- paste0("(?i)(", paste(patterns, collapse = "|"), ")")
  
  uncovered <- inventory %>%
    filter(!str_detect(APPDESCRIPTION, combined))
  
  cat("Patterns checked: ", length(patterns), "\n")
  cat("Uncovered apps:   ", nrow(uncovered), "\n")
  cat("Uncovered rows:   ", sum(uncovered$n_rows), "\n")
  cat("Uncovered users:  ", sum(uncovered$n_users), "\n\n")
  
  print(uncovered, n = Inf)
  invisible(uncovered)
}

# ---- Preview: test a full candidate map before committing -------------------
# Pass a tribble of ~pattern, ~canonical. Shows you what each bucket would
# contain, including the "Other AI" leftover bucket.
preview_map <- function(candidate_map,
                        inventory = NULL,
                        usage_df = Usage_df) {
  
  if (is.null(inventory)) inventory <- build_ai_inventory(usage_df)
  
  classify_one <- function(app_name) {
    if (is.na(app_name)) return(NA_character_)
    for (i in seq_len(nrow(candidate_map))) {
      if (str_detect(app_name, candidate_map$pattern[i])) {
        return(candidate_map$canonical[i])
      }
    }
    return("Other AI")
  }
  
  classified <- inventory %>%
    mutate(bucket = map_chr(APPDESCRIPTION, classify_one))
  
  summary_tbl <- classified %>%
    group_by(bucket) %>%
    summarise(
      n_apps         = n(),
      n_rows         = sum(n_rows),
      n_users        = sum(n_users),
      example_apps   = paste(head(APPDESCRIPTION, 3), collapse = " | "),
      .groups        = "drop"
    ) %>%
    arrange(desc(n_rows))
  
  cat("=== Bucket summary ===\n")
  print(summary_tbl, n = Inf)
  
  cat("\n=== Full classification (head) ===\n")
  print(classified %>% arrange(desc(n_users)), n = 30)
  
  invisible(classified)
}

# =============================================================================
# USAGE EXAMPLES (do not run automatically, call interactively)
# =============================================================================
#
# # Step 1: Build the inventory once and reuse it
# inv <- build_ai_inventory()
# print(inv, n = Inf)
#
# # Step 2: Search for specific brands to see all variants
# search_ai("chatgpt|openai|\\bgpt\\b", inv)
# search_ai("gemini|bard", inv)
# search_ai("claude|anthropic", inv)
# search_ai("copilot", inv)
# search_ai("character.?ai", inv)
# search_ai("perplexity", inv)
#
# # Step 3: Test a candidate bucket map
# candidate <- tribble(
#   ~pattern,                          ~canonical,
#   "(?i)chatgpt|openai|\\bgpt\\b",    "ChatGPT",
#   "(?i)gemini|bard",                 "Gemini",
#   "(?i)\\bclaude\\b|anthropic",      "Claude",
#   "(?i)copilot",                     "Microsoft Copilot",
#   "(?i)character.?ai",               "Character.AI"
# )
# preview_map(candidate, inv)
#
# # Step 4: See what "Other AI" would contain
# find_uncovered_ai(
#   patterns = c("chatgpt", "openai", "\\bgpt\\b", "gemini", "bard",
#                "claude", "anthropic", "copilot", "character.?ai"),
#   inventory = inv
# )