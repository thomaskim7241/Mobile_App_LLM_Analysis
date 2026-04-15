# install and load libraries
library(tidyverse)
library(arules) # used for apriori algorithm

# loading weekly_usage data
weekly_usage <- read_csv('weekly_usage.csv', col_types = cols(.default = "c"))
weekly_usage

# checking duplicates and Na's
sum(duplicated(weekly_usage))
sum(is.na(weekly_usage))

# cleaning duplicates and omitting Na's
weekly_usage <- weekly_usage %>%
    distinct() %>% drop_na()

# ******************************************************************************
# *** Prepare data for association rule mining
# ******************************************************************************

# 1st step: create binary variable USED to represent FOREGROUNDDURATION
weekly_usage <- weekly_usage %>% 
  mutate(USED= ifelse(FOREGROUNDDURATION > 0, 1, 0))

# 2nd step: encode system apps with a single label
system_apps <- c(
  "Mobile Performance Meter", "Settings", "Files", "Contacts", "Phone", "Call Management",
  "Camera", "Call", "Finder","Call settings", "Call Settings", "Weather", "Media", "Photo Editor", 
  "Wallpaper and style","Software update", "Software Update", "Apps", "Video Editor", "Wi-Fi Calling",
  "Phone Services", "Bluetooth", "Wallpapers", "Voicemail", "Emergency SOS", "Default Print Service",
  "Wireless Emergency Alerts", "Media and devices", "Reminder", "Phone calls", "Media Storeage", "Tags"
)

weekly_usage$APPDESCRIPTION <- ifelse(
  weekly_usage$APPDESCRIPTION %in% system_apps,
  "System", weekly_usage$APPDESCRIPTION
)

# 3rd step: encode company apps with a single label
weekly_usage$APPDESCRIPTION <- dplyr::case_when(
  # Samsung apps
  grepl("samsung|galaxy|Smart|Quick|Modes|Bixby|Tips|quality|Secure|Customization|Separate|Editor|Find", 
        weekly_usage$APPDESCRIPTION, ignore.case = TRUE) ~ "Samsung",
  
  # Google apps
  grepl("google|gmail|chrome|Android|Package|Management|Permission|Wireless|Digital|Suggestions|Selector|UI|Accessibility|MTP|Gboard|Intent|Device|Dialogs|Restore", 
        weekly_usage$APPDESCRIPTION, ignore.case = TRUE) ~ "Google",
  
  # Microsoft apps
  grepl("Microsoft|Companion",
        weekly_usage$APPDESCRIPTION, ignore.case = TRUE) ~ "Microsoft",
  # Amazon apps
  grepl("Amazon",
        weekly_usage$APPDESCRIPTION, ignore.case = TRUE) ~ "Amazon",
  
  # Facebook apps
  grepl("Facebook",
        weekly_usage$APPDESCRIPTION, ignore.case = TRUE) ~ "Facebook",
  
  # Alibaba apps
  grepl("Alibaba|Aliexpress",
        weekly_usage$APPDESCRIPTION, ignore.case = TRUE) ~ "Alibaba",
  
  # AT&T apps
  grepl("AT&T",
        weekly_usage$APPDESCRIPTION, ignore.case = TRUE) ~ "AT&T",
  
  # T-Mobile apps
  grepl("T-Mobile|Visual|Metro",
        weekly_usage$APPDESCRIPTION, ignore.case = TRUE) ~ "T-Mobile",
  
  # Verizon apps
  grepl("Verizon|Digital",
        weekly_usage$APPDESCRIPTION, ignore.case = TRUE) ~ "Verizon",
  
  # YouTube apps
  grepl("YouTube",
        weekly_usage$APPDESCRIPTION, ignore.case = TRUE) ~ "YouTube",
  
  # Everything else stays the same
  TRUE ~ weekly_usage$APPDESCRIPTION
)

# 4th step: keep only PANELISTID, APPDESCRIPTION, and USED
new_weekly_usage <- weekly_usage %>% 
  select(PANELISTID, APPDESCRIPTION, USED)

# 5th step: collapse all app descriptions to one row per PANELISTID if they ever used it
weekly_usage_group <- new_weekly_usage %>% 
  group_by(PANELISTID, APPDESCRIPTION) %>%
  summarise(USED = max(USED), .groups="drop")

# 6th step: create large 0/1 data frame
weekly_usage_wide <- weekly_usage_group %>%
  pivot_wider(
    names_from = APPDESCRIPTION,
    values_from = USED,
    values_fill = 0
  )

# 7th step: convert to matrix (drop PANELISTID)
weekly_usage_matrix <- weekly_usage_wide %>%
  select(-PANELISTID) %>%
  as.matrix()

# 8th step: convert to binary values to logical values for arules
weekly_usage_matrix <- weekly_usage_matrix > 0

# 9th step: convert to transactions
weekly_usage_trans <- as(weekly_usage_matrix, "transactions")

# ******************************************************************************
# *** Inspect and summarize transactions
# ******************************************************************************

# inspect the top 10 transactions
inspect(head(weekly_usage_trans, n=10))

# descending order of the most frequent items/apps
item_freq <- itemFrequency(weekly_usage_trans, type='absolute') 
head(sort(item_freq, decreasing = TRUE), 200)

# overall summary of basket and item frequency
summary(weekly_usage_trans)
summary(item_freq)

# check the frequency for LLM's of interest
llm_items <- c("ChatGPT", "Perplexity", "Gemini")
item_freq[llm_items]

# ******************************************************************************
# *** Run apriori algorithm on "ChatGPT, Gemini, Perplexity"
# ******************************************************************************

# create association rules
weekly_usage_rules <- apriori(
  weekly_usage_trans,
  parameter = list(
    supp = 0.001,
    conf = 0.5,
    minlen = 2,
    maxlen = 2
  )
)

# filter for LHS rules with LLM's of interest
llm_rules_lhs <- subset(weekly_usage_rules, lhs %in% llm_items)
inspect(llm_rules_lhs)

# filter for RHS rules with LLM's of interest
llm_rules_rhs <- subset(weekly_usage_rules, rhs %in% llm_items)
inspect(llm_rules_rhs)

# filter for all rules with LLM's of interest
llm_rules_all <- subset(weekly_usage_rules, items %in% llm_items)
inspect(llm_rules_all)



