# install and load libraries
library(data.table)
library(Matrix)
library(arules) # used for apriori algorithm

# loading weekly_usage data
# Select only the two columns we actually need — WEEK is never used
# Use correct column types so R doesn't store numbers as strings
weekly_usage <- fread('weekly_usage.csv',
                      select = c("PANELISTID", "APPDESCRIPTION", "FOREGROUNDDURATION"),
                      colClasses = list(character = c("PANELISTID", "APPDESCRIPTION"),
                                        numeric  = "FOREGROUNDDURATION"))
gc()

# checking duplicates and Na's
sum(duplicated(weekly_usage))
sum(is.na(weekly_usage))

# cleaning duplicates and omitting Na's
weekly_usage <- unique(weekly_usage)
weekly_usage <- na.omit(weekly_usage)

# Drop zero-duration rows immediately — FOREGROUNDDURATION == 0 means USED = 0,
# and missing panelist-app pairs are filled with 0 in the final wide step anyway.
# This can remove a large fraction of the 20M rows before any further processing.
weekly_usage <- weekly_usage[FOREGROUNDDURATION > 0]

# ******************************************************************************
# *** Prepare data for association rule mining
# ******************************************************************************

# 1st step: FOREGROUNDDURATION no longer needed — drop it now
weekly_usage[, FOREGROUNDDURATION := NULL]

# 2nd step: encode system apps with a single label
system_apps <- c(
  "Mobile Performance Meter", "Settings", "Files", "Contacts", "Phone", "Call Management",
  "Camera", "Call", "Finder","Call settings", "Call Settings", "Weather", "Media", "Photo Editor",
  "Wallpaper and style","Software update", "Software Update", "Apps", "Video Editor", "Wi-Fi Calling",
  "Phone Services", "Bluetooth", "Wallpapers", "Voicemail", "Emergency SOS", "Default Print Service",
  "Wireless Emergency Alerts", "Media and devices", "Reminder", "Phone calls", "Media Storeage", "Tags"
)

weekly_usage[APPDESCRIPTION %in% system_apps, APPDESCRIPTION := "System"]

# 3rd step: encode company apps with a single label
weekly_usage[, APPDESCRIPTION := fcase(
  grepl("samsung|galaxy|Smart|Quick|Modes|Bixby|Tips|quality|Secure|Customization|Separate|Editor|Find",
        APPDESCRIPTION, ignore.case = TRUE), "Samsung",
  grepl("google|gmail|chrome|Android|Package|Management|Permission|Wireless|Digital|Suggestions|Selector|UI|Accessibility|MTP|Gboard|Intent|Device|Dialogs|Restore",
        APPDESCRIPTION, ignore.case = TRUE), "Google",
  grepl("Microsoft|Companion",
        APPDESCRIPTION, ignore.case = TRUE), "Microsoft",
  grepl("Amazon",
        APPDESCRIPTION, ignore.case = TRUE), "Amazon",
  grepl("Facebook",
        APPDESCRIPTION, ignore.case = TRUE), "Facebook",
  grepl("Alibaba|Aliexpress",
        APPDESCRIPTION, ignore.case = TRUE), "Alibaba",
  grepl("AT&T",
        APPDESCRIPTION, ignore.case = TRUE), "AT&T",
  grepl("T-Mobile|Visual|Metro",
        APPDESCRIPTION, ignore.case = TRUE), "T-Mobile",
  grepl("Verizon|Digital",
        APPDESCRIPTION, ignore.case = TRUE), "Verizon",
  grepl("YouTube",
        APPDESCRIPTION, ignore.case = TRUE), "YouTube",
  default = APPDESCRIPTION
)]

# 4th step: collapse to unique panelist-app pairs
# Since all remaining rows have FOREGROUNDDURATION > 0 (USED = 1),
# deduplication is all that's needed — no aggregation required
weekly_usage_group <- unique(weekly_usage)
# rm(weekly_usage)
gc()

# 5th step: build a named list of app vectors — one element per panelist
# arules coerces list -> transactions natively, so no matrix is ever needed
# Items must be sorted within each transaction or arules throws an ngCMatrix error
trans_list <- split(weekly_usage_group$APPDESCRIPTION, weekly_usage_group$PANELISTID)
trans_list <- lapply(trans_list, function(x) sort(unique(x), method = "radix"))
# rm(weekly_usage_group)
gc()

# 6th step: convert list directly to transactions
weekly_usage_trans <- as(trans_list, "transactions")
# rm(trans_list)
gc()

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
