# install and load libraries
install.packages('tidyverse')
install.packages('dplyr')
install.packages('arules')
library(tidyverse)
library(dplyr)
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

# 2nd step: keep only PANELISTID, APPDESCRIPTION, and USED
new_weekly_usage <- weekly_usage %>% 
  select(PANELISTID, APPDESCRIPTION, USED)

# 3rd step: collapse all app descriptions to one row per PANELISTID if they ever used it
weekly_usage_group <- new_weekly_usage %>% 
  group_by(PANELISTID, APPDESCRIPTION) %>%
  summarise(USED = max(USED), .groups="drop")

# 4th step: create large 0/1 data frame
weekly_usage_wide <- weekly_usage_group %>%
  pivot_wider(
    names_from = APPDESCRIPTION,
    values_from = USED,
    values_fill = 0
  )

# 5th step: convert to matrix (drop PANELISTID)
weekly_usage_matrix <- weekly_usage_wide %>%
  select(-PANELISTID) %>%
  as.matrix()

# 6th step: convert to binary values to logical values for arules
weekly_usage_matrix <- weekly_usage_matrix > 0

# 7th step: convert to transactions
weekly_usage_trans <- as(weekly_usage_matrix, "transactions")

# ******************************************************************************
# *** analyzing weekly_usage_trans with apriori algorithm
# ******************************************************************************

inspect(head(weekly_usage_trans, n=10))

summary(weekly_usage_trans)

item_freq <- itemFrequency(movies_trans) 
head(sort(item_freq, decreasing = TRUE))
item_count <- itemFrequency(movies_trans, type = "absolute")
head(sort(item_count, decreasing = TRUE))
