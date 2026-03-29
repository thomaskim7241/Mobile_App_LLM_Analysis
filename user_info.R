install.packages("gitcreds")
gitcreds::gitcreds_set()

library(tidyverse)
user_info <- read_csv("user_info.csv")

# Inspect the basic structure of the data
glimpse(user_info)
dim(user_info)
names(user_info)
head(user_info)

# Check missing values by column
colSums(is.na(user_info))
colMeans(is.na(user_info))

# Check exact duplicate rows
sum(duplicated(user_info))

# Check whether PANELISTID appears more than once
sum(duplicated(user_info$PANELISTID))

# Count repeated PANELISTID values
user_info %>%
  count(PANELISTID) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Show how many users appear once, four times, etc.
user_info %>%
  count(PANELISTID) %>%
  count(n)

# Check categorical variables for consistency
table(user_info$GENDER, useNA = "ifany")
table(user_info$TREATED, useNA = "ifany")

# Count city values
user_info %>%
  count(CITY, sort = TRUE)

# Summary statistics for all variables
summary(user_info)

# Check min and max values of numeric variables
user_info %>%
  summarise(
    min_age = min(AGE, na.rm = TRUE),
    max_age = max(AGE, na.rm = TRUE),
    min_weekday = min(Weekday, na.rm = TRUE),
    max_weekday = max(Weekday, na.rm = TRUE),
    min_weekend = min(Weekend, na.rm = TRUE),
    max_weekend = max(Weekend, na.rm = TRUE),
    min_afternoon = min(Afternoon, na.rm = TRUE),
    max_afternoon = max(Afternoon, na.rm = TRUE),
    min_morning = min(Morning, na.rm = TRUE),
    max_morning = max(Morning, na.rm = TRUE),
    min_night = min(Night, na.rm = TRUE),
    max_night = max(Night, na.rm = TRUE)
  )
