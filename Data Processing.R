library(dplyr)
library(dplyr)
install.packages("readr")
library(readr)

user_df <- read_csv("user_info.csv")

dim(user_df)
colSums(is.na(user_df))

user_df_clean <- user_df %>%
  mutate(
    AGE = ifelse(is.na(AGE), median(AGE, na.rm = TRUE), AGE),
    CITY = ifelse(is.na(CITY), "Unknown", CITY),
    Weekday = ifelse(is.na(Weekday), 0, Weekday),
    Weekend = ifelse(is.na(Weekend), 0, Weekend),
    Afternoon = ifelse(is.na(Afternoon), 0, Afternoon),
    Morning = ifelse(is.na(Morning), 0, Morning),
    Night = ifelse(is.na(Night), 0, Night)
  )


user_df_final <- user_df_clean %>%
  group_by(PANELISTID) %>%
  summarise(
    AGE = mean(AGE, na.rm = TRUE),
    GENDER = names(sort(table(GENDER), decreasing = TRUE))[1],
    CITY = names(sort(table(CITY), decreasing = TRUE))[1],
    TREATED = max(TREATED, na.rm = TRUE),
    Weekday = mean(Weekday, na.rm = TRUE),
    Weekend = mean(Weekend, na.rm = TRUE),
    Afternoon = mean(Afternoon, na.rm = TRUE),
    Morning = mean(Morning, na.rm = TRUE),
    Night = mean(Night, na.rm = TRUE),
    .groups = "drop"
  )


dim(user_df_final)
sum(duplicated(user_df_final$PANELISTID))
summary(user_df_final)

install.packages("data.table")
library(data.table)

usage_dt <- fread("weekly_usage.csv")
dim(usage_dt)
colSums(is.na(usage_dt))

exact_dups <- nrow(usage_dt) - nrow(unique(usage_dt))
exact_dups

usage_dt_nodup <- unique(usage_dt)
dim(usage_dt_nodup)

combo_check <- usage_dt_nodup[, .N, by = .(PANELISTID, WEEK, APPDESCRIPTION)][N > 1]
nrow(combo_check)

usage_dt_agg <- usage_dt_nodup[, .(
  FOREGROUNDDURATION = sum(FOREGROUNDDURATION, na.rm = TRUE)
), by = .(PANELISTID, WEEK, APPDESCRIPTION)]

dim(usage_dt_agg)


usage_dt_agg[, .N, by = .(PANELISTID, WEEK, APPDESCRIPTION)][N > 1, .N]

summary(usage_dt_agg$FOREGROUNDDURATION)

sum(usage_dt_agg$FOREGROUNDDURATION == 0, na.rm = TRUE)

uniqueN(usage_dt_agg$APPDESCRIPTION)

panel_dt <- copy(usage_dt_agg)

panel_dt[, LLM_APP := fifelse(
  grepl("chatgpt", APPDESCRIPTION, ignore.case = TRUE) |
    grepl("gemini", APPDESCRIPTION, ignore.case = TRUE) |
    grepl("perplexity", APPDESCRIPTION, ignore.case = TRUE),
  1, 0
)]

panel_dt[, .N, by = LLM_APP]

llm_week <- panel_dt[, .(
  LLM_USED = as.integer(any(LLM_APP == 1))
), by = .(PANELISTID, WEEK)]

dim(llm_week)
llm_week[, .N, by = LLM_USED]
llm_week[, mean(LLM_USED)]

top_apps_freq <- panel_dt[, .N, by = APPDESCRIPTION][order(-N)][1:30]
top_apps_freq

selected_apps <- c(
  "Chrome Browser",
  "Google Search",
  "Facebook",
  "Gmail",
  "YouTube",
  "Facebook Messenger",
  "Google Maps",
  "Amazon Shopping",
  "Instagram",
  "Tik Tok",
  "Messages (Google)",
  "Phone",
  "Call"
)

feature_dt <- panel_dt[APPDESCRIPTION %in% selected_apps]

feature_wide <- dcast(
  feature_dt,
  PANELISTID + WEEK ~ APPDESCRIPTION,
  value.var = "FOREGROUNDDURATION",
  fun.aggregate = sum,
  fill = 0
)

dim(feature_wide)
head(feature_wide)

panel_final <- merge(llm_week, feature_wide, by = c("PANELISTID", "WEEK"), all.x = TRUE)
panel_final <- merge(panel_final, user_df_final, by = "PANELISTID", all.x = TRUE)

app_cols <- setdiff(names(feature_wide), c("PANELISTID", "WEEK"))
for (col in app_cols) {
  panel_final[[col]][is.na(panel_final[[col]])] <- 0
}

dim(panel_final)
summary(panel_final$LLM_USED)
head(panel_final)

summary(panel_final[, c(
  "LLM_USED", "AGE", "TREATED", "Weekday", "Weekend",
  "Morning", "Afternoon", "Night",
  "Chrome Browser", "Google Search", "Facebook", "Gmail",
  "YouTube", "Facebook Messenger", "Google Maps",
  "Amazon Shopping", "Instagram", "Tik Tok",
  "Messages (Google)", "Phone", "Call"
)])
