## CONTEXT
Read CLAUDE_BRIEFING.md first for full project context.
The dataset has already been partially cleaned:
- Usage_df: 20,868,730 rows, 4 columns (PANELISTID, WEEK, APPDESCRIPTION, FOREGROUNDDURATION)
- User_df: 11,793 rows, 10 columns including TREATED variable
- IS_AI column already exists in Usage_df flagging LLM apps
- Exact duplicates already removed from Usage_df with distinct()

## TASK 1 — FINISH DATA CLEANING
Step 1: Load libraries and data
library(tidyverse)
library(caret)
library(randomForest)
library(writexl)
setwd("~/Documents/IDSC_4521/LLM_App_Analytics")
Usage_df <- read_csv("weekly_usage.csv")
User_df <- read_csv("user_info.csv")
AI_App_Names_df  <- read_csv('AI_App_Names.csv')

Step 2: Remove exact duplicates from Usage_df
Usage_df <- Usage_df %>% distinct()

Step 3: Fix User_df duplicates — keep second record per PANELISTID
User_df <- User_df %>%
  group_by(PANELISTID) %>%
  slice_tail(n = 1) %>%
  ungroup()

Step 4: Flag missing AGE and Unknown gender
User_df <- User_df %>%
  mutate(AGE_MISSING = is.na(AGE))

Step 5: Handle FOREGROUNDDURATION outliers using IQR method
Q1 <- quantile(Usage_df$FOREGROUNDDURATION, 0.25, na.rm = TRUE)
Q3 <- quantile(Usage_df$FOREGROUNDDURATION, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
Usage_df <- Usage_df %>%
  filter(FOREGROUNDDURATION <= Q3 + 1.5 * IQR_val,
         FOREGROUNDDURATION >= 0)

Step 6: Flag AI apps in Usage_df
Usage_df <- Usage_df %>%
  mutate(IS_AI = APPDESCRIPTION %in% AI_App_Names_df$APPDESCRIPTION)

## TASK 2 — BUILD PANEL-LEVEL FEATURES FOR ML

Create a user-level summary dataset that aggregates Usage_df to one row per 
panelist. This becomes the feature matrix for the Random Forest.

panel_df <- Usage_df %>%
  group_by(PANELISTID) %>%
  summarise(
    total_duration = sum(FOREGROUNDDURATION, na.rm = TRUE),
    avg_weekly_duration = mean(FOREGROUNDDURATION, na.rm = TRUE),
    n_unique_apps = n_distinct(APPDESCRIPTION),
    n_weeks_observed = n_distinct(WEEK),
    ai_app_duration = sum(FOREGROUNDDURATION[IS_AI == TRUE], na.rm = TRUE),
    non_ai_duration = sum(FOREGROUNDDURATION[IS_AI == FALSE], na.rm = TRUE),
    pct_ai_usage = ai_app_duration / total_duration,
    n_ai_apps_used = n_distinct(APPDESCRIPTION[IS_AI == TRUE])
  ) %>%
  left_join(User_df %>% select(PANELISTID, AGE, GENDER, CITY, TREATED,
            Weekday, Weekend, Morning, Afternoon, Night, AGE_MISSING),
            by = "PANELISTID")

## TASK 3 — ADDRESS CLASS IMBALANCE

Step 1: Check the imbalance
table(panel_df$TREATED)
prop.table(table(panel_df$TREATED))

Step 2: Prepare features for Random Forest
Remove non-numeric columns and handle remaining NAs

rf_df <- panel_df %>%
  select(-PANELISTID, -CITY) %>%
  mutate(
    GENDER = as.factor(GENDER),
    TREATED = as.factor(TREATED),
    AGE = ifelse(is.na(AGE), median(AGE, na.rm = TRUE), AGE),
    AGE_MISSING = as.integer(AGE_MISSING)
  ) %>%
  drop_na()

Step 3: Stratified train/test split (70/30)
set.seed(42)
train_index <- createDataPartition(rf_df$TREATED, p = 0.7, 
                                    list = FALSE, times = 1)
train_df <- rf_df[train_index, ]
test_df  <- rf_df[-train_index, ]

Verify stratification worked:
prop.table(table(train_df$TREATED))
prop.table(table(test_df$TREATED))

Step 4: Train Random Forest WITH class weighting to handle imbalance
Calculate class weights inversely proportional to class frequency:
class_counts <- table(train_df$TREATED)
class_weights <- max(class_counts) / class_counts

rf_model <- randomForest(
  TREATED ~ .,
  data = train_df,
  ntree = 500,
  classwt = class_weights,
  importance = TRUE,
  seed = 42
)

Step 5: Evaluate using proper metrics for imbalanced data
predictions <- predict(rf_model, test_df)
conf_matrix <- confusionMatrix(predictions, test_df$TREATED, 
                                positive = "1")
print(conf_matrix)

Get AUC-ROC:
library(pROC)
prob_predictions <- predict(rf_model, test_df, type = "prob")[,2]
roc_obj <- roc(as.numeric(test_df$TREATED) - 1, prob_predictions)
auc(roc_obj)

## TASK 4 — SUMMARY STATISTICS FOR WORD DOCUMENT

Produce these summary statistics tables for the submission:

# Overall panel summary
summary(panel_df)

# Summary by treatment group
panel_df %>%
  group_by(TREATED) %>%
  summarise(
    n = n(),
    avg_total_duration = mean(total_duration, na.rm = TRUE),
    avg_unique_apps = mean(n_unique_apps, na.rm = TRUE),
    avg_weeks = mean(n_weeks_observed, na.rm = TRUE),
    avg_ai_duration = mean(ai_app_duration, na.rm = TRUE),
    avg_pct_ai = mean(pct_ai_usage, na.rm = TRUE),
    avg_age = mean(AGE, na.rm = TRUE),
    pct_female = mean(GENDER == "Female", na.rm = TRUE),
    pct_male = mean(GENDER == "Male", na.rm = TRUE)
  )

# Class imbalance before and after weighting
table(panel_df$TREATED)
prop.table(table(panel_df$TREATED))

# Feature importance from Random Forest
importance_df <- as.data.frame(importance(rf_model)) %>%
  rownames_to_column("Feature") %>%
  arrange(desc(MeanDecreaseGini))
print(importance_df)

## TASK 5 — EXPORT SUMMARY STATS TO EXCEL FOR WORD DOC

write_xlsx(list(
  "Panel Summary by Treatment" = panel_df %>%
    group_by(TREATED) %>%
    summarise(across(where(is.numeric), 
              list(mean = ~mean(.x, na.rm = TRUE),
                   sd = ~sd(.x, na.rm = TRUE)),
              .names = "{.col}_{.fn}")),
  "Feature Importance" = importance_df,
  "Class Distribution" = as.data.frame(table(panel_df$TREATED)) %>%
    mutate(Proportion = Freq / sum(Freq))
), "ML_Assignment_Summary.xlsx")

## CODING STANDARDS
- Add a comment above every code block explaining what it does
- Use section dividers: # === SECTION NAME ===
- Use descriptive variable names throughout
- Print outputs at each step so progress is visible
- Save the final script as ML_Assignment.R