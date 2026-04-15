## CONTEXT
Read CLAUDE_BRIEFING.md first for full project context.
The dataset has already been partially cleaned:
- Usage_df: 20,868,730 rows, 4 columns (PANELISTID, WEEK, APPDESCRIPTION, FOREGROUNDDURATION)
- User_df: 11,793 rows, 10 columns including TREATED variable
- IS_AI column already exists in Usage_df flagging LLM apps
- Exact duplicates already removed from Usage_df with distinct()

## TASK 1 — Data Cleaning in Cleaning Data.R

## TASK 2 — ADDRESS CLASS IMBALANCE WITH CLASS WEIGHTING

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
set.seed(777)
train_index <- createDataPartition(rf_df$TREATED, p = 0.7, 
                                    list = FALSE, times = 1)
train_df <- rf_df[train_index, ]
test_df  <- rf_df[-train_index, ]

Verify stratification worked
prop.table(table(train_df$TREATED))
prop.table(table(test_df$TREATED))

Step 4: Train Random Forest WITH class weighting to handle imbalance
Calculate class weights inversely proportional to class frequency
class_counts <- table(train_df$TREATED)
class_weights <- max(class_counts) / class_counts

rf_model <- randomForest(
  TREATED ~ .,
  data = train_df,
  ntree = 500,
  classwt = class_weights,
  importance = TRUE,
  seed = 777
)

Step 5: Evaluate using proper metrics for imbalanced data
predictions <- predict(rf_model, test_df)
conf_matrix <- confusionMatrix(predictions, test_df$TREATED, 
                                positive = "1")
print(conf_matrix)

Get AUC-ROC
library(pROC)
prob_predictions <- predict(rf_model, test_df, type = "prob")[,2]
roc_obj <- roc(as.numeric(test_df$TREATED) - 1, prob_predictions)
auc(roc_obj)

## TASK 3 — SUMMARY STATISTICS

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