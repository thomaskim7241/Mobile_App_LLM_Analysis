# =============================================================================
# LLM App Analytics — Model Comparison: Class Weighting vs Bootstrapping
# Assumes Cleaning.R has already been run
# Environment requires: panel_df
# =============================================================================

library(randomForest)
library(caret)
library(pROC)

# =============================================================================
# PREPARE DATA FOR RANDOM FOREST
# =============================================================================

rf_df <- panel_df %>%
  select(-PANELISTID, -CITY, -AGE, -AGE_GROUP) %>%
  mutate(
    GENDER      = as.factor(GENDER),
    TREATED     = as.factor(TREATED),
    AGE_MISSING = as.integer(AGE_MISSING)
  ) %>%
  drop_na()

cat("Class distribution:\n")
prop.table(table(rf_df$TREATED))

# =============================================================================
# STRATIFIED TRAIN/TEST SPLIT — uses original unbalanced data
# Test set is always the original distribution so evaluation is realistic
# =============================================================================

set.seed(42)
train_index    <- createDataPartition(rf_df$TREATED, p = 0.7, list = FALSE)
train_original <- rf_df[train_index, ]
test_df        <- rf_df[-train_index, ]

cat("\nTest set distribution:\n")
prop.table(table(test_df$TREATED))

# =============================================================================
# MODEL 1 — CLASS WEIGHTING
# Penalizes misclassification of treated users proportionally to imbalance
# No data is added or removed — only the loss function is adjusted
# =============================================================================

class_counts  <- table(train_original$TREATED)
class_weights <- max(class_counts) / class_counts

set.seed(42)
rf_weighted <- randomForest(
  TREATED ~ .,
  data       = train_original,
  ntree      = 500,
  classwt    = class_weights,
  importance = TRUE
)

pred_weighted <- predict(rf_weighted, test_df)
prob_weighted <- predict(rf_weighted, test_df, type = "prob")[, 2]
cm_weighted   <- confusionMatrix(pred_weighted, test_df$TREATED, positive = "1")
roc_weighted  <- roc(as.numeric(test_df$TREATED) - 1, prob_weighted, quiet = TRUE)

cat("\n=== MODEL 1: Class Weighting ===\n")
print(cm_weighted)
cat("AUC:", round(auc(roc_weighted), 4), "\n")

imp_weighted <- as.data.frame(importance(rf_weighted)) %>%
  rownames_to_column("Feature") %>%
  arrange(desc(MeanDecreaseGini))

# =============================================================================
# MODEL 2 — BOOTSTRAPPING
# Resamples treated users with replacement to match control group size
# Only uses real observations — no synthetic data introduced
# Train on balanced bootstrapped set, evaluate on original unbalanced test set
# =============================================================================

treated_train <- train_original %>% filter(TREATED == 1)
control_train <- train_original %>% filter(TREATED == 0)

cat("\nOriginal training set — Treated:", nrow(treated_train),
    "Control:", nrow(control_train), "\n")

set.seed(42)
treated_boot <- treated_train %>%
  slice_sample(n = nrow(control_train), replace = TRUE)

train_boot <- bind_rows(treated_boot, control_train)

cat("Bootstrapped training set — Treated:", nrow(treated_boot),
    "Control:", nrow(control_train), "\n")

set.seed(42)
rf_boot <- randomForest(
  TREATED ~ .,
  data       = train_boot,
  ntree      = 500,
  importance = TRUE
)

pred_boot <- predict(rf_boot, test_df)
prob_boot <- predict(rf_boot, test_df, type = "prob")[, 2]
cm_boot   <- confusionMatrix(pred_boot, test_df$TREATED, positive = "1")
roc_boot  <- roc(as.numeric(test_df$TREATED) - 1, prob_boot, quiet = TRUE)

cat("\n=== MODEL 2: Bootstrapping ===\n")
print(cm_boot)
cat("AUC:", round(auc(roc_boot), 4), "\n")

imp_boot <- as.data.frame(importance(rf_boot)) %>%
  rownames_to_column("Feature") %>%
  arrange(desc(MeanDecreaseGini))

# =============================================================================
# COMPARISON TABLE
# =============================================================================

comparison_df <- data.frame(
  Metric = c(
    "AUC-ROC",
    "Overall Accuracy",
    "Sensitivity / Recall (Treated)",
    "Specificity / Recall (Control)",
    "Precision (Treated)",
    "F1 Score (Treated)"
  ),
  Class_Weighting = c(
    round(auc(roc_weighted), 4),
    round(cm_weighted$overall["Accuracy"], 4),
    round(cm_weighted$byClass["Sensitivity"], 4),
    round(cm_weighted$byClass["Specificity"], 4),
    round(cm_weighted$byClass["Precision"], 4),
    round(cm_weighted$byClass["F1"], 4)
  ),
  Bootstrapping = c(
    round(auc(roc_boot), 4),
    round(cm_boot$overall["Accuracy"], 4),
    round(cm_boot$byClass["Sensitivity"], 4),
    round(cm_boot$byClass["Specificity"], 4),
    round(cm_boot$byClass["Precision"], 4),
    round(cm_boot$byClass["F1"], 4)
  )
)

cat("\n=== Performance Comparison ===\n")
print(comparison_df)

# =============================================================================
# FEATURE IMPORTANCE COMPARISON
# =============================================================================

imp_comparison <- imp_weighted %>%
  select(Feature, Weighted_Gini = MeanDecreaseGini) %>%
  left_join(
    imp_boot %>% select(Feature, Boot_Gini = MeanDecreaseGini),
    by = "Feature"
  ) %>%
  mutate(
    Rank_Weighted = rank(-Weighted_Gini),
    Rank_Boot     = rank(-Boot_Gini),
    Rank_Diff     = abs(Rank_Weighted - Rank_Boot)
  ) %>%
  arrange(Rank_Weighted)

cat("\n=== Feature Importance Comparison ===\n")
print(imp_comparison)

# =============================================================================
# ROC CURVE COMPARISON PLOT
# =============================================================================

plot(roc_weighted, col = "steelblue", lwd = 2,
     main = "ROC Curve: Class Weighting vs Bootstrapping")
lines(roc_boot, col = "tomato", lwd = 2)
legend("bottomright",
       legend = c(
         paste("Class Weighting  AUC:", round(auc(roc_weighted), 3)),
         paste("Bootstrapping    AUC:", round(auc(roc_boot), 3))
       ),
       col = c("steelblue", "tomato"), lwd = 2)

# =============================================================================
# FEATURE IMPORTANCE PLOT — TOP 10 BY GINI FOR EACH MODEL
# =============================================================================

imp_weighted %>%
  head(10) %>%
  ggplot(aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance: Class Weighting",
       x = "", y = "Mean Decrease Gini") +
  theme_minimal()

imp_boot %>%
  head(10) %>%
  ggplot(aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Feature Importance: Bootstrapping",
       x = "", y = "Mean Decrease Gini") +
  theme_minimal()