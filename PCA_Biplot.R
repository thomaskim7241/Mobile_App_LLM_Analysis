# =============================================================================
# LLM App Analytics — PCA Biplot
# Purpose: Reduce behavioral feature space to 2D and visualize whether
#          AI adopters (TREATED=1) cluster separately from control users.
#          Arrows show which usage dimensions drive separation.
# =============================================================================
library(rlang)
library(factoextra)
# === SELECT NUMERIC FEATURES FOR PCA ===
# Exclude ai_app_duration / non_ai_duration — they are linear components of
# total_duration and would inflate that axis. pct_ai_usage captures the same
# signal without multicollinearity.
pca_features <- c(
  "total_duration",
  "avg_weekly_duration",
  "n_unique_apps",
  "n_weeks_observed",
  "pct_ai_usage",
  "n_ai_apps_used",
  "Weekday",
  "Weekend",
  "Morning",
  "Afternoon",
  "Night"
)

# Drop rows with any NA in the feature set (a small share have missing usage data)
pca_input <- panel_df %>%
  select(PANELISTID, TREATED, AGE_GROUP, all_of(pca_features)) %>%
  drop_na()

# === RUN PCA ===
# scale. = TRUE is required — features are on very different scales
# (minutes vs counts vs proportions)
pca_result <- prcomp(pca_input[, pca_features], scale. = TRUE)

# Variance explained by each component
var_explained <- summary(pca_result)$importance
cat("Variance explained by PC1 and PC2:\n")
cat(sprintf("  PC1: %.1f%%\n", var_explained[2, 1] * 100))
cat(sprintf("  PC2: %.1f%%\n", var_explained[2, 2] * 100))
cat(sprintf("  Combined: %.1f%%\n", var_explained[3, 2] * 100))


# === BIPLOT — TREATED vs CONTROL ===
# Individual points colored by treatment status, arrows show feature loadings.
# If AI adopters behave differently overall, they should cluster separately.

treatment_labels <- ifelse(pca_input$TREATED == 1, "AI Adopter", "Control")

biplot_treated <- fviz_pca_biplot(
  pca_result,
  # --- individuals ---
  geom.ind     = "point",
  col.ind      = treatment_labels,
  palette      = c("Control" = "#999999", "AI Adopter" = "#E05C4B"),
  alpha.ind    = 0.35,
  pointsize    = 1.2,
  # --- variables (arrows) ---
  col.var      = "#2C6B9E",
  alpha.var    = 0.85,
  repel        = TRUE,           # prevent arrow label overlap
  labelsize    = 3.5,
  # --- appearance ---
  title        = "PCA Biplot: App Usage Behavior by AI Adoption Status",
  legend.title = "Group"
) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

print(biplot_treated)
ggsave("pca_biplot_treated.png", biplot_treated, width = 9, height = 7, dpi = 150)


# === BIPLOT — COLORED BY AGE GROUP ===
# Secondary view: does age group structure the behavioral space?
# Helps answer heterogeneity objective (Objective 3).

# Sample 20% of observations for readability
set.seed(42)
age_sample_idx <- sample(nrow(pca_input), size = floor(0.20 * nrow(pca_input)))

biplot_age <- fviz_pca_biplot(
  pca_result,
  select.ind   = list(name = as.character(age_sample_idx)),
  geom.ind     = "point",
  col.ind      = pca_input$AGE_GROUP,
  palette      = c(
    "15-22 (Student)"            = "#4CAF50",
    "23-37 (Young Professional)" = "#2196F3",
    "38-57 (Established Career)" = "#FF9800",
    "58+ (Pre-Retirement)"       = "#9C27B0",
    "Unknown"                    = "#BDBDBD"
  ),
  alpha.ind    = 0.35,
  pointsize    = 1.2,
  col.var      = "#555555",
  alpha.var    = 0.75,
  repel        = TRUE,
  labelsize    = 3.5,
  title        = "PCA Biplot: App Usage Behavior by Age Group",
  legend.title = "Age Group"
) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

print(biplot_age)
ggsave("pca_biplot_age.png", biplot_age, width = 9, height = 7, dpi = 150)


# === BIPLOT — COLORED BY AGE GROUP (LOG-TRANSFORMED FEATURES) ===
# log1p applied to all features before scaling to spread dense clusters
# and reduce the leverage of extreme usage values.

pca_input_log <- pca_input %>%
  mutate(across(all_of(pca_features), log1p))

pca_result_log <- prcomp(pca_input_log[, pca_features], scale. = TRUE)

biplot_age_log <- fviz_pca_biplot(
  pca_result_log,
  select.ind   = list(name = as.character(age_sample_idx)),
  geom.ind     = "point",
  col.ind      = pca_input_log$AGE_GROUP,
  palette      = c(
    "15-22 (Student)"            = "#4CAF50",
    "23-37 (Young Professional)" = "#2196F3",
    "38-57 (Established Career)" = "#FF9800",
    "58+ (Pre-Retirement)"       = "#9C27B0",
    "Unknown"                    = "#BDBDBD"
  ),
  alpha.ind    = 0.35,
  pointsize    = 1.2,
  col.var      = "#555555",
  alpha.var    = 0.75,
  repel        = TRUE,
  labelsize    = 3.5,
  title        = "PCA Biplot: App Usage Behavior by Age Group (Log-Transformed)",
  legend.title = "Age Group"
) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

print(biplot_age_log)
ggsave("pca_biplot_age_log.png", biplot_age_log, width = 9, height = 7, dpi = 150)


# === SCREE PLOT ===
# Shows how many components are needed to explain meaningful variance.
# Helps justify why PC1+PC2 is (or isn't) sufficient for this biplot.

scree <- fviz_eig(
  pca_result,
  addlabels = TRUE,
  ylim      = c(0, 60),
  title     = "Scree Plot — Variance Explained by Principal Component"
) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(scree)
ggsave("pca_scree.png", scree, width = 7, height = 5, dpi = 150)


# === LOADING TABLE ===
# Print the top contributing variables to PC1 and PC2 for interpretation.
loadings <- as.data.frame(pca_result$rotation[, 1:2]) %>%
  rownames_to_column("Feature") %>%
  mutate(
    PC1_abs = abs(PC1),
    PC2_abs = abs(PC2)
  ) %>%
  arrange(desc(PC1_abs))

cat("\n--- Feature Loadings on PC1 and PC2 ---\n")
print(loadings %>% select(Feature, PC1, PC2), digits = 3)
