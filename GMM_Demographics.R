# =============================================================================
# LLM App Analytics — Gaussian Mixture Model: Demographics → AI App Usage
# Assumes: Cleaning Data.R has already been run (panel_df is in environment)
# Purpose: Discover latent user segments defined jointly by demographics and
#          AI usage frequency. Reveals which demographic profiles are
#          associated with high vs. low AI adoption.
# =============================================================================
library(tidyverse)
library(mclust)
library(ggplot2)

# === FEATURE PREPARATION ===
# Demographic inputs: AGE, GENDER, time-of-day/day-of-week patterns
# AI usage outcomes included jointly so GMM surfaces usage-demographic clusters

gmm_raw <- panel_df %>%
  select(
    PANELISTID,
    AGE, GENDER,
    Weekday, Weekend, Morning, Afternoon, Night,
    pct_ai_usage, n_ai_apps_used
  ) %>%
  # Impute missing AGE with median (small share flagged AGE_MISSING)
  mutate(
    AGE = if_else(is.na(AGE), median(AGE, na.rm = TRUE), as.double(AGE)),
    # Encode GENDER: Male = 1, Female = 0, other/NA = 0.5
    GENDER_NUM = case_when(
      str_to_lower(GENDER) == "male"   ~ 1,
      str_to_lower(GENDER) == "female" ~ 0,
      TRUE                              ~ 0.5
    )
  ) %>%
  # Replace any remaining NAs in usage columns with 0
  mutate(across(c(pct_ai_usage, n_ai_apps_used), ~replace_na(.x, 0)))

# Matrix for mclust (numeric only, scaled)
gmm_features <- gmm_raw %>%
  select(AGE, GENDER_NUM, Weekday, Weekend, Morning, Afternoon, Night,
         pct_ai_usage, n_ai_apps_used)

gmm_scaled <- scale(gmm_features)

# === FIT GMM — BIC SELECTS NUMBER OF COMPONENTS ===
# mclust tests G = 1..9 components and all covariance structures;
# BIC picks the best-fitting model automatically.
set.seed(42)
gmm_fit <- Mclust(gmm_scaled)

cat(sprintf("\nBIC-selected model: %s with G = %d components\n",
            gmm_fit$modelName, gmm_fit$G))
cat(sprintf("BIC: %.1f\n\n", gmm_fit$bic))

# === ATTACH CLUSTER LABELS ===
gmm_raw <- gmm_raw %>%
  mutate(Cluster = factor(gmm_fit$classification))

# === CLUSTER PROFILES ===
# Mean of each feature per cluster to characterise what each segment looks like
profile <- gmm_raw %>%
  group_by(Cluster) %>%
  summarise(
    n                = n(),
    mean_age         = mean(AGE),
    pct_male         = mean(GENDER_NUM == 1),
    mean_weekday     = mean(Weekday),
    mean_weekend     = mean(Weekend),
    mean_morning     = mean(Morning),
    mean_afternoon   = mean(Afternoon),
    mean_night       = mean(Night),
    mean_pct_ai      = mean(pct_ai_usage),
    mean_n_ai_apps   = mean(n_ai_apps_used),
    .groups = "drop"
  )

cat("--- Cluster Profile Summary ---\n")
print(profile, digits = 3)

# === PLOT 1: AI Usage by Cluster ===
p_ai <- ggplot(gmm_raw, aes(x = Cluster, y = pct_ai_usage, fill = Cluster)) +
  geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.15, alpha = 0.07, size = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title    = "AI App Usage Share by GMM Cluster",
    subtitle = "Each cluster represents a distinct demographic-usage segment",
    x        = "Cluster",
    y        = "% of Total Usage on AI Apps"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

print(p_ai)
ggsave("gmm_ai_usage_by_cluster.png", p_ai, width = 8, height = 6, dpi = 150)

# === PLOT 2: Age Distribution by Cluster ===
p_age <- ggplot(gmm_raw, aes(x = AGE, fill = Cluster)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Age Distribution by GMM Cluster",
    x     = "Age",
    y     = "Density",
    fill  = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

print(p_age)
ggsave("gmm_age_by_cluster.png", p_age, width = 8, height = 6, dpi = 150)

# === PLOT 3: Cluster Heatmap — Feature Means ===
# Reshape profile to long for a heatmap view of each cluster's character
profile_long <- profile %>%
  select(Cluster, mean_age, pct_male, mean_weekday, mean_weekend,
         mean_morning, mean_afternoon, mean_night,
         mean_pct_ai, mean_n_ai_apps) %>%
  pivot_longer(-Cluster, names_to = "Feature", values_to = "Mean") %>%
  group_by(Feature) %>%
  mutate(Scaled_Mean = as.numeric(scale(Mean))) %>%
  ungroup()

p_heat <- ggplot(profile_long, aes(x = Cluster, y = Feature, fill = Scaled_Mean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Mean, 2)), size = 3) +
  scale_fill_gradient2(
    low      = "#2196F3",
    mid      = "white",
    high     = "#E05C4B",
    midpoint = 0,
    name     = "Z-score\nacross clusters"
  ) +
  labs(
    title    = "GMM Cluster Profiles: Demographic & AI Usage Features",
    subtitle = "Color = relative value (z-score); number = raw cluster mean",
    x        = "Cluster",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y   = element_text(size = 10)
  )

print(p_heat)
ggsave("gmm_cluster_heatmap.png", p_heat, width = 9, height = 6, dpi = 150)

# === PLOT 4: PCA Projection with Cluster Ellipses ===
# Reduce the scaled GMM feature matrix to 2D so clusters can be shown in a
# single scatter plot. stat_ellipse draws a 1-SD normal confidence region
# around each cluster centroid, making separation (or overlap) explicit.

gmm_pca      <- prcomp(gmm_scaled)
pct_var      <- summary(gmm_pca)$importance[2, 1:2] * 100

gmm_plot_df <- as.data.frame(gmm_pca$x[, 1:2]) %>%
  mutate(
    Cluster   = gmm_raw$Cluster,
    max_prob  = apply(gmm_fit$z, 1, max)
  )

# Sample 20% for readability, same approach as PCA_Biplot.R
set.seed(42)
gmm_plot_sample <- gmm_plot_df %>% slice_sample(prop = 0.20)

p_ellipse <- ggplot(gmm_plot_sample, aes(x = PC1, y = PC2, color = Cluster)) +
  # Draw full-data ellipses first (uses all rows so ellipses are accurate)
  stat_ellipse(
    data  = gmm_plot_df,
    aes(x = PC1, y = PC2, color = Cluster, fill = Cluster),
    type  = "norm", level = 0.68,
    geom  = "polygon", alpha = 0.12, linewidth = 0.8
  ) +
  geom_point(alpha = 0.4, size = 1.0) +
  labs(
    title    = "GMM Clusters in PCA Space",
    subtitle = sprintf("Ellipses = 1 SD (68%% region)  |  PC1: %.1f%%  PC2: %.1f%% variance explained",
                       pct_var[1], pct_var[2]),
    x        = sprintf("PC1 (%.1f%% variance)", pct_var[1]),
    y        = sprintf("PC2 (%.1f%% variance)", pct_var[2]),
    color    = "Cluster",
    fill     = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(p_ellipse)
ggsave("gmm_cluster_ellipses.png", p_ellipse, width = 9, height = 7, dpi = 150)

# === UNCERTAINTY ===
# mclust provides posterior probabilities; flag low-confidence assignments
gmm_raw <- gmm_raw %>%
  mutate(max_prob = apply(gmm_fit$z, 1, max))

cat(sprintf("\nMedian cluster assignment certainty: %.1f%%\n",
            median(gmm_raw$max_prob) * 100))
cat(sprintf("Observations with < 70%% certainty: %d (%.1f%%)\n",
            sum(gmm_raw$max_prob < 0.70),
            mean(gmm_raw$max_prob < 0.70) * 100))
