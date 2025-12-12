# =============================================================================
# ELSA Hearing & Cognitive Trajectories Project
# Script 06: Create Dashboard Data (Aggregated Summaries Only)
# =============================================================================
#
# PURPOSE:
# Creates aggregated summary data for the Shiny dashboard.
# This allows the dashboard to be deployed publicly without sharing
# individual-level ELSA data.
#
# INPUT FILES:
# - data/processed/analytic_sample.rds
# - data/processed/attrition_summary.rds
# - output/tables/*.csv (model results)
#
# OUTPUT FILES:
# - app/data/sample_summary.rds
# - app/data/table1_summary.rds
# - app/data/trajectory_means.rds
# - app/data/retention_summary.rds
# - app/data/attrition_counts.rds
# - app/data/model_comparison.csv (copy)
# - app/data/model_results_fully_adjusted.csv (copy)
#
# =============================================================================

library(tidyverse)

# Create app/data directory
dir.create("app/data", showWarnings = FALSE, recursive = TRUE)

# Load the analytic sample
analytic_sample <- readRDS("data/processed/analytic_sample.rds")
attrition_summary <- readRDS("data/processed/attrition_summary.rds")

# =============================================================================
# 1. Sample Summary (for Overview tab)
# =============================================================================

message("Creating sample summary...")

baseline <- analytic_sample %>% filter(wave == 7)

sample_summary <- list(
  n_participants = n_distinct(baseline$idauniq),
  n_observations = nrow(analytic_sample),
  waves = "7-11 (2014-2023)",

  hearing_distribution = baseline %>%
    count(hearing_acuity) %>%
    mutate(pct = round(100 * n / sum(n), 1)),

  age_mean = round(mean(baseline$age, na.rm = TRUE), 1),
  age_sd = round(sd(baseline$age, na.rm = TRUE), 1)
)

saveRDS(sample_summary, "app/data/sample_summary.rds")

# =============================================================================
# 2. Table 1 Summary (for Baseline tab)
# =============================================================================

message("Creating Table 1 summary...")

table1_summary <- baseline %>%
  group_by(hearing_acuity) %>%
  summarise(
    N = n(),
    `Age (mean)` = round(mean(age, na.rm = TRUE), 1),
    `Age (SD)` = round(sd(age, na.rm = TRUE), 1),
    `Female (%)` = round(100 * mean(sex_label == "Female", na.rm = TRUE), 1),
    `Verbal Fluency (mean)` = round(mean(cf_animals, na.rm = TRUE), 1),
    `Verbal Fluency (SD)` = round(sd(cf_animals, na.rm = TRUE), 1),
    `Delayed Recall (mean)` = round(mean(cf_delayed_recall, na.rm = TRUE), 1),
    `Delayed Recall (SD)` = round(sd(cf_delayed_recall, na.rm = TRUE), 1),
    `Immediate Recall (mean)` = round(mean(cf_imm_recall_total, na.rm = TRUE), 1),
    `Immediate Recall (SD)` = round(sd(cf_imm_recall_total, na.rm = TRUE), 1),
    `Serial 7s (mean)` = round(mean(cf_serial7_total, na.rm = TRUE), 1),
    `Serial 7s (SD)` = round(sd(cf_serial7_total, na.rm = TRUE), 1),
    `Depression CES-D (mean)` = round(mean(cesd_total, na.rm = TRUE), 1),
    `Depression CES-D (SD)` = round(sd(cesd_total, na.rm = TRUE), 1),
    `Diabetes (%)` = round(100 * mean(has_diabetes, na.rm = TRUE), 1),
    `CVD (%)` = round(100 * mean(has_cvd, na.rm = TRUE), 1),
    `Hypertension (%)` = round(100 * mean(has_hypertension, na.rm = TRUE), 1),
    .groups = "drop"
  )

saveRDS(table1_summary, "app/data/table1_summary.rds")

# =============================================================================
# 3. Trajectory Means (for Trajectories tab)
# =============================================================================

message("Creating trajectory means...")

# Calculate means and CIs for each outcome at each wave
cognitive_outcomes <- c("cf_animals", "cf_delayed_recall",
                        "cf_imm_recall_total", "cf_serial7_total")

trajectory_means <- map_dfr(cognitive_outcomes, function(outcome) {
  analytic_sample %>%
    group_by(wave, time, hearing_acuity) %>%
    summarise(
      mean_score = mean(.data[[outcome]], na.rm = TRUE),
      sd_score = sd(.data[[outcome]], na.rm = TRUE),
      n = sum(!is.na(.data[[outcome]])),
      se_score = sd_score / sqrt(n),
      ci_lower = mean_score - 1.96 * se_score,
      ci_upper = mean_score + 1.96 * se_score,
      .groups = "drop"
    ) %>%
    mutate(outcome = outcome)
})

saveRDS(trajectory_means, "app/data/trajectory_means.rds")

# =============================================================================
# 4. Retention Summary (for Attrition tab)
# =============================================================================

message("Creating retention summary...")

retention_summary <- analytic_sample %>%
  group_by(wave, hearing_acuity) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(hearing_acuity) %>%
  mutate(
    n_baseline = n[wave == 7],
    retention_pct = round(100 * n / n_baseline, 1)
  ) %>%
  ungroup()

saveRDS(retention_summary, "app/data/retention_summary.rds")

# =============================================================================
# 5. Attrition Counts (for Attrition tab)
# =============================================================================

message("Creating attrition counts...")

attrition_counts <- attrition_summary %>%
  count(n_waves, hearing_group) %>%
  group_by(hearing_group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

saveRDS(attrition_counts, "app/data/attrition_counts.rds")

# =============================================================================
# 6. Copy model results CSVs
# =============================================================================

message("Copying model results...")

file.copy("output/tables/model_comparison.csv",
          "app/data/model_comparison.csv", overwrite = TRUE)
file.copy("output/tables/model_results_fully_adjusted.csv",
          "app/data/model_results_fully_adjusted.csv", overwrite = TRUE)
file.copy("output/tables/model_results_unadjusted.csv",
          "app/data/model_results_unadjusted.csv", overwrite = TRUE)
file.copy("output/tables/model_results_sociodemographic.csv",
          "app/data/model_results_sociodemographic.csv", overwrite = TRUE)
file.copy("output/tables/model_quadratic_results.csv",
          "app/data/model_quadratic_results.csv", overwrite = TRUE)
file.copy("output/tables/model_B_results.csv",
          "app/data/model_B_results.csv", overwrite = TRUE)

# =============================================================================
# 7. Copy predicted trajectories
# =============================================================================

message("Copying predicted trajectories...")

if (file.exists("output/predicted_trajectories.rds")) {
  file.copy("output/predicted_trajectories.rds",
            "app/data/predicted_trajectories.rds", overwrite = TRUE)
  cat("  - Predicted trajectories copied\n")
} else {
  warning("predicted_trajectories.rds not found - run 05_longitudinal_analysis.R first")
}

# =============================================================================
# Summary
# =============================================================================

message("\n========== Dashboard Data Created ==========\n")
cat("Files created in app/data/:\n")
cat("  - sample_summary.rds\n")
cat("  - table1_summary.rds\n")
cat("  - trajectory_means.rds\n")
cat("  - retention_summary.rds\n")
cat("  - attrition_counts.rds\n")
cat("  - predicted_trajectories.rds\n")
cat("  - model_comparison.csv\n")
cat("  - model_quadratic_results.csv\n")
cat("  - model_B_results.csv\n")
cat("  - model_results_*.csv\n")
cat("\nThese files contain AGGREGATED data only.\n")
cat("No individual-level ELSA data is included.\n")
cat("\nYou can now deploy the dashboard to Posit Connect.\n")
