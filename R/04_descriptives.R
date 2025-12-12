# =============================================================================
# ELSA Hearing & Cognitive Trajectories Project
# Script 04: Descriptive Statistics
# =============================================================================
#
# PURPOSE:
# This script generates descriptive statistics and visualizations:
# 1. Table 1: Baseline characteristics by hearing group
# 2. Cognitive scores over time by hearing status
# 3. Trajectory plots showing cognitive change
# 4. Attrition analysis (who drops out?)
# 5. Missing data summary
#
# These descriptives help you understand your data before running models!
#
# INPUT FILES:
# - data/processed/analytic_sample.rds
# - data/processed/attrition_summary.rds
#
# OUTPUT FILES:
# - output/tables/table1_baseline_characteristics.html
# - output/tables/cognitive_summary_by_wave.csv
# - output/tables/table_attrition.html
# - output/tables/missing_data_summary.csv
# - output/figures/trajectory_*.png
#
# =============================================================================

# -----------------------------------------------------------------------------
# STEP 1: Load required packages
# -----------------------------------------------------------------------------

library(tidyverse)

# gtsummary: Creates beautiful publication-ready tables
# It automatically calculates means, SDs, counts, percentages
library(gtsummary)

# kableExtra: For formatting tables (optional, used by gtsummary)
library(kableExtra)

# -----------------------------------------------------------------------------
# STEP 2: Load the analysis data
# -----------------------------------------------------------------------------

analytic_sample <- readRDS("data/processed/analytic_sample.rds")
attrition_summary <- readRDS("data/processed/attrition_summary.rds")

# -----------------------------------------------------------------------------
# STEP 3: Create baseline dataset (Wave 7 only)
# -----------------------------------------------------------------------------

# For "Table 1" we want baseline characteristics
# So we filter to just Wave 7 data

baseline <- analytic_sample %>%
  filter(wave == 7)

cat("Baseline sample size:", nrow(baseline), "\n")

# -----------------------------------------------------------------------------
# STEP 4: Create Table 1 - Baseline characteristics by hearing group
# -----------------------------------------------------------------------------

message("\n========== Table 1: Baseline Characteristics ==========\n")

# This is the classic "Table 1" you see in every epidemiology paper
# It compares characteristics across your exposure groups

# Select variables to include in Table 1
table1_vars <- baseline %>%
  select(
    hearing_acuity,           # Our grouping variable (exposure)
    age, sex_label, age_group,  # Demographics
    # Cognitive outcomes at baseline
    cf_animals, cf_delayed_recall, cf_imm_recall_total,
    cf_serial7_total, cf_memory_composite
  )

# Create Table 1 using gtsummary's tbl_summary()
# This is a powerful function that automatically:
# - Calculates means & SDs for continuous variables
# - Calculates counts & percentages for categorical variables
table1 <- table1_vars %>%
  tbl_summary(
    # Group by hearing acuity (creates columns for each group)
    by = hearing_acuity,

    # How to display statistics:
    # {mean} ({sd}) for continuous variables
    # {n} ({p}%) for categorical variables
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),

    # Human-readable labels for variables
    label = list(
      age ~ "Age (years)",
      sex_label ~ "Sex",
      age_group ~ "Age group",
      cf_animals ~ "Verbal fluency (animals)",
      cf_delayed_recall ~ "Word-list delayed recall",
      cf_imm_recall_total ~ "Word-list immediate recall",
      cf_serial7_total ~ "Serial 7s (correct)",
      cf_memory_composite ~ "Memory composite"
    ),

    # Show missing data if any
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  # Add an "Overall" column
  add_overall() %>%
  # Add p-values comparing groups
  add_p() %>%
  # Customize headers
  modify_header(label = "**Characteristic**") %>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Hearing Acuity**")

# Print Table 1 to console
print(table1)

# Save Table 1 as HTML (you can open this in a web browser)
table1 %>%
  as_gt() %>%
  gt::gtsave("output/tables/table1_baseline_characteristics.html")

cat("\nTable 1 saved to: output/tables/table1_baseline_characteristics.html\n")

# -----------------------------------------------------------------------------
# STEP 5: Calculate cognitive scores by wave and hearing group
# -----------------------------------------------------------------------------

message("\n========== Cognitive Scores Over Time ==========\n")

# This shows how cognitive scores change across waves
# Stratified by hearing group

cognitive_summary <- analytic_sample %>%
  # Group by wave AND hearing status
  group_by(wave, hearing_acuity) %>%
  # Calculate summary statistics for each cognitive outcome
  summarise(
    n = n(),  # Sample size

    # For each outcome, calculate mean, SD, and valid N
    # The ~ syntax creates a formula; . refers to the column
    across(c(cf_animals, cf_delayed_recall, cf_imm_recall_total,
             cf_serial7_total, cf_memory_composite),
           list(
             mean = ~mean(., na.rm = TRUE),      # Mean (ignoring NAs)
             sd = ~sd(., na.rm = TRUE),          # Standard deviation
             n_valid = ~sum(!is.na(.))           # Count of non-missing
           )),
    .groups = "drop"
  )

# Show sample sizes
cat("Sample size per wave and hearing group:\n")
print(
  cognitive_summary %>%
    select(wave, hearing_acuity, n) %>%
    # pivot_wider reshapes from long to wide format
    pivot_wider(names_from = hearing_acuity, values_from = n)
)

# Show mean verbal fluency
cat("\nMean verbal fluency (animals) by wave and hearing group:\n")
print(
  cognitive_summary %>%
    select(wave, hearing_acuity, cf_animals_mean) %>%
    mutate(cf_animals_mean = round(cf_animals_mean, 2)) %>%
    pivot_wider(names_from = hearing_acuity, values_from = cf_animals_mean)
)

# Show mean delayed recall
cat("\nMean delayed recall by wave and hearing group:\n")
print(
  cognitive_summary %>%
    select(wave, hearing_acuity, cf_delayed_recall_mean) %>%
    mutate(cf_delayed_recall_mean = round(cf_delayed_recall_mean, 2)) %>%
    pivot_wider(names_from = hearing_acuity, values_from = cf_delayed_recall_mean)
)

# Save the full summary to CSV
write_csv(cognitive_summary, "output/tables/cognitive_summary_by_wave.csv")

# -----------------------------------------------------------------------------
# STEP 6: Create trajectory plots
# -----------------------------------------------------------------------------

message("\n========== Creating Trajectory Plots ==========\n")

# Plots showing cognitive change over time are essential for understanding
# your longitudinal data!

# First, calculate means and standard errors for plotting
plot_data <- analytic_sample %>%
  group_by(wave, time, hearing_acuity) %>%
  summarise(
    # Mean scores
    mean_animals = mean(cf_animals, na.rm = TRUE),
    mean_delayed = mean(cf_delayed_recall, na.rm = TRUE),
    mean_imm = mean(cf_imm_recall_total, na.rm = TRUE),
    mean_serial7 = mean(cf_serial7_total, na.rm = TRUE),

    # Standard errors (SE = SD / sqrt(N))
    # SE is used for confidence intervals
    se_animals = sd(cf_animals, na.rm = TRUE) / sqrt(sum(!is.na(cf_animals))),
    se_delayed = sd(cf_delayed_recall, na.rm = TRUE) / sqrt(sum(!is.na(cf_delayed_recall))),
    se_imm = sd(cf_imm_recall_total, na.rm = TRUE) / sqrt(sum(!is.na(cf_imm_recall_total))),
    se_serial7 = sd(cf_serial7_total, na.rm = TRUE) / sqrt(sum(!is.na(cf_serial7_total))),

    n = n(),
    .groups = "drop"
  )

# Define colors for hearing groups (colorblind-friendly palette)
hearing_colors <- c(
  "Good (6 tones)" = "#2E86AB",                    # Blue
  "Mild difficulty (3-5 tones)" = "#F6AE2D",       # Orange/Yellow
  "Moderate-severe (0-2 tones)" = "#E94F37"        # Red
)

# ---- PLOT 1: Verbal Fluency (Animals) ----

p_animals <- ggplot(plot_data,
                    aes(x = time,
                        y = mean_animals,
                        color = hearing_acuity,
                        group = hearing_acuity)) +
  # Draw lines connecting the means
  geom_line(linewidth = 1) +
  # Add points at each wave
  geom_point(size = 3) +
  # Add error bars (95% CI = mean ± 1.96*SE)
  geom_errorbar(aes(ymin = mean_animals - 1.96*se_animals,
                    ymax = mean_animals + 1.96*se_animals),
                width = 0.3) +
  # Use our custom colors
  scale_color_manual(values = hearing_colors) +
  # Custom x-axis labels showing wave and years
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8),
                     labels = c("W7\n(0)", "W8\n(2)", "W9\n(4)",
                                "W10\n(6)", "W11\n(8)")) +
  # Labels
  labs(
    title = "Verbal Fluency (Animal Naming) by Hearing Status",
    x = "Wave (Years from baseline)",
    y = "Mean number of animals",
    color = "Hearing Acuity"
  ) +
  # Clean theme
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12)
  )

# Save the plot
ggsave("output/figures/trajectory_verbal_fluency.png", p_animals,
       width = 8, height = 6, dpi = 300)

# ---- PLOT 2: Delayed Recall ----

p_delayed <- ggplot(plot_data,
                    aes(x = time,
                        y = mean_delayed,
                        color = hearing_acuity,
                        group = hearing_acuity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_delayed - 1.96*se_delayed,
                    ymax = mean_delayed + 1.96*se_delayed),
                width = 0.3) +
  scale_color_manual(values = hearing_colors) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8),
                     labels = c("W7\n(0)", "W8\n(2)", "W9\n(4)",
                                "W10\n(6)", "W11\n(8)")) +
  labs(
    title = "Word-List Delayed Recall by Hearing Status",
    x = "Wave (Years from baseline)",
    y = "Mean delayed recall score",
    color = "Hearing Acuity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12)
  )

ggsave("output/figures/trajectory_delayed_recall.png", p_delayed,
       width = 8, height = 6, dpi = 300)

# ---- PLOT 3: Serial 7s ----

p_serial7 <- ggplot(plot_data,
                    aes(x = time,
                        y = mean_serial7,
                        color = hearing_acuity,
                        group = hearing_acuity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_serial7 - 1.96*se_serial7,
                    ymax = mean_serial7 + 1.96*se_serial7),
                width = 0.3) +
  scale_color_manual(values = hearing_colors) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8),
                     labels = c("W7\n(0)", "W8\n(2)", "W9\n(4)",
                                "W10\n(6)", "W11\n(8)")) +
  labs(
    title = "Serial 7s Performance by Hearing Status",
    x = "Wave (Years from baseline)",
    y = "Mean correct subtractions (0-5)",
    color = "Hearing Acuity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12)
  )

ggsave("output/figures/trajectory_serial7.png", p_serial7,
       width = 8, height = 6, dpi = 300)

cat("Trajectory plots saved to: output/figures/\n")

# -----------------------------------------------------------------------------
# STEP 7: Analyze attrition (dropout)
# -----------------------------------------------------------------------------

message("\n========== Attrition Analysis ==========\n")

# Attrition analysis is CRITICAL for longitudinal studies
# We need to check if dropout is related to our exposure (hearing)
# or outcome (cognition) - this could bias our results!

# Compare people who completed all waves vs those who dropped out
baseline_with_completion <- baseline %>%
  left_join(
    attrition_summary %>%
      select(idauniq, n_waves) %>%
      # Create a simple completer vs non-completer variable
      mutate(completer = ifelse(n_waves == 5, "All 5 waves", "< 5 waves")),
    by = "idauniq"
  )

# Create table comparing completers vs non-completers
attrition_table <- baseline_with_completion %>%
  select(
    completer,            # Grouping variable
    age, sex_label, hearing_acuity,  # Characteristics
    cf_animals, cf_delayed_recall    # Baseline cognition
  ) %>%
  tbl_summary(
    by = completer,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age ~ "Age (years)",
      sex_label ~ "Sex",
      hearing_acuity ~ "Hearing acuity",
      cf_animals ~ "Verbal fluency",
      cf_delayed_recall ~ "Delayed recall"
    )
  ) %>%
  add_p()

print(attrition_table)

# Save attrition table
attrition_table %>%
  as_gt() %>%
  gt::gtsave("output/tables/table_attrition.html")

cat("\nAttrition table saved to: output/tables/table_attrition.html\n")

# INTERPRETATION:
# If completers are younger, healthier, or have better cognition than
# non-completers, we have "informative dropout" which could bias results.
# Sensitivity analyses (like multiple imputation) may be needed.

# -----------------------------------------------------------------------------
# STEP 8: Summarize missing data
# -----------------------------------------------------------------------------

message("\n========== Missing Data Summary ==========\n")

# Check how much missing data we have for each outcome at each wave
missing_summary <- analytic_sample %>%
  group_by(wave) %>%
  summarise(
    n_total = n(),

    # Calculate % missing for each cognitive outcome
    # is.na() returns TRUE for missing, mean() gives proportion
    pct_missing_animals = round(100 * mean(is.na(cf_animals)), 1),
    pct_missing_delayed = round(100 * mean(is.na(cf_delayed_recall)), 1),
    pct_missing_imm = round(100 * mean(is.na(cf_imm_recall_total)), 1),
    pct_missing_serial7 = round(100 * mean(is.na(cf_serial7_total)), 1)
  )

cat("Percentage of missing data by wave:\n")
print(missing_summary)

write_csv(missing_summary, "output/tables/missing_data_summary.csv")

# INTERPRETATION:
# High missing data (>20%) might indicate problems
# Look for patterns - is missingness increasing over waves?
# Is it different for different outcomes?

message("\n========== Script 04 Complete ==========\n")
message("Next step: Run 05_longitudinal_analysis.R")
message("\nCheck output/tables/ and output/figures/ for results!")
