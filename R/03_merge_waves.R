# =============================================================================
# ELSA Hearing & Cognitive Trajectories Project
# Script 03: Merge Waves into Long Format
# =============================================================================
#
# PURPOSE:
# This script creates a longitudinal dataset by:
# 1. Extracting cognitive outcomes from each wave
# 2. Stacking them into "long format" (one row per person per wave)
# 3. Adding time variables for longitudinal modeling
# 4. Merging Wave 7 baseline variables (hearing, demographics)
# 5. Merging confounders (depression, comorbidities, wealth, education, etc.)
# 6. Creating the final analytic sample
#
# WHAT IS "LONG FORMAT"?
# Wide format: One row per person, columns for each wave (person1: score_w7, score_w8, ...)
# Long format: Multiple rows per person, one per wave (person1/w7, person1/w8, ...)
# Long format is required for mixed-effects models!
#
# INPUT FILES:
# - data/processed/elsa_waves_clean.rds
# - data/processed/hearing_data.rds
# - data/processed/covariates.rds
# - data/processed/hcap_clean.rds
# - data/processed/depression_data.rds
# - data/processed/comorbidities.rds
# - data/processed/wealth_data.rds
# - data/processed/education_data.rds
# - data/processed/nurse_clean.rds
# - data/processed/social_engagement.rds
#
# OUTPUT FILES:
# - data/processed/analysis_data_full.rds
# - data/processed/analytic_sample.rds
# - data/processed/analytic_sample.csv
# - data/processed/attrition_summary.rds
#
# =============================================================================

# -----------------------------------------------------------------------------
# STEP 1: Load required packages
# -----------------------------------------------------------------------------

library(tidyverse)

# -----------------------------------------------------------------------------
# STEP 2: Load cleaned data from previous script
# -----------------------------------------------------------------------------

elsa_clean <- readRDS("data/processed/elsa_waves_clean.rds")
hearing_data <- readRDS("data/processed/hearing_data.rds")
covariates <- readRDS("data/processed/covariates.rds")
hcap_clean <- readRDS("data/processed/hcap_clean.rds")

# NEW: Load additional confounder data
depression_data <- readRDS("data/processed/depression_data.rds")
comorbidities <- readRDS("data/processed/comorbidities.rds")
wealth_data <- readRDS("data/processed/wealth_data.rds")
education_data <- readRDS("data/processed/education_data.rds")
nurse_data <- readRDS("data/processed/nurse_clean.rds")
social_data <- readRDS("data/processed/social_engagement.rds")

# -----------------------------------------------------------------------------
# STEP 3: Define which cognitive outcomes we want to analyze
# -----------------------------------------------------------------------------

# These are the outcome variables we'll track across waves
cognitive_outcomes <- c(
  "cf_animals",          # Verbal fluency (animal naming)
  "cf_delayed_recall",   # Word-list delayed recall
  "cf_imm_recall_total", # Word-list immediate recall (sum)
  "cf_serial7_total",    # Serial 7s total correct
  "cf_memory_composite", # Combined memory score
  "cf_date_score"        # Orientation/date score
)

# -----------------------------------------------------------------------------
# STEP 4: Extract cognitive scores from each wave
# -----------------------------------------------------------------------------

message("\n========== Extracting Cognitive Scores by Wave ==========\n")

# Function to extract just the cognitive variables from a wave
# This standardizes what we pull from each wave
extract_cognitive <- function(df, wave_num) {
  df %>%
    # Select ID, wave, and any cognitive outcomes that exist
    select(idauniq, wave, any_of(cognitive_outcomes)) %>%
    # Make sure wave is set correctly
    mutate(wave = wave_num)
}

# Apply to each wave
cog_w7 <- extract_cognitive(elsa_clean$wave7, 7)
cog_w8 <- extract_cognitive(elsa_clean$wave8, 8)
cog_w9 <- extract_cognitive(elsa_clean$wave9, 9)
cog_w10 <- extract_cognitive(elsa_clean$wave10, 10)
cog_w11 <- extract_cognitive(elsa_clean$wave11, 11)

# Report which variables we found in each wave
cat("Cognitive variables available per wave:\n")
for (w in list(cog_w7, cog_w8, cog_w9, cog_w10, cog_w11)) {
  wave_num <- unique(w$wave)
  vars_present <- names(w)[names(w) %in% cognitive_outcomes]
  cat(sprintf("  Wave %d: %s\n", wave_num, paste(vars_present, collapse = ", ")))
}

# -----------------------------------------------------------------------------
# STEP 5: Stack all waves into one long dataset
# -----------------------------------------------------------------------------

message("\n========== Creating Long Format Dataset ==========\n")

# bind_rows() stacks dataframes on top of each other
# Result: one row per person per wave
cognitive_long <- bind_rows(cog_w7, cog_w8, cog_w9, cog_w10, cog_w11)

# -----------------------------------------------------------------------------
# STEP 6: Create time variables for longitudinal modeling
# -----------------------------------------------------------------------------

# ELSA waves are collected approximately every 2 years:
#   Wave 7: 2014-2015 (our baseline, time = 0)
#   Wave 8: 2016-2017 (time = 2 years)
#   Wave 9: 2018-2019 (time = 4 years)
#   Wave 10: 2020-2021 (time = 6 years)
#   Wave 11: 2022-2023 (time = 8 years)

cognitive_long <- cognitive_long %>%
  mutate(
    # TIME (continuous): Years since baseline (Wave 7)
    # Formula: (wave - 7) * 2 gives us 0, 2, 4, 6, 8
    time = (wave - 7) * 2,

    # TIME SQUARED: For quadratic/curved trajectories
    # If cognition declines faster over time, we need this term
    time_sq = time^2,

    # WAVE FACTOR: For dummy-coded (categorical) time models
    # This lets us estimate separate effects at each wave
    wave_factor = factor(wave, levels = c(7, 8, 9, 10, 11),
                         labels = c("Wave 7", "Wave 8", "Wave 9",
                                    "Wave 10", "Wave 11"))
  )

cat("Long format dataset created:\n")
cat(sprintf("  Total observations: %d\n", nrow(cognitive_long)))
cat(sprintf("  Unique participants: %d\n", n_distinct(cognitive_long$idauniq)))
cat("\nObservations per wave:\n")
print(table(cognitive_long$wave))

# -----------------------------------------------------------------------------
# STEP 7: Merge baseline variables (hearing status from Wave 7)
# -----------------------------------------------------------------------------

message("\n========== Merging Baseline Variables ==========\n")

# IMPORTANT: Hearing was only measured in Wave 7
# So we use Wave 7 hearing status as a TIME-INVARIANT exposure
# This means each person keeps the same hearing status across all waves

# Prepare baseline hearing data
baseline_hearing <- hearing_data %>%
  select(idauniq, hearing_acuity, hearing_impaired, hearing_selfreport, hehrab)

# Prepare baseline covariates
baseline_covariates <- covariates %>%
  select(idauniq, sex, sex_label, age, age_group, marital_status)

# Merge: Add hearing and covariate data to EVERY row (every wave)
# left_join keeps all rows from cognitive_long and adds matching data
analysis_data <- cognitive_long %>%
  left_join(baseline_hearing, by = "idauniq") %>%
  left_join(baseline_covariates, by = "idauniq") %>%
  mutate(
    # Calculate age at each wave (baseline age + years since baseline)
    age_at_wave = age + time,

    # Center age at 70 for easier interpretation
    # In models, the intercept will represent a 70-year-old
    age_centered = age - 70
  )

cat("Analysis dataset after basic merging:\n")
cat(sprintf("  Total observations: %d\n", nrow(analysis_data)))
cat(sprintf("  With hearing data: %d\n",
            sum(!is.na(analysis_data$hearing_acuity))))

# -----------------------------------------------------------------------------
# STEP 7b: Merge additional confounders
# -----------------------------------------------------------------------------

message("\n========== Merging Additional Confounders ==========\n")

# All confounders are TIME-INVARIANT (measured at Wave 7 baseline)
# They will be applied to all waves for each participant

# Depression (CES-D)
analysis_data <- analysis_data %>%
  left_join(depression_data %>%
              select(idauniq, cesd_total, depression_binary, depression_cat),
            by = "idauniq")

cat(sprintf("  With depression data: %d\n",
            sum(!is.na(analysis_data$cesd_total[analysis_data$wave == 7]))))

# Comorbidities
analysis_data <- analysis_data %>%
  left_join(comorbidities %>%
              select(idauniq, has_hypertension, has_diabetes, has_cvd,
                     has_stroke, comorbidity_count, self_rated_health),
            by = "idauniq")

cat(sprintf("  With comorbidity data: %d\n",
            sum(!is.na(analysis_data$comorbidity_count[analysis_data$wave == 7]))))

# Wealth
analysis_data <- analysis_data %>%
  left_join(wealth_data %>%
              select(idauniq, wealth_quintile, low_wealth),
            by = "idauniq")

cat(sprintf("  With wealth data: %d\n",
            sum(!is.na(analysis_data$wealth_quintile[analysis_data$wave == 7]))))

# Education (from IFS derived)
analysis_data <- analysis_data %>%
  left_join(education_data %>%
              select(idauniq, education_3cat, higher_education),
            by = "idauniq")

cat(sprintf("  With education data: %d\n",
            sum(!is.na(analysis_data$education_3cat[analysis_data$wave == 7]))))

# Nurse data (grip strength, blood pressure)
analysis_data <- analysis_data %>%
  left_join(nurse_data %>%
              select(idauniq, systolic_bp, diastolic_bp, measured_hypertension,
                     grip_max),
            by = "idauniq")

cat(sprintf("  With nurse data: %d\n",
            sum(!is.na(analysis_data$grip_max[analysis_data$wave == 7]))))

# Social engagement
analysis_data <- analysis_data %>%
  left_join(social_data %>%
              select(idauniq, org_memberships, social_engagement_cat),
            by = "idauniq")

cat(sprintf("  With social engagement data: %d\n",
            sum(!is.na(analysis_data$org_memberships[analysis_data$wave == 7]))))

cat("\nAnalysis dataset after all merges:\n")
cat(sprintf("  Total observations: %d\n", nrow(analysis_data)))
cat(sprintf("  Unique participants: %d\n", n_distinct(analysis_data$idauniq)))

# -----------------------------------------------------------------------------
# STEP 8: Merge HCAP data (optional subsample analysis)
# -----------------------------------------------------------------------------

message("\n========== Merging HCAP Data ==========\n")

# HCAP provides detailed cognitive factor scores for a SUBSAMPLE
# We merge these for potential sensitivity analyses

analysis_data <- analysis_data %>%
  left_join(hcap_clean, by = "idauniq")

# Create flag to identify HCAP participants
analysis_data <- analysis_data %>%
  mutate(hcap_sample = ifelse(!is.na(fgcp), 1, 0))

cat("HCAP subsample:\n")
cat(sprintf("  Participants with HCAP data: %d\n",
            n_distinct(analysis_data$idauniq[analysis_data$hcap_sample == 1])))

# -----------------------------------------------------------------------------
# STEP 9: Create the final analytic sample
# -----------------------------------------------------------------------------

message("\n========== Creating Analytic Sample ==========\n")

# Our analytic sample: Only participants with valid Wave 7 hearing test
# This ensures we have our exposure variable for everyone
analytic_sample <- analysis_data %>%
  filter(!is.na(hearing_acuity))

cat("Analytic sample (participants with valid hearing test at Wave 7):\n")
cat(sprintf("  Total observations: %d\n", nrow(analytic_sample)))
cat(sprintf("  Unique participants: %d\n", n_distinct(analytic_sample$idauniq)))

# Cross-tabulation: observations per wave by hearing group
cat("\nObservations per wave by hearing group:\n")
print(table(analytic_sample$wave, analytic_sample$hearing_acuity))

# -----------------------------------------------------------------------------
# STEP 10: Analyze attrition (dropout) patterns
# -----------------------------------------------------------------------------

message("\n========== Attrition Analysis ==========\n")

# Attrition = participants who don't complete all waves
# This is important because dropout might be related to cognition!

# Count how many waves each participant completed
obs_per_person <- analytic_sample %>%
  group_by(idauniq) %>%
  summarise(
    # How many waves did this person participate in?
    n_waves = n_distinct(wave),
    # Which specific waves were they in?
    waves_present = paste(sort(unique(wave)), collapse = ","),
    # What was their hearing status?
    hearing_group = first(hearing_acuity),  # first() gets the first value
    .groups = "drop"
  )

cat("Number of waves completed per participant:\n")
print(table(obs_per_person$n_waves))

cat("\nNumber of waves completed BY hearing group:\n")
cat("(Rows = number of waves, Columns = hearing group)\n")
print(table(obs_per_person$n_waves, obs_per_person$hearing_group))

# Calculate retention rates
# Retention = % of baseline participants still in study at each wave
retention <- analytic_sample %>%
  group_by(wave, hearing_acuity) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(hearing_acuity) %>%
  mutate(
    n_baseline = n[wave == 7],  # Get the Wave 7 count
    retention_pct = round(100 * n / n_baseline, 1)  # Calculate percentage
  )

cat("\nRetention rates by wave and hearing group:\n")
cat("(What % of Wave 7 participants are still in each subsequent wave?)\n\n")
print(retention %>% select(wave, hearing_acuity, n, retention_pct))

# -----------------------------------------------------------------------------
# STEP 11: Save the analysis datasets
# -----------------------------------------------------------------------------

message("\n========== Saving Analysis Datasets ==========\n")

# Save full dataset (including those without hearing data)
saveRDS(analysis_data, "data/processed/analysis_data_full.rds")

# Save analytic sample (our main analysis dataset)
saveRDS(analytic_sample, "data/processed/analytic_sample.rds")

# Save attrition summary
saveRDS(obs_per_person, "data/processed/attrition_summary.rds")

# Also save as CSV so you can open it in Excel if needed
write_csv(analytic_sample, "data/processed/analytic_sample.csv")

message("Analysis datasets saved to: data/processed/")

# -----------------------------------------------------------------------------
# STEP 12: Print final dataset summary
# -----------------------------------------------------------------------------

message("\n========== Final Dataset Summary ==========\n")

cat("Variables in the analytic sample:\n")
cat(paste(" ", names(analytic_sample), collapse = "\n"))

cat("\n\nCognitive outcome summaries (across all waves):\n")
analytic_sample %>%
  select(all_of(cognitive_outcomes[cognitive_outcomes %in% names(analytic_sample)])) %>%
  summary() %>%
  print()

cat("\nYour analysis dataset is ready!\n")
cat("Key variables:\n")
cat("  - idauniq: Participant ID\n")
cat("  - wave: Wave number (7-11)\n")
cat("  - time: Years from baseline (0, 2, 4, 6, 8)\n")
cat("  - time_sq: Time squared (for quadratic models)\n")
cat("  - wave_factor: Wave as categorical variable\n")
cat("  - hearing_acuity: 3-level hearing status (exposure)\n")
cat("  - cf_*: Cognitive outcomes\n")
cat("\nDemographic covariates:\n")
cat("  - age, age_centered, sex_label: Basic demographics\n")
cat("  - education_3cat: Education (3 categories)\n")
cat("  - wealth_quintile: Wealth quintile (Q1-Q5)\n")
cat("\nHealth confounders:\n")
cat("  - cesd_total, depression_cat: Depression (CES-D)\n")
cat("  - has_diabetes, has_cvd, has_hypertension: Comorbidities\n")
cat("  - comorbidity_count: Total number of conditions (0-8)\n")
cat("  - self_rated_health: Self-rated health (5 categories)\n")
cat("  - grip_max: Grip strength from nurse visit (kg)\n")
cat("  - systolic_bp, diastolic_bp: Blood pressure from nurse visit\n")
cat("\nSocial confounders:\n")
cat("  - org_memberships: Number of organization memberships\n")
cat("  - social_engagement_cat: Social engagement (3 categories)\n")

message("\n========== Script 03 Complete ==========\n")
message("Next step: Run 04_descriptives.R")
