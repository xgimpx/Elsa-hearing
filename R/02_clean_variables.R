# =============================================================================
# ELSA Hearing & Cognitive Trajectories Project
# Script 02: Clean Variables
# =============================================================================
#
# PURPOSE:
# This script cleans the raw ELSA data by:
# 1. Converting ELSA's missing value codes (-1, -8, -9) to proper NA values
# 2. Standardizing variable names across waves
# 3. Creating derived cognitive scores (totals, composites)
# 4. Creating labeled hearing acuity groups
# 5. Preparing demographic covariates
# 6. Creating depression score (CES-D 8-item)
# 7. Creating comorbidity indicators
# 8. Processing wealth quintiles and education
# 9. Processing nurse data (grip strength, blood pressure)
# 10. Creating social engagement score
#
# INPUT FILES:
# - data/processed/elsa_waves_raw.rds
# - data/processed/hcap_data_raw.rds
# - data/processed/financial_derived_raw.rds
# - data/processed/ifs_derived_raw.rds
# - data/processed/nurse_data_raw.rds
#
# OUTPUT FILES:
# - data/processed/elsa_waves_clean.rds
# - data/processed/baseline_wave7.rds
# - data/processed/hearing_data.rds
# - data/processed/covariates.rds
# - data/processed/hcap_clean.rds
# - data/processed/depression_data.rds
# - data/processed/comorbidities.rds
# - data/processed/wealth_data.rds
# - data/processed/nurse_clean.rds
# - data/processed/social_engagement.rds
#
# =============================================================================

# -----------------------------------------------------------------------------
# STEP 1: Load required packages
# -----------------------------------------------------------------------------

library(tidyverse)

# -----------------------------------------------------------------------------
# STEP 2: Load the raw data we saved in script 01
# -----------------------------------------------------------------------------

# readRDS() loads R objects that were saved with saveRDS()
elsa_waves <- readRDS("data/processed/elsa_waves_raw.rds")
hcap_data <- readRDS("data/processed/hcap_data_raw.rds")
financial_data <- readRDS("data/processed/financial_derived_raw.rds")
ifs_data <- readRDS("data/processed/ifs_derived_raw.rds")
nurse_data <- readRDS("data/processed/nurse_data_raw.rds")

# -----------------------------------------------------------------------------
# STEP 3: Create function to handle ELSA missing value codes
# -----------------------------------------------------------------------------

# ELSA uses NEGATIVE numbers to indicate missing data:
#   -1 = Not applicable (e.g., question was skipped)
#   -2 = Schedule not applicable
#   -8 = Don't know (participant couldn't answer)
#   -9 = Refused (participant wouldn't answer)
#
# We need to convert these to R's NA (Not Available) so R handles them correctly

recode_missing <- function(x) {
  # Only process numeric columns
  if (is.numeric(x)) {
    # Replace any negative value with NA
    x[x < 0] <- NA
  }
  return(x)
}

# -----------------------------------------------------------------------------
# STEP 4: Create function to standardize column names
# -----------------------------------------------------------------------------

# Problem: Wave 7 might have "CfAni" but Wave 8 has "cfani"
# Solution: Convert everything to lowercase for consistency

standardize_names <- function(df) {
  names(df) <- tolower(names(df))
  return(df)
}

# -----------------------------------------------------------------------------
# STEP 5: Create function to clean one wave of data
# -----------------------------------------------------------------------------

clean_wave <- function(df, wave_num) {

  message(paste("Cleaning Wave", wave_num))

  # Step 5a: Make all column names lowercase
  df <- standardize_names(df)

  # Step 5b: Convert all missing value codes to NA
  # across() applies a function to multiple columns
  # where(is.numeric) selects only numeric columns
  df <- df %>%
    mutate(across(where(is.numeric), recode_missing))

  # Step 5c: Rename cognitive variables to clearer names
  # rename_with() renames columns based on a function
  # We use case_when() to map old names to new names
  df <- df %>%
    rename_with(~ case_when(
      # Verbal fluency
      . == "cfani" ~ "cf_animals",

      # Word list encoding
      . == "cflisen" ~ "cf_list_encoding",

      # Immediate recall (5 positions from the word list)
      . == "cflisp1" ~ "cf_imm_recall_1",
      . == "cflisp2" ~ "cf_imm_recall_2",
      . == "cflisp3" ~ "cf_imm_recall_3",
      . == "cflisp4" ~ "cf_imm_recall_4",
      . == "cflisp5" ~ "cf_imm_recall_5",

      # Delayed recall
      . == "cflisd" ~ "cf_delayed_recall",

      # Serial 7s (5 subtractions: 100-7=93, 93-7=86, etc.)
      . == "cfsva" ~ "cf_serial7_1",
      . == "cfsvb" ~ "cf_serial7_2",
      . == "cfsvc" ~ "cf_serial7_3",
      . == "cfsvd" ~ "cf_serial7_4",
      . == "cfsve" ~ "cf_serial7_5",
      . == "cfsvch" ~ "cf_serial7_check",

      # Other cognitive scores
      . == "cfdscr" ~ "cf_date_score",
      . == "cfmscr" ~ "cf_memory_score",

      # Keep other names unchanged
      TRUE ~ .
    ), .cols = everything())

  # Step 5d: Create derived cognitive scores
  df <- df %>%
    mutate(
      # Serial 7s total: Sum of correct subtractions (0-5)
      # We sum the 5 individual serial 7 items
      # na.rm = FALSE means if ANY item is missing, the total is also missing
      cf_serial7_total = rowSums(
        select(., starts_with("cf_serial7_")) %>%
          select(-any_of("cf_serial7_check")),  # Exclude the check variable
        na.rm = FALSE
      ),

      # Immediate recall total: Sum of all 5 word positions (0-10 typically)
      cf_imm_recall_total = rowSums(
        select(., starts_with("cf_imm_recall_")),
        na.rm = FALSE
      ),

      # Composite memory score: Immediate + Delayed recall combined
      # This gives an overall memory performance measure
      cf_memory_composite = cf_imm_recall_total + cf_delayed_recall
    )

  return(df)
}

# -----------------------------------------------------------------------------
# STEP 6: Clean all ELSA waves
# -----------------------------------------------------------------------------

message("\n========== Cleaning ELSA Wave Data ==========\n")

# Apply our cleaning function to each wave
elsa_clean <- list(
  wave7 = clean_wave(elsa_waves$wave7, 7),
  wave8 = clean_wave(elsa_waves$wave8, 8),
  wave9 = clean_wave(elsa_waves$wave9, 9),
  wave10 = clean_wave(elsa_waves$wave10, 10),
  wave11 = clean_wave(elsa_waves$wave11, 11)
)

# -----------------------------------------------------------------------------
# STEP 7: Create hearing acuity variable from Wave 7
# -----------------------------------------------------------------------------

message("\n========== Processing Hearing Variables ==========\n")

# The hearing test was only done in Wave 7, so we extract it separately
# HeHrab is the main hearing acuity variable from the HearCheck screener

wave7_hearing <- elsa_clean$wave7 %>%
  # Select only hearing-related columns
  # starts_with() selects columns whose names start with a string
  select(idauniq, starts_with("hehrab"), starts_with("hehra"),
         starts_with("hehear"), starts_with("hehaid"),
         starts_with("heart")) %>%
  mutate(
    # Create a labeled factor variable for hearing acuity
    # factor() converts numbers to categories with labels
    hearing_acuity = factor(
      hehrab,
      levels = c(1, 2, 3),
      labels = c("Good (6 tones)",
                 "Mild difficulty (3-5 tones)",
                 "Moderate-severe (0-2 tones)")
    ),

    # Create a simple binary variable: impaired vs not impaired
    # case_when() is like if-else but cleaner for multiple conditions
    hearing_impaired = case_when(
      hehrab == 1 ~ 0,           # Good hearing = NOT impaired (code as 0)
      hehrab %in% c(2, 3) ~ 1,   # Mild or moderate-severe = impaired (code as 1)
      TRUE ~ NA_real_            # Everything else is missing
    ),

    # Also create a labeled version of self-reported hearing
    # HeHear is on a 1-5 scale (Excellent to Poor)
    hearing_selfreport = factor(
      hehear,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor")
    )
  )

# Print distributions to check our coding
cat("Hearing Acuity (HeHrab) distribution:\n")
cat("This is our PRIMARY exposure variable\n\n")
print(table(wave7_hearing$hearing_acuity, useNA = "ifany"))

cat("\n\nHearing Impairment (binary) distribution:\n")
cat("0 = Good hearing, 1 = Any impairment\n\n")
print(table(wave7_hearing$hearing_impaired, useNA = "ifany"))

cat("\n\nSelf-reported Hearing distribution:\n")
print(table(wave7_hearing$hearing_selfreport, useNA = "ifany"))

# -----------------------------------------------------------------------------
# STEP 8: Clean covariate variables from Wave 7
# -----------------------------------------------------------------------------

message("\n========== Processing Covariate Variables ==========\n")

# Covariates are variables we'll control for in our analysis
# We extract them from Wave 7 (our baseline)

wave7_covariates <- elsa_clean$wave7 %>%
  # Select potential covariate columns
  # any_of() selects columns if they exist, doesn't error if they don't
  select(idauniq, any_of(c("dhsex", "disex", "indsex",
                            "heage", "indager",
                            "dimar", "hedimar",
                            "edqual", "w7edqual", "fqend"))) %>%
  mutate(
    # Sex: Different waves use different variable names, so we try multiple
    # coalesce() returns the first non-NA value
    sex = coalesce(dhsex, disex, indsex),

    # Create labeled sex variable
    sex_label = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),

    # Age: Again, try multiple variable names
    age = coalesce(heage, indager),

    # Create age groups for descriptive statistics
    # cut() divides a continuous variable into categories
    age_group = cut(age,
                    breaks = c(0, 59, 69, 79, Inf),  # Inf = infinity
                    labels = c("<60", "60-69", "70-79", "80+"),
                    right = FALSE),  # right=FALSE means [60,70) not (60,70]

    # Marital status
    marital_status = coalesce(dimar, hedimar)
  ) %>%
  # Keep only the cleaned variables
  select(idauniq, sex, sex_label, age, age_group, marital_status,
         any_of(c("edqual", "w7edqual", "fqend")))

# Print distributions
cat("Sex distribution:\n")
print(table(wave7_covariates$sex_label, useNA = "ifany"))

cat("\nAge distribution:\n")
print(summary(wave7_covariates$age))

cat("\nAge group distribution:\n")
print(table(wave7_covariates$age_group, useNA = "ifany"))

# -----------------------------------------------------------------------------
# STEP 9: Create combined Wave 7 baseline dataset
# -----------------------------------------------------------------------------

message("\n========== Creating Baseline Dataset ==========\n")

# Combine cognitive scores, hearing data, and covariates for Wave 7
# This is our "baseline" dataset from which we'll track trajectories

baseline_w7 <- elsa_clean$wave7 %>%
  # Select cognitive variables (but not the individual serial7 items)
  select(idauniq, wave,
         starts_with("cf_"),
         -any_of(c("cf_serial7_1", "cf_serial7_2", "cf_serial7_3",
                   "cf_serial7_4", "cf_serial7_5"))) %>%
  # Add hearing data using left_join
  # left_join keeps all rows from the first dataset and adds matching columns from the second
  left_join(wave7_hearing %>% select(idauniq, hearing_acuity,
                                      hearing_impaired, hearing_selfreport,
                                      hehrab, hehear),
            by = "idauniq") %>%
  # Add covariates
  left_join(wave7_covariates, by = "idauniq")

cat("Baseline Wave 7 dataset:\n")
cat(sprintf("  Total participants: %d\n", nrow(baseline_w7)))
cat(sprintf("  With hearing test: %d\n",
            sum(!is.na(baseline_w7$hearing_acuity))))

# -----------------------------------------------------------------------------
# STEP 10: Clean HCAP data
# -----------------------------------------------------------------------------

message("\n========== Processing HCAP Data ==========\n")

# HCAP provides factor scores for different cognitive domains
# These are derived from detailed cognitive testing using item response theory

hcap_clean <- hcap_data$scores_2018 %>%
  rename(idauniq = idauniq) %>%  # Ensure consistent naming
  # Recode missing values
  mutate(across(where(is.numeric), recode_missing)) %>%
  # Select the factor scores we need
  select(idauniq,
         # Each domain has a point estimate and a Bayesian estimate
         fgcp, fgcp_bayes3,     # General cognitive performance (g-factor)
         fmem, fmem_bayes3,     # Memory factor
         fexf, fexf_bayes3,     # Executive function factor
         flang, flang_bayes3,   # Language factor
         forient, forient_bayes3  # Orientation factor
  )

cat("HCAP harmonised scores:\n")
cat(sprintf("  Total participants: %d\n", nrow(hcap_clean)))
cat("\nFactor score summaries (higher = better cognition):\n")
print(summary(hcap_clean %>% select(fgcp, fmem, fexf, flang, forient)))

# -----------------------------------------------------------------------------
# STEP 11: Create Depression Score (CES-D 8-item)
# -----------------------------------------------------------------------------

message("\n========== Processing Depression (CES-D) ==========\n")

# CES-D 8-item scale measures depressive symptoms
# Items are: depressed, effort, sleep, happy (R), lonely, enjoyed (R), sad, going
# Response options: 1=Rarely (<1 day), 2=Some (1-2 days), 3=Occasionally (3-4 days), 4=Most (5-7 days)
# Two items are REVERSE coded: happy and enjoyed life

wave7_depression <- elsa_clean$wave7 %>%
  select(idauniq, any_of(c("psceda", "pscedb", "pscedc", "pscedd",
                           "pscede", "pscedf", "pscedg", "pscedh"))) %>%
  mutate(
    # Recode to 0-3 scale (standard CES-D scoring)
    # Original: 1-4, recode to 0-3
    cesd_depressed = psceda - 1,
    cesd_effort = pscedb - 1,
    cesd_sleep = pscedc - 1,
    cesd_happy = 3 - (pscedd - 1),      # REVERSE: high = less happy = more depressed
    cesd_lonely = pscede - 1,
    cesd_enjoyed = 3 - (pscedf - 1),    # REVERSE: high = less enjoyed = more depressed
    cesd_sad = pscedg - 1,
    cesd_going = pscedh - 1,

    # Total CES-D score (0-24 range)
    # Higher scores = more depressive symptoms
    cesd_total = cesd_depressed + cesd_effort + cesd_sleep + cesd_happy +
                 cesd_lonely + cesd_enjoyed + cesd_sad + cesd_going,

    # Binary depression indicator (score >= 4 is common cutoff for 8-item CES-D)
    depression_binary = ifelse(cesd_total >= 4, 1, 0),

    # Categorical depression
    depression_cat = case_when(
      cesd_total < 4 ~ "None/minimal",
      cesd_total < 8 ~ "Mild",
      cesd_total >= 8 ~ "Moderate/severe",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("None/minimal", "Mild", "Moderate/severe"))
  ) %>%
  select(idauniq, cesd_total, depression_binary, depression_cat)

cat("CES-D depression scores:\n")
print(summary(wave7_depression$cesd_total))
cat("\nDepression categories:\n")
print(table(wave7_depression$depression_cat, useNA = "ifany"))

# -----------------------------------------------------------------------------
# STEP 12: Create Comorbidity Indicators
# -----------------------------------------------------------------------------

message("\n========== Processing Comorbidities ==========\n")

# Create binary indicators for key health conditions
# Response coding: 1 = Yes, 2 = No (need to recode to 0/1)

wave7_comorbidities <- elsa_clean$wave7 %>%
  select(idauniq, any_of(c("hediabp", "hediaan", "hediami", "hediahf",
                           "hediaar", "hediadi", "hediast", "hediach",
                           "hehelf"))) %>%
  mutate(
    # Recode each condition: 1 = has condition, 0 = does not
    has_hypertension = ifelse(hediabp == 1, 1, ifelse(hediabp == 2, 0, NA)),
    has_angina = ifelse(hediaan == 1, 1, ifelse(hediaan == 2, 0, NA)),
    has_heart_attack = ifelse(hediami == 1, 1, ifelse(hediami == 2, 0, NA)),
    has_heart_failure = ifelse(hediahf == 1, 1, ifelse(hediahf == 2, 0, NA)),
    has_arrhythmia = ifelse(hediaar == 1, 1, ifelse(hediaar == 2, 0, NA)),
    has_diabetes = ifelse(hediadi == 1, 1, ifelse(hediadi == 2, 0, NA)),
    has_stroke = ifelse(hediast == 1, 1, ifelse(hediast == 2, 0, NA)),
    has_high_cholesterol = ifelse(hediach == 1, 1, ifelse(hediach == 2, 0, NA)),

    # Composite cardiovascular disease indicator
    # ANY of: angina, heart attack, heart failure, stroke
    has_cvd = case_when(
      has_angina == 1 | has_heart_attack == 1 | has_heart_failure == 1 | has_stroke == 1 ~ 1,
      is.na(has_angina) & is.na(has_heart_attack) & is.na(has_heart_failure) & is.na(has_stroke) ~ NA_real_,
      TRUE ~ 0
    ),

    # Self-rated health (1=excellent to 5=poor)
    self_rated_health = factor(hehelf,
                               levels = 1:5,
                               labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))
  )

# Calculate comorbidity count separately (rowSums with across works better)
wave7_comorbidities <- wave7_comorbidities %>%
  mutate(
    comorbidity_count = rowSums(
      across(c(has_hypertension, has_angina, has_heart_attack, has_heart_failure,
               has_arrhythmia, has_diabetes, has_stroke, has_high_cholesterol)),
      na.rm = TRUE
    )
  ) %>%
  select(idauniq, has_hypertension, has_diabetes, has_cvd, has_stroke,
         comorbidity_count, self_rated_health)

cat("Comorbidity summary:\n")
cat(sprintf("  Hypertension: %.1f%%\n", 100 * mean(wave7_comorbidities$has_hypertension, na.rm = TRUE)))
cat(sprintf("  Diabetes: %.1f%%\n", 100 * mean(wave7_comorbidities$has_diabetes, na.rm = TRUE)))
cat(sprintf("  CVD (any): %.1f%%\n", 100 * mean(wave7_comorbidities$has_cvd, na.rm = TRUE)))
cat(sprintf("  Stroke: %.1f%%\n", 100 * mean(wave7_comorbidities$has_stroke, na.rm = TRUE)))
cat("\nComorbidity count distribution:\n")
print(table(wave7_comorbidities$comorbidity_count, useNA = "ifany"))

# -----------------------------------------------------------------------------
# STEP 13: Process Wealth Quintiles
# -----------------------------------------------------------------------------

message("\n========== Processing Wealth Data ==========\n")

# Wealth quintiles from financial derived variables
# totwq5_bu_s = total wealth quintile (benefit unit level)
# 1 = lowest wealth, 5 = highest wealth

wave7_wealth <- financial_data %>%
  filter(wave == 7) %>%
  mutate(across(where(is.numeric), recode_missing)) %>%
  mutate(
    # Create labeled wealth quintile
    wealth_quintile = factor(totwq5_bu_s,
                             levels = 1:5,
                             labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4", "Q5 (Highest)")),

    # Binary: low wealth (Q1-Q2) vs higher wealth (Q3-Q5)
    low_wealth = ifelse(totwq5_bu_s %in% c(1, 2), 1,
                        ifelse(totwq5_bu_s %in% c(3, 4, 5), 0, NA))
  ) %>%
  select(idauniq, nettotw_bu_s, totwq5_bu_s, wealth_quintile, low_wealth)

cat("Wealth quintile distribution:\n")
print(table(wave7_wealth$wealth_quintile, useNA = "ifany"))

# -----------------------------------------------------------------------------
# STEP 14: Process Education from IFS Derived
# -----------------------------------------------------------------------------

message("\n========== Processing Education ==========\n")

# Education from IFS derived variables (harmonized across waves)
# edqual: 1=Degree+, 2=Below degree, 3=A-level, 4=O-level, 5=CSE, 6=Foreign/other, 7=None
# qual3: 3-category version

education_data <- ifs_data %>%
  mutate(across(where(is.numeric), recode_missing)) %>%
  mutate(
    # 3-category education (common in ELSA analyses)
    education_3cat = case_when(
      edqual %in% c(1, 2) ~ "Higher (degree/equiv)",
      edqual %in% c(3, 4) ~ "Intermediate (A/O level)",
      edqual %in% c(5, 6, 7) ~ "Low (CSE/none)",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Low (CSE/none)", "Intermediate (A/O level)", "Higher (degree/equiv)")),

    # Binary: higher education vs not
    higher_education = ifelse(edqual %in% c(1, 2), 1,
                              ifelse(edqual %in% c(3, 4, 5, 6, 7), 0, NA))
  ) %>%
  select(idauniq, edqual, education_3cat, higher_education)

cat("Education distribution (3 categories):\n")
print(table(education_data$education_3cat, useNA = "ifany"))

# -----------------------------------------------------------------------------
# STEP 15: Process Nurse Data (Grip Strength, Blood Pressure)
# -----------------------------------------------------------------------------

message("\n========== Processing Nurse Data ==========\n")

# Grip strength is a marker of frailty and physical function
# Blood pressure relates to vascular health (important for cognition)

nurse_clean <- nurse_data %>%
  mutate(across(where(is.numeric), recode_missing)) %>%
  mutate(
    # Average systolic BP (from 2nd and 3rd readings - first is often unreliable)
    systolic_bp = rowMeans(select(., sys2, sys3), na.rm = TRUE),

    # Average diastolic BP
    diastolic_bp = rowMeans(select(., dias2, dias3), na.rm = TRUE),

    # Hypertension from measured BP (systolic >= 140 or diastolic >= 90)
    measured_hypertension = case_when(
      systolic_bp >= 140 | diastolic_bp >= 90 ~ 1,
      is.na(systolic_bp) & is.na(diastolic_bp) ~ NA_real_,
      TRUE ~ 0
    ),

    # Maximum grip strength (dominant hand)
    # Take the max of the 3 trials
    grip_dominant = pmax(mmgsd1, mmgsd2, mmgsd3, na.rm = TRUE),

    # Maximum grip strength (non-dominant hand)
    grip_nondominant = pmax(mmgsn1, mmgsn2, mmgsn3, na.rm = TRUE),

    # Overall max grip strength
    grip_max = pmax(grip_dominant, grip_nondominant, na.rm = TRUE)
  ) %>%
  select(idauniq, systolic_bp, diastolic_bp, measured_hypertension,
         grip_dominant, grip_nondominant, grip_max)

cat("Blood pressure summary:\n")
cat(sprintf("  Mean systolic BP: %.1f mmHg\n", mean(nurse_clean$systolic_bp, na.rm = TRUE)))
cat(sprintf("  Mean diastolic BP: %.1f mmHg\n", mean(nurse_clean$diastolic_bp, na.rm = TRUE)))
cat(sprintf("  Measured hypertension: %.1f%%\n", 100 * mean(nurse_clean$measured_hypertension, na.rm = TRUE)))
cat("\nGrip strength summary (kg):\n")
print(summary(nurse_clean$grip_max))

# -----------------------------------------------------------------------------
# STEP 16: Process Social Engagement
# -----------------------------------------------------------------------------

message("\n========== Processing Social Engagement ==========\n")

# Social engagement: organizational membership and social contact
# scorgn = number of organizations (0-8)
# scchd, scfrd, scfam = contact frequency (lower = more frequent)

wave7_social <- elsa_clean$wave7 %>%
  select(idauniq, any_of(c("scorgn", "scorg01", "scorg02", "scorg03", "scorg04",
                           "scorg05", "scorg06", "scorg07", "scorg08"))) %>%
  mutate(
    # Number of organizational memberships
    org_memberships = scorgn,

    # Binary: any organizational membership
    any_org_membership = ifelse(scorgn > 0, 1, ifelse(scorgn == 0, 0, NA)),

    # Social engagement category
    social_engagement_cat = case_when(
      scorgn == 0 ~ "None",
      scorgn %in% c(1, 2) ~ "Low (1-2)",
      scorgn >= 3 ~ "High (3+)",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("None", "Low (1-2)", "High (3+)"))
  ) %>%
  select(idauniq, org_memberships, any_org_membership, social_engagement_cat)

cat("Social engagement distribution:\n")
print(table(wave7_social$social_engagement_cat, useNA = "ifany"))

# -----------------------------------------------------------------------------
# STEP 17: Save all cleaned data
# -----------------------------------------------------------------------------

message("\n========== Saving Cleaned Data ==========\n")

# Save each dataset separately for flexibility
saveRDS(elsa_clean, "data/processed/elsa_waves_clean.rds")
saveRDS(baseline_w7, "data/processed/baseline_wave7.rds")
saveRDS(wave7_hearing, "data/processed/hearing_data.rds")
saveRDS(wave7_covariates, "data/processed/covariates.rds")
saveRDS(hcap_clean, "data/processed/hcap_clean.rds")

# NEW: Save additional confounder data
saveRDS(wave7_depression, "data/processed/depression_data.rds")
saveRDS(wave7_comorbidities, "data/processed/comorbidities.rds")
saveRDS(wave7_wealth, "data/processed/wealth_data.rds")
saveRDS(education_data, "data/processed/education_data.rds")
saveRDS(nurse_clean, "data/processed/nurse_clean.rds")
saveRDS(wave7_social, "data/processed/social_engagement.rds")

message("Cleaned data saved to: data/processed/")

message("\n========== Script 02 Complete ==========\n")
message("Next step: Run 03_merge_waves.R")
