# =============================================================================
# ELSA Hearing & Cognitive Trajectories Project
# Script 01: Load Data
# =============================================================================
#
# PURPOSE:
# This script loads the raw ELSA data files from the UK Data Archive.
# We load waves 7-11 of the main ELSA study and supplementary data.
#
# INPUT FILES:
# - UKDA-5050-tab/tab/wave_X_elsa_data*.tab (main ELSA interview data)
# - UKDA-5050-tab/tab/wave_X_financial_derived_variables.tab (wealth/income)
# - UKDA-5050-tab/tab/wave_X_ifs_derived_variables.tab (harmonized variables)
# - UKDA-5050-tab/tab/wave_8_elsa_nurse_data_eul_v1.tab (objective health)
# - UKDA-8502-tab/tab/hcap_*.tab (HCAP cognitive assessment data)
#
# OUTPUT FILES:
# - data/processed/elsa_waves_raw.rds (list of all wave data)
# - data/processed/hcap_data_raw.rds (HCAP data)
# - data/processed/financial_derived_raw.rds (wealth/income data)
# - data/processed/ifs_derived_raw.rds (harmonized education etc.)
# - data/processed/nurse_data_raw.rds (objective health measures)
#
# =============================================================================

# -----------------------------------------------------------------------------
# STEP 1: Load required packages
# -----------------------------------------------------------------------------

# tidyverse: A collection of R packages for data manipulation and visualization
#   - includes dplyr (data manipulation), ggplot2 (plotting), tidyr (reshaping)
library(tidyverse)

# data.table: Fast data reading and manipulation
#   - fread() is much faster than read.csv() for large files
library(data.table)

# -----------------------------------------------------------------------------
# STEP 2: Define file paths
# -----------------------------------------------------------------------------

# These paths are RELATIVE to the project folder (where the .Rproj file is)
# The ELSA data is stored in tab-delimited (.tab) format

# Path to the main ELSA interview data (waves 1-11)
elsa_path <- "UKDA-5050-tab/tab/"

# Path to the HCAP (Healthy Cognitive Ageing Project) data
hcap_path <- "UKDA-8502-tab/tab/"

# Where we'll save our processed data
output_path <- "data/processed/"

# -----------------------------------------------------------------------------
# STEP 3: Define which variables we want to extract
# -----------------------------------------------------------------------------

# We don't need ALL variables from these huge datasets (6000+ columns!)
# So we specify which ones to keep to save memory and processing time

# Participant identifier - this is the key to link across waves
id_vars <- c("idauniq")

# Hearing variables (only measured in Wave 7)
# HeHrab is the key variable: hearing acuity classification (1=good, 2=mild, 3=severe)
hearing_vars <- c(
  "HeHrab",   # PRIMARY: Hearing acuity from HearCheck screener
              #   1 = Good (heard all 6 tones)
              #   2 = Mild difficulty (heard 3-5 tones)
              #   3 = Moderate-severe (heard 0-2 tones)
  "HeHra",    # Whether the hearing test was conducted
  "HeHear",   # Self-reported hearing difficulty (1-5 scale)
  "HeHAid",   # Whether participant uses a hearing aid
  "heartoa",  # Hearing test outcome - alternative coding
  "heartra",  # Hearing test range
  "heartot"   # Hearing test total
)

# Cognitive function variables
# NOTE: Variable names have different cases across waves (CfAni vs cfani)
#       So we include both versions to catch them all
cognitive_vars <- c(
  # Verbal fluency - how many animals can you name in 1 minute?
  "CfAni", "cfani",

  # Word list memory test
  "CfWrds", "cfwrds",         # Which word list version was used
  "CfLisEn", "cflisen",       # List encoding (was the list presented?)
  "CfLisP1", "cflisp1",       # Immediate recall - word from position 1

"CfLisP2", "cflisp2",       # Immediate recall - word from position 2
  "CfLisP3", "cflisp3",       # Immediate recall - word from position 3
  "CfLisP4", "cflisp4",       # Immediate recall - word from position 4
  "CfLisP5", "cflisp5",       # Immediate recall - word from position 5
  "CfLisD", "cflisd",         # Delayed recall (asked again later)

  # Serial 7s - count backwards from 100 by 7s (100, 93, 86, 79, 72)
  "CfSvA", "cfsva",           # First subtraction correct? (93)
  "CfSvB", "cfsvb",           # Second subtraction correct? (86)
  "CfSvC", "cfsvc",           # Third subtraction correct? (79)
  "CfSvD", "cfsvd",           # Fourth subtraction correct? (72)
  "CfSvE", "cfsve",           # Fifth subtraction correct? (65)
  "CfSvCh", "cfsvch",         # Total serial 7s score

  # Other cognitive measures
  "CfDScr", "cfdscr",         # Date/orientation score
  "CfMScr", "cfmscr"          # Overall memory score
)

# Demographic and covariate variables
covariate_vars <- c(
  "DhSex", "DiSex",           # Sex (1=Male, 2=Female)
  "HeAge", "indager",         # Age in years
  "DiMar", "hedimar",         # Marital status
  "indsex",                   # Sex (alternative variable name)
  "edqual",                   # Highest educational qualification
  "w7edqual",                 # Education at wave 7
  "fqend",                    # Age finished full-time education
  "w7lwgt",                   # Longitudinal weight
  "w7xwgt"                    # Cross-sectional weight
)

# Depression variables (CES-D 8-item scale)
# Higher scores = more depressive symptoms
depression_vars <- c(
  "PScedA", "psceda",         # Felt depressed

  "PScedB", "pscedb",         # Everything was an effort
  "PScedC", "pscedc",         # Sleep was restless
  "PScedD", "pscedd",         # Was happy (reverse coded)
  "PScedE", "pscede",         # Felt lonely
  "PScedF", "pscedf",         # Enjoyed life (reverse coded)
  "PScedG", "pscedg",         # Felt sad
  "PScedH", "pscedh"          # Could not get going
)

# Health condition variables (comorbidities)
health_vars <- c(
  # Cardiovascular conditions
  "Hediabp", "hediabp",       # High blood pressure diagnosed
  "Hediaan", "hediaan",       # Angina diagnosed
  "Hediami", "hediami",       # Heart attack (MI) diagnosed
  "Hediahf", "hediahf",       # Heart failure diagnosed
  "Hediahm", "hediahm",       # Heart murmur diagnosed
  "Hediaar", "hediaar",       # Abnormal heart rhythm diagnosed
  "Hediadi", "hediadi",       # Diabetes diagnosed
  "Hediast", "hediast",       # Stroke diagnosed
  "Hediach", "hediach",       # High cholesterol diagnosed
  # Heart problems summary
  "Hehrta", "hehrta",         # Heart problems - any
  "HEHRT", "hehrt",           # Heart disease summary
  # General health
  "Hehelf", "hehelf"          # Self-rated health (1=excellent to 5=poor)
)

# Social engagement variables
social_vars <- c(
  # Organization membership (civic participation)
  "scorg01",                  # Political party/trade union/environmental group
  "scorg02",                  # Tenants/residents group
  "scorg03",                  # Church/religious group
  "scorg04",                  # Charitable association
  "scorg05",                  # Education/arts/music group
  "scorg06",                  # Social club
  "scorg07",                  # Sports club/gym
  "scorg08",                  # Other group
  "scorgn",                   # Number of organizations
  # Social contact
  "scchd",                    # How often meet with children
  "scchda", "scchdb", "scchdc", # Contact frequency items
  "scfrd",                    # How often meet with friends
  "scfam"                     # How often meet with family
)

# Combine all variable names into one list
# unique() removes duplicates
all_vars <- unique(c(id_vars, hearing_vars, cognitive_vars, covariate_vars,
                     depression_vars, health_vars, social_vars))

# -----------------------------------------------------------------------------
# STEP 4: Create a function to load one wave of ELSA data
# -----------------------------------------------------------------------------

# This function:
# 1. Reads the tab-delimited file
# 2. Selects only the variables we need
# 3. Adds a wave number column
# 4. Returns the data as a tibble (tidyverse data frame)

load_elsa_wave <- function(file_path, wave_num, vars_to_select) {

  # Print a message so we know what's happening
  message(paste("Loading Wave", wave_num, "from:", file_path))

  # Read the tab-delimited file using fread() for speed
  # sep = "\t" means tab-separated
  # header = TRUE means first row contains column names
  # na.strings specifies which values should be treated as missing
  df <- fread(file_path, sep = "\t", header = TRUE,
              na.strings = c("", "NA"),
              stringsAsFactors = FALSE)

  # PROBLEM: Variable names might have different cases (CfAni vs cfani)
  # SOLUTION: Convert everything to lowercase for matching
  names_lower <- tolower(names(df))        # Column names in data
  vars_lower <- tolower(vars_to_select)    # Our requested variables

  # Find which of our requested variables actually exist in this dataset
  existing_vars <- names(df)[names_lower %in% vars_lower]

  # Always make sure we have the participant ID
  if (!"idauniq" %in% existing_vars) {
    existing_vars <- c("idauniq", existing_vars)
  }

  # Select only the columns we need
  # The ..existing_vars syntax is data.table's way of using a variable as column names
  df_selected <- df[, ..existing_vars]

  # Add a column to identify which wave this data comes from
  df_selected$wave <- wave_num

  # Print summary
  message(paste("  Loaded", nrow(df_selected), "rows,",
                ncol(df_selected), "columns"))

  # Convert to tibble (tidyverse format) and return
  return(as_tibble(df_selected))
}

# -----------------------------------------------------------------------------
# STEP 5: Load all ELSA waves (7-11)
# -----------------------------------------------------------------------------

message("\n========== Loading ELSA Main Interview Data ==========\n")

# Wave 7 (2014-2015) - BASELINE for our analysis (first wave with hearing test)
wave7 <- load_elsa_wave(
  paste0(elsa_path, "wave_7_elsa_data.tab"),  # paste0 combines text strings
  wave_num = 7,
  vars_to_select = all_vars
)

# Wave 8 (2016-2017)
wave8 <- load_elsa_wave(
  paste0(elsa_path, "wave_8_elsa_data_eul_v2.tab"),
  wave_num = 8,
  vars_to_select = all_vars
)

# Wave 9 (2018-2019)
wave9 <- load_elsa_wave(
  paste0(elsa_path, "wave_9_elsa_data_eul_v2.tab"),
  wave_num = 9,
  vars_to_select = all_vars
)

# Wave 10 (2020-2021) - During COVID pandemic
wave10 <- load_elsa_wave(
  paste0(elsa_path, "wave_10_elsa_data_eul_v4.tab"),
  wave_num = 10,
  vars_to_select = all_vars
)

# Wave 11 (2022-2023)
wave11 <- load_elsa_wave(
  paste0(elsa_path, "wave_11_elsa_data_eul_v1.tab"),
  wave_num = 11,
  vars_to_select = all_vars
)

# -----------------------------------------------------------------------------
# STEP 6: Load HCAP data (2018 and 2023)
# -----------------------------------------------------------------------------

message("\n========== Loading HCAP Data ==========\n")

# HCAP = Harmonised Cognitive Assessment Protocol
# This is a detailed cognitive assessment done on a SUBSAMPLE of ELSA participants
# It provides more detailed cognitive scores than the standard ELSA measures

# HCAP 2018 respondent archive - contains raw test responses
hcap_respondent_2018 <- fread(
  paste0(hcap_path, "hcap_2018_eul_respondent_archive.tab"),
  sep = "\t", header = TRUE
) %>% as_tibble()

message(paste("HCAP 2018 respondent:", nrow(hcap_respondent_2018), "rows,",
              ncol(hcap_respondent_2018), "columns"))

# HCAP 2018 harmonised scores - contains derived cognitive factor scores
hcap_scores_2018 <- fread(
  paste0(hcap_path, "hcap_2018_harmonised_scores.tab"),
  sep = "\t", header = TRUE
) %>% as_tibble()

message(paste("HCAP 2018 harmonised scores:", nrow(hcap_scores_2018), "rows,",
              ncol(hcap_scores_2018), "columns"))

# HCAP 2023 respondent archive - longitudinal follow-up
hcap_respondent_2023 <- fread(
  paste0(hcap_path, "hcap2_2023_eul_respondent_archive_v2.tab"),
  sep = "\t", header = TRUE
) %>% as_tibble()

message(paste("HCAP 2023 respondent:", nrow(hcap_respondent_2023), "rows,",
              ncol(hcap_respondent_2023), "columns"))

# IMPORTANT: HCAP uses "id_elsa" but main ELSA uses "idauniq"
# We need to rename so we can merge the datasets later
hcap_scores_2018 <- hcap_scores_2018 %>%
  rename(idauniq = id_elsa)

# -----------------------------------------------------------------------------
# STEP 7: Load Financial Derived Variables (Wealth & Income)
# -----------------------------------------------------------------------------

message("\n========== Loading Financial Derived Variables ==========\n")

# IMPORTANT: These are IMPUTED financial data
# Use variables with "_i" suffix for analysis (imputed values)
# Use "_s" suffix for summary variables like total wealth

financial_vars <- c(
  "idauniq",
  "nettotw_bu_s",    # Net total wealth (benefit unit) - SUMMARY
  "totwq5_bu_s",     # Total wealth quintiles (5 groups)
  "totwq10_bu_s"     # Total wealth deciles (10 groups)
)

# Wave 7 financial data
fin_w7 <- fread(
  paste0(elsa_path, "wave_7_financial_derived_variables.tab"),
  sep = "\t", header = TRUE,
  select = financial_vars
) %>% as_tibble() %>%
  mutate(wave = 7)

message(paste("Wave 7 financial:", nrow(fin_w7), "rows"))

# Wave 8 financial data
fin_w8 <- fread(
  paste0(elsa_path, "wave_8_elsa_financial_dvs_eul_v1.tab"),
  sep = "\t", header = TRUE,
  select = financial_vars
) %>% as_tibble() %>%
  mutate(wave = 8)

message(paste("Wave 8 financial:", nrow(fin_w8), "rows"))

# Combine financial data
financial_derived <- bind_rows(fin_w7, fin_w8)

# -----------------------------------------------------------------------------
# STEP 8: Load IFS Derived Variables (Harmonized Education)
# -----------------------------------------------------------------------------

message("\n========== Loading IFS Derived Variables ==========\n")

# IFS = Institute for Fiscal Studies
# These contain harmonized variables including education across waves

ifs_vars <- c(
  "idauniq",
  "edqual",          # Harmonized education qualification
  "qual2",           # Alternative education coding (2 categories)
  "qual3"            # Alternative education coding (3 categories)
)

ifs_w7 <- fread(
  paste0(elsa_path, "wave_7_ifs_derived_variables.tab"),
  sep = "\t", header = TRUE,
  select = ifs_vars
) %>% as_tibble()

message(paste("IFS derived (Wave 7):", nrow(ifs_w7), "rows"))

# -----------------------------------------------------------------------------
# STEP 9: Load Nurse Visit Data (Objective Health Measures)
# -----------------------------------------------------------------------------

message("\n========== Loading Nurse Visit Data ==========\n")

# Nurse visits provide OBJECTIVE health measures
# Wave 8 nurse data is closest to our baseline (Wave 7)

nurse_vars <- c(
  "idauniq",
  # Blood pressure
  "sys1", "sys2", "sys3",      # Systolic BP (3 readings)
  "dias1", "dias2", "dias3",   # Diastolic BP (3 readings)
  "pulse1", "pulse2", "pulse3", # Pulse (3 readings)
  # Grip strength (marker of frailty/physical function)
  "mmgsd1", "mmgsd2", "mmgsd3", # Dominant hand grip (3 trials)
  "mmgsn1", "mmgsn2", "mmgsn3", # Non-dominant hand grip (3 trials)
  "mmgsdom"                     # Which hand is dominant
)

nurse_w8 <- fread(
  paste0(elsa_path, "wave_8_elsa_nurse_data_eul_v1.tab"),
  sep = "\t", header = TRUE,
  select = nurse_vars
) %>% as_tibble()

message(paste("Nurse data (Wave 8):", nrow(nurse_w8), "rows"))

# -----------------------------------------------------------------------------
# STEP 10: Save the loaded data
# -----------------------------------------------------------------------------

message("\n========== Saving Loaded Data ==========\n")

# Store all waves in a named list for easy access
elsa_waves <- list(
  wave7 = wave7,
  wave8 = wave8,
  wave9 = wave9,
  wave10 = wave10,
  wave11 = wave11
)

# Store HCAP data in a list (both 2018 and 2023)
hcap_data <- list(
  respondent_2018 = hcap_respondent_2018,
  scores_2018 = hcap_scores_2018,
  respondent_2023 = hcap_respondent_2023
)

# Save as RDS files
# RDS is R's native format - it preserves data types and is fast to read/write
saveRDS(elsa_waves, paste0(output_path, "elsa_waves_raw.rds"))
saveRDS(hcap_data, paste0(output_path, "hcap_data_raw.rds"))
saveRDS(financial_derived, paste0(output_path, "financial_derived_raw.rds"))
saveRDS(ifs_w7, paste0(output_path, "ifs_derived_raw.rds"))
saveRDS(nurse_w8, paste0(output_path, "nurse_data_raw.rds"))

message("Data saved to:", output_path)

# -----------------------------------------------------------------------------
# STEP 11: Print summary of what we loaded
# -----------------------------------------------------------------------------

message("\n========== Data Loading Summary ==========\n")

cat("ELSA Main Interview Data:\n")
cat(sprintf("  Wave 7:  %d participants\n", nrow(wave7)))
cat(sprintf("  Wave 8:  %d participants\n", nrow(wave8)))
cat(sprintf("  Wave 9:  %d participants\n", nrow(wave9)))
cat(sprintf("  Wave 10: %d participants\n", nrow(wave10)))
cat(sprintf("  Wave 11: %d participants\n", nrow(wave11)))

cat("\nHCAP Data:\n")
cat(sprintf("  2018 Respondent archive: %d participants\n", nrow(hcap_respondent_2018)))
cat(sprintf("  2018 Harmonised scores:  %d participants\n", nrow(hcap_scores_2018)))
cat(sprintf("  2023 Respondent archive: %d participants\n", nrow(hcap_respondent_2023)))

cat("\nDerived Data:\n")
cat(sprintf("  Financial (Wave 7): %d participants\n", nrow(fin_w7)))
cat(sprintf("  Financial (Wave 8): %d participants\n", nrow(fin_w8)))
cat(sprintf("  IFS derived (Wave 7): %d participants\n", nrow(ifs_w7)))
cat(sprintf("  Nurse data (Wave 8): %d participants\n", nrow(nurse_w8)))

# Check which hearing variables we found in Wave 7
cat("\nHearing variables found in Wave 7:\n")
hearing_in_w7 <- names(wave7)[tolower(names(wave7)) %in% tolower(hearing_vars)]
cat(paste(" ", hearing_in_w7, collapse = "\n"))

# Check which depression variables we found
cat("\n\nDepression (CES-D) variables found in Wave 7:\n")
cesd_in_w7 <- names(wave7)[tolower(names(wave7)) %in% tolower(depression_vars)]
cat(paste(" ", cesd_in_w7, collapse = "\n"))

# Check which health variables we found
cat("\n\nHealth condition variables found in Wave 7:\n")
health_in_w7 <- names(wave7)[tolower(names(wave7)) %in% tolower(health_vars)]
cat(paste(" ", health_in_w7, collapse = "\n"))

# Show the distribution of the key hearing variable (HeHrab)
if ("HeHrab" %in% names(wave7)) {
  cat("\n\nHearing acuity (HeHrab) distribution in Wave 7:\n")
  cat("  1 = Good hearing (all 6 tones)\n")
  cat("  2 = Mild difficulty (3-5 tones)\n")
  cat("  3 = Moderate-severe (0-2 tones)\n")
  cat("  Negative values = missing data\n\n")
  print(table(wave7$HeHrab, useNA = "ifany"))
}

message("\n========== Script 01 Complete ==========\n")
message("Next step: Run 02_clean_variables.R")
