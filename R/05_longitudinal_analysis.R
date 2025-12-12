# =============================================================================
# ELSA Hearing & Cognitive Trajectories Project
# Script 05: Longitudinal Analysis (Mixed-Effects Models)
# =============================================================================
#
# PURPOSE:
# This script fits mixed-effects models to examine:
# "Do people with hearing impairment show faster cognitive decline?"
#
# THREE MODELING APPROACHES:
#
# MODEL 1 (UNADJUSTED): Basic demographics only (age, sex)
#   - Tests crude association between hearing and cognitive trajectories
#
# MODEL 2 (SOCIODEMOGRAPHIC): + Education + Wealth
#   - Controls for socioeconomic confounding
#
# MODEL 3 (FULLY ADJUSTED): + Health confounders
#   - Adds depression, diabetes, CVD, social engagement
#   - This is the primary model for inference
#
# Additionally includes:
# - Inverse probability weighting (IPW) for attrition
# - Sensitivity analyses
#
# WHAT ARE MIXED-EFFECTS MODELS?
# - "Fixed effects": The average effects we want to estimate (like hearing effect)
# - "Random effects": Allow each person to have their own baseline and trajectory
# - They handle the fact that repeated measures from the same person are correlated
# - They can handle unbalanced data (different # of observations per person)
#
# INPUT FILES:
# - data/processed/analytic_sample.rds
#
# OUTPUT FILES:
# - output/tables/model_results_unadjusted.csv
# - output/tables/model_results_sociodemographic.csv
# - output/tables/model_results_fully_adjusted.csv
# - output/tables/model_comparison.csv
# - output/figures/predicted_trajectory_*.png
# - output/models_fitted.rds
#
# =============================================================================

# -----------------------------------------------------------------------------
# STEP 1: Load required packages
# -----------------------------------------------------------------------------

library(tidyverse)

# lme4: The main package for fitting mixed-effects models
# lmer() fits linear mixed models
library(lme4)

# lmerTest: Adds p-values to lmer output
# (lme4 doesn't give p-values by default because of debates about degrees of freedom)
library(lmerTest)

# emmeans: Calculates "estimated marginal means" (predicted values at specific points)
library(emmeans)

# broom.mixed: Tidies up model output into nice dataframes
library(broom.mixed)

# -----------------------------------------------------------------------------
# STEP 2: Load and prepare the data
# -----------------------------------------------------------------------------

analytic_sample <- readRDS("data/processed/analytic_sample.rds")

# Prepare the data for modeling
analysis_df <- analytic_sample %>%
  # Keep only people with hearing data
  filter(!is.na(hearing_acuity)) %>%
  mutate(
    # Set reference category to "Good hearing"
    # This means all effects are RELATIVE to good hearing
    hearing_acuity = relevel(hearing_acuity, ref = "Good (6 tones)"),

    # Center age at 70 years
    # This makes the intercept = predicted score for a 70-year-old
    # Centering improves model convergence
    age_c = age - 70,

    # Make sure sex is a factor
    sex_factor = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),

    # Ensure education is properly factored with reference category
    education_3cat = relevel(factor(education_3cat),
                             ref = "Low (CSE/none)"),

    # Ensure wealth quintile is properly factored
    wealth_quintile = relevel(factor(wealth_quintile),
                              ref = "Q1 (Lowest)"),

    # Depression: continuous (centered) and binary
    cesd_c = cesd_total - mean(cesd_total, na.rm = TRUE),

    # Self-rated health as factor
    self_rated_health = relevel(factor(self_rated_health),
                                ref = "Excellent"),

    # Social engagement as factor (may have missing data)
    social_engagement_cat = relevel(factor(social_engagement_cat),
                                    ref = "None")
  ) %>%
  # Drop unused factor levels to avoid contrast issues
  mutate(across(where(is.factor), droplevels))

cat("Analysis sample:\n")
cat(sprintf("  Observations: %d\n", nrow(analysis_df)))
cat(sprintf("  Participants: %d\n", n_distinct(analysis_df$idauniq)))

# -----------------------------------------------------------------------------
# STEP 2b: Calculate Inverse Probability Weights for Attrition
# -----------------------------------------------------------------------------

message("\n========== Calculating Attrition Weights ==========\n")

# IPW adjusts for differential dropout
# We model probability of completing all waves based on baseline characteristics
# Then weight observations by inverse of this probability

# Create completion indicator at baseline
baseline_for_weights <- analysis_df %>%
  filter(wave == 7) %>%
  group_by(idauniq) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(
    analysis_df %>%
      group_by(idauniq) %>%
      summarise(n_waves = n_distinct(wave), .groups = "drop"),
    by = "idauniq"
  ) %>%
  mutate(
    # Did they complete at least 4 of 5 waves?
    completer = ifelse(n_waves >= 4, 1, 0)
  )

# Fit logistic regression to predict completion
# Using baseline characteristics that might predict dropout
# Use simpler model to avoid too much missing data
attrition_model <- glm(
  completer ~ age_c + sex_factor + hearing_acuity,
  data = baseline_for_weights,
  family = binomial()
)

# Calculate predicted probabilities and weights
# Use newdata to ensure predictions align with full dataset
baseline_for_weights <- baseline_for_weights %>%
  mutate(
    # Predicted probability of completion
    p_complete = predict(attrition_model, newdata = ., type = "response"),

    # Stabilized inverse probability weight
    # Stabilized weights have better properties than raw 1/p weights
    ipw = mean(completer, na.rm = TRUE) / p_complete,

    # Truncate extreme weights (winsorize at 1st and 99th percentile)
    ipw_truncated = pmin(pmax(ipw, quantile(ipw, 0.01, na.rm = TRUE)),
                         quantile(ipw, 0.99, na.rm = TRUE))
  )

# Merge weights back to full dataset
analysis_df <- analysis_df %>%
  left_join(
    baseline_for_weights %>% select(idauniq, ipw_truncated),
    by = "idauniq"
  ) %>%
  mutate(
    # Default weight of 1 if missing
    weight = ifelse(is.na(ipw_truncated), 1, ipw_truncated)
  )

cat("Attrition weights summary:\n")
print(summary(analysis_df$weight[analysis_df$wave == 7]))

# =============================================================================
# HIERARCHICAL MODELING APPROACH
# =============================================================================
#
# We fit THREE models with increasing adjustment:
#   Model 1: Unadjusted (age, sex only)
#   Model 2: + Sociodemographic (education, wealth)
#   Model 3: + Health confounders (depression, diabetes, CVD, social engagement)
#
# This allows us to see how confounders attenuate (or don't) the hearing effect

message("\n========== HIERARCHICAL MODELS ==========\n")

# -----------------------------------------------------------------------------
# Define model formulas
# -----------------------------------------------------------------------------

# Base random effects structure (same for all models)
random_effects <- "(1 + time | idauniq)"

# Model 1: Unadjusted (demographics only)
model1_fixed <- "hearing_acuity * time + age_c + sex_factor"

# Model 2: + Sociodemographic confounders
model2_fixed <- paste0(model1_fixed, " + education_3cat + wealth_quintile")

# Model 3: + Health confounders (FULLY ADJUSTED)
# Note: Removed social_engagement_cat due to missing data issues
model3_fixed <- paste0(model2_fixed,
                       " + cesd_total + has_diabetes + has_cvd")

# -----------------------------------------------------------------------------
# Function to fit hierarchical models
# -----------------------------------------------------------------------------

fit_hierarchical_models <- function(outcome_var, data) {

  results <- list()

  # Model 1: Unadjusted
  message(paste("  Model 1 (Unadjusted):", outcome_var))
  formula1 <- as.formula(paste(outcome_var, "~", model1_fixed, "+", random_effects))
  results$model1 <- lmer(formula1, data = data,
                         control = lmerControl(optimizer = "bobyqa"))

  # Model 2: + Sociodemographic
  message(paste("  Model 2 (Sociodemographic):", outcome_var))
  formula2 <- as.formula(paste(outcome_var, "~", model2_fixed, "+", random_effects))
  results$model2 <- lmer(formula2, data = data,
                         control = lmerControl(optimizer = "bobyqa"))

  # Model 3: Fully Adjusted
  message(paste("  Model 3 (Fully Adjusted):", outcome_var))
  formula3 <- as.formula(paste(outcome_var, "~", model3_fixed, "+", random_effects))
  results$model3 <- lmer(formula3, data = data,
                         control = lmerControl(optimizer = "bobyqa"))

  return(results)
}

# -----------------------------------------------------------------------------
# Fit models for each cognitive outcome
# -----------------------------------------------------------------------------

message("\nFitting models for Verbal Fluency (Animals)...")
models_animals <- fit_hierarchical_models("cf_animals", analysis_df)

message("\nFitting models for Delayed Recall...")
models_delayed <- fit_hierarchical_models("cf_delayed_recall", analysis_df)

message("\nFitting models for Immediate Recall...")
models_imm <- fit_hierarchical_models("cf_imm_recall_total", analysis_df)

message("\nFitting models for Serial 7s...")
models_serial7 <- fit_hierarchical_models("cf_serial7_total", analysis_df)

# -----------------------------------------------------------------------------
# Extract and compare key hearing effects across models
# -----------------------------------------------------------------------------

extract_hearing_effects <- function(models_list, outcome_name) {

  # Extract hearing coefficients from each model
  extract_from_model <- function(model, model_name) {
    tidy(model, effects = "fixed") %>%
      filter(grepl("hearing_acuity", term)) %>%
      mutate(
        model = model_name,
        outcome = outcome_name
      )
  }

  bind_rows(
    extract_from_model(models_list$model1, "Model 1: Unadjusted"),
    extract_from_model(models_list$model2, "Model 2: Sociodemographic"),
    extract_from_model(models_list$model3, "Model 3: Fully Adjusted")
  )
}

# Combine all hearing effects
all_hearing_effects <- bind_rows(
  extract_hearing_effects(models_animals, "Verbal Fluency"),
  extract_hearing_effects(models_delayed, "Delayed Recall"),
  extract_hearing_effects(models_imm, "Immediate Recall"),
  extract_hearing_effects(models_serial7, "Serial 7s")
)

# Format nicely
hearing_comparison <- all_hearing_effects %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = round(p.value, 4),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    ),
    # Calculate 95% CI
    ci_lower = round(estimate - 1.96 * std.error, 3),
    ci_upper = round(estimate + 1.96 * std.error, 3),
    estimate_ci = paste0(estimate, " [", ci_lower, ", ", ci_upper, "]")
  ) %>%
  select(outcome, model, term, estimate_ci, p.value, sig)

cat("\n", rep("=", 70), "\n")
cat("COMPARISON OF HEARING EFFECTS ACROSS ADJUSTMENT LEVELS\n")
cat(rep("=", 70), "\n")
cat("\nKey: Mild = Mild hearing difficulty, Mod-sev = Moderate-severe\n")
cat("     :time = interaction (differential slope)\n\n")
print(hearing_comparison, n = 50)

# Save comparison table
write_csv(hearing_comparison, "output/tables/model_comparison.csv")

# -----------------------------------------------------------------------------
# Print full results for the FULLY ADJUSTED model
# -----------------------------------------------------------------------------

cat("\n", rep("=", 70), "\n")
cat("FULLY ADJUSTED MODEL (Model 3): Verbal Fluency\n")
cat(rep("=", 70), "\n")
print(summary(models_animals$model3))

cat("\n", rep("=", 70), "\n")
cat("FULLY ADJUSTED MODEL (Model 3): Delayed Recall\n")
cat(rep("=", 70), "\n")
print(summary(models_delayed$model3))

# -----------------------------------------------------------------------------
# Extract full results from each model level
# -----------------------------------------------------------------------------

extract_full_results <- function(models_list, model_num, model_name, outcomes_list) {
  bind_rows(
    tidy(outcomes_list$animals[[model_num]], effects = "fixed") %>%
      mutate(outcome = "Verbal Fluency"),
    tidy(outcomes_list$delayed[[model_num]], effects = "fixed") %>%
      mutate(outcome = "Delayed Recall"),
    tidy(outcomes_list$imm[[model_num]], effects = "fixed") %>%
      mutate(outcome = "Immediate Recall"),
    tidy(outcomes_list$serial7[[model_num]], effects = "fixed") %>%
      mutate(outcome = "Serial 7s")
  ) %>%
    mutate(
      model = model_name,
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      p.value = round(p.value, 4),
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.1 ~ ".",
        TRUE ~ ""
      )
    ) %>%
    select(outcome, term, estimate, std.error, p.value, sig)
}

# Create list of all models by outcome
all_models <- list(
  animals = models_animals,
  delayed = models_delayed,
  imm = models_imm,
  serial7 = models_serial7
)

# Extract results for each model level
results_model1 <- extract_full_results(all_models, "model1", "Unadjusted", all_models)
results_model2 <- extract_full_results(all_models, "model2", "Sociodemographic", all_models)
results_model3 <- extract_full_results(all_models, "model3", "Fully Adjusted", all_models)

# Save each level
write_csv(results_model1, "output/tables/model_results_unadjusted.csv")
write_csv(results_model2, "output/tables/model_results_sociodemographic.csv")
write_csv(results_model3, "output/tables/model_results_fully_adjusted.csv")

# =============================================================================
# MODEL B: Dummy-Coded Time Indicators
# =============================================================================

message("\n========== MODEL B: Dummy-Coded Time ==========\n")

# MODEL B FORMULA EXPLAINED:
#
# outcome ~ hearing_acuity * wave_factor + age_c + sex_factor + (1 | idauniq)
#
# Here, wave_factor is a CATEGORICAL variable (Wave 7, 8, 9, 10, 11)
# This lets us estimate SEPARATE effects at each wave
#
# FIXED EFFECTS:
#   hearing_acuity           = Difference at Wave 7 (reference wave)
#   wave_factor[Wave 8/9/10/11] = Change from Wave 7 for Good hearing
#   hearing_acuity:wave_factor  = DIFFERENTIAL change at each wave
#
# Example: hearing_acuity[Mild]:wave_factor[Wave 11]
#   = Does the Mild group show MORE decline by Wave 11 than Good hearing?
#
# NOTE: We only have random intercepts here (1 | idauniq)
# Adding random slopes for wave_factor would be too complex

# -----------------------------------------------------------------------------
# Function to fit Model B
# -----------------------------------------------------------------------------

fit_model_B <- function(outcome_var, data) {

  formula_str <- paste0(
    outcome_var,
    " ~ hearing_acuity * wave_factor + ",
    "age_c + sex_factor + (1 | idauniq)"
  )

  model <- lmer(
    as.formula(formula_str),
    data = data,
    control = lmerControl(optimizer = "bobyqa")
  )

  return(model)
}

# -----------------------------------------------------------------------------
# Fit Model B for each cognitive outcome
# -----------------------------------------------------------------------------

message("Fitting Model B: Verbal Fluency (Animals)...")
model_B_animals <- fit_model_B("cf_animals", analysis_df)

message("Fitting Model B: Delayed Recall...")
model_B_delayed <- fit_model_B("cf_delayed_recall", analysis_df)

message("Fitting Model B: Immediate Recall Total...")
model_B_imm <- fit_model_B("cf_imm_recall_total", analysis_df)

message("Fitting Model B: Serial 7s...")
model_B_serial7 <- fit_model_B("cf_serial7_total", analysis_df)

# -----------------------------------------------------------------------------
# Print Model B results
# -----------------------------------------------------------------------------

cat("\n", rep("=", 60), "\n")
cat("Model B: Verbal Fluency (Animals)\n")
cat(rep("=", 60), "\n")
print(summary(model_B_animals))

cat("\n", rep("=", 60), "\n")
cat("Model B: Delayed Recall\n")
cat(rep("=", 60), "\n")
print(summary(model_B_delayed))

# -----------------------------------------------------------------------------
# Extract and format all Model B results
# -----------------------------------------------------------------------------

model_B_results <- bind_rows(
  tidy(model_B_animals, effects = "fixed") %>% mutate(outcome = "Verbal Fluency"),
  tidy(model_B_delayed, effects = "fixed") %>% mutate(outcome = "Delayed Recall"),
  tidy(model_B_imm, effects = "fixed") %>% mutate(outcome = "Immediate Recall"),
  tidy(model_B_serial7, effects = "fixed") %>% mutate(outcome = "Serial 7s")
)

model_B_table <- model_B_results %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 2),
    p.value = round(p.value, 4),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  select(outcome, term, estimate, std.error, statistic, p.value, sig)

cat("\n", rep("=", 60), "\n")
cat("Model B: Summary of All Outcomes\n")
cat(rep("=", 60), "\n")
print(model_B_table, n = 80)

write_csv(model_B_table, "output/tables/model_B_results.csv")

# =============================================================================
# SENSITIVITY ANALYSIS: IPW-Weighted Model
# =============================================================================

message("\n========== SENSITIVITY: IPW-Weighted Model ==========\n")

# Fit the fully adjusted model with inverse probability weights
# This accounts for potential bias due to differential dropout

fit_weighted_model <- function(outcome_var, data) {
  formula_str <- paste0(
    outcome_var, " ~ ", model3_fixed, " + ", random_effects
  )
  model <- lmer(
    as.formula(formula_str),
    data = data,
    weights = weight,
    control = lmerControl(optimizer = "bobyqa")
  )
  return(model)
}

# Fit weighted models for key outcomes
message("Fitting IPW-weighted models...")
model_weighted_animals <- fit_weighted_model("cf_animals", analysis_df)
model_weighted_delayed <- fit_weighted_model("cf_delayed_recall", analysis_df)

# Compare weighted vs unweighted hearing effects
cat("\n", rep("=", 70), "\n")
cat("SENSITIVITY ANALYSIS: Unweighted vs IPW-Weighted\n")
cat(rep("=", 70), "\n")

# Extract hearing effects from both
unweighted_effects <- tidy(models_animals$model3, effects = "fixed") %>%
  filter(grepl("hearing_acuity", term)) %>%
  mutate(model = "Unweighted", outcome = "Verbal Fluency")

weighted_effects <- tidy(model_weighted_animals, effects = "fixed") %>%
  filter(grepl("hearing_acuity", term)) %>%
  mutate(model = "IPW-Weighted", outcome = "Verbal Fluency")

sensitivity_comparison <- bind_rows(unweighted_effects, weighted_effects) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = round(p.value, 4)
  ) %>%
  select(outcome, model, term, estimate, std.error, p.value)

print(sensitivity_comparison)
write_csv(sensitivity_comparison, "output/tables/sensitivity_ipw.csv")

# =============================================================================
# ESTIMATED MARGINAL MEANS
# =============================================================================

message("\n========== Estimated Marginal Means ==========\n")

# emmeans calculates predicted values at specific combinations of predictors
# Using the FULLY ADJUSTED model (Model 3)

# Get predicted scores at time 0, 4, 8 years
emm_animals <- emmeans(
  models_animals$model3,
  specs = ~ hearing_acuity | time,
  at = list(time = c(0, 4, 8))
)

cat("\nPredicted Verbal Fluency at each time point (Fully Adjusted Model):\n")
cat("(Averaged over covariates)\n\n")
print(summary(emm_animals))

# Pairwise contrasts: Compare hearing groups at each time point
cat("\nPairwise comparisons between hearing groups:\n")
cat("(Positive = first group higher, Negative = first group lower)\n\n")
print(pairs(emm_animals, simple = "hearing_acuity"))

# Same for delayed recall
emm_delayed <- emmeans(
  models_delayed$model3,
  specs = ~ hearing_acuity | time,
  at = list(time = c(0, 4, 8))
)

cat("\nPredicted Delayed Recall at each time point (Fully Adjusted Model):\n")
print(summary(emm_delayed))

# =============================================================================
# MODEL FIT COMPARISON
# =============================================================================

message("\n========== Model Fit Comparison ==========\n")

# Compare fit across adjustment levels using AIC/BIC
model_fit <- tibble(
  Model = c("Model 1: Unadjusted", "Model 2: Sociodemographic", "Model 3: Fully Adjusted"),
  AIC_Animals = c(AIC(models_animals$model1), AIC(models_animals$model2), AIC(models_animals$model3)),
  BIC_Animals = c(BIC(models_animals$model1), BIC(models_animals$model2), BIC(models_animals$model3)),
  AIC_Delayed = c(AIC(models_delayed$model1), AIC(models_delayed$model2), AIC(models_delayed$model3)),
  BIC_Delayed = c(BIC(models_delayed$model1), BIC(models_delayed$model2), BIC(models_delayed$model3))
) %>%
  mutate(across(where(is.numeric), round, 1))

cat("\nModel Fit Comparison (lower AIC/BIC = better fit):\n\n")
print(model_fit)
write_csv(model_fit, "output/tables/model_fit_comparison.csv")

# =============================================================================
# PREDICTED TRAJECTORY PLOTS
# =============================================================================

message("\n========== Creating Predicted Trajectory Plots ==========\n")

# These plots show MODEL-BASED predictions from the FULLY ADJUSTED model
# Predictions are for a "reference" individual (age 70, female, median covariates)

# Create a grid of predictor values for prediction
# Need to include ALL covariates in the fully adjusted model
pred_grid <- expand_grid(
  time = seq(0, 8, by = 0.5),
  hearing_acuity = levels(analysis_df$hearing_acuity),
  age_c = 0,
  sex_factor = "Female",
  education_3cat = "Intermediate (A/O level)",
  wealth_quintile = "Q3",
  cesd_total = median(analysis_df$cesd_total, na.rm = TRUE),
  has_diabetes = 0,
  has_cvd = 0,
  social_engagement_cat = "Low (1-2)"
) %>%
  mutate(
    hearing_acuity = factor(hearing_acuity, levels = levels(analysis_df$hearing_acuity)),
    education_3cat = factor(education_3cat, levels = levels(analysis_df$education_3cat)),
    wealth_quintile = factor(wealth_quintile, levels = levels(analysis_df$wealth_quintile)),
    social_engagement_cat = factor(social_engagement_cat, levels = levels(analysis_df$social_engagement_cat))
  )

# Get predictions from the FULLY ADJUSTED model (Model 3)
pred_grid$pred_animals <- predict(models_animals$model3, newdata = pred_grid, re.form = NA)
pred_grid$pred_delayed <- predict(models_delayed$model3, newdata = pred_grid, re.form = NA)

# Define colors
hearing_colors <- c(
  "Good (6 tones)" = "#2E86AB",
  "Mild difficulty (3-5 tones)" = "#F6AE2D",
  "Moderate-severe (0-2 tones)" = "#E94F37"
)

# ---- Plot: Predicted Verbal Fluency Trajectories ----

p_pred_animals <- ggplot(pred_grid,
                         aes(x = time, y = pred_animals, color = hearing_acuity)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = hearing_colors) +
  scale_x_continuous(
    breaks = c(0, 2, 4, 6, 8),
    labels = c("W7\n(0)", "W8\n(2)", "W9\n(4)", "W10\n(6)", "W11\n(8)")
  ) +
  labs(
    title = "Predicted Cognitive Trajectories: Verbal Fluency",
    subtitle = "Fully adjusted model (age 70, female, median covariates)",
    x = "Wave (Years from baseline)",
    y = "Predicted verbal fluency score",
    color = "Hearing Acuity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

ggsave("output/figures/predicted_trajectory_animals.png", p_pred_animals,
       width = 8, height = 6, dpi = 300)

# ---- Plot: Predicted Delayed Recall Trajectories ----

p_pred_delayed <- ggplot(pred_grid,
                         aes(x = time, y = pred_delayed, color = hearing_acuity)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = hearing_colors) +
  scale_x_continuous(
    breaks = c(0, 2, 4, 6, 8),
    labels = c("W7\n(0)", "W8\n(2)", "W9\n(4)", "W10\n(6)", "W11\n(8)")
  ) +
  labs(
    title = "Predicted Cognitive Trajectories: Delayed Recall",
    subtitle = "Fully adjusted model (age 70, female, median covariates)",
    x = "Wave (Years from baseline)",
    y = "Predicted delayed recall score",
    color = "Hearing Acuity"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

ggsave("output/figures/predicted_trajectory_delayed.png", p_pred_delayed,
       width = 8, height = 6, dpi = 300)

cat("Predicted trajectory plots saved to: output/figures/\n")

# =============================================================================
# SAVE ALL MODELS
# =============================================================================

message("\n========== Saving Fitted Models ==========\n")

models_list <- list(
  # Hierarchical models by outcome
  animals = models_animals,
  delayed = models_delayed,
  immediate = models_imm,
  serial7 = models_serial7,
  # Sensitivity analyses
  weighted = list(
    animals = model_weighted_animals,
    delayed = model_weighted_delayed
  ),
  # Model B (dummy-coded time)
  model_B = list(
    animals = model_B_animals,
    delayed = model_B_delayed,
    immediate = model_B_imm,
    serial7 = model_B_serial7
  )
)

saveRDS(models_list, "output/models_fitted.rds")

# =============================================================================
# INTERPRETATION GUIDE
# =============================================================================

message("\n", rep("=", 70))
message("HOW TO INTERPRET YOUR RESULTS")
message(rep("=", 70), "\n")

cat("
HIERARCHICAL MODEL INTERPRETATION:
==================================

You have three models with increasing adjustment:
  Model 1: Age + Sex only (unadjusted for confounders)
  Model 2: + Education + Wealth (sociodemographic confounders)
  Model 3: + Depression + Diabetes + CVD + Social engagement (FULLY ADJUSTED)

COMPARING ACROSS MODELS:
------------------------
- If hearing effects ATTENUATE (get smaller) from Model 1 to Model 3:
  Confounders partially explain the hearing-cognition association

- If hearing effects remain SIMILAR across models:
  The association is independent of measured confounders

- If hearing effects INCREASE from Model 1 to Model 3:
  Negative confounding (confounders suppressed the true effect)

KEY COEFFICIENTS TO EXAMINE:
----------------------------
1. hearing_acuity[Mild/Moderate-severe] (main effect):
   = Baseline cognitive difference at Wave 7 vs Good hearing
   Negative = lower cognition for impaired group

2. hearing_acuity:time (interaction):
   = DIFFERENTIAL RATE OF COGNITIVE CHANGE
   Negative = FASTER decline for hearing-impaired group
   Example: -0.15 means 0.15 points MORE decline per year

CONFOUNDER EFFECTS (Model 3):
-----------------------------
- education_3cat: Education gradient in cognition
- wealth_quintile: SES gradient in cognition
- cesd_total: Depression associated with lower cognition
- has_diabetes/has_cvd: Health conditions associated with cognition
- social_engagement_cat: Social activity associated with cognition

SENSITIVITY ANALYSIS:
---------------------
IPW-weighted models adjust for potential attrition bias.
Compare weighted vs unweighted estimates:
- If SIMILAR: Results robust to differential dropout
- If DIFFERENT: Attrition may bias unweighted estimates

STATISTICAL SIGNIFICANCE:
------------------------
*** p < 0.001 (very strong evidence)
**  p < 0.01  (strong evidence)
*   p < 0.05  (moderate evidence)
.   p < 0.10  (weak evidence, 'trend')

OUTPUT FILES SUMMARY:
---------------------
- model_comparison.csv: Hearing effects across adjustment levels
- model_results_fully_adjusted.csv: Complete Model 3 results
- sensitivity_ipw.csv: Weighted vs unweighted comparison
- model_fit_comparison.csv: AIC/BIC for model comparison
- predicted_trajectory_*.png: Visualizations

")

message("\n========== Script 05 Complete ==========\n")
message("All analyses saved to: output/")
message("\nCongratulations! Your longitudinal analysis is complete.")
