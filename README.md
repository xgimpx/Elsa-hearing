# ELSA Hearing & Cognitive Trajectories

Analysis of hearing impairment and cognitive decline using the English Longitudinal Study of Ageing (ELSA).

## Research Question

Do people with hearing impairment show faster cognitive decline over time?

## Data

This project uses data from:
- **ELSA Waves 7-11** (2014-2023) - Main interview data
- **HCAP 2018 & 2023** - Harmonized Cognitive Assessment Protocol

Data must be obtained from the [UK Data Service](https://ukdataservice.ac.uk/) (Study Number 5050).

## Project Structure

```
ELSA/
├── R/                          # Analysis scripts
│   ├── 01_load_data.R          # Load raw ELSA data
│   ├── 02_clean_variables.R    # Clean and derive variables
│   ├── 03_merge_waves.R        # Create longitudinal dataset
│   ├── 04_descriptives.R       # Descriptive statistics
│   └── 05_longitudinal_analysis.R  # Mixed-effects models
├── data/processed/             # Processed datasets (gitignored)
├── output/
│   ├── figures/                # Generated plots
│   └── tables/                 # Results tables
├── UKDA-5050-tab/              # Raw ELSA data (gitignored)
├── UKDA-8502-tab/              # HCAP data (gitignored)
└── elsa_hearing_cognition.Rproj
```

## Analysis Pipeline

1. **01_load_data.R**: Loads raw data including:
   - Core interview data (waves 7-11)
   - Financial derived variables (wealth)
   - IFS derived variables (education)
   - Nurse visit data (blood pressure, grip strength)
   - HCAP cognitive assessments

2. **02_clean_variables.R**: Creates derived variables:
   - Hearing acuity categories (from HearCheck screener)
   - Cognitive scores (verbal fluency, memory, serial 7s)
   - Depression (CES-D 8-item)
   - Comorbidities (diabetes, CVD, hypertension)
   - Wealth quintiles, education categories

3. **03_merge_waves.R**: Creates analysis dataset:
   - Long format (one row per person per wave)
   - Time variables for longitudinal modeling
   - Merges all confounders

4. **04_descriptives.R**: Generates:
   - Table 1 (baseline characteristics by hearing group)
   - Cognitive trajectories by hearing status
   - Attrition analysis

5. **05_longitudinal_analysis.R**: Fits mixed-effects models:
   - Model 1: Unadjusted (age, sex)
   - Model 2: + Education, wealth
   - Model 3: + Depression, diabetes, CVD, social engagement
   - IPW sensitivity analysis for attrition

## Key Variables

| Variable | Description |
|----------|-------------|
| `hearing_acuity` | HearCheck result: Good / Mild / Moderate-severe |
| `cf_animals` | Verbal fluency (animal naming) |
| `cf_delayed_recall` | Word-list delayed recall |
| `cesd_total` | Depression score (0-24) |
| `wealth_quintile` | Net wealth quintile (Q1-Q5) |
| `education_3cat` | Education level (3 categories) |

## Requirements

```r
install.packages(c("tidyverse", "data.table", "lme4", "lmerTest",
                   "emmeans", "broom.mixed", "gtsummary"))
```

## Usage

```r
# Set working directory to project root
setwd("path/to/ELSA")

# Run scripts in order
source("R/01_load_data.R")
source("R/02_clean_variables.R")
source("R/03_merge_waves.R")
source("R/04_descriptives.R")
source("R/05_longitudinal_analysis.R")
```

## Output

- `output/tables/model_comparison.csv` - Hearing effects across adjustment levels
- `output/tables/model_results_fully_adjusted.csv` - Full model results
- `output/figures/predicted_trajectory_*.png` - Cognitive trajectories by hearing status

## Interactive Dashboard

A Shiny dashboard is included for exploring results interactively.

### Local Usage

```r
# After running the analysis pipeline:
source("run_dashboard.R")
```

### Deploy to Posit Connect Cloud

The dashboard uses **aggregated data only** (no individual-level ELSA data), making it safe for public deployment.

```r
# Step 1: Run the analysis pipeline (scripts 01-05)
# Step 2: Create aggregated dashboard data
source("R/06_create_dashboard_data.R")

# Step 3: Regenerate manifest and deploy
setwd("app")
rsconnect::writeManifest()
# Then deploy via Posit Connect Cloud interface
```

### Dashboard Features

- **Overview**: Study design and sample summary
- **Baseline**: Table 1 characteristics by hearing group
- **Trajectories**: Interactive mean trajectory plots with 95% CI
- **Models**: Hierarchical model comparison with forest plots
- **Attrition**: Study retention analysis by hearing status

### Dashboard Requirements

```r
install.packages(c("shiny", "bslib", "plotly", "DT", "tidyverse"))
```

## License

Analysis code is provided under MIT License. ELSA data is subject to UK Data Service terms.
