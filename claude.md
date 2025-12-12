# ELSA Project - Claude Code Instructions

## Project Overview

This project analyzes data from the **English Longitudinal Study of Ageing (ELSA)**, a longitudinal panel study covering Waves 0-11 (1998-2023). The primary focus is on hearing and cognition research (`elsa_hearing_cognition.Rproj`).

## Directory Structure

```
ELSA/
├── R/                          # Analysis scripts (01-05 pipeline)
├── UKDA-5050-tab/              # Main ELSA dataset (3.0 GB, 78 files)
│   ├── tab/                    # Tab-delimited data files
│   ├── mrdoc/                  # Documentation (PDFs, Excel dictionaries)
│   └── code/stata/             # Stata syntax
├── UKDA-8502-tab/              # HCAP cognitive assessment data
│   └── tab/                    # 5 HCAP data files
├── data/processed/             # Processed datasets (output)
├── output/figures/             # Generated figures
└── output/tables/              # Generated tables
```

## Key Identifiers (Linking Keys)

| Variable | Description |
|----------|-------------|
| **IDAUNIQ** | Unique person ID across all waves - PRIMARY KEY for merging |
| **IDAHHW[x]** | Household ID (x = wave number) - changes when person moves |

## Data File Types & Naming Conventions

### Core Data Files
Pattern: `wave_[N]_elsa_data_[version].tab`
- Contains CAPI interview responses
- Organized by modules (prefixes):
  - **HD**: Household Demographics
  - **ID**: Individual Demographics (age, marital status)
  - **HE**: Health (diagnoses, pain, ADLs)
  - **WP**: Work and Pensions
  - **IA**: Income and Assets
  - **HO**: Housing
  - **CF**: Cognitive Function
  - **EX**: Expectations
  - **PS**: Psychosocial (wellbeing)

### Derived Variables (IMPORTANT - Use These for Analysis!)

**Financial Derived Variables** (`financial_derived_variables`):
- Use INSTEAD of raw financial data from core files
- Raw data uses "Unfolding Brackets" which are hard to analyze
- These files contain imputed continuous values
- Suffixes:
  - `_i`: Imputed value (USE THIS)
  - `_f`: Validation flag (missing reason)
  - `_s`: Summary variable (totals)
- Levels: `_r` (respondent), `_p` (partner), `_bu` (benefit unit)

**IFS Derived Variables** (`ifs_derived_variables`):
- Created by Institute for Fiscal Studies
- Harmonized education, composite health indices
- Contains "fed-forward" data from earlier waves

**Pension Grids** (`pensiongrid`):
- Long format (one row per pension, not per person)
- Use `penid` to link pensions across waves

### Specialized Datasets

| Dataset | Waves | Content |
|---------|-------|---------|
| Nurse Data (`nurse`) | 2, 4, 6, 8, 9 | Blood pressure, lung function, blood samples, BMI |
| End of Life (`eol`) | Various | Proxy interviews for deceased participants |
| Cortisol (`cortisol`) | Various | Salivary biomarkers |
| Accelerometry (`accelrmtry`) | 10+ | Physical activity monitoring |
| Nutrition (`nutrition`) | Various | Dietary intake |
| HCAP (UKDA-8502) | 2018, 2023 | Harmonized Cognitive Assessment Protocol |

### Harmonized ELSA
- Cross-study comparable with US (HRS) and European (SHARE) studies
- Variables prefixed with `R[wave]...` for compatibility
- Research-ready, pre-cleaned

## Analysis Workflow (Recommended)

1. **Define Population**: Load Index File to determine who participated in each wave, mortality status
2. **Sociodemographics & Health**: Load from Core Files (`wave_x_elsa_data_...`)
3. **Income & Wealth**: Load from **Financial Derived Variables** (NOT raw core data)
4. **Objective Health**: Load from **Nurse Data** files
5. **Merge**: Join everything using `IDAUNIQ`

## Missing Value Codes

These must be recoded as NA before analysis:
- `-1`: Not applicable
- `-8`: Don't know
- `-9`: Refused

## R Analysis Pipeline

| Script | Purpose |
|--------|---------|
| `01_load_data.R` | Data loading |
| `02_clean_variables.R` | Variable cleaning and recoding |
| `03_merge_waves.R` | Wave merging |
| `04_descriptives.R` | Descriptive statistics |
| `05_longitudinal_analysis.R` | Longitudinal analysis |

## Weighting Variables

- **Standard weights**: Provided in core files
- **Nurse visit weights**: `nurwt`
- **Blood sample weights**: `bldwt`

## Data Sensitivity Levels (Wave 8+)

- **EUL**: End User Licence (standard)
- **SL**: Special Licence (detailed geography, pension age)
- **SA**: Secure Access (most sensitive)

## File Size Reference

Largest files:
- `h_elsa_g3.tab`: 573 MB (Generation 3 harmonized)
- `wave_9_elsa_data_eul_v1.tab`: 287 MB
- Core data files: typically 50-200 MB each

## Tips for Claude Code

1. **Tab files**: Use `read.delim()` in R or `read_tsv()` from readr
2. **Large files**: Consider reading only needed columns with `select()`
3. **Variable lookup**: Check Excel dictionaries in `mrdoc/` folders
4. **Documentation**: Main PDF is `ELSA_Dataset_Waves_0-11vF.pdf`
5. **Merging across waves**: Always use `IDAUNIQ`, not household IDs
