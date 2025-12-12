# =============================================================================
# Launch ELSA Hearing & Cognition Dashboard
# =============================================================================
#
# This script launches the Shiny dashboard for exploring the analysis results.
#
# Prerequisites:
# 1. Run the analysis pipeline first (scripts 01-05)
# 2. Install required packages (see below)
#
# =============================================================================

# Install required packages if not already installed
required_packages <- c(
  "shiny",
  "bslib",
  "tidyverse",
  "plotly",
  "DT",
  "gtsummary"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing", pkg, "..."))
    install.packages(pkg)
  }
}

invisible(sapply(required_packages, install_if_missing))

# Check if data exists
if (!file.exists("data/processed/analytic_sample.rds")) {
  stop(
    "\n",
    "=======================================================================\n",
    "DATA NOT FOUND!\n",
    "=======================================================================\n",
    "\n",
    "Please run the analysis pipeline first:\n",
    "  source('R/01_load_data.R')\n",
    "  source('R/02_clean_variables.R')\n",
    "  source('R/03_merge_waves.R')\n",
    "  source('R/04_descriptives.R')\n",
    "  source('R/05_longitudinal_analysis.R')\n",
    "\n",
    "Note: You need ELSA data from the UK Data Service.\n",
    "======================================================================="
  )
}

# Launch the dashboard
message("\n")
message("=======================================================================")
message("Launching ELSA Hearing & Cognition Dashboard...")
message("=======================================================================")
message("\n")
message("The dashboard will open in your web browser.")
message("To stop the app, press Ctrl+C in the console or close the browser.\n")

shiny::runApp("app", launch.browser = TRUE)
