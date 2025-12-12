# =============================================================================
# ELSA Hearing & Cognitive Trajectories - Shiny Dashboard
# =============================================================================
#
# This dashboard presents findings from the longitudinal analysis of
# hearing impairment and cognitive decline in the English Longitudinal
# Study of Ageing (ELSA).
#
# Run with: shiny::runApp("app")
#
# =============================================================================

# -----------------------------------------------------------------------------
# Load required packages
# -----------------------------------------------------------------------------

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)
library(gtsummary)

# -----------------------------------------------------------------------------
# Load data (will be created by the analysis pipeline)
# -----------------------------------------------------------------------------

# Check if processed data exists
data_path <- "../data/processed/"

# Function to safely load data
load_data <- function(file, default = NULL) {

  path <- paste0(data_path, file)
  if (file.exists(path)) {
    readRDS(path)
  } else {
    default
  }
}

# Load datasets
analytic_sample <- load_data("analytic_sample.rds")
attrition_summary <- load_data("attrition_summary.rds")

# Load model results from output
output_path <- "../output/"
models_fitted <- load_data("../output/models_fitted.rds")

# Define color palette
hearing_colors <- c(

"Good (6 tones)" = "#2E86AB",
"Mild difficulty (3-5 tones)" = "#F6AE2D",
"Moderate-severe (0-2 tones)" = "#E94F37"
)

# -----------------------------------------------------------------------------
# UI Definition
# -----------------------------------------------------------------------------

ui <- page_navbar(
title = "ELSA Hearing & Cognition",
id = "nav",
theme = bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2E86AB"
),

# --- Tab 1: Overview ---
nav_panel(
  title = "Overview",
  icon = icon("home"),

  layout_columns(
    col_widths = c(8, 4),

    card(
      card_header("Study Overview"),
      card_body(
        h4("Research Question"),
        p("Do people with hearing impairment show faster cognitive decline over time?"),

        hr(),

        h4("Data Source"),
        p(strong("English Longitudinal Study of Ageing (ELSA)"),
          "- Waves 7-11 (2014-2023)"),
        p("A nationally representative panel study of adults aged 50+ in England."),

        hr(),

        h4("Key Exposure"),
        p(strong("Hearing Acuity"), "measured using the HearCheck screener at Wave 7:"),
        tags$ul(
          tags$li(tags$span(style = "color: #2E86AB; font-weight: bold;",
                            "Good:"), "Heard all 6 tones"),
          tags$li(tags$span(style = "color: #F6AE2D; font-weight: bold;",
                            "Mild difficulty:"), "Heard 3-5 tones"),
          tags$li(tags$span(style = "color: #E94F37; font-weight: bold;",
                            "Moderate-severe:"), "Heard 0-2 tones")
        ),

        hr(),

        h4("Cognitive Outcomes"),
        tags$ul(
          tags$li(strong("Verbal Fluency:"), "Number of animals named in 1 minute"),
          tags$li(strong("Delayed Recall:"), "Word-list memory after delay"),
          tags$li(strong("Immediate Recall:"), "Word-list memory (immediate)"),
          tags$li(strong("Serial 7s:"), "Counting backwards from 100 by 7s")
        ),

        hr(),

        h4("Statistical Approach"),
        p("Hierarchical mixed-effects models with increasing covariate adjustment:"),
        tags$ol(
          tags$li("Model 1: Age + Sex (unadjusted)
"),
          tags$li("Model 2: + Education + Wealth"),
          tags$li("Model 3: + Depression + Diabetes + CVD + Social Engagement")
        )
      )
    ),

    card(
      card_header("Sample Summary"),
      card_body(
        uiOutput("sample_summary")
      )
    )
  )
),

# --- Tab 2: Baseline Characteristics ---
nav_panel(
  title = "Baseline",
  icon = icon("table"),

  card(
    card_header("Table 1: Baseline Characteristics by Hearing Status (Wave 7)"),
    card_body(
      DTOutput("table1")
    )
  )
),

# --- Tab 3: Cognitive Trajectories ---
nav_panel(
  title = "Trajectories",
  icon = icon("chart-line"),

  layout_sidebar(
    sidebar = sidebar(
      title = "Options",
      width = 250,

      selectInput(
        "outcome_var",
        "Cognitive Outcome:",
        choices = c(
          "Verbal Fluency (Animals)" = "cf_animals",
          "Delayed Recall" = "cf_delayed_recall",
          "Immediate Recall" = "cf_imm_recall_total",
          "Serial 7s" = "cf_serial7_total"
        ),
        selected = "cf_animals"
      ),

      radioButtons(
        "plot_type",
        "Plot Type:",
        choices = c(
          "Mean Trajectories" = "mean",
          "Individual Trajectories (sample)" = "individual"
        ),
        selected = "mean"
      ),

      conditionalPanel(
        condition = "input.plot_type == 'individual'",
        sliderInput(
          "n_sample",
          "Number of individuals:",
          min = 10, max = 100, value = 30, step = 10
        )
      ),

      checkboxInput("show_ci", "Show 95% CI", value = TRUE)
    ),

    card(
      card_header(textOutput("trajectory_title")),
      card_body(
        plotlyOutput("trajectory_plot", height = "500px")
      )
    )
  )
),

# --- Tab 4: Model Results ---
nav_panel(
  title = "Models",
  icon = icon("calculator"),

  layout_sidebar(
    sidebar = sidebar(
      title = "Model Selection",
      width = 250,

      selectInput(
        "model_outcome",
        "Cognitive Outcome:",
        choices = c(
          "Verbal Fluency" = "animals",
          "Delayed Recall" = "delayed",
          "Immediate Recall" = "imm",
          "Serial 7s" = "serial7"
        ),
        selected = "animals"
      ),

      hr(),

      p(strong("Model Hierarchy:")),
      tags$small(
        tags$div(style = "color: #666;",
          "M1: Age + Sex", tags$br(),
          "M2: + Education + Wealth", tags$br(),
          "M3: + Depression + Health + Social"
        )
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Hearing Effects Across Models"),
        card_body(
          plotlyOutput("model_comparison_plot", height = "400px")
        )
      ),

      card(
        card_header("Model Coefficients (Fully Adjusted)"),
        card_body(
          DTOutput("model_coef_table")
        )
      )
    ),

    card(
      card_header("Key Interpretation"),
      card_body(
        uiOutput("model_interpretation")
      )
    )
  )
),

# --- Tab 5: Attrition ---
nav_panel(
  title = "Attrition",
  icon = icon("user-minus"),

  layout_columns(
    col_widths = c(6, 6),

    card(
      card_header("Study Retention by Hearing Group"),
      card_body(
        plotlyOutput("retention_plot", height = "400px")
      )
    ),

    card(
      card_header("Waves Completed by Hearing Group"),
      card_body(
        plotlyOutput("attrition_bar", height = "400px")
      )
    )
  ),

  card(
    card_header("Attrition Summary"),
    card_body(
      DTOutput("attrition_table")
    )
  )
),

# --- Tab 6: About ---
nav_panel(
  title = "About",
  icon = icon("info-circle"),

  card(
    card_header("About This Dashboard"),
    card_body(
      h4("Data"),
      p("This analysis uses data from the English Longitudinal Study of Ageing (ELSA),
        available from the UK Data Service (Study Number 5050)."),

      hr(),

      h4("Methods"),
      p("Mixed-effects models were fitted using the ", code("lme4"), " package in R.
        All models include random intercepts and slopes for time at the individual level."),

      hr(),

      h4("Code"),
      p("Analysis code is available at: ",
        tags$a(href = "https://github.com/xgimpx/Elsa-hearing",
               "github.com/xgimpx/Elsa-hearing", target = "_blank")),

      hr(),

      h4("Citation"),
      p("If using this analysis, please cite both the ELSA data and this repository."),

      hr(),

      h4("Contact"),
      p("For questions about this analysis, please open an issue on GitHub.")
    )
  )
),

# Footer
nav_spacer(),
nav_item(
  tags$small(
    class = "text-muted",
    "ELSA Waves 7-11 | ",
    format(Sys.Date(), "%Y")
  )
)
)

# -----------------------------------------------------------------------------
# Server Logic
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

# --- Sample Summary ---
output$sample_summary <- renderUI({
  if (is.null(analytic_sample)) {
    return(p("Data not loaded. Run the analysis pipeline first."))
  }

  baseline <- analytic_sample %>% filter(wave == 7)
  n_total <- n_distinct(baseline$idauniq)
  n_obs <- nrow(analytic_sample)

  hearing_dist <- baseline %>%
    count(hearing_acuity) %>%
    mutate(pct = round(100 * n / sum(n), 1))

  tagList(
    h5("Sample Size"),
    tags$table(
      class = "table table-sm",
      tags$tr(tags$td("Participants:"), tags$td(strong(format(n_total, big.mark = ",")))),
      tags$tr(tags$td("Total observations:"), tags$td(strong(format(n_obs, big.mark = ",")))),
      tags$tr(tags$td("Waves:"), tags$td(strong("7-11 (2014-2023)")))
    ),

    hr(),

    h5("Hearing Status"),
    tags$table(
      class = "table table-sm",
      tags$tr(
        tags$td(tags$span(style = "color: #2E86AB;", icon("circle"))),
        tags$td("Good:"),
        tags$td(paste0(hearing_dist$n[1], " (", hearing_dist$pct[1], "%)"))
      ),
      tags$tr(
        tags$td(tags$span(style = "color: #F6AE2D;", icon("circle"))),
        tags$td("Mild:"),
        tags$td(paste0(hearing_dist$n[2], " (", hearing_dist$pct[2], "%)"))
      ),
      tags$tr(
        tags$td(tags$span(style = "color: #E94F37;", icon("circle"))),
        tags$td("Mod-Severe:"),
        tags$td(paste0(hearing_dist$n[3], " (", hearing_dist$pct[3], "%)"))
      )
    ),

    hr(),

    h5("Age at Baseline"),
    p(paste0(
      "Mean: ", round(mean(baseline$age, na.rm = TRUE), 1), " years",
      " (SD: ", round(sd(baseline$age, na.rm = TRUE), 1), ")"
    ))
  )
})

# --- Table 1 ---
output$table1 <- renderDT({
  if (is.null(analytic_sample)) {
    return(NULL)
  }

  baseline <- analytic_sample %>%
    filter(wave == 7) %>%
    select(
      hearing_acuity, age, sex_label,
      cf_animals, cf_delayed_recall, cf_imm_recall_total, cf_serial7_total,
      education_3cat, wealth_quintile, cesd_total,
      has_diabetes, has_cvd, has_hypertension
    )

  # Create summary table
  summary_df <- baseline %>%
    group_by(hearing_acuity) %>%
    summarise(
      N = n(),
      `Age (mean)` = round(mean(age, na.rm = TRUE), 1),
      `Age (SD)` = round(sd(age, na.rm = TRUE), 1),
      `Female (%)` = round(100 * mean(sex_label == "Female", na.rm = TRUE), 1),
      `Verbal Fluency` = round(mean(cf_animals, na.rm = TRUE), 1),
      `Delayed Recall` = round(mean(cf_delayed_recall, na.rm = TRUE), 1),
      `Depression (CES-D)` = round(mean(cesd_total, na.rm = TRUE), 1),
      `Diabetes (%)` = round(100 * mean(has_diabetes, na.rm = TRUE), 1),
      `CVD (%)` = round(100 * mean(has_cvd, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    pivot_longer(-hearing_acuity, names_to = "Variable", values_to = "value") %>%
    pivot_wider(names_from = hearing_acuity, values_from = value)

  datatable(
    summary_df,
    options = list(
      pageLength = 15,
      dom = 't',
      ordering = FALSE
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      columns = 2:4,
      backgroundColor = styleEqual(
        c("Good (6 tones)", "Mild difficulty (3-5 tones)", "Moderate-severe (0-2 tones)"),
        c("#E8F4F8", "#FEF5E7", "#FDEDEC")
      )
    )
})

# --- Trajectory Plot ---
output$trajectory_title <- renderText({
  outcome_labels <- c(
    "cf_animals" = "Verbal Fluency (Animal Naming)",
    "cf_delayed_recall" = "Word-List Delayed Recall",
    "cf_imm_recall_total" = "Word-List Immediate Recall",
    "cf_serial7_total" = "Serial 7s Performance"
  )
  paste("Cognitive Trajectory:", outcome_labels[input$outcome_var])
})

output$trajectory_plot <- renderPlotly({
  if (is.null(analytic_sample)) {
    return(NULL)
  }

  outcome_var <- input$outcome_var

  if (input$plot_type == "mean") {
    # Calculate means and CIs
    plot_data <- analytic_sample %>%
      group_by(wave, time, hearing_acuity) %>%
      summarise(
        mean_score = mean(.data[[outcome_var]], na.rm = TRUE),
        se_score = sd(.data[[outcome_var]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[outcome_var]]))),
        n = sum(!is.na(.data[[outcome_var]])),
        .groups = "drop"
      ) %>%
      mutate(
        ci_lower = mean_score - 1.96 * se_score,
        ci_upper = mean_score + 1.96 * se_score
      )

    p <- ggplot(plot_data, aes(x = time, y = mean_score, color = hearing_acuity,
                                group = hearing_acuity)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3)

    if (input$show_ci) {
      p <- p + geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = hearing_acuity),
                           alpha = 0.2, color = NA)
    }

    p <- p +
      scale_color_manual(values = hearing_colors) +
      scale_fill_manual(values = hearing_colors) +
      scale_x_continuous(
        breaks = c(0, 2, 4, 6, 8),
        labels = c("W7\n(0y)", "W8\n(2y)", "W9\n(4y)", "W10\n(6y)", "W11\n(8y)")
      ) +
      labs(
        x = "Wave (Years from baseline)",
        y = "Mean Score",
        color = "Hearing",
        fill = "Hearing"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

  } else {
    # Individual trajectories (sample)
    set.seed(42)
    sample_ids <- analytic_sample %>%
      filter(wave == 7) %>%
      group_by(hearing_acuity) %>%
      sample_n(min(input$n_sample / 3, n())) %>%
      pull(idauniq)

    plot_data <- analytic_sample %>%
      filter(idauniq %in% sample_ids)

    p <- ggplot(plot_data, aes(x = time, y = .data[[outcome_var]],
                                group = idauniq, color = hearing_acuity)) +
      geom_line(alpha = 0.4) +
      scale_color_manual(values = hearing_colors) +
      scale_x_continuous(
        breaks = c(0, 2, 4, 6, 8),
        labels = c("W7", "W8", "W9", "W10", "W11")
      ) +
      labs(
        x = "Wave (Years)",
        y = "Score",
        color = "Hearing"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  }

  ggplotly(p) %>%
    layout(legend = list(orientation = "h", y = -0.15))
})

# --- Model Comparison Plot ---
output$model_comparison_plot <- renderPlotly({
  if (is.null(analytic_sample)) {
    return(NULL)
  }

  # Read model results from CSV if available
  results_path <- "../output/tables/model_comparison.csv"

  if (file.exists(results_path)) {
    results <- read_csv(results_path, show_col_types = FALSE)

    # Filter for selected outcome
    outcome_label <- switch(input$model_outcome,
      "animals" = "Verbal Fluency",
      "delayed" = "Delayed Recall",
      "imm" = "Immediate Recall",
      "serial7" = "Serial 7s"
    )

    plot_data <- results %>%
      filter(outcome == outcome_label) %>%
      filter(grepl("hearing_acuity", term)) %>%
      mutate(
        # Parse estimate and CI from estimate_ci column
        estimate = as.numeric(str_extract(estimate_ci, "^-?[0-9.]+")),
        ci_lower = as.numeric(str_extract(estimate_ci, "(?<=\\[)-?[0-9.]+")),
        ci_upper = as.numeric(str_extract(estimate_ci, "(?<=, )-?[0-9.]+(?=\\])")),
        # Clean term names
        term_clean = case_when(
          grepl("Mild.*:time", term) ~ "Mild x Time",
          grepl("Moderate.*:time", term) ~ "Mod-Sev x Time",
          grepl("Mild", term) ~ "Mild (baseline)",
          grepl("Moderate", term) ~ "Mod-Sev (baseline)",
          TRUE ~ term
        ),
        model_short = case_when(
          grepl("Model 1", model) ~ "M1",
          grepl("Model 2", model) ~ "M2",
          grepl("Model 3", model) ~ "M3"
        )
      )

    p <- ggplot(plot_data, aes(x = model_short, y = estimate, color = term_clean)) +
      geom_point(position = position_dodge(0.5), size = 3) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                    position = position_dodge(0.5), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        x = "Model",
        y = "Coefficient (95% CI)",
        color = "Effect"
      ) +
      theme_minimal() +
      theme(legend.position = "right")

    ggplotly(p)
  } else {
    # Create placeholder if results not available
    plot_ly() %>%
      layout(
        annotations = list(
          text = "Run analysis pipeline to generate results",
          showarrow = FALSE,
          font = list(size = 16)
        )
      )
  }
})

# --- Model Coefficients Table ---
output$model_coef_table <- renderDT({
  results_path <- "../output/tables/model_results_fully_adjusted.csv"

  if (file.exists(results_path)) {
    results <- read_csv(results_path, show_col_types = FALSE)

    outcome_label <- switch(input$model_outcome,
      "animals" = "Verbal Fluency",
      "delayed" = "Delayed Recall",
      "imm" = "Immediate Recall",
      "serial7" = "Serial 7s"
    )

    table_data <- results %>%
      filter(outcome == outcome_label) %>%
      select(term, estimate, std.error, p.value, sig) %>%
      mutate(
        estimate = round(estimate, 3),
        std.error = round(std.error, 3),
        p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
      )

    datatable(
      table_data,
      options = list(
        pageLength = 20,
        dom = 'tp',
        scrollY = "300px"
      ),
      rownames = FALSE
    )
  } else {
    NULL
  }
})

# --- Model Interpretation ---
output$model_interpretation <- renderUI({
  results_path <- "../output/tables/model_comparison.csv"

  if (!file.exists(results_path)) {
    return(p("Run the analysis pipeline to see interpretations."))
  }

  results <- read_csv(results_path, show_col_types = FALSE)

  outcome_label <- switch(input$model_outcome,
    "animals" = "Verbal Fluency",
    "delayed" = "Delayed Recall",
    "imm" = "Immediate Recall",
    "serial7" = "Serial 7s"
  )

  # Get key effects
  mild_baseline <- results %>%
    filter(outcome == outcome_label,
           grepl("Model 3", model),
           grepl("Mild.*difficulty", term),
           !grepl("time", term))

  mild_slope <- results %>%
    filter(outcome == outcome_label,
           grepl("Model 3", model),
           grepl("Mild.*:time", term))

  tagList(
    h5(icon("lightbulb"), " Key Findings for ", outcome_label),

    if (nrow(mild_baseline) > 0) {
      p(
        strong("Baseline difference (Mild vs Good): "),
        mild_baseline$estimate_ci[1],
        if (mild_baseline$p.value[1] < 0.05)
          tags$span(class = "badge bg-success", "Significant")
        else
          tags$span(class = "badge bg-secondary", "Not significant")
      )
    },

    if (nrow(mild_slope) > 0) {
      p(
        strong("Rate of change (Mild vs Good): "),
        mild_slope$estimate_ci[1], " points/year",
        if (mild_slope$p.value[1] < 0.05)
          tags$span(class = "badge bg-warning", "Faster decline")
        else
          tags$span(class = "badge bg-secondary", "Similar trajectory")
      )
    },

    hr(),

    tags$small(
      class = "text-muted",
      "Note: Negative coefficients indicate lower scores or faster decline for the hearing-impaired group."
    )
  )
})

# --- Retention Plot ---
output$retention_plot <- renderPlotly({
  if (is.null(analytic_sample)) {
    return(NULL)
  }

  retention <- analytic_sample %>%
    group_by(wave, hearing_acuity) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(hearing_acuity) %>%
    mutate(
      n_baseline = n[wave == 7],
      retention_pct = 100 * n / n_baseline
    )

  p <- ggplot(retention, aes(x = wave, y = retention_pct,
                              color = hearing_acuity, group = hearing_acuity)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = hearing_colors) +
    scale_y_continuous(limits = c(0, 105)) +
    labs(
      x = "Wave",
      y = "% of Baseline Sample",
      color = "Hearing"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggplotly(p) %>%
    layout(legend = list(orientation = "h", y = -0.15))
})

# --- Attrition Bar Plot ---
output$attrition_bar <- renderPlotly({
  if (is.null(attrition_summary)) {
    return(NULL)
  }

  plot_data <- attrition_summary %>%
    count(n_waves, hearing_group) %>%
    group_by(hearing_group) %>%
    mutate(pct = 100 * n / sum(n))

  p <- ggplot(plot_data, aes(x = factor(n_waves), y = pct, fill = hearing_group)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = hearing_colors) +
    labs(
      x = "Number of Waves Completed",
      y = "% of Group",
      fill = "Hearing"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggplotly(p) %>%
    layout(legend = list(orientation = "h", y = -0.15))
})

# --- Attrition Table ---
output$attrition_table <- renderDT({
  if (is.null(attrition_summary)) {
    return(NULL)
  }

  table_data <- attrition_summary %>%
    group_by(hearing_group, n_waves) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = n_waves, values_from = n, names_prefix = "Waves_") %>%
    rename(`Hearing Group` = hearing_group)

  datatable(
    table_data,
    options = list(dom = 't'),
    rownames = FALSE
  )
})

}

# -----------------------------------------------------------------------------
# Run App
# -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
