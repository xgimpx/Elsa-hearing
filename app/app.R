# =============================================================================
# ELSA Hearing & Cognitive Trajectories - Shiny Dashboard
# =============================================================================
#
# This dashboard presents findings from the longitudinal analysis of
# hearing impairment and cognitive decline in the English Longitudinal
# Study of Ageing (ELSA).
#
# IMPORTANT: This version uses AGGREGATED DATA ONLY.
# No individual-level ELSA data is included, making it safe for public deployment.
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

# -----------------------------------------------------------------------------
# Load AGGREGATED data (no individual-level data)
# -----------------------------------------------------------------------------

data_path <- "data/"

# Function to safely load data
load_data <- function(file, default = NULL) {
  path <- paste0(data_path, file)
  if (file.exists(path)) {
    if (grepl("\\.csv$", file)) {
      read_csv(path, show_col_types = FALSE)
    } else {
      readRDS(path)
    }
  } else {
    default
  }
}

# Load aggregated datasets
sample_summary <- load_data("sample_summary.rds")
table1_summary <- load_data("table1_summary.rds")
trajectory_means <- load_data("trajectory_means.rds")
retention_summary <- load_data("retention_summary.rds")
attrition_counts <- load_data("attrition_counts.rds")
model_comparison <- load_data("model_comparison.csv")
model_results <- load_data("model_results_fully_adjusted.csv")
predicted_trajectories <- load_data("predicted_trajectories.rds")
model_quadratic <- load_data("model_quadratic_results.csv")
model_dummy <- load_data("model_B_results.csv")

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
            tags$li("Model 1: Age + Sex (unadjusted)"),
            tags$li("Model 2: + Education + Wealth"),
            tags$li("Model 3: + Depression + Diabetes + CVD + Social Engagement")
          )
        )
      ),

      card(
        card_header("Sample Summary"),
        card_body(
          uiOutput("sample_summary_ui")
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
        width = 280,

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

        hr(),

        radioButtons(
          "trajectory_type",
          "Display Type:",
          choices = c(
            "Observed Means" = "observed",
            "Model Predictions" = "predicted"
          ),
          selected = "observed"
        ),

        conditionalPanel(
          condition = "input.trajectory_type == 'observed'",
          checkboxInput("show_ci", "Show 95% CI", value = TRUE)
        ),

        conditionalPanel(
          condition = "input.trajectory_type == 'predicted'",
          selectInput(
            "model_type",
            "Model Type:",
            choices = c(
              "Linear Time" = "Linear",
              "Quadratic Time" = "Quadratic",
              "Dummy (Wave Indicators)" = "Dummy (Wave)"
            ),
            selected = "Linear"
          ),
          tags$small(
            class = "text-muted",
            tags$strong("Linear:"), " time as continuous", tags$br(),
            tags$strong("Quadratic:"), " time + time\u00B2", tags$br(),
            tags$strong("Dummy:"), " separate wave effects"
          )
        ),

        hr(),

        tags$small(
          class = "text-muted",
          "Predictions adjusted for age (70), sex (female),",
          " education (intermediate), wealth (Q3),",
          " depression (median), no diabetes/CVD.",
          br(), br(),
          "Data: ELSA Waves 7-11 (2014-2023)"
        )
      ),

      card(
        card_header(textOutput("trajectory_title")),
        card_body(
          plotlyOutput("trajectory_plot", height = "500px")
        ),
        card_footer(
          textOutput("trajectory_caption")
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
        width = 280,

        selectInput(
          "model_outcome",
          "Cognitive Outcome:",
          choices = c(
            "Verbal Fluency" = "Verbal Fluency",
            "Delayed Recall" = "Delayed Recall",
            "Immediate Recall" = "Immediate Recall",
            "Serial 7s" = "Serial 7s"
          ),
          selected = "Verbal Fluency"
        ),

        hr(),

        radioButtons(
          "model_specification",
          "Model Specification:",
          choices = c(
            "Linear Time (Hierarchical)" = "linear",
            "Quadratic Time" = "quadratic",
            "Dummy Time (Wave Indicators)" = "dummy"
          ),
          selected = "linear"
        ),

        hr(),

        conditionalPanel(
          condition = "input.model_specification == 'linear'",
          p(strong("Hierarchical Adjustment:")),
          tags$small(
            tags$div(style = "color: #666;",
              "M1: Age + Sex", tags$br(),
              "M2: + Education + Wealth", tags$br(),
              "M3: + Depression + Diabetes + CVD"
            )
          )
        ),

        conditionalPanel(
          condition = "input.model_specification == 'quadratic'",
          p(strong("Quadratic Model:")),
          tags$small(
            tags$div(style = "color: #666;",
              "Includes time + time\u00B2", tags$br(),
              "Tests acceleration/deceleration", tags$br(),
              "Fully adjusted (M3 covariates)"
            )
          )
        ),

        conditionalPanel(
          condition = "input.model_specification == 'dummy'",
          p(strong("Dummy (Wave) Model:")),
          tags$small(
            tags$div(style = "color: #666;",
              "Separate effect at each wave", tags$br(),
              "No assumed functional form", tags$br(),
              "Adjusted for age + sex"
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(textOutput("model_plot_title")),
          card_body(
            plotlyOutput("model_comparison_plot", height = "400px")
          )
        ),

        card(
          card_header(textOutput("model_table_title")),
          card_body(
            DTOutput("model_coef_table")
          ),
          card_footer(
            class = "text-muted small",
            icon("info-circle"), " ",
            "Categorical variables (e.g., hearing, education, wealth) show multiple coefficients - ",
            "one for each level compared to the reference category. ",
            "For example, education has 3 levels (Low, Intermediate, Higher), ",
            "so you see 2 coefficients comparing Intermediate vs Low and Higher vs Low."
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
        p(tags$em("Note: This dashboard displays aggregated summary statistics only.
          No individual-level data is included.")),

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
      "ELSA Waves 7-11 | Aggregated Data Only | ",
      format(Sys.Date(), "%Y")
    )
  )
)

# -----------------------------------------------------------------------------
# Server Logic
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  # --- Sample Summary ---
  output$sample_summary_ui <- renderUI({
    if (is.null(sample_summary)) {
      return(p("Data not loaded. Run script 06_create_dashboard_data.R first."))
    }

    hearing_dist <- sample_summary$hearing_distribution

    tagList(
      h5("Sample Size"),
      tags$table(
        class = "table table-sm",
        tags$tr(tags$td("Participants:"),
                tags$td(strong(format(sample_summary$n_participants, big.mark = ",")))),
        tags$tr(tags$td("Total observations:"),
                tags$td(strong(format(sample_summary$n_observations, big.mark = ",")))),
        tags$tr(tags$td("Waves:"),
                tags$td(strong(sample_summary$waves)))
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
        "Mean: ", sample_summary$age_mean, " years",
        " (SD: ", sample_summary$age_sd, ")"
      ))
    )
  })

  # --- Table 1 ---
  output$table1 <- renderDT({
    if (is.null(table1_summary)) {
      return(NULL)
    }

    # Reshape for display
    display_df <- table1_summary %>%
      pivot_longer(-hearing_acuity, names_to = "Variable", values_to = "value") %>%
      pivot_wider(names_from = hearing_acuity, values_from = value)

    datatable(
      display_df,
      options = list(
        pageLength = 20,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE
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

    if (input$trajectory_type == "observed") {
      paste("Observed Means:", outcome_labels[input$outcome_var])
    } else {
      paste("Model Predicted:", outcome_labels[input$outcome_var],
            paste0("(", input$model_type, " time)"))
    }
  })

  output$trajectory_caption <- renderText({
    if (input$trajectory_type == "observed") {
      "Observed sample means with 95% confidence intervals"
    } else {
      model_desc <- switch(input$model_type,
        "Linear" = "Linear model: cognitive score = b0 + b1*time + b2*hearing + b3*hearing*time",
        "Quadratic" = "Quadratic model: includes time\u00B2 to capture acceleration/deceleration",
        "Dummy (Wave)" = "Dummy model: separate estimates at each wave (no assumed functional form)"
      )
      paste("Predicted for reference person (age 70, female, intermediate education).", model_desc)
    }
  })

  output$trajectory_plot <- renderPlotly({
    # Map outcome variable to outcome name for predictions
    outcome_map <- c(
      "cf_animals" = "Verbal Fluency",
      "cf_delayed_recall" = "Delayed Recall"
    )

    if (input$trajectory_type == "observed") {
      # OBSERVED MEANS
      if (is.null(trajectory_means)) {
        return(plot_ly() %>%
                 layout(annotations = list(
                   text = "Run script 06 to generate data", showarrow = FALSE)))
      }

      plot_data <- trajectory_means %>%
        filter(outcome == input$outcome_var)

      p <- ggplot(plot_data, aes(x = time, y = mean_score, color = hearing_acuity,
                                 group = hearing_acuity)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3)

      if (input$show_ci) {
        p <- p + geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = hearing_acuity),
                             alpha = 0.2, color = NA)
      }

      y_label <- "Mean Score"

    } else {
      # MODEL PREDICTIONS
      if (is.null(predicted_trajectories)) {
        return(plot_ly() %>%
                 layout(annotations = list(
                   text = "Run analysis pipeline to generate predictions", showarrow = FALSE)))
      }

      # Only Verbal Fluency and Delayed Recall have predictions
      if (!(input$outcome_var %in% c("cf_animals", "cf_delayed_recall"))) {
        return(plot_ly() %>%
                 layout(annotations = list(
                   text = "Model predictions only available for Verbal Fluency and Delayed Recall",
                   showarrow = FALSE)))
      }

      outcome_name <- outcome_map[input$outcome_var]

      plot_data <- predicted_trajectories %>%
        filter(outcome == outcome_name,
               model_type == input$model_type)

      p <- ggplot(plot_data, aes(x = time, y = predicted, color = hearing_acuity,
                                 group = hearing_acuity)) +
        geom_line(linewidth = 1.2)

      # Add points for dummy model
      if (input$model_type == "Dummy (Wave)") {
        p <- p + geom_point(size = 3)
      }

      y_label <- "Predicted Score"
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
        y = y_label,
        color = "Hearing",
        fill = "Hearing"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.15))
  })

  # --- Model Plot Title ---
  output$model_plot_title <- renderText({
    switch(input$model_specification,
      "linear" = "Hearing Effects Across Hierarchical Models",
      "quadratic" = "Quadratic Model: Hearing Effects",
      "dummy" = "Wave-Specific Hearing Effects"
    )
  })

  # --- Model Table Title ---
  output$model_table_title <- renderText({
    switch(input$model_specification,
      "linear" = "Fully Adjusted Linear Model Coefficients",
      "quadratic" = "Quadratic Model Coefficients",
      "dummy" = "Dummy (Wave) Model Coefficients"
    )
  })

  # --- Model Comparison Plot ---
  output$model_comparison_plot <- renderPlotly({

    if (input$model_specification == "linear") {
      # LINEAR HIERARCHICAL MODELS
      if (is.null(model_comparison)) {
        return(plot_ly() %>%
                 layout(annotations = list(
                   text = "Run analysis pipeline to generate results",
                   showarrow = FALSE, font = list(size = 16))))
      }

      plot_data <- model_comparison %>%
        filter(outcome == input$model_outcome) %>%
        filter(grepl("hearing_acuity", term)) %>%
        mutate(
          estimate = as.numeric(str_extract(estimate_ci, "^-?[0-9.]+")),
          ci_lower = as.numeric(str_extract(estimate_ci, "(?<=\\[)-?[0-9.]+")),
          ci_upper = as.numeric(str_extract(estimate_ci, "(?<=, )-?[0-9.]+(?=\\])")),
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
        labs(x = "Model", y = "Coefficient (95% CI)", color = "Effect") +
        theme_minimal() +
        theme(legend.position = "right")

    } else if (input$model_specification == "quadratic") {
      # QUADRATIC MODEL
      if (is.null(model_quadratic)) {
        return(plot_ly() %>%
                 layout(annotations = list(
                   text = "Quadratic model results not available",
                   showarrow = FALSE)))
      }

      plot_data <- model_quadratic %>%
        filter(outcome == input$model_outcome) %>%
        filter(grepl("hearing_acuity|time", term)) %>%
        mutate(
          term_clean = case_when(
            term == "time" ~ "Time (linear)",
            term == "time_sq" ~ "Time\u00B2 (quadratic)",
            grepl("Mild.*:time_sq", term) ~ "Mild x Time\u00B2",
            grepl("Moderate.*:time_sq", term) ~ "Mod-Sev x Time\u00B2",
            grepl("Mild.*:time", term) ~ "Mild x Time",
            grepl("Moderate.*:time", term) ~ "Mod-Sev x Time",
            grepl("Mild", term) ~ "Mild (baseline)",
            grepl("Moderate", term) ~ "Mod-Sev (baseline)",
            TRUE ~ term
          )
        ) %>%
        filter(term_clean != term)  # Only keep cleaned terms

      p <- ggplot(plot_data, aes(x = term_clean, y = estimate,
                                  fill = ifelse(p.value < 0.05, "Significant", "Not significant"))) +
        geom_col() +
        geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                          ymax = estimate + 1.96*std.error), width = 0.2) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_fill_manual(values = c("Significant" = "#2E86AB", "Not significant" = "gray70")) +
        labs(x = "", y = "Coefficient", fill = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    } else {
      # DUMMY (WAVE) MODEL
      if (is.null(model_dummy)) {
        return(plot_ly() %>%
                 layout(annotations = list(
                   text = "Dummy model results not available",
                   showarrow = FALSE)))
      }

      plot_data <- model_dummy %>%
        filter(outcome == input$model_outcome) %>%
        filter(grepl("hearing_acuity.*wave_factor", term)) %>%
        mutate(
          wave = str_extract(term, "Wave \\d+"),
          hearing = case_when(
            grepl("Mild", term) ~ "Mild",
            grepl("Moderate", term) ~ "Mod-Severe"
          )
        ) %>%
        filter(!is.na(wave), !is.na(hearing))

      if (nrow(plot_data) == 0) {
        return(plot_ly() %>%
                 layout(annotations = list(
                   text = "No wave interaction data for this outcome",
                   showarrow = FALSE)))
      }

      p <- ggplot(plot_data, aes(x = wave, y = estimate, color = hearing, group = hearing)) +
        geom_point(size = 3) +
        geom_line() +
        geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                          ymax = estimate + 1.96*std.error), width = 0.2) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(x = "Wave", y = "Differential Effect vs Good Hearing", color = "Hearing Group") +
        theme_minimal()
    }

    ggplotly(p)
  })

  # --- Model Coefficients Table ---
  output$model_coef_table <- renderDT({
    # Select data source based on model specification
    table_data <- switch(input$model_specification,
      "linear" = {
        if (is.null(model_results)) return(NULL)
        model_results %>%
          filter(outcome == input$model_outcome)
      },
      "quadratic" = {
        if (is.null(model_quadratic)) return(NULL)
        model_quadratic %>%
          filter(outcome == input$model_outcome)
      },
      "dummy" = {
        if (is.null(model_dummy)) return(NULL)
        model_dummy %>%
          filter(outcome == input$model_outcome)
      }
    )

    if (is.null(table_data)) return(NULL)

    table_data <- table_data %>%
      select(term, estimate, std.error, p.value, sig) %>%
      mutate(
        estimate = round(estimate, 3),
        std.error = round(std.error, 3),
        p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
      )

    datatable(
      table_data,
      options = list(pageLength = 20, dom = 'tp', scrollY = "300px"),
      rownames = FALSE
    )
  })

  # --- Model Interpretation ---
  output$model_interpretation <- renderUI({

    if (input$model_specification == "linear") {
      # LINEAR HIERARCHICAL INTERPRETATION
      if (is.null(model_comparison)) {
        return(p("Run the analysis pipeline to see interpretations."))
      }

      mild_baseline <- model_comparison %>%
        filter(outcome == input$model_outcome,
               grepl("Model 3", model),
               grepl("Mild.*difficulty", term),
               !grepl("time", term))

      mild_slope <- model_comparison %>%
        filter(outcome == input$model_outcome,
               grepl("Model 3", model),
               grepl("Mild.*:time", term))

      tagList(
        h5(icon("lightbulb"), " Key Findings for ", input$model_outcome, " (Linear Model)"),

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
        tags$small(class = "text-muted",
          "Note: Negative coefficients indicate lower scores or faster decline for the hearing-impaired group.")
      )

    } else if (input$model_specification == "quadratic") {
      # QUADRATIC MODEL INTERPRETATION
      if (is.null(model_quadratic)) {
        return(p("Run the analysis pipeline to see quadratic model results."))
      }

      quad_data <- model_quadratic %>%
        filter(outcome == input$model_outcome)

      time_linear <- quad_data %>% filter(term == "time")
      time_sq <- quad_data %>% filter(term == "time_sq")
      mild_time <- quad_data %>% filter(grepl("Mild.*:time$", term))
      mild_time_sq <- quad_data %>% filter(grepl("Mild.*:time_sq", term))

      tagList(
        h5(icon("lightbulb"), " Key Findings for ", input$model_outcome, " (Quadratic Model)"),

        h6("Overall Time Effects (Good Hearing Reference)"),
        if (nrow(time_linear) > 0) {
          p(
            strong("Linear time: "),
            round(time_linear$estimate[1], 3), " (SE=", round(time_linear$std.error[1], 3), ")",
            if (time_linear$p.value[1] < 0.05)
              tags$span(class = "badge bg-primary", "Significant")
            else
              tags$span(class = "badge bg-secondary", "NS")
          )
        },
        if (nrow(time_sq) > 0) {
          p(
            strong("Quadratic time: "),
            round(time_sq$estimate[1], 3), " (SE=", round(time_sq$std.error[1], 3), ")",
            if (time_sq$p.value[1] < 0.05) {
              if (time_sq$estimate[1] > 0)
                tags$span(class = "badge bg-success", "Deceleration")
              else
                tags$span(class = "badge bg-danger", "Acceleration")
            } else {
              tags$span(class = "badge bg-secondary", "NS")
            }
          )
        },

        hr(),
        h6("Hearing Group Differences"),
        if (nrow(mild_time) > 0) {
          p(
            strong("Mild hearing x Time: "),
            round(mild_time$estimate[1], 3),
            if (mild_time$p.value[1] < 0.05)
              tags$span(class = "badge bg-warning", "Different slope")
            else
              tags$span(class = "badge bg-secondary", "Similar slope")
          )
        },
        if (nrow(mild_time_sq) > 0) {
          p(
            strong("Mild hearing x Time\u00B2: "),
            round(mild_time_sq$estimate[1], 3),
            if (mild_time_sq$p.value[1] < 0.05)
              tags$span(class = "badge bg-warning", "Different curvature")
            else
              tags$span(class = "badge bg-secondary", "Similar curvature")
          )
        },

        hr(),
        tags$small(class = "text-muted",
          "Positive time\u00B2 = decline slows over time; Negative time\u00B2 = decline accelerates.")
      )

    } else {
      # DUMMY (WAVE) MODEL INTERPRETATION
      if (is.null(model_dummy)) {
        return(p("Run the analysis pipeline to see dummy model results."))
      }

      dummy_data <- model_dummy %>%
        filter(outcome == input$model_outcome)

      # Get hearing main effects
      mild_main <- dummy_data %>% filter(grepl("Mild", term), !grepl("wave_factor", term))
      mod_main <- dummy_data %>% filter(grepl("Moderate", term), !grepl("wave_factor", term))

      # Get interaction effects
      interactions <- dummy_data %>%
        filter(grepl("hearing_acuity.*wave_factor", term)) %>%
        mutate(
          wave = str_extract(term, "Wave \\d+"),
          hearing = ifelse(grepl("Mild", term), "Mild", "Mod-Severe"),
          sig_star = ifelse(p.value < 0.05, "*", "")
        )

      tagList(
        h5(icon("lightbulb"), " Key Findings for ", input$model_outcome, " (Wave Dummy Model)"),

        h6("Baseline Differences (Wave 7)"),
        if (nrow(mild_main) > 0) {
          p(
            strong("Mild vs Good: "),
            round(mild_main$estimate[1], 3),
            if (mild_main$p.value[1] < 0.05)
              tags$span(class = "badge bg-warning", "Significant")
            else
              tags$span(class = "badge bg-secondary", "NS")
          )
        },
        if (nrow(mod_main) > 0) {
          p(
            strong("Mod-Severe vs Good: "),
            round(mod_main$estimate[1], 3),
            if (mod_main$p.value[1] < 0.05)
              tags$span(class = "badge bg-danger", "Significant")
            else
              tags$span(class = "badge bg-secondary", "NS")
          )
        },

        hr(),
        h6("Wave-Specific Interactions"),
        if (nrow(interactions) > 0) {
          n_sig <- sum(interactions$p.value < 0.05)
          p(paste0(n_sig, " of ", nrow(interactions),
                   " wave-by-hearing interactions are significant (p<0.05)."))
        } else {
          p("No wave-by-hearing interactions found.")
        },

        hr(),
        tags$small(class = "text-muted",
          "Dummy models estimate separate effects at each wave, making no assumptions about the shape of change over time.")
      )
    }
  })

  # --- Retention Plot ---
  output$retention_plot <- renderPlotly({
    if (is.null(retention_summary)) {
      return(NULL)
    }

    p <- ggplot(retention_summary, aes(x = wave, y = retention_pct,
                                        color = hearing_acuity, group = hearing_acuity)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = hearing_colors) +
      scale_y_continuous(limits = c(0, 105)) +
      labs(x = "Wave", y = "% of Baseline Sample", color = "Hearing") +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.15))
  })

  # --- Attrition Bar Plot ---
  output$attrition_bar <- renderPlotly({
    if (is.null(attrition_counts)) {
      return(NULL)
    }

    p <- ggplot(attrition_counts, aes(x = factor(n_waves), y = pct, fill = hearing_group)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = hearing_colors) +
      labs(x = "Number of Waves Completed", y = "% of Group", fill = "Hearing") +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.15))
  })

  # --- Attrition Table ---
  output$attrition_table <- renderDT({
    if (is.null(attrition_counts)) {
      return(NULL)
    }

    table_data <- attrition_counts %>%
      select(hearing_group, n_waves, n) %>%
      pivot_wider(names_from = n_waves, values_from = n, names_prefix = "Waves_") %>%
      rename(`Hearing Group` = hearing_group)

    datatable(table_data, options = list(dom = 't'), rownames = FALSE)
  })
}

# -----------------------------------------------------------------------------
# Run App
# -----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
