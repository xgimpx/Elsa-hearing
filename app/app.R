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

    # Section header with model descriptions
    card(
      card_body(
        class = "py-2",
        tags$div(
          class = "d-flex justify-content-between flex-wrap",
          tags$div(
            tags$strong("Linear (M1/M2/M3):"), " Hierarchical adjustment - ",
            tags$span(class = "text-muted", "M1: Age+Sex | M2: +Education+Wealth | M3: +Depression+Diabetes+CVD")
          ),
          tags$div(
            tags$strong("Quadratic:"), " ",
            tags$span(class = "text-muted", "Time + Time\u00B2, fully adjusted")
          ),
          tags$div(
            tags$strong("Dummy:"), " ",
            tags$span(class = "text-muted", "Wave indicators, age+sex adjusted")
          )
        )
      )
    ),

    # Section 1: Verbal Fluency
    h4("Verbal Fluency", class = "mt-3 mb-2"),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("Linear Hierarchical (M1 \u2192 M2 \u2192 M3)"),
        card_body(class = "p-2", DTOutput("model_vf_linear"))
      ),
      card(
        card_header("Quadratic Time"),
        card_body(class = "p-2", DTOutput("model_vf_quad"))
      ),
      card(
        card_header("Dummy (Wave)"),
        card_body(class = "p-2", DTOutput("model_vf_dummy"))
      )
    ),

    # Section 2: Delayed Recall
    h4("Delayed Recall", class = "mt-3 mb-2"),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("Linear Hierarchical (M1 \u2192 M2 \u2192 M3)"),
        card_body(class = "p-2", DTOutput("model_dr_linear"))
      ),
      card(
        card_header("Quadratic Time"),
        card_body(class = "p-2", DTOutput("model_dr_quad"))
      ),
      card(
        card_header("Dummy (Wave)"),
        card_body(class = "p-2", DTOutput("model_dr_dummy"))
      )
    ),

    # Section 3: Immediate Recall
    h4("Immediate Recall", class = "mt-3 mb-2"),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("Linear Hierarchical (M1 \u2192 M2 \u2192 M3)"),
        card_body(class = "p-2", DTOutput("model_ir_linear"))
      ),
      card(
        card_header("Quadratic Time"),
        card_body(class = "p-2", DTOutput("model_ir_quad"))
      ),
      card(
        card_header("Dummy (Wave)"),
        card_body(class = "p-2", DTOutput("model_ir_dummy"))
      )
    ),

    # Section 4: Serial 7s
    h4("Serial 7s", class = "mt-3 mb-2"),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header("Linear Hierarchical (M1 \u2192 M2 \u2192 M3)"),
        card_body(class = "p-2", DTOutput("model_s7_linear"))
      ),
      card(
        card_header("Quadratic Time"),
        card_body(class = "p-2", DTOutput("model_s7_quad"))
      ),
      card(
        card_header("Dummy (Wave)"),
        card_body(class = "p-2", DTOutput("model_s7_dummy"))
      )
    ),

    # Footer with interpretation note
    card(
      card_body(
        class = "text-muted small",
        icon("info-circle"), " ",
        tags$strong("Reading the tables:"), " ",
        "Coefficients show hearing group effects compared to Good hearing (reference). ",
        "Mild = Mild difficulty (3-5 tones), Mod-Sev = Moderate-severe (0-2 tones). ",
        "Negative values indicate lower scores or faster decline. ",
        tags$strong("Sig:"), " *** p<0.001, ** p<0.01, * p<0.05, . p<0.1"
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
      "cf_delayed_recall" = "Delayed Recall",
      "cf_imm_recall_total" = "Immediate Recall",
      "cf_serial7_total" = "Serial 7s"
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

  # --- Helper functions for model tables ---

  # Function to create compact linear hierarchical table (showing M1, M2, M3 side by side)
  make_linear_table <- function(outcome_name) {
    if (is.null(model_comparison)) return(NULL)

    model_comparison %>%
      filter(outcome == outcome_name) %>%
      filter(grepl("hearing_acuity", term)) %>%
      mutate(
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
        ),
        coef = paste0(estimate_ci, sig)
      ) %>%
      select(term_clean, model_short, coef) %>%
      pivot_wider(names_from = model_short, values_from = coef) %>%
      rename(Term = term_clean)
  }

  # Function to create compact quadratic table
  make_quadratic_table <- function(outcome_name) {
    if (is.null(model_quadratic)) return(NULL)

    model_quadratic %>%
      filter(outcome == outcome_name) %>%
      filter(grepl("hearing_acuity|^time", term)) %>%
      mutate(
        Term = case_when(
          term == "time" ~ "Time",
          term == "time_sq" ~ "Time\u00B2",
          grepl("Mild.*:time_sq", term) ~ "Mild x Time\u00B2",
          grepl("Moderate.*:time_sq", term) ~ "Mod-Sev x Time\u00B2",
          grepl("Mild.*:time$", term) ~ "Mild x Time",
          grepl("Moderate.*:time$", term) ~ "Mod-Sev x Time",
          grepl("Mild", term) ~ "Mild (baseline)",
          grepl("Moderate", term) ~ "Mod-Sev (baseline)",
          TRUE ~ term
        ),
        Coef = round(estimate, 3),
        SE = round(std.error, 3),
        Sig = sig
      ) %>%
      filter(Term != term) %>%
      select(Term, Coef, SE, Sig)
  }

  # Function to create compact dummy table
  make_dummy_table <- function(outcome_name) {
    if (is.null(model_dummy)) return(NULL)

    model_dummy %>%
      filter(outcome == outcome_name) %>%
      filter(grepl("hearing_acuity", term)) %>%
      mutate(
        Term = case_when(
          grepl("Mild.*:wave_factorWave 8", term) ~ "Mild x W8",
          grepl("Mild.*:wave_factorWave 9", term) ~ "Mild x W9",
          grepl("Mild.*:wave_factorWave 10", term) ~ "Mild x W10",
          grepl("Mild.*:wave_factorWave 11", term) ~ "Mild x W11",
          grepl("Moderate.*:wave_factorWave 8", term) ~ "Mod-Sev x W8",
          grepl("Moderate.*:wave_factorWave 9", term) ~ "Mod-Sev x W9",
          grepl("Moderate.*:wave_factorWave 10", term) ~ "Mod-Sev x W10",
          grepl("Moderate.*:wave_factorWave 11", term) ~ "Mod-Sev x W11",
          grepl("Mild", term) ~ "Mild (baseline)",
          grepl("Moderate", term) ~ "Mod-Sev (baseline)",
          TRUE ~ term
        ),
        Coef = round(estimate, 3),
        SE = round(std.error, 3),
        Sig = sig
      ) %>%
      filter(Term != term) %>%
      select(Term, Coef, SE, Sig)
  }

  # DT options for compact tables (no scroll, show all rows)
  dt_options <- list(
    pageLength = 20,
    dom = 't',
    ordering = FALSE,
    autoWidth = TRUE,
    paging = FALSE
  )

  # --- Verbal Fluency tables ---
  output$model_vf_linear <- renderDT({
    datatable(make_linear_table("Verbal Fluency"), options = dt_options, rownames = FALSE)
  })
  output$model_vf_quad <- renderDT({
    datatable(make_quadratic_table("Verbal Fluency"), options = dt_options, rownames = FALSE)
  })
  output$model_vf_dummy <- renderDT({
    datatable(make_dummy_table("Verbal Fluency"), options = dt_options, rownames = FALSE)
  })

  # --- Delayed Recall tables ---
  output$model_dr_linear <- renderDT({
    datatable(make_linear_table("Delayed Recall"), options = dt_options, rownames = FALSE)
  })
  output$model_dr_quad <- renderDT({
    datatable(make_quadratic_table("Delayed Recall"), options = dt_options, rownames = FALSE)
  })
  output$model_dr_dummy <- renderDT({
    datatable(make_dummy_table("Delayed Recall"), options = dt_options, rownames = FALSE)
  })

  # --- Immediate Recall tables ---
  output$model_ir_linear <- renderDT({
    datatable(make_linear_table("Immediate Recall"), options = dt_options, rownames = FALSE)
  })
  output$model_ir_quad <- renderDT({
    datatable(make_quadratic_table("Immediate Recall"), options = dt_options, rownames = FALSE)
  })
  output$model_ir_dummy <- renderDT({
    datatable(make_dummy_table("Immediate Recall"), options = dt_options, rownames = FALSE)
  })

  # --- Serial 7s tables ---
  output$model_s7_linear <- renderDT({
    datatable(make_linear_table("Serial 7s"), options = dt_options, rownames = FALSE)
  })
  output$model_s7_quad <- renderDT({
    datatable(make_quadratic_table("Serial 7s"), options = dt_options, rownames = FALSE)
  })
  output$model_s7_dummy <- renderDT({
    datatable(make_dummy_table("Serial 7s"), options = dt_options, rownames = FALSE)
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
