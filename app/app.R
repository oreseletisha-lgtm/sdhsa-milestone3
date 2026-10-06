# =============================================================
# SDHSA – Household Vulnerability Decision-Support Tool
# Task 4 Deployment – Group 13
# =============================================================
library(shiny)
library(tidyverse)
library(tidymodels)
library(DT)
library(readr)
library(recipes)

# ---- Load artefacts produced in Task 2 ----
final_model  <- readRDS("../artefacts/final_model.rds")
final_recipe <- readRDS("../artefacts/final_recipe.rds")

# Feature list derived from the training set (post leakage removal)
required_features <- c(
  "province", "province_code",
  "head_population_group_code", "head_sex_code", "head_age",
  "main_dwelling_code", "wall_material_code", "roof_material_code",
  "floor_material_code", "wall_condition_code", "roof_condition_code",
  "floor_condition_code", "total_rooms", "tenure_code", "rent_raw",
  "rdp_housing_code", "housing_subsidy_code", "household_size",
  "geo_type_code", "metro_code",
  "survey_year_d05", "province_d05", "province_code_d05",
  "geo_type_code_d05", "household_weight_d05",
  "survey_year_d06", "province_d06", "province_code_d06",
  "cooking_energy_code", "lighting_energy_code",
  "space_heating_energy_code", "loadshedding_frequency_code",
  "alternative_lighting_code", "alternative_cooking_code",
  "geo_type_code_d06", "household_weight_d06",
  "survey_year_d07", "province_d07", "province_code_d07",
  "skipped_meal_days", "ate_less_days", "ran_out_days",
  "household_size_d07", "geo_type_code_d07", "household_weight_d07",
  "survey_year_d09", "province_d09", "province_code_d09",
  "expenditure_band_code", "total_vehicles",
  "household_size_d09", "household_weight_d09",
  "hh_size_person", "n_children", "n_elderly",
  "hh_head_education", "dependency_ratio", "hh_size"
)

# ---- Validation ----
validate_new_data <- function(df) {
  errors <- character(0)
  
  # Target must NOT be present
  if ("vulnerable" %in% names(df)) {
    errors <- c(errors,
                "The uploaded file must NOT contain the 'vulnerable' column. Remove it and retry.")
  }
  
  # Missing columns
  missing_cols <- setdiff(required_features, names(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors,
                paste("Missing required columns:",
                      paste(missing_cols, collapse = ", ")))
  }
  
  # Type sanity checks on numeric features
  numeric_feats <- c("head_age","total_rooms","rent_raw","household_size",
                     "skipped_meal_days","ate_less_days","ran_out_days",
                     "hh_size_person","n_children","n_elderly",
                     "dependency_ratio","hh_size","total_vehicles")
  for (f in intersect(numeric_feats, names(df))) {
    if (!is.numeric(df[[f]])) {
      errors <- c(errors, paste0("Column '", f, "' must be numeric."))
    }
  }
  
  # Basic range checks (training-informed)
  if ("head_age" %in% names(df) && is.numeric(df$head_age)) {
    if (any(df$head_age < 0 | df$head_age > 120, na.rm = TRUE)) {
      errors <- c(errors, "head_age contains values outside 0–120.")
    }
  }
  
  errors
}

# ---- Scoring ----
score_new_data <- function(df) {
  # Apply the SAVED recipe (bake) — never re-fit
  baked <- bake(final_recipe, new_data = df)
  preds <- predict(final_model, new_data = baked, type = "prob") %>%
    bind_cols(predict(final_model, new_data = baked))
  bind_cols(df, preds)
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("SDHSA – Household Vulnerability Decision-Support Tool"),
  tags$div(
    style = "background:#fff4e5;border-left:5px solid #d9822b;padding:10px;margin-bottom:15px;",
    tags$strong("Responsible-use notice: "),
    "This tool produces a model-based probability, not an official classification. ",
    "It supports screening and prioritisation only. All decisions affecting households ",
    "must be reviewed by a human analyst."
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload household CSV",
                accept = c(".csv")),
      actionButton("score", "Validate & Score", class = "btn-primary"),
      hr(),
      helpText("The file must contain all required feature columns and must NOT contain the 'vulnerable' column."),
      downloadButton("download", "Download scored CSV")
    ),
    mainPanel(
      h4("Validation"),
      verbatimTextOutput("validation"),
      h4("Scored households"),
      DTOutput("results")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  scored_data <- reactiveVal(NULL)
  validation_msg <- reactiveVal("Awaiting upload…")
  
  observeEvent(input$score, {
    req(input$file)
    
    df <- tryCatch(
      readr::read_csv(input$file$datapath, show_col_types = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(df)) {
      validation_msg("Could not read the uploaded file as a CSV.")
      scored_data(NULL)
      return()
    }
    
    errs <- validate_new_data(df)
    
    if (length(errs) > 0) {
      validation_msg(paste(errs, collapse = "\n"))
      scored_data(NULL)
    } else {
      result <- tryCatch(
        score_new_data(df),
        error = function(e) paste("Scoring failed:", conditionMessage(e))
      )
      if (is.character(result)) {
        validation_msg(result)
        scored_data(NULL)
      } else {
        validation_msg(sprintf(
          "Validation passed. %d households scored using Random Forest (final model).",
          nrow(result)))
        scored_data(result)
      }
    }
  })
  
  output$validation <- renderText(validation_msg())
  
  output$results <- renderDT({
    req(scored_data())
    datatable(scored_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$download <- downloadHandler(
    filename = function() paste0("sdhsa_scored_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(scored_data())
      readr::write_csv(scored_data(), file)
    }
  )
}

shinyApp(ui, server)