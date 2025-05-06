################################################################################
## Script:          02_1dim_irt.R
## Project:         LernIRT - Item Response Theory Analysis Framework
## Description:     One-dimensional IRT analysis using TAM
## Author:          [Your Name]
## Created:         [Date]
################################################################################

# Clear environment
rm(list = ls())
gc()

# Load required packages
library(tidyverse)
library(TAM)
library(readxl)

# Source utility functions
source("R/utils.R")
source("R/config.R")
source("R/data_processing.R")
source("R/modeling.R")
source("R/visualization.R")
source("R/reporting.R")

# Load configuration
config <- get_config()

# Run one-dimensional analysis for specific key
run_1dim_analysis <- function(key) {
  message("Starting one-dimensional analysis for ", key)
  
  # Load prepared data
  prepared_data_path <- file.path("Rdata", "prepared", paste0("prepared_data_", key, ".rds"))
  if (!file.exists(prepared_data_path)) {
    # Fall back to direct data loading if prepared data doesn't exist
    message("Prepared data not found. Loading directly...")
    data_obj <- load_analysis_data(key)
    responses <- prepare_response_data(data_obj, key)
  } else {
    # Load prepared data
    prepared_data <- readRDS(prepared_data_path)
    data_obj <- prepared_data$data_obj
    responses <- prepared_data$responses
  }
  
  # Prepare anchors for TAM with DifficultyOverall values
  anchors_matrix <- prepare_anchors_tam(
    data_obj$anchors,
    difficulty_col = "DifficultyOverall"
  )
  
  # Create output directories
  output_dirs <- create_output_dirs(
    prefix = "FinalNormMitAnkerGesamt_1dim",
    key = key
  )
  
  # Fit one-dimensional model
  message("Fitting one-dimensional model...")
  # [CODE FROM ORIGINAL SCRIPT TO BE ADDED HERE]
  
  # Process results
  message("Processing results...")
  # [CODE FROM ORIGINAL SCRIPT TO BE ADDED HERE]
  
  # Create visualizations
  message("Creating visualizations...")
  # [CODE FROM ORIGINAL SCRIPT TO BE ADDED HERE]
  
  # Save outputs
  message("Saving outputs...")
  # [CODE FROM ORIGINAL SCRIPT TO BE ADDED HERE]
  
  message("Completed one-dimensional analysis for ", key)
  return(TRUE)
}

# Run analyses for all keys
run_all_1dim_analyses <- function() {
  all_keys <- c(config$subjects$math, config$subjects$english)
  
  for (key in all_keys) {
    success <- run_1dim_analysis(key)
    if (!success) {
      warning("Analysis failed for ", key)
    }
  }
}

# Run all analyses
run_all_1dim_analyses()

message("All one-dimensional analyses completed!")