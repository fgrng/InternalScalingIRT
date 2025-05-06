################################################################################
## Script:          03_multidim_irt.R
## Project:         LernIRT - Item Response Theory Analysis Framework
## Description:     Multi-dimensional IRT analysis using mirt
## Author:          [Your Name]
## Created:         [Date]
################################################################################

# Clear environment
rm(list = ls())
gc()

# Load required packages
library(tidyverse)
library(mirt)
library(readxl)

# Source utility functions
source("R/utils.R")
source("R/config.R")
source("R/data_processing.R")
source("R/modeling.R")
source("R/visualization.R")
source("R/reporting.R")

# Set up logging
log_file <- file.path("logs", paste0("multidim_irt_", format(Sys.Date(), "%Y%m%d"), ".log"))
dir.create("logs", showWarnings = FALSE)

# Start logging
cat(paste0("Starting multi-dimensional IRT analysis at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"),
    file = log_file)

# Load configuration
config <- get_config()

# Run multi-dimensional analysis for specific subject and grade
run_multidim_analysis <- function(subject, grade) {
  key <- paste0(subject, grade)
  
  # Log start of analysis
  cat(paste0("\nAnalysis for ", key, " started at ", format(Sys.time(), "%H:%M:%S"), "\n"),
      file = log_file, append = TRUE)
  
  tryCatch({
    message("Starting multi-dimensional analysis for ", key)
    
    # Load prepared data
    prepared_data_path <- file.path("Rdata", "prepared", paste0("prepared_data_", key, ".rds"))
    if (!file.exists(prepared_data_path)) {
      # Fall back to direct data loading if prepared data doesn't exist
      message("Prepared data not found. Loading directly...")
      data_obj <- load_analysis_data(key)
      responses <- prepare_response_data(data_obj, key)
    } else {
      # Load prepared data
      message("Loading prepared data from ", prepared_data_path)
      prepared_data <- readRDS(prepared_data_path)
      data_obj <- prepared_data$data_obj
      responses <- prepared_data$responses
    }
    
    # Prepare MIRT structure
    message("Preparing MIRT structure...")
    mirt_structure <- prepare_mirt_structure(data_obj, responses, key)
    
    # Create output directories
    output_dirs <- create_output_dirs(
      prefix = "FinalMitAnker_MultiDim-MIRT_EstMean_FreeCov",
      key = key
    )
    
    # Log directory creation
    cat(paste0("Output directories created at ", output_dirs$base, "\n"),
        file = log_file, append = TRUE)
    
    # Prepare anchors with DifficultyDimension for multi-dimensional model
    anchors_df <- data_obj$anchors %>%
      dplyr::rename(difficulty = DifficultyDimension)
    
    # Fit multi-dimensional model
    message("Fitting multi-dimensional model...")
    mirt_model <- fit_multidim_model(
      responses = responses,
      item_structure = mirt_structure$item_structure,
      weights = data_obj$weights,
      pars_mirt = mirt_structure$pars.mirt,
      anchors_df = anchors_df,
      method = config$model$mirt$method
    )
    
    # Extract parameters
    message("Extracting model parameters...")
    results <- extract_multidim_parameters(
      mirt_model = mirt_model,
      metadata = data_obj$metadata
    )
    
    # Log parameter extraction
    cat(paste0("Extracted parameters for ", nrow(results$item_params), " items and ", 
               nrow(results$person_params), " persons\n"),
        file = log_file, append = TRUE)
    
    # Save outputs
    message("Saving outputs...")
    output_paths <- save_multidim_outputs(
      mirt_model = mirt_model,
      results = results,
      mirt_structure = mirt_structure,
      key = key,
      dir_paths = output_dirs
    )
    
    # Log completion
    cat(paste0("Analysis for ", key, " completed successfully at ", format(Sys.time(), "%H:%M:%S"), "\n"),
        file = log_file, append = TRUE)
    
    message("Completed multi-dimensional analysis for ", key)
    return(TRUE)
  }, error = function(e) {
    # Log error
    cat(paste0("ERROR in analysis for ", key, ": ", e$message, "\n"),
        file = log_file, append = TRUE)
    
    warning("Analysis failed for ", key, ": ", e$message)
    return(FALSE)
  })
}

# Run analyses for all subjects and grades
run_all_multidim_analyses <- function() {
  # Get all subjects and grades
  subjects <- names(config$competency_areas)
  grades <- 7:9
  
  # Create results dataframe
  results <- expand.grid(
    subject = subjects,
    grade = grades,
    stringsAsFactors = FALSE
  )
  results$key <- paste0(results$subject, results$grade)
  results$success <- FALSE
  results$time_started <- NA
  results$time_completed <- NA
  
  # Run analysis for each combination
  for (i in 1:nrow(results)) {
    subject <- results$subject[i]
    grade <- results$grade[i]
    
    # Record start time
    results$time_started[i] <- format(Sys.time(), "%H:%M:%S")
    
    # Run analysis
    results$success[i] <- run_multidim_analysis(subject, grade)
    
    # Record completion time
    results$time_completed[i] <- format(Sys.time(), "%H:%M:%S")
    
    # Clean up environment after each analysis
    clean_environment(keep = c("config", "run_multidim_analysis", "run_all_multidim_analyses", 
                              "results", "i", "subject", "grade", "subjects", "grades", "log_file"))
  }
  
  # Save results summary
  write_table_standard(
    results,
    file.path("logs", paste0("multidim_analysis_summary_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  )
  
  # Log completion
  cat(paste0("\nAll multi-dimensional analyses completed at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
             "\nSuccessful: ", sum(results$success), " of ", nrow(results), "\n"),
      file = log_file, append = TRUE)
  
  return(results)
}

# Run all analyses
tryCatch({
  analysis_results <- run_all_multidim_analyses()
  message("All multi-dimensional analyses completed!")
  message("Successful: ", sum(analysis_results$success), " of ", nrow(analysis_results))
}, error = function(e) {
  # Log fatal error
  cat(paste0("FATAL ERROR: ", e$message, "\n"),
      file = log_file, append = TRUE)
  
  stop("Fatal error in analysis: ", e$message)
})
