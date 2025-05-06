################################################################################
## Script:          01_data_prep.R
## Project:         LernIRT - Item Response Theory Analysis Framework
## Description:     Data preparation for IRT analysis
## Author:          [Your Name]
## Created:         [Date]
################################################################################

# Clear environment
rm(list = ls())
gc()

# Load required packages
library(tidyverse)
library(readxl)

# Source utility functions
source("R/utils.R")
source("R/config.R")
source("R/data_processing.R")

# Set up logging
log_file <- file.path("logs", paste0("data_prep_", format(Sys.Date(), "%Y%m%d"), ".log"))
dir.create("logs", showWarnings = FALSE)

# Start logging
cat(paste0("Starting data preparation at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"),
    file = log_file)

# Load configuration
config <- get_config()

# Process data for a specific key
process_data <- function(key) {
  # Log start of processing
  cat(paste0("\nProcessing data for ", key, " started at ", format(Sys.time(), "%H:%M:%S"), "\n"),
      file = log_file, append = TRUE)
  
  tryCatch({
    message("Processing data for ", key)
    
    # Load raw data
    data_obj <- load_analysis_data(key)
    
    # Log data loading
    cat(paste0("Loaded data: ", nrow(data_obj$metadata), " persons, ", 
               ncol(data_obj$answers), " items, ",
               nrow(data_obj$anchors), " anchors\n"),
        file = log_file, append = TRUE)
    
    # Prepare response data
    responses <- prepare_response_data(data_obj, key)
    
    # Log response preparation
    cat(paste0("Prepared responses: ", ncol(responses), " columns\n"),
        file = log_file, append = TRUE)
    
    # For multi-dimensional analysis, prepare MIRT structure if subject has competency areas
    subject <- substr(key, 1, 3)
    if (subject %in% names(config$competency_areas)) {
      message("Preparing MIRT structure...")
      mirt_structure <- prepare_mirt_structure(data_obj, responses, key)
      
      # Log MIRT preparation
      cat(paste0("Prepared MIRT structure with ", 
                 length(unique(mirt_structure$item_structure)), " dimensions\n"),
          file = log_file, append = TRUE)
    } else {
      mirt_structure <- NULL
    }
    
    # Create output directory
    save_dir <- file.path("Rdata", "prepared")
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Save prepared data
    save_path <- file.path(save_dir, paste0("prepared_data_", key, ".rds"))
    saveRDS(
      list(
        data_obj = data_obj,
        responses = responses,
        mirt_structure = mirt_structure,
        metadata = list(
          key = key,
          subject = subject,
          grade = substr(key, 4, 4),
          preparation_date = Sys.Date(),
          item_count = ncol(responses),
          person_count = nrow(responses)
        )
      ),
      file = save_path
    )
    
    # Log completion
    cat(paste0("Data for ", key, " processed and saved to ", save_path, "\n"),
        file = log_file, append = TRUE)
    
    message("Completed processing for ", key)
    return(TRUE)
  }, error = function(e) {
    # Log error
    cat(paste0("ERROR processing data for ", key, ": ", e$message, "\n"),
        file = log_file, append = TRUE)
    
    warning("Processing failed for ", key, ": ", e$message)
    return(FALSE)
  })
}

# Process data for all keys
process_all_data <- function() {
  # Get all keys to process
  all_keys <- c(config$subjects$math, config$subjects$english)
  
  # Create results dataframe
  results <- data.frame(
    key = all_keys,
    success = FALSE,
    time_started = NA,
    time_completed = NA,
    stringsAsFactors = FALSE
  )
  
  # Process each key
  for (i in seq_along(all_keys)) {
    key <- all_keys[i]
    
    # Record start time
    results$time_started[i] <- format(Sys.time(), "%H:%M:%S")
    
    # Process data
    results$success[i] <- process_data(key)
    
    # Record completion time
    results$time_completed[i] <- format(Sys.time(), "%H:%M:%S")
  }
  
  # Save results summary
  write_table_standard(
    results,
    file.path("logs", paste0("data_prep_summary_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  )
  
  # Log completion
  cat(paste0("\nAll data preparation completed at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
             "\nSuccessful: ", sum(results$success), " of ", nrow(results), "\n"),
      file = log_file, append = TRUE)
  
  return(results)
}

# Run data preparation
tryCatch({
  prep_results <- process_all_data()
  message("All data preparation completed!")
  message("Successful: ", sum(prep_results$success), " of ", nrow(prep_results))
}, error = function(e) {
  # Log fatal error
  cat(paste0("FATAL ERROR: ", e$message, "\n"),
      file = log_file, append = TRUE)
  
  stop("Fatal error in data preparation: ", e$message)
})
