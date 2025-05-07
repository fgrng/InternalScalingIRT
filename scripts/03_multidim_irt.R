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
library(tidyverse)  # Data-Science Package
library(mirt)       # multidim IRT Package
library(readxl)     # Excel Dateien einlesen

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

      # Load metadata, answers and items
      data_obj <- load_analysis_data(key)
      responses <- prepare_response_data(data_obj, key)

      # Map items to dimensions based on competency areas
      message("Creating item-dimension mapping...")
      item_structure <- tibble::tibble(TestdriveID = colnames(responses)) %>%
        dplyr::left_join(data_obj$items, by = "TestdriveID") %>%
        dplyr::mutate(Dimension = dplyr::case_when(
          # Math dimensions
          Kompetenzbereich == "ZV"   ~ 1,
          Kompetenzbereich == "GFDZ" ~ 2,
          Kompetenzbereich == "FR"   ~ 3,
          # English dimensions
          Kompetenzbereich == "LES"  ~ 1,
          Kompetenzbereich == "HOE"  ~ 2,
          Kompetenzbereich == "SIF"  ~ 3,
          TRUE ~ NA_real_
        )) %>%
        dplyr::pull(Dimension)

      # Create dummy data for MIRT model specification
      message("Creating model specification...")
      dummy_answers <- matrix(
        c(rep(1, ncol(responses)), rep(0, ncol(responses))),
        nrow = 2,
        byrow = TRUE
      )
      colnames(dummy_answers) <- colnames(responses)

      # Get initial model parameters
      pars.mirt <- mirt::mirt(
        data = dummy_answers,
        model = item_structure,
        itemtype = "Rasch",
        pars = 'values'
      )

      mirt_structure <- list(
        item_structure = item_structure,
        dummy_answers = dummy_answers,
        pars.mirt = pars.mirt
      )
    } else {
      # Load prepared data
      message("Loading prepared data from ", prepared_data_path)
      prepared_data <- readRDS(prepared_data_path)
      data_obj <- prepared_data$data_obj
      responses <- prepared_data$responses
      mirt_structure <- prepared_data$mirt_structure
    }

    # Create output directories
    output_dirs <- create_output_dirs(
      prefix = "FinalMitAnker_MultiDim-MIRT_EstMean_FreeCov",
      key = key
    )

    # Log directory creation
    cat(paste0("Output directories created at ", output_dirs$base, "\n"),
        file = log_file, append = TRUE)

    # Prepare anchors with DifficultyDimension for multi-dimensional model
    message("Preparing anchors with DifficultyDimension...")
    anchors_df <- data_obj$anchors %>%
      dplyr::rename(difficulty = DifficultyDimension)

    # Prepare anchor parameters for MIRT
    message("Setting up MIRT parameters...")
    update_pars <- anchors_df %>%
      dplyr::rename(item = ID) %>%
      dplyr::mutate(est = FALSE, name = "d", value = -difficulty) %>%
      dplyr::select(item, name, value, est)

    # Add configuration for covariance structure
    message("Configuring covariance structure...")
    update_pars <- update_pars %>% dplyr::bind_rows(
      # Specify covariance structure
      tibble::tibble(item = "GROUP", name = "COV_21", value = 0.25, est = TRUE),
      tibble::tibble(item = "GROUP", name = "COV_31", value = 0.25, est = TRUE),
      tibble::tibble(item = "GROUP", name = "COV_32", value = 0.25, est = TRUE),
      # Estimate mean structure
      tibble::tibble(item = "GROUP", name = "MEAN_1", value = 0.00, est = TRUE),
      tibble::tibble(item = "GROUP", name = "MEAN_2", value = 0.00, est = TRUE),
      tibble::tibble(item = "GROUP", name = "MEAN_3", value = 0.00, est = TRUE)
    )

    # Update parameters for anchored items
    message("Updating model parameters...")
    pars.mirt <- dplyr::rows_update(
      mirt_structure$pars.mirt,
      update_pars,
      by = c("item", "name")
    )

    # Setup parallel processing
    message("Setting up parallel processing...")
    mirt::mirtCluster()

    # Fit the MIRT model
    message("Fitting multi-dimensional model...")
    fit.mirt <- mirt::mirt(
      data = responses,
      model = mirt_structure$item_structure,
      itemtype = 'Rasch',
      pars = pars.mirt,
      survey.weights = data_obj$weights,
      method = config$model$mirt$method
    )

    # Save RDS of the model
    model_path <- file.path(output_dirs$base, paste0("Skalierung_mirt-3dim_", key, ".RDS"))
    message("Saving model to ", model_path)
    saveRDS(fit.mirt, file = model_path)

    # Extract model parameters
    message("Extracting model parameters...")
    model_parameters <- mirt::coef(fit.mirt, simplify = TRUE)

    # Extract person parameters
    message("Calculating person parameters...")
    person_parameters <- mirt::fscores(fit.mirt, full.scores.SE = TRUE, method = "EAP")

    # Generate plausible values
    message("Generating plausible values...")
    person_pvs <- mirt::fscores(fit.mirt, method = "EAP", plausible.draws = 10)

    # Save parameter specifications
    param_spec_path <- file.path(output_dirs$Table, paste0("ParameterSpecification_mirt-3dim_", key, ".csv"))
    message("Saving parameter specifications to ", param_spec_path)
    write_table_standard(pars.mirt, param_spec_path, row_names = TRUE)

    # Save item parameters
    item_path <- file.path(output_dirs$Table, paste0("ItemParameter_mirt-3dim_", key, ".csv"))
    message("Saving item parameters to ", item_path)
    write_table_standard(model_parameters$items, item_path, row_names = TRUE)

    # Save covariance matrix
    cov_path <- file.path(output_dirs$Table, paste0("FactorCovMatrix_mirt-3dim_", key, ".csv"))
    message("Saving factor covariance matrix to ", cov_path)
    write_table_standard(model_parameters$cov, cov_path, row_names = TRUE)

    # Save factor means
    means_path <- file.path(output_dirs$Table, paste0("FactorMeans_mirt-3dim_", key, ".csv"))
    message("Saving factor means to ", means_path)
    write_table_standard(model_parameters$means, means_path, row_names = TRUE)

    # Combine person parameters with metadata and plausible values
    message("Combining person parameters with metadata...")
    full_person_params <- dplyr::bind_cols(data_obj$metadata, person_parameters)

    # Format and add plausible values
    message("Adding plausible values...")
    for (pv in 1:10) {
      pv_table <- person_pvs[[pv]]
      colnames(pv_table) <- c(
        paste0("PV", pv, "_F1"),
        paste0("PV", pv, "_F2"),
        paste0("PV", pv, "_F3")
      )
      full_person_params <- dplyr::bind_cols(full_person_params, pv_table)
    }

    # Save person parameters
    person_path <- file.path(output_dirs$WLE, paste0("PersonParameter_mirt-3dim_", key, ".csv"))
    message("Saving person parameters to ", person_path)
    write_table_standard(full_person_params, person_path)

    # Generate summary report
    report_path <- file.path(output_dirs$base, paste0(format(Sys.Date(), "%Y%m%d"), "_Summary_", key, ".txt"))
    message("Generating summary report to ", report_path)

    cat(
      paste0("Multi-dimensional Analysis Summary Report for ", key, "\n"),
      paste0("Date: ", format(Sys.Date(), "%Y-%m-%d"), "\n"),
      paste0("Number of Items: ", ncol(responses), "\n"),
      paste0("Number of Persons: ", nrow(responses), "\n"),
      paste0("Number of Dimensions: 3\n"),
      paste0("Factor Correlations:\n",
             "F1-F2: ", round(model_parameters$cov[2,1], 3), "\n",
             "F1-F3: ", round(model_parameters$cov[3,1], 3), "\n",
             "F2-F3: ", round(model_parameters$cov[3,2], 3), "\n"),
      paste0("Factor Means:\n",
             "F1: ", round(model_parameters$means[1], 3), "\n",
             "F2: ", round(model_parameters$means[2], 3), "\n",
             "F3: ", round(model_parameters$means[3], 3), "\n"),
      file = report_path,
      sep = "\n"
    )

    # Clean up parallel processing
    message("Cleaning up parallel resources...")
    mirt::mirtCluster(remove = TRUE)

    # Log completion
    cat(paste0("Analysis for ", key, " completed successfully at ", format(Sys.time(), "%H:%M:%S"), "\n"),
        file = log_file, append = TRUE)
    cat(paste0("Items analyzed: ", ncol(responses), ", Persons: ", nrow(responses), "\n"),
        file = log_file, append = TRUE)

    message("Completed multi-dimensional analysis for ", key)
    return(TRUE)
  }, error = function(e) {
    # Clean up parallel processing on error
    message("Cleaning up parallel resources on error...")
    mirt::mirtCluster(remove = TRUE)

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
  message("Starting multi-dimensional IRT analyses...")
  analysis_results <- run_all_multidim_analyses()
  message("All multi-dimensional analyses completed!")
  message("Successful: ", sum(analysis_results$success), " of ", nrow(analysis_results))
}, error = function(e) {
  # Clean up parallel processing
  mirt::mirtCluster(remove = TRUE)

  # Log fatal error
  cat(paste0("FATAL ERROR: ", e$message, "\n"),
      file = log_file, append = TRUE)

  stop("Fatal error in analysis: ", e$message)
})
