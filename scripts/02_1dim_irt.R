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
library(tidyverse)  # Data-Science Package
library(TAM)        # IRT Package
library(readxl)     # Excel Dateien einlesen

# Source utility functions
source("R/utils.R")
source("R/config.R")
source("R/data_processing.R")
source("R/modeling.R")
source("R/visualization.R")
source("R/reporting.R")

# Set up logging
log_file <- file.path("logs", paste0("1dim_irt_", format(Sys.Date(), "%Y%m%d"), ".log"))
dir.create("logs", showWarnings = FALSE)

# Start logging
cat(paste0("Starting one-dimensional IRT analysis at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n"),
    file = log_file)

# Load configuration
config <- get_config()

# Run one-dimensional analysis for specific key
run_1dim_analysis <- function(key) {
  # Log start of analysis
  cat(paste0("\nAnalysis for ", key, " started at ", format(Sys.time(), "%H:%M:%S"), "\n"),
      file = log_file, append = TRUE)

  tryCatch({
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
      message("Loading prepared data from ", prepared_data_path)
      prepared_data <- readRDS(prepared_data_path)
      data_obj <- prepared_data$data_obj
      responses <- prepared_data$responses
    }

    # Prepare anchors for TAM with DifficultyOverall values
    message("Preparing anchors with DifficultyOverall values...")
    anchors_matrix <- prepare_anchors_tam(
      data_obj$anchors,
      difficulty_col = "DifficultyOverall"
    )

    # Create output directories
    output_dirs <- create_output_dirs(
      prefix = "FinalNormMitAnkerGesamt_1dim",
      key = key
    )

    # Log directory creation
    cat(paste0("Output directories created at ", output_dirs$base, "\n"),
        file = log_file, append = TRUE)

    # Fit one-dimensional model
    message("Fitting one-dimensional model...")
    tam_model <- fit_1dim_model(
      responses = responses,
      pids = data_obj$pids,
      weights = data_obj$weights,
      anchors_matrix = anchors_matrix,
      control = list(
        increment.factor = 1.05,
        fac.oldxsi = 0.8,
        Msteps = 10,
        maxiter = 1000
      )
    )

    # Save TAM model as RDS
    model_path <- file.path(output_dirs$base, paste0("Skalierung_tam-mml_", key, ".RDS"))
    message("Saving model to ", model_path)
    saveRDS(tam_model, file = model_path)

    # Extract parameters and calculate fit statistics
    message("Extracting model parameters and calculating fit statistics...")

    # Extract item parameters
    item_params <- tam_model$xsi
    item_params <- tibble::rownames_to_column(tam_model$xsi)

    # Calculate item fit statistics
    item_fit <- TAM::msq.itemfit(tam_model)$itemfit
    item_fit$rowname <- as.character(item_fit[, "item"])
    item_fit <- item_fit[, c("rowname", "Infit", "Infit_t", "Outfit", "Outfit_t"), drop = FALSE]

    # Calculate WLEs and reliability
    message("Calculating ability parameters (WLEs)...")
    wle_results <- TAM::tam.wle(tam_model)
    wle_rel <- wle_results[["WLE.rel"]][1]
    names(wle_rel) <- "WLE_Reliability"

    # Extract ability parameters (WLEs)
    ability_params <- wle_results[, c(1, 5, 6)]

    # Generate plausible values
    message("Generating plausible values...")
    plausible_values <- TAM::IRT.drawPV(tam_model, NPV = 10)

    # Save WLE reliability
    rel_path <- file.path(output_dirs$Table, paste0(format(Sys.Date(), "%Y%m%d"), "_FinalNormMitAnkerGesamt_WLE-Reliability_", key, ".csv"))
    write_table_standard(wle_rel, rel_path, row_names = TRUE)

    # Combine with metadata and save WLE parameters
    message("Combining person parameters with metadata...")
    person_params <- cbind(ability_params, plausible_values)
    full_person_params <- dplyr::left_join(
      person_params,
      data_obj$metadata %>% dplyr::rename(pid = PupilId_unique)
    )

    # Save person parameters
    wle_path <- file.path(output_dirs$WLE, paste0(format(Sys.Date(), "%Y%m%d"), "_FinalNormMitAnkerGesamt_1dim_WLE_", key, ".csv"))
    message("Saving person parameters to ", wle_path)
    write_table_standard(full_person_params, wle_path)

    # Calculate CTT values
    message("Calculating classical test theory values...")
    ctt_values <- TAM::tam.ctt2(responses, wle_results$theta)
    ctt_values$rowname <- as.character(ctt_values[, "item"])
    ctt_values <- ctt_values[ctt_values$Categ == 1, c("rowname", "N", "RelFreq", "rpb.WLE"), drop = FALSE]
    ctt_values$RelFreq <- ctt_values$RelFreq * 100  # Convert to percentage

    # Combine all item parameters
    message("Creating final item parameter table...")
    final_item_params <- list(item_params, item_fit, ctt_values) %>%
      purrr::reduce(dplyr::left_join) %>%
      dplyr::rename(
        Item = rowname,
        Schwierigkeit = xsi,
        SE = se.xsi,
        "Anzahl Bearbeitungen" = N,
        "Richtiglösungen %" = RelFreq,
        Diskrimination = rpb.WLE
      ) %>%
      dplyr::select(
        Item, "Anzahl Bearbeitungen", "Richtiglösungen %",
        Schwierigkeit, SE, Infit, Infit_t, Outfit, Outfit_t, Diskrimination
      ) %>%
      dplyr::mutate(
        Eignung = (Infit >= config$item_fit$infit$lower & Infit <= config$item_fit$infit$upper) &
          (Outfit >= config$item_fit$outfit$lower & Outfit <= config$item_fit$outfit$upper) &
          (Diskrimination >= config$item_fit$discr_min) &
          (`Richtiglösungen %` >= config$item_fit$correct_pct_min) &
          (Schwierigkeit <= config$item_fit$diff_max)
      )

    # Save item parameters
    item_path <- file.path(output_dirs$Table, paste0(format(Sys.Date(), "%Y%m%d"), "_FinalNormMitAnkerGesamt_ItemParameter_", key, ".csv"))
    message("Saving item parameters to ", item_path)
    write_table_standard(final_item_params, item_path)

    # Create ICC plots
    message("Creating ICC plots...")
    if (dev.cur() != 1) {
      dev.off()  # Ensure device is closed if open
    }

    # Create date prefix
    date_prefix <- format(Sys.Date(), "%Y%m%d")
    # Create absolute output path
    output_path <- file.path(output_dir, paste0(date_prefix, "_ICC_", key, ".pdf"))

    # Create ICC PDF
    tryCatch({
      message("Creating ICC plots for ", key)

      pdf(
        output_path,
        width = 7,
        height = 7,
        onefile = TRUE
      )

      # Plot all ICCs
      TAM::plot(tam_model, items = 1:ncol(tam_model$resp), export = FALSE)

      # Close the device
      dev.off()

      message("ICC plots saved to ", output_path)
      return(output_path)
    }, error = function(e) {
      # Ensure device is closed on error
      if (dev.cur() != 1) {
        dev.off()
      }
      warning("Error creating ICC plots: ", e$message)
      return(NULL)
    })

    # Create distribution plots
    message("Creating distribution plots...")

    # Extract person abilities and item difficulties
    person_abilities <- wle_results$theta
    item_difficulties <- final_item_params$Schwierigkeit


    # Person abilities histogram
    persons_path <- file.path(output_dirs$Verteilung, paste0(format(Sys.Date(), "%Y%m%d"), "_VertPersonen_", key, ".jpg"))
    jpeg(persons_path, width = 1200, height = 900, res = 100)
    hist(
      person_abilities,
      xlim = c(-4.5, 4.5),
      breaks = 40,
      main = paste0(key, " Verteilung Schüler_innenfähigkeiten"),
      ylab = "Häufigkeiten",
      xlab = "",
      cex.main = 2,
      cex.lab = 1.5,
      cex.axis = 1.5
    )
    dev.off()

    # Item difficulties histogram
    items_path <- file.path(output_dirs$Verteilung, paste0(format(Sys.Date(), "%Y%m%d"), "_VertItems_", key, ".jpg"))
    jpeg(items_path, width = 1200, height = 900, res = 100)
    hist(
      item_difficulties,
      xlim = c(-4.5, 4.5),
      breaks = 30,
      main = paste0(key, " Verteilung Aufgabenschwierigkeiten"),
      ylab = "Häufigkeiten",
      xlab = "",
      cex.main = 2,
      cex.lab = 1.5,
      cex.axis = 1.5
    )
    dev.off()

    message("Distribution plots saved to ", output_dirs$Verteilung)

    # Generate summary report
    report_path <- file.path(output_dirs$base, paste0(format(Sys.Date(), "%Y%m%d"), "_Summary_", key, ".txt"))
    message("Generating summary report to ", report_path)

    cat(
      paste0("Summary Report for ", key, "\n"),
      paste0("Date: ", format(Sys.Date(), "%Y-%m-%d"), "\n"),
      paste0("Number of Items: ", nrow(final_item_params), "\n"),
      paste0("Number of Persons: ", nrow(full_person_params), "\n"),
      paste0("WLE Reliability: ", wle_rel, "\n"),
      paste0("Item Difficulty Range: ",
             round(min(final_item_params$Schwierigkeit), 3), " to ",
             round(max(final_item_params$Schwierigkeit), 3), "\n"),
      paste0("Person Ability Range: ",
             round(min(full_person_params$theta), 3), " to ",
             round(max(full_person_params$theta), 3), "\n"),
      paste0("Items with Poor Fit: ",
             sum(!final_item_params$Eignung), " of ",
             nrow(final_item_params), "\n"),
      file = report_path,
      sep = "\n"
    )

    # Log completion
    cat(paste0("Analysis for ", key, " completed successfully at ", format(Sys.time(), "%H:%M:%S"), "\n"),
        file = log_file, append = TRUE)
    cat(paste0("Items analyzed: ", nrow(final_item_params), ", Persons: ", nrow(full_person_params), "\n"),
        file = log_file, append = TRUE)

    message("Completed one-dimensional analysis for ", key)

    # Clean up environment
    if (dev.cur() != 1) {
      dev.off()  # Ensure graphics device is closed
    }

    return(TRUE)
  }, error = function(e) {
    # Log error
    cat(paste0("ERROR in analysis for ", key, ": ", e$message, "\n"),
        file = log_file, append = TRUE)

    # Ensure graphics device is closed on error
    if (dev.cur() != 1) {
      dev.off()
    }

    warning("Analysis failed for ", key, ": ", e$message)
    return(FALSE)
  })
}

# Run analyses for all keys
run_all_1dim_analyses <- function() {
  # Get all keys to analyze
  all_keys <- c(config$subjects$math, config$subjects$english)

  # Track results
  results <- data.frame(
    key = all_keys,
    success = FALSE,
    time_started = NA,
    time_completed = NA,
    stringsAsFactors = FALSE
  )

  # Run analysis for each key
  for (i in seq_along(all_keys)) {
    key <- all_keys[i]

    # Record start time
    results$time_started[i] <- format(Sys.time(), "%H:%M:%S")

    # Run analysis
    results$success[i] <- run_1dim_analysis(key)

    # Record completion time
    results$time_completed[i] <- format(Sys.time(), "%H:%M:%S")

    # Clean up environment after each analysis
    clean_environment(keep = c("config", "run_1dim_analysis", "run_all_1dim_analyses",
                               "results", "i", "key", "all_keys", "log_file"))
  }

  # Save results summary
  write_table_standard(
    results,
    file.path("logs", paste0("1dim_analysis_summary_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  )

  # Log completion
  cat(paste0("\nAll one-dimensional analyses completed at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             "\nSuccessful: ", sum(results$success), " of ", nrow(results), "\n"),
      file = log_file, append = TRUE)

  return(results)
}

# Run all analyses
tryCatch({
  message("Starting one-dimensional IRT analyses...")
  analysis_results <- run_all_1dim_analyses()
  message("All one-dimensional analyses completed!")
  message("Successful: ", sum(analysis_results$success), " of ", nrow(analysis_results))
}, error = function(e) {
  # Log fatal error
  cat(paste0("FATAL ERROR: ", e$message, "\n"),
      file = log_file, append = TRUE)

  # Ensure graphics device is closed
  if (dev.cur() != 1) {
    dev.off()
  }

  stop("Fatal error in analysis: ", e$message)
})
