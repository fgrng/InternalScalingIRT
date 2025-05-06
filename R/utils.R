################################################################################
## File:            utils.R
## Project:         LernIRT - Item Response Theory Analysis Framework
## Description:     Utility functions for IRT analysis
## Author:          [Your Name]
## Created:         [Date]
################################################################################

#' Create necessary output directories for analysis results
#'
#' @param prefix Character string to prefix directory names
#' @param key Analysis key (e.g., "MAT7", "ENG8")
#' @param date Optional date string, defaults to current date
#' @return List with paths to created directories
#' @export
create_output_dirs <- function(prefix, key, date = format(Sys.Date(), "%Y%m%d")) {
  # Create base path with date and key
  output_path_base <- file.path("_Output", paste0(date, "_", prefix, "_", key))
  
  # Define subdirectories
  subdirs <- c("WLE", "Table", "Plots", "Model")
  if (grepl("MultiDim", prefix)) {
    # Add multidimensional model specific directories
    subdirs <- c(subdirs, "FactorAnalysis")
  } else {
    # Add one-dimensional model specific directories
    subdirs <- c(subdirs, "Verteilung", "ICC", "DIF")
  }
  
  # Create base directory
  dir.create(output_path_base, showWarnings = FALSE, recursive = TRUE)
  
  # Create all subdirectories
  paths <- list(base = output_path_base)
  for (subdir in subdirs) {
    path <- file.path(output_path_base, subdir)
    dir.create(path, showWarnings = FALSE)
    paths[[subdir]] <- path
  }
  
  return(paths)
}

#' Standard function for writing tables
#'
#' @param data Data frame to write
#' @param filepath Path to output file
#' @param row_names Whether to include row names
#' @export
write_table_standard <- function(data, filepath, row_names = FALSE) {
  write.table(
    data,
    file = filepath,
    row.names = row_names,
    sep = ";",
    dec = ".",
    na = " "
  )
}

#' Not-in operator for convenience
#'
#' @param x Vector to test
#' @param y Vector to test against
#' @return Logical vector
#' @export
`%notin%` <- Negate(`%in%`)

#' Clean up R environment
#'
#' @param keep Character vector of variable names to keep
#' @param run_gc Whether to run garbage collection
#' @export
clean_environment <- function(keep = NULL, run_gc = TRUE) {
  # Get all objects in global environment
  all_objs <- ls(envir = .GlobalEnv)
  
  # Filter objects to remove
  if (!is.null(keep)) {
    to_remove <- all_objs[all_objs %notin% keep]
  } else {
    to_remove <- all_objs
  }
  
  # Remove objects
  rm(list = to_remove, envir = .GlobalEnv)
  
  # Run garbage collection if requested
  if (run_gc) {
    gc()
  }
}

#' Check if item fits quality criteria
#'
#' @param infit Item infit value
#' @param outfit Item outfit value
#' @param discrimination Item discrimination value
#' @param correct_pct Percentage of correct answers
#' @param difficulty Item difficulty parameter
#' @param config Configuration list with thresholds
#' @return Logical indicating if item meets all criteria
#' @export
check_item_quality <- function(infit, outfit, discrimination, correct_pct, difficulty, config) {
  # Check all quality criteria
  meets_criteria <- 
    (infit >= config$infit$lower) & 
    (infit <= config$infit$upper) &
    (outfit >= config$outfit$lower) & 
    (outfit <= config$outfit$upper) &
    (discrimination >= config$discr_min) &
    (correct_pct >= config$correct_pct_min) &
    (difficulty <= config$diff_max)
  
  return(meets_criteria)
}