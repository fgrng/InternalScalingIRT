################################################################################
## File:            reporting.R
## Project:         LernIRT - Item Response Theory Analysis Framework
## Description:     Output and reporting functions for IRT analysis
## Author:          [Your Name]
## Created:         [Date]
################################################################################

#' Save TAM model and parameters to output directory
#'
#' @param tam_model Fitted TAM model
#' @param results Results from extract_1dim_parameters
#' @param key Analysis key
#' @param dir_paths Directory paths from create_output_dirs
#' @return List with paths to created files
#' @export
save_1dim_outputs <- function(tam_model, results, key, dir_paths) {
  # Create date prefix
  date_prefix <- format(Sys.Date(), "%Y%m%d")

  message("Saving one-dimensional model results for ", key)

  # Define output paths
  model_path <- file.path(dir_paths$base, paste0("Skalierung_tam-mml_", key, ".RDS"))
  wle_path <- file.path(dir_paths$WLE, paste0(date_prefix, "_FinalNormMitAnkerGesamt_1dim_WLE_", key, ".csv"))
  item_path <- file.path(dir_paths$Table, paste0(date_prefix, "_FinalNormMitAnkerGesamt_ItemParameter_", key, ".csv"))
  rel_path <- file.path(dir_paths$Table, paste0(date_prefix, "_FinalNormMitAnkerGesamt_WLE-Reliability_", key, ".csv"))

  # Save RDS of the model
  message("Saving model to ", model_path)
  saveRDS(tam_model, file = model_path)

  # Save WLE parameters
  message("Saving person parameters to ", wle_path)
  write_table_standard(results$person_params, wle_path)

  # Save item parameters
  message("Saving item parameters to ", item_path)
  write_table_standard(results$item_params, item_path)

  # Save WLE reliability
  message("Saving reliability to ", rel_path)
  write_table_standard(results$wle_reliability, rel_path, row_names = TRUE)

  return(list(
    model = model_path,
    wle = wle_path,
    item = item_path,
    reliability = rel_path
  ))
}

#' Save MIRT model and parameters to output directory
#'
#' @param mirt_model Fitted MIRT model
#' @param results Results from extract_multidim_parameters
#' @param mirt_structure MIRT structure including initial parameters
#' @param key Analysis key
#' @param dir_paths Directory paths from create_output_dirs
#' @return List with paths to created files
#' @export
save_multidim_outputs <- function(mirt_model, results, mirt_structure, key, dir_paths) {
  # Create date prefix
  date_prefix <- format(Sys.Date(), "%Y%m%d")

  message("Saving multi-dimensional model results for ", key)

  # Define output paths
  model_path <- file.path(dir_paths$base, paste0("Skalierung_mirt-3dim_", key, ".RDS"))
  param_spec_path <- file.path(dir_paths$Table, paste0("ParameterSpecification_mirt-3dim_", key, ".csv"))
  item_path <- file.path(dir_paths$Table, paste0("ItemParameter_mirt-3dim_", key, ".csv"))
  cov_path <- file.path(dir_paths$Table, paste0("FactorCovMatrix_mirt-3dim_", key, ".csv"))
  means_path <- file.path(dir_paths$Table, paste0("FactorMeans_mirt-3dim_", key, ".csv"))
  person_path <- file.path(dir_paths$WLE, paste0("PersonParameter_mirt-3dim_", key, ".csv"))

  # Save RDS of the model
  message("Saving model to ", model_path)
  saveRDS(mirt_model, file = model_path)

  # Save parameter specifications
  message("Saving parameter specifications to ", param_spec_path)
  write_table_standard(mirt_structure$pars.mirt, param_spec_path, row_names = TRUE)

  # Save item parameters
  message("Saving item parameters to ", item_path)
  write_table_standard(results$item_params, item_path, row_names = TRUE)

  # Save covariance matrix
  message("Saving factor covariance matrix to ", cov_path)
  write_table_standard(results$covariance, cov_path, row_names = TRUE)

  # Save factor means
  message("Saving factor means to ", means_path)
  write_table_standard(results$means, means_path, row_names = TRUE)

  # Save person parameters
  message("Saving person parameters to ", person_path)
  write_table_standard(results$person_params, person_path)

  return(list(
    model = model_path,
    param_spec = param_spec_path,
    item = item_path,
    covariance = cov_path,
    means = means_path,
    person = person_path
  ))
}

#' Generate a summary report of one-dimensional IRT analysis results
#'
#' @param results List of results from extract_1dim_parameters
#' @param key Analysis key
#' @param output_dir Output directory for report
#' @return Path to created report
#' @export
generate_1dim_summary_report <- function(results, key, output_dir) {
  # Create date prefix
  date_prefix <- format(Sys.Date(), "%Y%m%d")

  # Define output path
  report_path <- file.path(output_dir, paste0(date_prefix, "_Summary_1dim_", key, ".txt"))

  # Create report content
  message("Generating one-dimensional summary report for ", key)

  # Create report with key statistics
  tryCatch({
    cat(
      paste0("One-Dimensional IRT Analysis Summary Report for ", key, "\n"),
      paste0("Date: ", format(Sys.Date(), "%Y-%m-%d"), "\n"),
      paste0("Number of Items: ", nrow(results$item_params), "\n"),
      paste0("Number of Persons: ", nrow(results$person_params), "\n"),
      paste0("WLE Reliability: ", results$wle_reliability, "\n"),
      paste0("Item Difficulty Range: ",
             round(min(results$item_params$Schwierigkeit), 3), " to ",
             round(max(results$item_params$Schwierigkeit), 3), "\n"),
      paste0("Person Ability Range: ",
             round(min(results$person_params$theta), 3), " to ",
             round(max(results$person_params$theta), 3), "\n"),
      paste0("Items with Poor Fit: ",
             sum(!results$item_params$Eignung), " of ",
             nrow(results$item_params), "\n"),
      paste0("Mean Infit: ", round(mean(results$item_params$Infit, na.rm = TRUE), 3), "\n"),
      paste0("Mean Outfit: ", round(mean(results$item_params$Outfit, na.rm = TRUE), 3), "\n"),
      paste0("Mean Discrimination: ", round(mean(results$item_params$Diskrimination, na.rm = TRUE), 3), "\n"),
      file = report_path,
      sep = "\n"
    )

    message("Summary report saved to ", report_path)
    return(report_path)
  }, error = function(e) {
    warning("Error generating summary report: ", e$message)
    return(NULL)
  })
}

#' Generate a summary report of multi-dimensional IRT analysis results
#'
#' @param results List of results from extract_multidim_parameters
#' @param key Analysis key
#' @param output_dir Output directory for report
#' @return Path to created report
#' @export
generate_multidim_summary_report <- function(results, key, output_dir) {
  # Create date prefix
  date_prefix <- format(Sys.Date(), "%Y%m%d")

  # Define output path
  report_path <- file.path(output_dir, paste0(date_prefix, "_Summary_multidim_", key, ".txt"))

  # Create report content
  message("Generating multi-dimensional summary report for ", key)

  # Create report with key statistics
  tryCatch({
    cat(
      paste0("Multi-Dimensional IRT Analysis Summary Report for ", key, "\n"),
      paste0("Date: ", format(Sys.Date(), "%Y-%m-%d"), "\n"),
      paste0("Number of Items: ", nrow(results$item_params), "\n"),
      paste0("Number of Persons: ", nrow(results$person_params), "\n"),
      paste0("Number of Dimensions: 3\n"),
      paste0("Factor Correlations:\n",
             "  F1-F2: ", round(results$covariance[2,1], 3), "\n",
             "  F1-F3: ", round(results$covariance[3,1], 3), "\n",
             "  F2-F3: ", round(results$covariance[3,2], 3), "\n"),
      paste0("Factor Means:\n",
             "  F1: ", round(results$means[1], 3), "\n",
             "  F2: ", round(results$means[2], 3), "\n",
             "  F3: ", round(results$means[3], 3), "\n"),
      paste0("Factor Variances:\n",
             "  F1: ", round(results$covariance[1,1], 3), "\n",
             "  F2: ", round(results$covariance[2,2], 3), "\n",
             "  F3: ", round(results$covariance[3,3], 3), "\n"),
      file = report_path,
      sep = "\n"
    )

    message("Summary report saved to ", report_path)
    return(report_path)
  }, error = function(e) {
    warning("Error generating summary report: ", e$message)
    return(NULL)
  })
}

#' Create a PDF report with details about item parameters
#'
#' @param results List of results from extract_1dim_parameters
#' @param key Analysis key
#' @param output_dir Output directory for report
#' @return Path to created report
#' @export
create_item_report <- function(results, key, output_dir) {
  # Check if required packages are available
  if (!requireNamespace("knitr", quietly = TRUE) ||
      !requireNamespace("rmarkdown", quietly = TRUE)) {
    warning("Packages 'knitr' and 'rmarkdown' needed for this function to work.")
    return(NULL)
  }

  # Create date prefix
  date_prefix <- format(Sys.Date(), "%Y%m%d")

  # Define output path
  report_path <- file.path(output_dir, paste0(date_prefix, "_ItemReport_", key, ".pdf"))

  # Create temporary Rmd file
  temp_rmd <- tempfile(fileext = ".Rmd")

  # Write Rmd content
  cat(
    "---",
    "title: \"Item Parameter Report\"",
    paste0("subtitle: \"", key, "\""),
    "date: \"`r format(Sys.Date(), '%B %d, %Y')`\"",
    "output: pdf_document",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE)",
    "library(knitr)",
    "library(ggplot2)",
    "library(dplyr)",
    "```",
    "",
    "## Item Parameters",
    "",
    "```{r item-table}",
    "kable(item_params %>% select(Item, Schwierigkeit, SE, Infit, Outfit, Diskrimination, Eignung),",
    "      caption = 'Item Parameters',",
    "      digits = 3)",
    "```",
    "",
    "## Distribution of Item Difficulties",
    "",
    "```{r difficulty-dist, fig.height=4}",
    "ggplot(item_params, aes(x = Schwierigkeit)) +",
    "  geom_histogram(bins = 30, fill = 'steelblue', color = 'black') +",
    "  labs(title = 'Distribution of Item Difficulties',",
    "       x = 'Difficulty',",
    "       y = 'Count') +",
    "  theme_minimal()",
    "```",
    "",
    "## Infit vs. Outfit",
    "",
    "```{r infit-outfit, fig.height=4}",
    "ggplot(item_params, aes(x = Infit, y = Outfit, color = Eignung)) +",
    "  geom_point() +",
    "  geom_hline(yintercept = c(0.8, 1.2), linetype = 'dashed') +",
    "  geom_vline(xintercept = c(0.8, 1.2), linetype = 'dashed') +",
    "  labs(title = 'Infit vs. Outfit',",
    "       color = 'Meets Criteria') +",
    "  theme_minimal()",
    "```",
    "",
    "## Items with Poor Fit",
    "",
    "```{r poor-fit}",
    "poor_fit <- item_params %>% filter(!Eignung) %>%",
    "  select(Item, Schwierigkeit, Infit, Outfit, Diskrimination, `Richtiglösungen %`)",
    "if(nrow(poor_fit) > 0) {",
    "  kable(poor_fit, caption = 'Items with Poor Fit', digits = 3)",
    "} else {",
    "  cat('All items meet quality criteria.')",
    "}",
    "```",
    file = temp_rmd,
    sep = "\n"
  )

  # Try to render the report
  tryCatch({
    message("Creating item parameter report for ", key)

    # Render the report
    rmarkdown::render(
      input = temp_rmd,
      output_file = report_path,
      envir = new.env(),
      params = list(
        item_params = results$item_params
      )
    )

    message("Item report saved to ", report_path)
    return(report_path)
  }, error = function(e) {
    warning("Error creating item report: ", e$message)
    return(NULL)
  }, finally = {
    # Clean up temporary file
    if (file.exists(temp_rmd)) file.remove(temp_rmd)
  })
}

#' Export results to Excel format
#'
#' @param results List of results from IRT analysis
#' @param key Analysis key
#' @param output_dir Output directory
#' @return Path to created Excel file
#' @export
export_to_excel <- function(results, key, output_dir) {
  # Check if required package is available
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("Package 'openxlsx' needed for this function to work.")
    return(NULL)
  }

  # Create date prefix
  date_prefix <- format(Sys.Date(), "%Y%m%d")

  # Define output path
  excel_path <- file.path(output_dir, paste0(date_prefix, "_Results_", key, ".xlsx"))

  # Try to create Excel file
  tryCatch({
    message("Creating Excel report for ", key)

    # Create workbook
    wb <- openxlsx::createWorkbook()

    # Add item parameter sheet
    openxlsx::addWorksheet(wb, "Item Parameters")
    openxlsx::writeData(wb, "Item Parameters", results$item_params)

    # Add styling
    header_style <- openxlsx::createStyle(textDecoration = "bold")
    openxlsx::addStyle(wb, "Item Parameters", header_style, rows = 1, cols = 1:ncol(results$item_params))

    # If person parameters exist, add them
    if (!is.null(results$person_params)) {
      # Add a sample of person parameters (first 1000 rows)
      openxlsx::addWorksheet(wb, "Person Parameters (Sample)")
      person_sample <- results$person_params
      if (nrow(person_sample) > 1000) {
        person_sample <- person_sample[1:1000,]
      }
      openxlsx::writeData(wb, "Person Parameters (Sample)", person_sample)
      openxlsx::addStyle(wb, "Person Parameters (Sample)", header_style,
                         rows = 1, cols = 1:ncol(person_sample))
    }

    # If model statistics exist, add them
    if (!is.null(results$wle_reliability)) {
      openxlsx::addWorksheet(wb, "Model Statistics")
      model_stats <- data.frame(
        Statistic = c("WLE Reliability"),
        Value = c(as.numeric(results$wle_reliability))
      )
      openxlsx::writeData(wb, "Model Statistics", model_stats)
      openxlsx::addStyle(wb, "Model Statistics", header_style, rows = 1, cols = 1:2)
    }

    # Save workbook
    openxlsx::saveWorkbook(wb, file = excel_path, overwrite = TRUE)

    message("Excel report saved to ", excel_path)
    return(excel_path)
  }, error = function(e) {
    warning("Error creating Excel report: ", e$message)
    return(NULL)
  })
}
