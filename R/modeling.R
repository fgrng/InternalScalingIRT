################################################################################
## File:            modeling.R
## Project:         LernIRT - Item Response Theory Analysis Framework
## Description:     IRT modeling functions
## Author:          [Your Name]
## Created:         [Date]
################################################################################

#' Fit one-dimensional IRT model using TAM
#'
#' @param responses Response data matrix
#' @param pids Person IDs
#' @param weights Person weights
#' @param anchors_matrix Matrix with fixed item parameters
#' @param control List with control parameters for TAM
#' @return Fitted TAM model
#' @export
fit_1dim_model <- function(responses, pids, weights, anchors_matrix, control = NULL) {
  # Log start of model fitting
  message("Fitting one-dimensional TAM model with ", ncol(responses), " items")

  # Create default control if not provided
  if (is.null(control)) {
    config <- get_config()
    control <- config$model$tam$control
  }

  # Fit the TAM model with anchoring
  tryCatch({
    mod <- TAM::tam.mml(
      responses,
      constraint = "cases",
      pid = pids,
      pweights = weights,
      xsi.fixed = anchors_matrix,
      control = control
    )

    message("Model fitting completed successfully")
    return(mod)
  }, error = function(e) {
    stop("Error in TAM model fitting: ", e$message)
  })
}

#' Extract item and person parameters from TAM model
#'
#' @param tam_model Fitted TAM model
#' @param responses Response data matrix
#' @param metadata Person metadata
#' @param config Configuration list with item fit thresholds
#' @return List with item and person parameters
#' @export
extract_1dim_parameters <- function(tam_model, responses, metadata, config = NULL) {
  if (is.null(config)) {
    config <- get_config()$item_fit
  }

  # Extract item parameters
  message("Extracting item parameters")
  item_params <- tam_model$xsi
  item_params <- tibble::rownames_to_column(item_params)

  # Calculate item fit
  message("Calculating item fit statistics")
  item_fit <- TAM::msq.itemfit(tam_model)$itemfit
  item_fit$rowname <- as.character(item_fit[, "item"])
  item_fit <- item_fit[, c("rowname", "Infit", "Infit_t", "Outfit", "Outfit_t"), drop = FALSE]

  # Calculate WLEs and reliability
  message("Calculating ability parameters")
  wle_results <- TAM::tam.wle(tam_model)
  wle_rel <- wle_results[["WLE.rel"]][1]
  names(wle_rel) <- "WLE_Reliability"

  # Extract ability parameters (WLEs)
  ability_params <- wle_results[, c(1, 5, 6)]

  # Generate plausible values
  message("Generating plausible values")
  plausible_values <- TAM::IRT.drawPV(tam_model, NPV = 10)

  # Combine with metadata
  person_params <- cbind(ability_params, plausible_values)
  full_person_params <- dplyr::left_join(
    person_params,
    metadata %>% dplyr::rename(pid = PupilId_unique)
  )

  # Calculate CTT values
  message("Calculating classical test theory values")
  ctt_values <- TAM::tam.ctt2(responses, wle_results$theta)
  ctt_values$rowname <- as.character(ctt_values[, "item"])
  ctt_values <- ctt_values[ctt_values$Categ == 1, c("rowname", "N", "RelFreq", "rpb.WLE"), drop = FALSE]
  ctt_values$RelFreq <- ctt_values$RelFreq * 100

  # Combine all item parameters
  message("Creating final parameter table")
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
      Eignung = (Infit >= config$infit$lower & Infit <= config$infit$upper) &
        (Outfit >= config$outfit$lower & Outfit <= config$outfit$upper) &
        (Diskrimination >= config$discr_min) &
        (`Richtiglösungen %` >= config$correct_pct_min) &
        (Schwierigkeit <= config$diff_max)
    )

  return(list(
    item_params = final_item_params,
    person_params = full_person_params,
    wle_reliability = wle_rel
  ))
}

#' Fit multi-dimensional IRT model using MIRT
#'
#' @param responses Response data matrix
#' @param item_structure Vector mapping items to dimensions
#' @param weights Person weights
#' @param pars_mirt Initial MIRT parameters
#' @param anchors_df Anchors data frame with difficulty values
#' @param method Estimation method
#' @return Fitted MIRT model
#' @export
fit_multidim_model <- function(responses, item_structure, weights, pars_mirt, anchors_df, method = "QMCEM") {
  message("Fitting multi-dimensional MIRT model with ", ncol(responses), " items")

  # Prepare anchor parameters for MIRT
  update_pars <- anchors_df %>%
    dplyr::rename(item = ID) %>%
    dplyr::mutate(est = FALSE, name = "d", value = -difficulty) %>%
    dplyr::select(item, name, value, est)

  # Add configuration for covariance structure
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
  message("Setting up model parameters")
  pars_mirt <- dplyr::rows_update(
    pars_mirt,
    update_pars,
    by = c("item", "name")
  )

  # Setup parallel processing
  message("Setting up parallel processing")
  mirt::mirtCluster()

  # Fit the MIRT model
  tryCatch({
    message("Estimating multi-dimensional model")
    fit_mirt <- mirt::mirt(
      data = responses,
      model = item_structure,
      itemtype = 'Rasch',
      pars = pars_mirt,
      survey.weights = weights,
      method = method
    )

    message("Model fitting completed successfully")
    return(fit_mirt)
  }, error = function(e) {
    # Clean up parallel processing even on error
    mirt::mirtCluster(remove = TRUE)
    stop("Error in MIRT model fitting: ", e$message)
  }, finally = {
    # Clean up parallel processing
    message("Cleaning up parallel resources")
    mirt::mirtCluster(remove = TRUE)
  })
}

#' Extract parameters from MIRT model
#'
#' @param mirt_model Fitted MIRT model
#' @param metadata Person metadata
#' @return List with model parameters
#' @export
extract_multidim_parameters <- function(mirt_model, metadata) {
  message("Extracting multi-dimensional model parameters")

  # Extract model parameters
  model_params <- mirt::coef(mirt_model, simplify = TRUE)

  # Extract person parameters
  message("Calculating person parameters")
  person_params <- mirt::fscores(mirt_model, full.scores.SE = TRUE, method = "EAP")

  # Generate plausible values
  message("Generating plausible values")
  plausible_values <- mirt::fscores(mirt_model, method = "EAP", plausible.draws = 10)

  # Combine with metadata
  message("Combining with metadata")
  full_person_params <- dplyr::bind_cols(metadata, person_params)

  # Format and add plausible values
  for (pv in 1:10) {
    pv_table <- plausible_values[[pv]]
    colnames(pv_table) <- c(
      paste0("PV", pv, "_F1"),
      paste0("PV", pv, "_F2"),
      paste0("PV", pv, "_F3")
    )
    full_person_params <- dplyr::bind_cols(full_person_params, pv_table)
  }

  return(list(
    item_params = model_params$items,
    covariance = model_params$cov,
    means = model_params$means,
    person_params = full_person_params
  ))
}
