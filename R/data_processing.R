################################################################################
## File:            data_processing.R
## Project:         LernIRT - Item Response Theory Analysis Framework
## Description:     Data loading and preparation functions for IRT analysis
## Author:          [Your Name]
## Created:         [Date]
################################################################################

#' Load metadata, responses, and items for a specific analysis key
#'
#' @param key Analysis key (e.g., "MAT7", "ENG8")
#' @param rdata_path Path to RData directory
#' @return List with loaded data components
#' @export
load_analysis_data <- function(key, rdata_path = "Rdata") {
  # Load metadata for output table
  metadata_path <- file.path(rdata_path, paste0("CleanData_pids_", key, ".rds"))
  if (!file.exists(metadata_path)) {
    stop("Metadata file not found: ", metadata_path)
  }
  metadata <- readRDS(metadata_path)
  
  # Load answer data
  answers_path <- file.path(rdata_path, paste0("CleanData_answers_", key, ".rds"))
  if (!file.exists(answers_path)) {
    stop("Answers file not found: ", answers_path)
  }
  answers <- readRDS(answers_path)
  
  # Extract person IDs and weights
  pids <- metadata$PupilId_unique
  weights <- metadata$Gewicht
  
  # Load anchor data
  anchors_path <- file.path(rdata_path, paste0("CleanData_anchor_", key, ".rds"))
  if (!file.exists(anchors_path)) {
    stop("Anchors file not found: ", anchors_path)
  }
  anchors <- readRDS(anchors_path)
  
  # Process anchors
  anchors <- anchors %>%
    dplyr::mutate(ID = paste0(TestdriveID, "_TaskStatusId")) %>%
    dplyr::filter(ID %in% colnames(answers))
  
  # Load items for dimension mapping
  items_path <- file.path(rdata_path, paste0("CleanData_items_", key, ".rds"))
  if (!file.exists(items_path)) {
    stop("Items file not found: ", items_path)
  }
  items <- readRDS(items_path)
  
  # Load selected items for final normalization
  subject_key <- substr(key, 1, 3)  # Extract subject from key
  items_list_path <- paste0("ubersicht_items_", subject_key, ".xlsx")
  if (!file.exists(items_list_path)) {
    stop("Items list file not found: ", items_list_path)
  }
  chosen_items <- readxl::read_excel(items_list_path) %>%
    dplyr::select("Item", "ID", "InPool") %>%
    dplyr::filter(InPool == 1)
  
  return(list(
    metadata = metadata,
    answers = answers,
    pids = pids,
    weights = weights,
    anchors = anchors,
    items = items,
    chosen_items = chosen_items
  ))
}

#' Prepare response data by filtering to relevant columns and reordering
#'
#' @param data_obj Data object from load_analysis_data
#' @param key Analysis key
#' @return Processed response data frame
#' @export
prepare_response_data <- function(data_obj, key) {
  # Extract subject from key
  subject <- substr(key, 1, 3)
  
  # Get competency areas
  config <- get_config()
  comp_areas <- config$competency_areas[[subject]]
  
  # Start with answers data
  responses <- data_obj$answers
  
  # Reorder columns by competency area (for multidimensional models)
  if (!is.null(comp_areas)) {
    for (kb in comp_areas) {
      # Relocate all items from this competency area to the start
      kb_items <- data_obj$items %>%
        dplyr::filter(Kompetenzbereich == kb) %>%
        dplyr::pull(TestdriveID)
      
      responses <- responses %>% dplyr::relocate(dplyr::any_of(kb_items))
      
      # Relocate all anchor items from this competency area to the start
      kb_anchors <- data_obj$anchors %>%
        dplyr::filter(Kompetenzbereich == kb) %>%
        dplyr::pull(ID)
      
      responses <- responses %>% dplyr::relocate(dplyr::any_of(kb_anchors))
    }
  }
  
  # Filter to chosen items and anchors
  responses <- responses %>% dplyr::select(
    dplyr::any_of(data_obj$anchors$ID),
    dplyr::any_of(data_obj$chosen_items$Item)
  )
  
  return(responses)
}

#' Prepare anchors for TAM model
#'
#' @param anchors Anchors data frame
#' @param difficulty_col Name of difficulty column to use
#' @return Matrix for use with TAM xsi.fixed parameter
#' @export
prepare_anchors_tam <- function(anchors, difficulty_col = "DifficultyOverall") {
  anchors %>%
    dplyr::select(dplyr::all_of(difficulty_col)) %>%
    dplyr::bind_cols(
      tibble::tibble(item_pos = seq(1, nrow(.))),
      .
    ) %>%
    as.matrix()
}

#' Prepare dimensions structure for MIRT model
#'
#' @param data_obj Data object from load_analysis_data
#' @param responses Processed response data frame
#' @param key Analysis key
#' @return List with MIRT model structure components
#' @export
prepare_mirt_structure <- function(data_obj, responses, key) {
  # Extract subject from key
  subject <- substr(key, 1, 3)
  
  # Map items to dimensions based on competency areas
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
  
  return(list(
    item_structure = item_structure,
    dummy_answers = dummy_answers,
    pars.mirt = pars.mirt
  ))
}