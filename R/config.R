################################################################################
## File:            config.R
## Project:         LernIRT - Item Response Theory Analysis Framework
## Description:     Configuration handling for IRT analysis
## Author:          [Your Name]
## Created:         [Date]
################################################################################

#' Load configuration from YAML file or create default config
#'
#' @param config_path Path to configuration YAML file
#' @return List with configuration parameters
#' @export
load_config <- function(config_path = "config/analysis_config.yml") {
  # Try to load from YAML file
  if (file.exists(config_path)) {
    tryCatch({
      config <- yaml::read_yaml(config_path)
      message("Configuration loaded from ", config_path)
      return(config)
    }, error = function(e) {
      warning("Failed to load configuration from ", config_path, ": ", e$message)
    })
  }
  
  # Create default configuration
  message("Using default configuration")
  return(create_default_config())
}

#' Create default configuration
#'
#' @return List with default configuration parameters
#' @export
create_default_config <- function() {
  list(
    # Item fit thresholds
    item_fit = list(
      infit = list(lower = 0.8, upper = 1.2),
      outfit = list(lower = 0.8, upper = 1.2),
      discr_min = 0.2,
      correct_pct_min = 5,
      diff_max = 5
    ),
    
    # Subject configurations
    subjects = list(
      math = c("MAT7", "MAT8", "MAT9"),
      english = c("ENG7", "ENG8", "ENG9")
    ),
    
    # Competency areas
    competency_areas = list(
      MAT = c("ZV", "GFDZ", "FR"),
      ENG = c("LES", "HOE", "SIF")
    ),
    
    # Output configuration
    output = list(
      base_path = "_Output",
      date_format = "%Y%m%d"
    ),
    
    # Model configuration
    model = list(
      tam = list(
        control = list(
          increment.factor = 1.05,
          fac.oldxsi = 0.8,
          Msteps = 10,
          maxiter = 1000
        )
      ),
      mirt = list(
        method = "QMCEM"
      )
    )
  )
}

#' Get global configuration object
#'
#' @return Configuration list
#' @export
get_config <- function() {
  # Check if config exists in global environment
  if (!exists("CONFIG", envir = .GlobalEnv)) {
    # Load config if not exists
    CONFIG <<- load_config()
  }
  
  return(CONFIG)
}