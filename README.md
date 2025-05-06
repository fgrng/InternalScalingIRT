# InternalScalingIRT

An integrated R environment for IRT analysis used in specific projects by the author (standardized workflows for educational assessment data, combining TAM and mirt packages with data processing and reporting).

## Project Structure

- `R/`: Core functions and utilities
  - `utils.R`: Helper functions
  - `config.R`: Configuration parameters
  - `data_processing.R`: Data loading and preparation
  - `modeling.R`: IRT modeling functions
  - `visualization.R`: Plotting functions
  - `reporting.R`: Output and reporting

- `scripts/`: Main analysis scripts
  - `01_data_prep.R`: Data preparation
  - `02_1dim_irt.R`: One-dimensional IRT
  - `03_multidim_irt.R`: Multi-dimensional IRT

- `config/`: External configuration
  - `analysis_config.yml`: Configuration parameters

## Dependencies

- tidyverse (dplyr, tidyr, ggplot2)
- TAM
- mirt
- readxl
- yaml

## Usage

1. Configure the analysis in `config/analysis_config.yml`
2. Run data preparation: `Rscript scripts/01_data_prep.R`
3. Run one-dimensional analysis: `Rscript scripts/02_1dim_irt.R`
4. Run multi-dimensional analysis: `Rscript scripts/03_multidim_irt.R`
