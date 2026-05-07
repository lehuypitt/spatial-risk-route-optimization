# R/00_setup.R
# Project: Spatial Risk Forecasting and Route Optimization
# Purpose: Load required packages for the cleaned SF route-risk project.
#
# Before running the Google Maps route functions, set your API key outside the script:
#   Sys.setenv(GOOGLE_MAPS_API_KEY = "your-key")
# or add it to ~/.Renviron:
#   GOOGLE_MAPS_API_KEY=your-key

required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "janitor",
  "lubridate",
  "tibble",
  "pscl",
  "googleway",
  "RJSONIO"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages before running the project: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))
