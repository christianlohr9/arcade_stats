#!/usr/bin/env Rscript

#' Update Data Script for Arcade Stats
#' 
#' Run this script to regenerate all data files used by the Shiny app.
#' Usage: Rscript update_data.R [year1] [year2] ...
#' Example: Rscript update_data.R 2024
#' Example: Rscript update_data.R 2023 2024

# Setup
suppressPackageStartupMessages({
  library(here)
  source("R/data_generation.R")
})

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Default to current year if no args provided
if (length(args) == 0) {
  years <- as.integer(format(Sys.Date(), "%Y"))
  message("No years specified. Using current year: ", years)
} else {
  years <- as.integer(args)
  message("Processing years: ", paste(years, collapse = ", "))
}

# Validate years
if (any(is.na(years)) || any(years < 1999) || any(years > as.integer(format(Sys.Date(), "%Y")) + 1)) {
  stop("Invalid years specified. Years must be between 1999 and ", as.integer(format(Sys.Date(), "%Y")) + 1)
}

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
  message("Created data/ directory")
}

# Run data generation
tryCatch({
  result <- update_all_data(years)
  
  message("\n=== SUCCESS ===")
  message("Data generation completed successfully!")
  message("Files created/updated:")
  
  # List all RDS files in data directory with timestamps
  rds_files <- list.files("data", pattern = "\\.rds$", full.names = TRUE)
  for (file in rds_files) {
    info <- file.info(file)
    message(sprintf("  %s (%.2f MB, %s)", 
                   basename(file), 
                   info$size / 1024^2,
                   format(info$mtime, "%Y-%m-%d %H:%M")))
  }
  
}, error = function(e) {
  message("\n=== ERROR ===")
  message("Data generation failed with error:")
  message(e$message)
  quit(status = 1)
})

message("\nData is ready for the Shiny app!")