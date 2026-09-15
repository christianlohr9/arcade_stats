#!/usr/bin/env Rscript
# Builds the app datasets and writes them to the output directory.
#
#   Rscript pipeline/build.R [output_dir]
#
# Exits non-zero when validation fails, so a scheduled run never publishes
# broken data. The files are still written for inspection.

source("pipeline/data.R")

out_dir <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(out_dir)) out_dir <- "build"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()
log_step("Current season ", current_season, ", week ", current_week)

datasets <- build_datasets(current_season)

for (name in names(datasets)) {
  path <- file.path(out_dir, paste0(name, ".parquet"))
  nanoparquet::write_parquet(datasets[[name]], path)
  log_step("Wrote ", path, " (", nrow(datasets[[name]]), " rows, ",
           round(file.size(path) / 1024^2, 1), " MB)")
}

manifest <- build_manifest(datasets, current_season, current_week)
jsonlite::write_json(manifest, file.path(out_dir, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)

problems <- validate_datasets(datasets, current_season, current_week)
if (length(problems) > 0) {
  message("Validation failed:\n", paste0("  - ", problems, collapse = "\n"))
  quit(status = 1)
}
log_step("Done")
