# Data store ---------------------------------------------------------------------
#
# The datasets are built by the scheduled GitHub workflow and published as
# assets of the "data" release. The app downloads them on startup and checks
# the manifest again once the cached copy is older than DATA_CHECK_INTERVAL, so
# new data shows up without redeploying.
#
# Set ARCADE_DATA_DIR to read a local pipeline output instead (development).

DATA_RELEASE_URL <- "https://github.com/christianlohr9/arcade_stats/releases/download/data"
DATA_CHECK_INTERVAL <- 60 * 60

data_store <- new.env()

data_source <- function(file) {
  local_dir <- Sys.getenv("ARCADE_DATA_DIR")
  if (nzchar(local_dir)) file.path(local_dir, file) else paste(DATA_RELEASE_URL, file, sep = "/")
}

fetch_file <- function(file) {
  src <- data_source(file)
  if (!grepl("^https?://", src)) return(src)
  dest <- file.path(tempdir(), file)
  curl::curl_download(src, dest, handle = curl::new_handle(followlocation = TRUE, timeout = 300))
  dest
}

read_manifest <- function() {
  jsonlite::read_json(fetch_file("manifest.json"), simplifyVector = TRUE)
}

load_datasets <- function(manifest) {
  list(
    manifest = manifest,
    offense = tibble::as_tibble(nanoparquet::read_parquet(fetch_file("offense.parquet"))),
    defense = tibble::as_tibble(nanoparquet::read_parquet(fetch_file("defense.parquet")))
  )
}

#' Current datasets, refreshed when the published manifest changed
get_datasets <- function(now = Sys.time()) {
  checked_at <- data_store$checked_at
  if (!is.null(data_store$data) && !is.null(checked_at) &&
      difftime(now, checked_at, units = "secs") < DATA_CHECK_INTERVAL) {
    return(data_store$data)
  }

  manifest <- tryCatch(read_manifest(), error = function(e) {
    message("Could not read data manifest: ", conditionMessage(e))
    NULL
  })

  if (is.null(manifest)) {
    if (is.null(data_store$data)) stop("Keine Daten verfügbar: ", data_source("manifest.json"), call. = FALSE)
    return(data_store$data)
  }

  if (is.null(data_store$data) || !identical(manifest$updated_at, data_store$data$manifest$updated_at)) {
    message("Loading datasets from ", data_source(""), " (", manifest$updated_at, ")")
    data_store$data <- load_datasets(manifest)
  }
  data_store$checked_at <- now
  data_store$data
}

data_status_text <- function(manifest) {
  updated <- as.POSIXct(manifest$updated_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  sprintf("Datenstand %s · Saison %s bis Woche %s",
          format(updated, "%d.%m.%Y %H:%M", tz = "Europe/Berlin"),
          max(manifest$seasons), manifest$latest_week)
}
