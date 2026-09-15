test_that("datasets are loaded once and reloaded when the manifest changes", {
  dir <- withr::local_tempdir()
  write_fake_release(dir, updated_at = "2025-10-01T10:00:00Z")
  withr::local_envvar(ARCADE_DATA_DIR = dir)
  rm(list = ls(app_env$data_store), envir = app_env$data_store)

  now <- as.POSIXct("2025-10-01 12:00:00", tz = "UTC")
  first <- app_env$get_datasets(now)
  expect_equal(first$manifest$updated_at, "2025-10-01T10:00:00Z")
  expect_true(nrow(first$offense) > 0)

  write_fake_release(dir, updated_at = "2025-10-02T10:00:00Z")
  # within the check interval the cached copy is used
  expect_equal(app_env$get_datasets(now + 60)$manifest$updated_at, "2025-10-01T10:00:00Z")
  # afterwards the new manifest is picked up
  later <- now + app_env$DATA_CHECK_INTERVAL + 1
  expect_equal(app_env$get_datasets(later)$manifest$updated_at, "2025-10-02T10:00:00Z")

  # an unreachable source keeps serving the last good data
  withr::local_envvar(ARCADE_DATA_DIR = file.path(dir, "missing"))
  expect_no_warning(res <- suppressMessages(app_env$get_datasets(later + app_env$DATA_CHECK_INTERVAL + 1)))
  expect_equal(res$manifest$updated_at, "2025-10-02T10:00:00Z")
})

test_that("status text shows the latest season and week", {
  text <- app_env$data_status_text(list(updated_at = "2025-10-01T10:00:00Z", seasons = 2024:2025, latest_week = 4))
  expect_equal(text, "Datenstand 01.10.2025 12:00 · Saison 2025 bis Woche 4")
})
