test_that("app starts, shows every tab and calculates PPR WAR", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if(is.null(chromote::find_chrome()), "Chrome not available")

  data_dir <- write_fake_release(withr::local_tempdir(), fake_datasets(seasons = 2024:2025))
  withr::local_envvar(ARCADE_DATA_DIR = data_dir)

  app <- shinytest2::AppDriver$new(root, name = "smoke", load_timeout = 60000, timeout = 30000)
  withr::defer(app$stop())
  # DT renders server-side, so rows are counted in the page.
  table_rows <- function(id) {
    app$wait_for_js(sprintf("document.querySelectorAll('#%s tbody tr td:not(.dataTables_empty)').length > 0", id))
    app$get_js(sprintf("document.querySelectorAll('#%s tbody tr').length", id))
  }

  expect_match(app$get_text(".data-status"), "Saison 2025 bis Woche 17")

  tabs <- app$get_js("Array.from(document.querySelectorAll('.navbar-nav a')).map(a => a.textContent.trim())")
  expect_equal(unlist(tabs), c("nflfastR Stats Offense", "nflfastR Stats Defense", "Player Stats Offense",
                               "Wins Above Replacement", "Values", "Help - Get League IDs"))

  expect_gt(table_rows("offense-table"), 0)

  app$run_js("$('.navbar-nav a:contains(\"Wins Above Replacement\")').tab('show')")
  app$click("war-calculate")
  app$wait_for_idle(timeout = 30000)
  expect_gt(table_rows("war-table"), 50)
  app$get_screenshot(file.path(Sys.getenv("SCREENSHOT_DIR", tempdir()), "war.png"))
  expect_null(app$get_js("document.querySelector('.shiny-notification-error')"))
})
