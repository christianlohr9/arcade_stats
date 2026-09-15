test_that("league input is parsed", {
  expect_equal(app_env$parse_league("PPR")$type, "ppr")
  expect_equal(app_env$parse_league("")$type, "ppr")
  expect_equal(app_env$parse_league("mPPR"), list(type = "mfl", id = "60206", rosters = FALSE))
  expect_equal(app_env$parse_league("mfl22686"), list(type = "mfl", id = "22686"))
  expect_equal(app_env$parse_league(" 1181734873299496960 "), list(type = "sleeper", id = "1181734873299496960"))
  expect_error(app_env$parse_league("ppr league"), "Unbekannte Liga")
})

test_that("single MFL records are turned into data frames", {
  expect_equal(nrow(app_env$as_records(list(id = "1", score = "3.5"))), 1)
  expect_equal(nrow(app_env$as_records(NULL)), 0)
})

test_that("leagues without rosters get a placeholder franchise", {
  df <- data.frame(player_id = "a", WAR = 1)
  out <- app_env$attach_franchises(df, list(type = "mfl", id = "1", rosters = FALSE), 2025)
  expect_equal(out$franchise_name, "Keine Liga ausgewählt")
})
