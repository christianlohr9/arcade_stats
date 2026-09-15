war_stats <- function(datasets = fake_datasets(seasons = 2025)) {
  app_env$war_input(datasets$offense, datasets$defense, list(type = "ppr"), 2025, 1:17)
}

test_that("replacement-level starter has zero WAR and better starters have more", {
  settings <- app_env$default_war_settings()
  war <- app_env$compute_war(war_stats(), 2025, 1:17, settings)

  rb <- war[war$Position == "RB", ]
  totals <- war_stats() |>
    dplyr::filter(position == "RB") |>
    dplyr::summarise(total = sum(fantasy_points_league), .by = "player_id") |>
    dplyr::arrange(dplyr::desc(total))
  replacement <- totals$player_id[settings$teams * settings$slots[["RB"]]]
  best <- totals$player_id[1]

  expect_equal(rb$WAR[rb$player_id == replacement], 0)
  expect_gt(rb$WAR[rb$player_id == best], 0)
  expect_true(all(war$Avr_Win_Percent > 0 & war$Avr_Win_Percent < 1))
})

test_that("positions without a starting slot are left out", {
  settings <- app_env$default_war_settings()
  settings$slots[["TE"]] <- 0
  war <- app_env$compute_war(war_stats(), 2025, 1:17, settings)

  expect_false("TE" %in% war$Position)
  expect_setequal(unique(war$Position), c("QB", "RB", "WR"))
  expect_false(any(c("DI", "DE", "LB", "CB", "S") %in% war$Position))
})

test_that("IDP slots add defensive players", {
  settings <- app_env$default_war_settings()
  settings$slots[c("DT", "LB")] <- c(1, 2)
  war <- app_env$compute_war(war_stats(), 2025, 1:17, settings)

  expect_true(all(c("DI", "LB") %in% war$Position))
  expect_false("CB" %in% war$Position)
})

test_that("min_games filters players and the week selection is respected", {
  settings <- app_env$default_war_settings()
  settings$min_games <- 5
  expect_equal(nrow(app_env$compute_war(war_stats(), 2025, 1:3, settings)), 0)

  settings$min_games <- 1
  one_week <- app_env$compute_war(war_stats(), 2025, 4, settings)
  expect_true(all(one_week$Games == 1))
})

test_that("empty input returns an empty result with the expected columns", {
  war <- app_env$compute_war(war_stats(), 1999, 1:17)
  expect_equal(nrow(war), 0)
  expect_true(all(c("player", "Position", "WAR", "WAA", "consistency") %in% names(war)))
})

test_that("team values sum the best WAR per slot and franchise", {
  war <- tibble::tibble(
    franchise_name = c("A", "A", "A", "B", "B", "Free Agent"),
    Position = c("QB", "QB", "RB", "QB", "WR", "QB"),
    WAR = c(2, 1, 0.5, 1.5, 3, 9)
  )
  settings <- list(slots = c(QB = 1, RB = 2, WR = 3, TE = 1))
  values <- app_env$compute_team_values(war, settings)

  expect_equal(values$Team, c("B", "A"))
  expect_equal(values$OVR, c(4.5, 2.5))
  expect_equal(values$QB[values$Team == "A"], 2)
})
