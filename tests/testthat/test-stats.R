test_that("offense summary aggregates per player", {
  d <- fake_datasets(seasons = 2025)
  out <- app_env$summarise_offense(d$offense, 2025, 1:17, unique(d$offense$team), "QB", "Passing")

  expect_equal(length(unique(out$Player)), 30)
  expect_equal(names(out)[1:3], c("Player", "Position", "Team"))
  expect_equal(sum(out$Att), sum(d$offense$attempts[d$offense$position == "QB"]))
})

test_that("defense summary applies thresholds", {
  d <- fake_datasets(seasons = 2025)
  all <- app_env$summarise_defense(d$defense, 2025, 1:17, "KC", c("LB", "CB"))
  some <- app_env$summarise_defense(d$defense, 2025, 1:17, "KC", c("LB", "CB"), min_sacks = 1e6)

  expect_equal(nrow(all), 40)
  expect_equal(nrow(some), 0)
})

test_that("player stats without a league use PPR and expected points", {
  d <- fake_datasets(seasons = 2025)
  out <- app_env$player_league_stats(d$offense, 2025, 1:17, unique(d$offense$team), c("WR", "TE"),
                                     "Rushing/Receiving")

  expect_equal(ncol(out), length(app_env$PLAYER_STATS_COLNAMES))
  expect_true(all(out$franchise_name == "Keine Liga ausgewählt"))
  expect_true(all(out$FPTS_League == 0))
  expect_equal(out$rank, seq_len(nrow(out)))
})

test_that("week and season choices come from the data", {
  d <- fake_datasets(seasons = 2024:2025, weeks = 1:3)
  expect_equal(app_env$season_choices(d$offense), c(2025, 2024))
  expect_equal(app_env$week_choices(d$offense, 2025), 1:3)
  expect_equal(app_env$week_choices(d$offense, 1999), app_env$FANTASY_SEASON_WEEKS)
})
