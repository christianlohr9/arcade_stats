test_that("IDP positions prefer the fantasy position and fall back to nflverse", {
  expect_equal(
    pipeline_env$idp_position(c("DE", NA, "DL", NA, NA, "WR"), c("OLB", "NT", "DE", "SAF", "DB", "LB")),
    c("DE", "DT", "DE", "S", "CB", "LB")
  )
})

test_that("IDP scoring weights tackles by position", {
  row <- as.data.frame(as.list(setNames(rep(0, length(pipeline_env$DEFENSE_COLUMNS)), pipeline_env$DEFENSE_COLUMNS)))
  row <- row[rep(1, 2), ]
  row$position <- c("DT", "LB")
  row$def_tackles_solo <- 2
  row$def_sacks <- 1
  row$def_sack_yards <- 10

  # DT: 2 * 2.5 tackles, LB: 2 * 1; both -0.5 per sack and +0.2 per sack yard
  expect_equal(pipeline_env$idp_points(row), c(5 - 0.5 + 2, 2 - 0.5 + 2))
})

validate <- function(d, current_season = 2025, current_week = 1) {
  pipeline_env$validate_datasets(d, current_season, current_week, first_season = 2024,
                                 min_rows = c(offense = 1000, defense = 1000))
}

test_that("valid data passes validation", {
  expect_equal(validate(fake_datasets(seasons = 2024:2025)), character())
})

test_that("validation catches duplicates, missing columns, thin seasons and stale weeks", {
  d <- fake_datasets(seasons = 2024:2026, weeks = 1:17)
  d$offense <- dplyr::bind_rows(d$offense, d$offense[1, ])
  d$defense$def_sacks <- NULL
  d$offense <- d$offense[!(d$offense$season == 2026 & d$offense$week > 2), ]
  d$offense <- d$offense[!(d$offense$season == 2025 & d$offense$week > 1), ]
  problems <- validate(d, current_season = 2026, current_week = 6)

  expect_match(problems, "offense: 1 duplicated", all = FALSE)
  expect_match(problems, "defense: missing columns def_sacks", all = FALSE)
  expect_match(problems, "offense: too few rows for seasons 2025", all = FALSE)
  expect_match(problems, "offense: season 2026 only reaches week 2, expected at least 4", all = FALSE)
})

test_that("missing expected points are flagged only for players with touches", {
  d <- fake_datasets(seasons = 2024:2025)
  d$offense$touches[d$offense$season == 2024] <- 0
  d$offense$total_fantasy_points_exp[d$offense$season == 2024] <- NA
  expect_equal(validate(d), character())

  d$offense$touches[d$offense$season == 2024] <- 5
  expect_match(validate(d), "expected points cover only", all = FALSE)
})
