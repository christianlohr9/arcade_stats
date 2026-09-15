# Wins Above Replacement --------------------------------------------------------
#
# A fantasy team's weekly score is modelled as normal(weekfp, weeksd), built from
# the average starter at every roster slot. A player's weekly win probability is
# pnorm() of that team score with the player swapped in for an average starter.
# WAR compares the player's expected wins against the replacement-level player
# (the worst starter at the position), WAA against the worst starter by win%.

WAR_POSITIONS <- c("QB", "RB", "WR", "TE", "DT", "DE", "LB", "CB", "S")

# Positions that feed the flex slots.
FLEX_POOLS <- list(
  REC  = c("WR", "TE"),
  FLEX = c("RB", "WR", "TE"),
  IDP  = c("DT", "DE", "LB", "CB", "S")
)

default_war_settings <- function() {
  list(
    teams = 12,
    min_games = 1,
    slots = c(QB = 1, RB = 2, WR = 3, TE = 1, REC = 0, FLEX = 1,
              DT = 0, DE = 0, LB = 0, CB = 0, S = 0, IDP = 0)
  )
}

# Player IDs ranked in the top `n` by total points (ties kept, like top_n()).
top_ids <- function(totals, n) {
  if (n <= 0 || nrow(totals) == 0) return(character())
  totals$player_id[dplyr::min_rank(dplyr::desc(totals$total)) <= n]
}

# The flex-eligible part of a position: from the best (starters + teams)
# players, the lower starters-many.
flex_ids <- function(totals, starters, teams) {
  n <- starters * teams
  if (n <= 0 || nrow(totals) == 0) return(character())
  pool <- totals[dplyr::min_rank(dplyr::desc(totals$total)) <= n + teams, ]
  pool$player_id[dplyr::min_rank(pool$total) <= n]
}

#' Compute WAR for one season and a set of weeks
#'
#' @param stats data frame with player_id, player_name, position, season, week
#'   and fantasy_points_league (one row per player-week).
#' @param season single season.
#' @param weeks integer vector of weeks to include.
#' @param settings list as returned by default_war_settings().
compute_war <- function(stats, season, weeks, settings = default_war_settings()) {
  teams <- as.numeric(settings$teams)
  slots <- settings$slots

  games <- stats |>
    dplyr::filter(.data$season == !!season, .data$week %in% weeks,
                  .data$position %in% WAR_POSITIONS,
                  !is.na(.data$fantasy_points_league)) |>
    dplyr::transmute(player_id = as.character(.data$player_id),
                     position = as.character(.data$position),
                     pts = .data$fantasy_points_league)

  if (nrow(games) == 0) return(empty_war())

  totals <- games |>
    dplyr::summarise(total = sum(.data$pts), .by = c("player_id", "position"))
  totals_by_pos <- split(totals, totals$position)
  pos_totals <- function(pos) totals_by_pos[[pos]] %||% totals[0, ]

  starter_ids <- lapply(stats::setNames(WAR_POSITIONS, WAR_POSITIONS), function(pos) {
    top_ids(pos_totals(pos), slots[[pos]] * teams)
  })
  starter_games <- lapply(stats::setNames(WAR_POSITIONS, WAR_POSITIONS), function(pos) {
    games$pts[games$position == pos & games$player_id %in% starter_ids[[pos]]]
  })

  flex_games <- lapply(FLEX_POOLS, function(positions) {
    pool <- games[0, ]
    for (pos in positions) {
      ids <- flex_ids(pos_totals(pos), slots[[pos]], teams)
      pool <- dplyr::bind_rows(pool, games[games$position == pos & games$player_id %in% ids, ])
    }
    pool
  })
  flex_starter_games <- lapply(names(FLEX_POOLS), function(slot) {
    pool <- flex_games[[slot]]
    pool_totals <- dplyr::summarise(pool, total = sum(.data$pts), .by = "player_id")
    pool$pts[pool$player_id %in% top_ids(pool_totals, slots[[slot]] * teams)]
  })
  names(flex_starter_games) <- names(FLEX_POOLS)

  slot_games <- c(starter_games, flex_starter_games)
  active <- names(slots)[slots > 0]

  weekfp <- sum(vapply(active, function(s) mean(slot_games[[s]], na.rm = TRUE) * slots[[s]], numeric(1)))
  weeksd <- sqrt(sum(vapply(active, function(s) stats::sd(slot_games[[s]], na.rm = TRUE)^2 * slots[[s]], numeric(1))))

  result <- lapply(intersect(WAR_POSITIONS, active), function(pos) {
    war_for_position(games[games$position == pos, ], pos, pos_totals(pos),
                     starters = slots[[pos]] * teams,
                     starter_mean = mean(starter_games[[pos]], na.rm = TRUE),
                     weekfp = weekfp, weeksd = weeksd)
  }) |>
    dplyr::bind_rows()

  if (nrow(result) == 0) return(empty_war())

  result |>
    dplyr::filter(.data$Games >= as.numeric(settings$min_games), !is.na(.data$WAR)) |>
    dplyr::mutate(season = as.integer(season), consistency = .data$consistency_temp * .data$WAR) |>
    dplyr::select(-"consistency_temp") |>
    dplyr::left_join(
      stats |>
        dplyr::filter(.data$season == !!season) |>
        dplyr::summarise(player = dplyr::first(.data$player_name), .by = "player_id") |>
        dplyr::mutate(player_id = as.character(.data$player_id)),
      by = "player_id"
    ) |>
    dplyr::arrange(dplyr::desc(.data$WAR))
}

war_for_position <- function(pos_games, pos, totals, starters, starter_mean, weekfp, weeksd) {
  if (nrow(pos_games) == 0 || starters <= 0) return(NULL)

  top <- totals[totals$player_id %in% top_ids(totals, starters), ]
  if (nrow(top) == 0) return(NULL)
  worst <- top[top$total == min(top$total), ]
  replacement_id <- min(worst$player_id)

  pos_games$win_prob <- stats::pnorm(pos_games$pts - starter_mean + weekfp, weekfp, weeksd)

  replacement_wp <- mean(pos_games$win_prob[pos_games$player_id == replacement_id])

  win_pct <- dplyr::summarise(pos_games, total = mean(.data$win_prob), .by = "player_id")
  average_wp <- min(win_pct$total[win_pct$player_id %in% top_ids(win_pct, starters)])

  pos_games |>
    dplyr::summarise(
      Position = if (pos == "DT") "DI" else pos,
      Ave_Week_Points = round(mean(.data$pts), 2),
      Avr_Win_Percent = mean(.data$win_prob),
      Games = dplyr::n(),
      WAR = round(.data$Avr_Win_Percent * .data$Games - replacement_wp * .data$Games, 2),
      WAA = round(.data$Avr_Win_Percent * .data$Games - average_wp * .data$Games, 2),
      consistency_temp = stats::sd(.data$win_prob),
      .by = "player_id"
    )
}

empty_war <- function() {
  tibble::tibble(
    player_id = character(), Position = character(), Ave_Week_Points = numeric(),
    Avr_Win_Percent = numeric(), Games = integer(), WAR = numeric(), WAA = numeric(),
    season = integer(), consistency = numeric(), player = character()
  )
}

# Team values: sum of WAR of each franchise's best players per starting slot.
compute_team_values <- function(war, settings = default_war_settings()) {
  positions <- c("QB", "RB", "WR", "TE")
  rostered <- war[!is.na(war$franchise_name) & war$franchise_name != "Free Agent" &
                    war$Position %in% positions, ]
  if (nrow(rostered) == 0) return(NULL)

  long <- rostered |>
    dplyr::mutate(slot_count = unname(settings$slots[.data$Position])) |>
    dplyr::arrange(dplyr::desc(.data$WAR)) |>
    dplyr::filter(dplyr::row_number() <= .data$slot_count, .by = c("franchise_name", "Position")) |>
    dplyr::summarise(war = sum(.data$WAR), .by = c("franchise_name", "Position"))

  wide <- tidyr::pivot_wider(long, names_from = "Position", values_from = "war", values_fill = 0)
  for (p in setdiff(positions, names(wide))) wide[[p]] <- 0

  wide |>
    dplyr::transmute(Team = .data$franchise_name,
                     OVR = .data$QB + .data$RB + .data$WR + .data$TE,
                     QB = .data$QB, RB = .data$RB, WR = .data$WR, TE = .data$TE) |>
    dplyr::arrange(dplyr::desc(.data$OVR))
}
