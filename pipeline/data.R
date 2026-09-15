# Data pipeline functions --------------------------------------------------------
#
# Everything is rebuilt from nflverse on each run. nflverse refreshes its own
# releases during the season, so there is no local history to append to or
# migrate at season end.

FIRST_SEASON <- 1999
FIRST_EP_SEASON <- 2006
FIRST_SNAP_SEASON <- 2012

OFFENSE_POSITIONS <- c("QB", "RB", "WR", "TE")
IDP_POSITIONS <- c("DT", "DE", "LB", "CB", "S")

OFFENSE_COLUMNS <- c(
  "player_id", "player_name", "player_display_name", "headshot_url", "position",
  "team", "season", "week",
  "completions", "attempts", "passing_yards", "passing_tds", "passing_interceptions",
  "sacks_suffered", "sack_yards_lost", "sack_fumbles", "sack_fumbles_lost",
  "passing_air_yards", "passing_yards_after_catch", "passing_first_downs", "passing_epa",
  "passing_2pt_conversions", "pacr",
  "carries", "rushing_yards", "rushing_tds", "rushing_fumbles", "rushing_fumbles_lost",
  "rushing_first_downs", "rushing_epa", "rushing_2pt_conversions",
  "receptions", "targets", "receiving_yards", "receiving_tds", "receiving_fumbles",
  "receiving_fumbles_lost", "receiving_air_yards", "receiving_yards_after_catch",
  "receiving_first_downs", "receiving_epa", "receiving_2pt_conversions",
  "racr", "target_share", "air_yards_share", "wopr", "fantasy_points_ppr"
)

EP_COLUMNS <- c(
  "pass_completions_exp", "pass_yards_gained_exp", "rec_yards_gained_exp",
  "rush_yards_gained_exp", "pass_touchdown_exp", "rec_touchdown_exp",
  "rush_touchdown_exp", "pass_first_down_exp", "rec_first_down_exp",
  "rush_first_down_exp", "pass_interception_exp", "total_fantasy_points_exp"
)

DEFENSE_COLUMNS <- c(
  "player_id", "player_name", "player_display_name", "position", "team", "season", "week",
  "def_tackles_solo", "def_tackles_with_assist", "def_tackle_assists",
  "def_tackles_for_loss", "def_tackles_for_loss_yards", "def_fumbles_forced",
  "def_sacks", "def_sack_yards", "def_qb_hits", "def_interceptions",
  "def_interception_yards", "def_pass_defended", "def_tds", "def_fumbles", "def_safeties",
  "fumble_recovery_own", "fumble_recovery_yards_own", "fumble_recovery_opp",
  "fumble_recovery_yards_opp", "penalties", "penalty_yards", "fantasy_points_ppr"
)

# Fallback when a defender has no fantasy position in the ffverse ID map.
NFLVERSE_IDP_POSITION <- c(
  DE = "DE", DL = "DE", OLB = "LB",
  DT = "DT", NT = "DT",
  LB = "LB", ILB = "LB", MLB = "LB",
  CB = "CB", DB = "CB",
  S = "S", SAF = "S", FS = "S", SS = "S"
)

log_step <- function(...) message(format(Sys.time(), "[%H:%M:%S] "), ...)

# Loads one nflreadr dataset per season, skipping seasons that are not
# published yet (e.g. snap counts early in a new season).
load_seasons <- function(loader, seasons, ...) {
  parts <- lapply(seasons, function(s) {
    tryCatch(loader(s, ...), error = function(e) {
      log_step("  season ", s, " not available: ", conditionMessage(e))
      NULL
    })
  })
  dplyr::bind_rows(parts)
}

build_datasets <- function(current_season = nflreadr::get_current_season()) {
  seasons <- FIRST_SEASON:current_season

  log_step("Loading player stats ", min(seasons), "-", max(seasons))
  stats <- load_seasons(nflreadr::load_player_stats, seasons, summary_level = "week") |>
    dplyr::filter(.data$season_type == "REG")

  log_step("Loading ff player IDs")
  ids <- nflreadr::load_ff_playerids()

  log_step("Loading snap counts")
  snaps <- load_seasons(nflreadr::load_snap_counts, FIRST_SNAP_SEASON:current_season)

  log_step("Loading expected fantasy points")
  ep <- load_seasons(nflreadr::load_ff_opportunity, FIRST_EP_SEASON:current_season,
                     stat_type = "weekly")

  log_step("Building offense")
  offense <- build_offense(stats, snaps, ep, ids)
  log_step("Building defense")
  defense <- build_defense(stats, ids)

  list(offense = offense, defense = defense)
}

build_offense <- function(stats, snaps, ep, ids) {
  sleeper <- ids |>
    dplyr::filter(!is.na(.data$gsis_id), !is.na(.data$sleeper_id)) |>
    dplyr::distinct(player_id = .data$gsis_id, .keep_all = TRUE) |>
    dplyr::transmute(.data$player_id, sleeper_id = as.character(.data$sleeper_id))

  snaps_weekly <- summarise_snaps(snaps, ids)

  ep_weekly <- ep |>
    dplyr::filter(!is.na(.data$player_id)) |>
    dplyr::mutate(season = as.integer(.data$season), week = as.integer(.data$week)) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(EP_COLUMNS), \(x) sum(x, na.rm = TRUE)),
                     .by = c("player_id", "season", "week"))

  stats |>
    dplyr::filter(.data$position %in% OFFENSE_POSITIONS) |>
    dplyr::select(dplyr::all_of(OFFENSE_COLUMNS)) |>
    dplyr::mutate(season = as.integer(.data$season), week = as.integer(.data$week)) |>
    dplyr::left_join(sleeper, by = "player_id") |>
    dplyr::left_join(snaps_weekly, by = c("player_id", "season", "week")) |>
    dplyr::left_join(ep_weekly, by = c("player_id", "season", "week")) |>
    dplyr::mutate(
      touches = .data$attempts + .data$targets + .data$carries,
      yps = (.data$passing_yards + .data$rushing_yards + .data$receiving_yards) / .data$snaps_off,
      ops = .data$touches / .data$snaps_off,
      epps = .data$total_fantasy_points_exp / .data$snaps_off,
      fpps = .data$fantasy_points_ppr / .data$snaps_off,
      stat = ifelse(.data$position == "QB", "Passing", "Rushing/Receiving")
    ) |>
    dplyr::mutate(dplyr::across(c("yps", "ops", "epps", "fpps"), \(x) ifelse(is.finite(x), x, NA_real_)))
}

summarise_snaps <- function(snaps, ids) {
  empty <- tibble::tibble(player_id = character(), season = integer(), week = integer(),
                          snaps_off = numeric(), snaps_def = numeric())
  if (nrow(snaps) == 0) return(empty)

  pfr <- ids |>
    dplyr::filter(!is.na(.data$gsis_id), !is.na(.data$pfr_id)) |>
    dplyr::distinct(.data$pfr_id, .keep_all = TRUE) |>
    dplyr::select(pfr_player_id = "pfr_id", player_id = "gsis_id")

  snaps |>
    dplyr::filter(.data$game_type == "REG") |>
    dplyr::inner_join(pfr, by = "pfr_player_id") |>
    dplyr::summarise(snaps_off = sum(.data$offense_snaps, na.rm = TRUE),
                     snaps_def = sum(.data$defense_snaps, na.rm = TRUE),
                     .by = c("player_id", "season", "week")) |>
    dplyr::mutate(season = as.integer(.data$season), week = as.integer(.data$week))
}

idp_position <- function(fantasy_position, nflverse_position) {
  fantasy <- ifelse(fantasy_position %in% IDP_POSITIONS, fantasy_position, NA_character_)
  dplyr::coalesce(fantasy, unname(NFLVERSE_IDP_POSITION[nflverse_position]))
}

build_defense <- function(stats, ids) {
  fantasy_positions <- ids |>
    dplyr::filter(!is.na(.data$gsis_id)) |>
    dplyr::distinct(player_id = .data$gsis_id, .keep_all = TRUE) |>
    dplyr::select("player_id", fantasy_position = "position")

  stats |>
    dplyr::filter(.data$position_group %in% c("DL", "LB", "DB")) |>
    dplyr::select(dplyr::all_of(setdiff(DEFENSE_COLUMNS, "fantasy_points_ppr"))) |>
    dplyr::left_join(fantasy_positions, by = "player_id") |>
    dplyr::mutate(
      season = as.integer(.data$season),
      week = as.integer(.data$week),
      position = idp_position(.data$fantasy_position, .data$position)
    ) |>
    dplyr::select(-"fantasy_position") |>
    dplyr::filter(.data$position %in% IDP_POSITIONS) |>
    dplyr::mutate(dplyr::across(where(is.numeric) & !dplyr::all_of(c("season", "week")),
                                \(x) dplyr::coalesce(x, 0))) |>
    dplyr::mutate(fantasy_points_ppr = idp_points(dplyr::pick(dplyr::everything())))
}

# Arcade IDP scoring.
idp_points <- function(d) {
  pos <- d$position
  pass_defended <- ifelse(pos %in% c("CB", "S"), 4, 3) * d$def_pass_defended
  tackles <- dplyr::case_when(
    pos == "DT" ~ 2.5 * d$def_tackles_solo + 1.5 * d$def_tackles_with_assist + 1.5 * d$def_tackle_assists,
    pos == "DE" ~ 2 * d$def_tackles_solo + 1 * d$def_tackles_with_assist + 1 * d$def_tackle_assists,
    pos == "LB" ~ 1 * d$def_tackles_solo + 0.5 * d$def_tackles_with_assist + 0.5 * d$def_tackle_assists,
    pos == "CB" ~ 1 * d$def_tackles_solo + 1 * d$def_tackles_with_assist + 1 * d$def_tackle_assists,
    pos == "S"  ~ 1 * d$def_tackles_solo + 0.5 * d$def_tackles_with_assist + 0.5 * d$def_tackle_assists,
    .default = 0
  )

  -4 * d$def_fumbles - 0.5 * d$def_sacks - 0.2 * d$penalty_yards +
    0.15 * (d$fumble_recovery_yards_opp + d$fumble_recovery_yards_own) +
    0.2 * d$def_sack_yards +
    1 * d$def_qb_hits +
    2 * (d$def_tackles_for_loss + d$def_safeties) +
    4 * d$fumble_recovery_own +
    5 * (d$fumble_recovery_opp + d$def_tds) +
    6 * (d$def_fumbles_forced + d$def_interceptions) +
    pass_defended + tackles
}

# Validation ---------------------------------------------------------------------

# Minimum regular-season rows for a completed season.
MIN_ROWS <- c(offense = 4000, defense = 6000)

validate_datasets <- function(datasets, current_season, current_week, today = Sys.Date()) {
  problems <- character()
  check <- function(ok, msg) if (!isTRUE(ok)) problems <<- c(problems, msg)

  required <- list(offense = c(OFFENSE_COLUMNS, EP_COLUMNS, "snaps_off", "sleeper_id", "stat"),
                   defense = DEFENSE_COLUMNS)

  for (name in names(required)) {
    d <- datasets[[name]]
    missing <- setdiff(required[[name]], names(d))
    check(length(missing) == 0, sprintf("%s: missing columns %s", name, paste(missing, collapse = ", ")))
    if (length(missing) > 0) next

    dupes <- sum(duplicated(d[c("player_id", "season", "week")]))
    check(dupes == 0, sprintf("%s: %d duplicated player/season/week rows", name, dupes))

    rows <- table(factor(d$season, levels = FIRST_SEASON:current_season))
    completed <- rows[as.character(FIRST_SEASON:(current_season - 1))]
    thin <- names(completed)[completed < MIN_ROWS[[name]]]
    check(length(thin) == 0, sprintf("%s: too few rows for seasons %s", name, paste(thin, collapse = ", ")))

    check(mean(is.na(d$fantasy_points_ppr)) < 0.01, sprintf("%s: fantasy_points_ppr mostly missing", name))
  }

  # During the regular season the current week must show up within a few days.
  # get_current_week() is the upcoming week, so the last completed one is one
  # behind; one more week of slack covers late nflverse updates.
  if (current_week >= 3) {
    latest <- suppressWarnings(max(datasets$offense$week[datasets$offense$season == current_season]))
    check(is.finite(latest) && latest >= current_week - 2,
          sprintf("offense: season %d only reaches week %s, expected at least %d",
                  current_season, latest, current_week - 2))
  }

  # Players without a single touch have no expected points row.
  with_touches <- datasets$offense |>
    dplyr::filter(.data$season %in% FIRST_EP_SEASON:(current_season - 1), .data$touches > 0)
  ep_share <- mean(!is.na(with_touches$total_fantasy_points_exp))
  check(ep_share > 0.95, sprintf("offense: expected points cover only %.0f%% of rows with touches", ep_share * 100))

  problems
}

build_manifest <- function(datasets, current_season, current_week) {
  offense <- datasets$offense
  list(
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    current_season = current_season,
    current_week = current_week,
    latest_week = suppressWarnings(max(offense$week[offense$season == max(offense$season)])),
    seasons = sort(unique(offense$season)),
    rows = lapply(datasets, nrow),
    nflreadr_version = as.character(utils::packageVersion("nflreadr"))
  )
}
