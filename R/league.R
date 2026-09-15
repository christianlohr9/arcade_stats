# League integrations (Sleeper / MFL via ffscrapr) ------------------------------
#
# A league is given as free text in the UI:
#   "PPR"        nflverse PPR points (IDP: Arcade scoring from the pipeline)
#   "mPPR"       scores of the Arcade MFL league
#   "mfl12345"   any MFL league
#   "123456789"  any Sleeper league

MPPR_MFL_LEAGUE_ID <- "60206"

parse_league <- function(x) {
  x <- trimws(x %||% "")
  if (x == "" || x == "PPR") return(list(type = "ppr"))
  if (x == "mPPR") return(list(type = "mfl", id = MPPR_MFL_LEAGUE_ID, rosters = FALSE))
  if (grepl("^mfl[0-9]+$", x, ignore.case = TRUE)) {
    return(list(type = "mfl", id = sub("^mfl", "", x, ignore.case = TRUE)))
  }
  if (grepl("^[0-9]+$", x)) return(list(type = "sleeper", id = x))
  stop("Unbekannte Liga: '", x, "'. Erlaubt sind PPR, mPPR, eine Sleeper League ID oder mfl<ID>.",
       call. = FALSE)
}

league_connect <- function(league, season) {
  switch(league$type,
    mfl = ffscrapr::mfl_connect(season = season, league_id = league$id),
    sleeper = ffscrapr::sleeper_connect(season = season, league_id = league$id)
  )
}

player_id_map <- function(platform_col) {
  ids <- ffscrapr::dp_playerids()
  ids <- ids[!is.na(ids[[platform_col]]) & !is.na(ids$gsis_id), c(platform_col, "gsis_id")]
  names(ids) <- c("platform_id", "player_id")
  ids$platform_id <- as.character(ids$platform_id)
  dplyr::distinct(ids, .data$platform_id, .keep_all = TRUE)
}

#' Weekly fantasy points of every player under a league's scoring
#' @return player_id, season, week, fantasy_points_league
league_points <- function(league, season, weeks) {
  conn <- league_connect(league, season)

  if (league$type == "mfl") {
    # Weekly MFL requests have returned zero points for some seasons; the
    # per-week scores are therefore fetched one week at a time and a week with
    # no scoring player is dropped instead of silently counting as zero.
    scores <- lapply(weeks, function(w) {
      s <- ffscrapr::ff_playerscores(conn, season = season, week = w)
      if (nrow(s) == 0 || all(s$points == 0, na.rm = TRUE)) return(NULL)
      s$week <- w
      s
    }) |>
      dplyr::bind_rows()
    if (nrow(scores) == 0) return(empty_league_points())

    return(
      scores |>
        dplyr::mutate(platform_id = as.character(.data$player_id)) |>
        dplyr::summarise(fantasy_points_league = mean(.data$points), .by = c("platform_id", "week")) |>
        dplyr::inner_join(player_id_map("mfl_id"), by = "platform_id") |>
        dplyr::transmute(.data$player_id, season = as.integer(season), week = as.integer(.data$week),
                         .data$fantasy_points_league)
    )
  }

  ffscrapr::ff_scoringhistory(conn, season = season) |>
    dplyr::filter(!is.na(.data$gsis_id), .data$week %in% weeks) |>
    dplyr::rename(player_id = "gsis_id") |>
    dplyr::summarise(fantasy_points_league = mean(.data$points), .by = c("player_id", "season", "week")) |>
    dplyr::mutate(season = as.integer(.data$season), week = as.integer(.data$week))
}

empty_league_points <- function() {
  tibble::tibble(player_id = character(), season = integer(), week = integer(),
                 fantasy_points_league = numeric())
}

#' Franchise of every rostered player
#' @return player_id, franchise_name
league_rosters <- function(league, season) {
  if (league$type == "ppr") return(tibble::tibble(player_id = character(), franchise_name = character()))
  conn <- league_connect(league, season)

  if (league$type == "mfl") {
    rosters <- ffscrapr::ff_rosters(conn)
    return(
      rosters |>
        dplyr::transmute(platform_id = as.character(.data$player_id), .data$franchise_name) |>
        dplyr::distinct(.data$platform_id, .keep_all = TRUE) |>
        dplyr::inner_join(player_id_map("mfl_id"), by = "platform_id") |>
        dplyr::select("player_id", "franchise_name")
    )
  }

  ffscrapr::ff_rosters(conn) |>
    dplyr::transmute(platform_id = as.character(.data$player_id), .data$franchise_name) |>
    dplyr::inner_join(player_id_map("sleeper_id"), by = "platform_id") |>
    dplyr::distinct(.data$player_id, .keep_all = TRUE) |>
    dplyr::select("player_id", "franchise_name")
}

attach_franchises <- function(df, league, season) {
  if (league$type == "ppr" || isFALSE(league$rosters)) {
    df$franchise_name <- "Keine Liga ausgewählt"
    return(df)
  }
  df |>
    dplyr::left_join(league_rosters(league, season), by = "player_id") |>
    dplyr::mutate(franchise_name = dplyr::coalesce(.data$franchise_name, "Free Agent"))
}

#' Scoring rules of a Sleeper league, one row per position, one column per event
sleeper_scoring_rules <- function(league_id, season) {
  ffscrapr::ff_scoring(ffscrapr::sleeper_connect(season = season, league_id = league_id)) |>
    dplyr::filter(.data$pos %in% c("QB", "RB", "WR", "TE")) |>
    dplyr::select("pos", "event", "points") |>
    dplyr::distinct(.data$pos, .data$event, .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = "event", values_from = "points", values_fill = 0) |>
    dplyr::rename(position = "pos")
}
