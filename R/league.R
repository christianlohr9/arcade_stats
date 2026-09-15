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

sleeper_conn <- function(league, season) {
  ffscrapr::sleeper_connect(season = season, league_id = league$id)
}

# MFL is queried through its export API directly: ffscrapr's MFL functions
# parse the league's full scoring rules first and fail on some leagues.
mfl_cache <- new.env()
MFL_CACHE_SECONDS <- 15 * 60
MFL_MIN_INTERVAL <- 1

mfl_export <- function(season, type, league_id, ...) {
  params <- c(TYPE = type, L = league_id, ..., JSON = 1)
  url <- sprintf("https://api.myfantasyleague.com/%s/export?%s", season,
                 paste(names(params), params, sep = "=", collapse = "&"))

  cached <- mfl_cache[[url]]
  if (!is.null(cached) && difftime(Sys.time(), cached$at, units = "secs") < MFL_CACHE_SECONDS) {
    return(cached$body)
  }

  handle <- curl::new_handle(followlocation = TRUE, useragent = "arcade-stats-shiny", timeout = 60)
  # MFL rate-limits bursts of requests with 429: space requests out and back
  # off when it happens anyway.
  for (attempt in 1:5) {
    wait <- MFL_MIN_INTERVAL - as.numeric(difftime(Sys.time(), mfl_cache$last_request %||% as.POSIXct(0), units = "secs"))
    if (wait > 0) Sys.sleep(wait)
    mfl_cache$last_request <- Sys.time()
    res <- curl::curl_fetch_memory(url, handle)
    if (res$status_code != 429) break
    Sys.sleep(5 * attempt)
  }
  if (res$status_code != 200) stop("MFL antwortet mit Status ", res$status_code, " für ", type, call. = FALSE)
  body <- jsonlite::fromJSON(rawToChar(res$content))
  if (!is.null(body$error)) stop("MFL: ", unlist(body$error)[1], call. = FALSE)

  mfl_cache[[url]] <- list(at = Sys.time(), body = body)
  body
}

# MFL returns a single record as an object instead of an array.
as_records <- function(x) {
  if (is.null(x) || length(x) == 0) return(data.frame())
  if (is.data.frame(x)) x else as.data.frame(x)
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
  if (league$type == "mfl") {
    scores <- lapply(weeks, function(w) {
      s <- as_records(mfl_export(season, "playerScores", league$id, W = w)$playerScores$playerScore)
      if (nrow(s) == 0) return(NULL)
      data.frame(platform_id = as.character(s$id), week = as.integer(w),
                 points = suppressWarnings(as.numeric(s$score)))
    }) |>
      dplyr::bind_rows()
    if (nrow(scores) == 0) return(empty_league_points())

    return(
      scores |>
        dplyr::filter(nzchar(.data$platform_id), !is.na(.data$points)) |>
        dplyr::inner_join(player_id_map("mfl_id"), by = "platform_id") |>
        dplyr::summarise(fantasy_points_league = mean(.data$points), .by = c("player_id", "week")) |>
        dplyr::mutate(season = as.integer(season), .after = "player_id")
    )
  }

  ffscrapr::ff_scoringhistory(sleeper_conn(league, season), season = season) |>
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
  if (league$type == "mfl") {
    info <- mfl_export(season, "league", league$id)$league
    franchises <- as_records(info$franchises$franchise)
    # Leagues split into conferences with separate player pools (like the
    # Arcade MFL league) roster every player once per conference; as before,
    # franchises of conference "01" are used.
    divisions <- as_records(info$divisions$division)
    if (nrow(divisions) > 0 && "conference" %in% names(divisions) && "01" %in% divisions$conference) {
      franchises <- franchises[franchises$division %in% divisions$id[divisions$conference == "01"], ]
    }
    rosters <- mfl_export(season, "rosters", league$id)$rosters$franchise
    players <- lapply(seq_len(NROW(rosters)), function(i) {
      p <- as_records(rosters$player[[i]])
      if (nrow(p) == 0) return(NULL)
      data.frame(franchise_id = rosters$id[i], platform_id = as.character(p$id))
    }) |>
      dplyr::bind_rows()
    if (nrow(players) == 0) return(tibble::tibble(player_id = character(), franchise_name = character()))

    return(
      players |>
        dplyr::inner_join(data.frame(franchise_id = franchises$id, franchise_name = franchises$name),
                          by = "franchise_id") |>
        dplyr::inner_join(player_id_map("mfl_id"), by = "platform_id") |>
        dplyr::distinct(.data$player_id, .keep_all = TRUE) |>
        dplyr::select("player_id", "franchise_name")
    )
  }

  ffscrapr::ff_rosters(sleeper_conn(league, season)) |>
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
  ffscrapr::ff_scoring(sleeper_conn(list(id = league_id), season)) |>
    dplyr::filter(.data$pos %in% c("QB", "RB", "WR", "TE")) |>
    dplyr::select("pos", "event", "points") |>
    dplyr::distinct(.data$pos, .data$event, .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = "event", values_from = "points", values_fill = 0) |>
    dplyr::rename(position = "pos")
}
