#########

.nflfastr_roster <- function(season){
  ros <- nflfastr_rosters(season) %>%
    dplyr::mutate(position = ifelse(.data$position %in% c("HB", "FB"), "RB", .data$position)) %>%
    dplyr::select(dplyr::any_of(c(
      "season","gsis_id","sportradar_id",
      "player_name"="full_name","pos"="position","team"
    ))) %>%
    dplyr::left_join(
      dp_playerids() %>%
        dplyr::select("sportradar_id","mfl_id","sleeper_id","espn_id","fleaflicker_id"),
      by = c("sportradar_id"),
      na_matches = "never"
    )
  
  return(ros)
}


.nflfastr_offense_long <- function(season){
  ps <- nflfastr_weekly(seasons = season, type = "offense") %>%
    dplyr::select(dplyr::any_of(c(
      "season", "week","player_id",
      "attempts", "carries", "completions", "interceptions", "passing_2pt_conversions", "passing_first_downs",
      "passing_tds", "passing_yards", "receiving_2pt_conversions", "receiving_first_downs",
      "receiving_fumbles", "receiving_fumbles_lost", "receiving_tds",
      "receiving_yards", "receptions", "rushing_2pt_conversions", "rushing_first_downs",
      "rushing_fumbles", "rushing_fumbles_lost", "rushing_tds", "rushing_yards",
      "sack_fumbles", "sack_fumbles_lost", "sack_yards", "sacks", "special_teams_tds",
      "targets")
    )) %>%
    tidyr::pivot_longer(
      names_to = "metric",
      cols = -c("season","week","player_id")
    )
  
  return(ps)
}

nflfastr_weekly <- function(seasons = TRUE,
                            type = c("offense", "kicking", "defense")) {
  
  type <- match.arg(type)
  
  if (type %in% c("offense", "kicking")) {
    df_weekly <- nflreadr::load_player_stats(seasons = seasons, stat_type = type)
  } else if (type %in% c("defense")) {
    df_weekly <- nflfastR::calculate_stats_def(nflfastR::load_pbp(seasons = seasons), weekly =TRUE)
  }
  
  return(df_weekly)
}

.nflfastr_defense_long <- function(season){
  ps <- nflfastr_weekly(seasons = season, type = "defense") %>%
    dplyr::select(dplyr::any_of(c(
      "season", "week","player_id",
      "def_tackles", "def_tackles_solo", "def_tackles_with_assist", "def_tackle_assist", "def_tackles_for_loss", "def_tackles_for_loss_yards",
      "def_fumbles_forced", "def_fumbles", "def_fumble_recovery_own", "def_fumble_recovery_yards_own", "def_fumble_recovery_opp", "def_fumble_recovery_yards_opp",
      "def_sacks", "def_sack_yards", "def_qb_hit",
      "def_interceptions", "def_interception_yards", "def_pass_defended",
      "def_tds", "def_safety", "def_penalty", "def_penalty_yards"
      )
    )) %>%
    tidyr::pivot_longer(
      names_to = "metric",
      cols = -c("season","week","player_id")
    )
  
  return(ps)
}

#########

ff_scoringhistory.sleeper_conn <- function(conn, season = 1999:nflreadr::most_recent_season(), ...) {
  checkmate::assert_numeric(season, lower = 1999, upper = as.integer(format(Sys.Date(), "%Y")))
  
  # Pull in scoring rules for that league
  league_rules <-
    ff_scoring(conn) %>%
    dplyr::left_join(
      ffscrapr_mapping,
      by = c("event" = "ff_event")
    )
  
  ros <- .nflfastr_roster(season)
  
  ps <- bind_rows(
    .nflfastr_offense_long(season), 
    .nflfastr_defense_long(season) 
    )
  
  # if("K" %in% league_rules$pos){
  #   ps <- dplyr::bind_rows(
  #     ps,
  #     .nflfastr_kicking_long(season))
  # }
  
  ros %>%
    dplyr::inner_join(ps, by = c("gsis_id"="player_id","season")) %>%
    dplyr::inner_join(league_rules, by = c("metric"="nflfastr_event","pos")) %>%
    dplyr::mutate(points = .data$value * .data$points) %>%
    dplyr::group_by(.data$season, .data$week, .data$gsis_id, .data$sportradar_id) %>%
    dplyr::mutate(points = round(sum(.data$points, na.rm = TRUE), 2)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      id_cols = c("season", "week", "gsis_id", "sportradar_id", "sleeper_id", "player_name", "pos", "team", "points"),
      names_from = "metric",
      values_from = "value",
      values_fill = 0,
      values_fn = max
    )
}
