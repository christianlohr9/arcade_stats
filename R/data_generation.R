#' Data Generation Script for Arcade Stats
#' 
#' This script contains the data processing and generation logic
#' that was originally embedded in the main Shiny app.
#' Run this script to update/regenerate the RDS data files.

library(nflfastR)
library(dplyr)
library(glue)
library(tidyr)
library(purrr)

#' Generate weekly stats for given years
#' 
#' This function loads PBP data and calculates player stats
#' @param years Vector of years to process
#' @export
generate_weekly_stats <- function(years) {
  
  years_short <- stringr::str_sub(years, start = -2)
  
  message(glue("Loading PBP data for years: {paste(years, collapse = ', ')}"))
  pbp <- nflfastR::load_pbp(years)
  
  message("Calculating weekly player stats...")
  # Use the new calculate_stats function which includes both offensive and defensive stats
  years_to_process <- unique(pbp$season)
  all_stats <- nflfastR::calculate_stats(seasons = years_to_process, summary_level = "week", stat_type = "player")
  
  # Split into offensive and defensive stats based on available columns
  weekly <- all_stats |> 
    dplyr::filter(!is.na(attempts) | !is.na(carries) | !is.na(targets)) # Players with offensive stats
    
  weekly_def <- all_stats |> 
    dplyr::filter(!is.na(def_tackles_solo) | !is.na(def_sacks) | !is.na(def_interceptions)) # Players with defensive stats
  
  # Save files
  weekly_file <- glue::glue("data/weekly_{years_short}.rds")
  weekly_def_file <- glue::glue("data/weekly_def_{years_short}.rds")
  
  saveRDS(weekly, weekly_file)
  saveRDS(weekly_def, weekly_def_file)
  
  message(glue("Saved {weekly_file} and {weekly_def_file}"))
  
  return(list(weekly = weekly, weekly_def = weekly_def))
}

#' Generate snap counts data
#' 
#' @param years Vector of years to process
#' @export
generate_snap_counts <- function(years) {
  
  years_short <- stringr::str_sub(years, start = -2)
  
  get_snaps <- function(year) {
    raw <- nflreadr::load_snap_counts(year)
    content <- raw |>
      dplyr::select(pfr_player_id, offense_snaps, defense_snaps, season, week) |>
      dplyr::left_join(
        nflfastR::fast_scraper_roster(year) |> 
          dplyr::select(player_id = gsis_id, pfr_player_id = pfr_id)
      )
    return(content)
  }
  
  message(glue("Generating snap counts for years: {paste(years, collapse = ', ')}"))
  all_snaps <- tidyr::crossing(year = years) |>
    purrr::pmap_dfr(get_snaps)
  
  snap_file <- glue::glue("data/all_snaps_{years_short}.rds")
  saveRDS(all_snaps, snap_file)
  
  message(glue("Saved {snap_file}"))
  
  return(all_snaps)
}

#' Generate expected points data
#' 
#' @export
generate_expected_points <- function() {
  
  if (!requireNamespace("ffopportunity", quietly = TRUE)) {
    warning("ffopportunity package not available. Skipping EP generation.")
    return(NULL)
  }
  
  message("Building expected points data...")
  ep_act_pre <- ffopportunity::ep_build()
  ep_act <- ep_act_pre$ep_weekly
  
  # Load existing EP data and bind
  if (file.exists("data/ep_past.rds")) {
    ep_existing <- readRDS("data/ep_past.rds") |> dplyr::filter(season != (lubridate::today() |> lubridate::year()))
    ep <- dplyr::bind_rows(ep_existing, ep_act)
  } else {
    ep <- ep_act
  }
  
  saveRDS(ep, "data/ep_past.rds")
  
  message("Saved data/ep_past.rds")
  
  return(ep)
}

#' Main data generation function
#' 
#' Run this to update all data files
#' @param years Vector of years to process (default: current year)
#' @export
update_all_data <- function(years = as.integer(format(Sys.Date(), "%Y"))) {
  
  message("=== Starting Data Generation ===")
  
  # Generate weekly stats
  weekly_data <- generate_weekly_stats(years)
  
  # Generate snap counts  
  snap_data <- generate_snap_counts(years)
  
  # Generate expected points
  ep_data <- generate_expected_points()
  
  # Generate final joined datasets (similar to utils.R logic)
  message("Creating final joined datasets...")
  
  # Load all historical data
  all_snaps <- bind_historical_data("all_snaps")
  weekly <- bind_historical_data("weekly") 
  weekly_def <- bind_historical_data("weekly_def")
  
  # Create joined weekly data
  weekly_join <- create_weekly_join(weekly, all_snaps, ep_data, max(years))
  weekly_join_def <- create_weekly_join_def(weekly_def, max(years))
  
  saveRDS(weekly_join, "data/weekly_stats.rds")
  saveRDS(weekly_join_def, "data/weekly_stats_def.rds")
  
  message("=== Data Generation Complete ===")
  
  return(list(
    weekly_join = weekly_join,
    weekly_join_def = weekly_join_def
  ))
}

#' Helper function to bind historical data files
bind_historical_data <- function(data_type) {
  
  files <- list.files("data", pattern = glue("^{data_type}_"), full.names = TRUE)
  files <- files[!grepl("stats\\.rds$", files)]  # Exclude final stats files
  
  if (length(files) == 0) {
    warning(glue("No {data_type} files found"))
    return(data.frame())
  }
  
  message(glue("Binding {length(files)} {data_type} files"))
  
  all_data <- files |>
    purrr::map(readRDS) |>
    purrr::map(function(df) {
      # Ensure season is integer across all datasets
      if("season" %in% names(df)) {
        df$season <- as.integer(as.character(df$season))
      }
      # Ensure week is integer across all datasets
      if("week" %in% names(df)) {
        df$week <- as.integer(as.character(df$week))
      }
      return(df)
    }) |>
    dplyr::bind_rows()
  
  return(all_data)
}

# Functions from utils.R adapted
create_weekly_join <- function(weekly, all_snaps, ep, max_year) {
  
  weekly_join <- weekly |>
    dplyr::left_join(
      nflfastR::fast_scraper_roster(1999:max_year) |> 
        dplyr::select(player_id = gsis_id, season, position, sleeper_id) |> 
        dplyr::distinct()
    ) |>
    dplyr::left_join(
      all_snaps |>
        dplyr::group_by(player_id, season, week) |>
        dplyr::summarise(
          snaps_off = sum(offense_snaps, na.rm = TRUE),
          snaps_def = sum(defense_snaps, na.rm = TRUE),
          .groups = "drop"
        )
    )
  
  if (!is.null(ep)) {
    weekly_join <- weekly_join |>
      dplyr::left_join(
        ep |>
          dplyr::mutate(season = as.integer(season)) |>
          dplyr::select(
            player_id, season, week,
            pass_completions_exp, pass_yards_gained_exp, rec_yards_gained_exp,
            rush_yards_gained_exp, pass_touchdown_exp, rec_touchdown_exp,
            rush_touchdown_exp, pass_first_down_exp, rec_first_down_exp,
            rush_first_down_exp, pass_interception_exp, total_fantasy_points_exp
          )
      )
  }
  
  weekly_join <- weekly_join |>
    dplyr::filter(position %in% c("QB", "RB", "WR", "TE")) |>
    dplyr::mutate(
      touches = attempts + targets + carries,
      yps = (passing_yards + rushing_yards + receiving_yards) / snaps_off,
      ops = (attempts + targets + carries) / snaps_off,
      epps = if ("total_fantasy_points_exp" %in% names(.)) total_fantasy_points_exp / snaps_off else NA,
      fpps = fantasy_points_ppr / snaps_off,
      stat = as.factor(ifelse(position == "QB", "Passing", "Rushing/Receiving")),
      player_id = as.factor(as.character(player_id)),
      season = as.factor(season),
      week = as.factor(week),
      position = as.factor(as.character(position)),
      recent_team = as.factor(as.character(recent_team))
    )
  
  return(weekly_join)
}

create_weekly_join_def <- function(weekly_def, max_year) {
  
  weekly_join_def <- weekly_def |>
    dplyr::left_join(
      nflfastR::fast_scraper_roster(1999:max_year) |> 
        dplyr::select(player_id = gsis_id, season, pff_id, sleeper_id, espn_id) |> 
        dplyr::distinct()
    ) |>
    dplyr::left_join(
      ffscrapr::espn_players() |> 
        dplyr::mutate(espn_id = as.character(player_id)) |> 
        dplyr::select(espn_id, pos) |> 
        dplyr::distinct()
    ) |>
    dplyr::mutate(position = pos) |>
    dplyr::filter(position %in% c("DT", "DE", "LB", "CB", "S")) |>
    dplyr::mutate(
      player_id = as.factor(as.character(player_id)),
      season = as.factor(season),
      week = as.factor(week),
      position = as.factor(as.character(position)),
      recent_team = as.factor(as.character(team)),
      fantasy_points_ppr = calculate_def_fantasy_points(.)
    )
  
  return(weekly_join_def)
}

#' Calculate defensive fantasy points
calculate_def_fantasy_points <- function(data) {
  data |>
    dplyr::mutate(
      fantasy_points_ppr =
        -4 * (def_fumbles) + -0.5 * (def_sacks) + -0.2 * (def_penalty_yards) +
        0.15 * (def_fumble_recovery_yards_opp + def_fumble_recovery_yards_own) +
        0.2 * (def_sack_yards) +
        1 * (def_qb_hits) +
        2 * (def_tackles_for_loss + def_safety) +
        4 * (def_fumble_recovery_own) +
        5 * (def_fumble_recovery_opp + def_tds) +
        6 * (def_fumbles_forced + def_interceptions) +
        dplyr::case_when(
          position %in% c("DT", "DE", "LB") ~ 3 * def_pass_defended,
          position %in% c("CB", "S") ~ 4 * def_pass_defended,
          TRUE ~ 0
        ) +
        dplyr::case_when(
          position == "DT" ~ 2.5 * def_tackles_solo + 1.5 * def_tackles_with_assist + 1.5 * def_tackle_assists,
          position %in% c("DE") ~ 2 * def_tackles_solo + 1 * def_tackles_with_assist + 1 * def_tackle_assists,
          position %in% c("LB") ~ 1 * def_tackles_solo + 0.5 * def_tackles_with_assist + 0.5 * def_tackle_assists,
          position == "CB" ~ 1 * def_tackles_solo + 1 * def_tackles_with_assist + 1 * def_tackle_assists,
          position %in% c("S") ~ 1 * def_tackles_solo + 0.5 * def_tackles_with_assist + 0.5 * def_tackle_assists,
          TRUE ~ 0
        )
    ) |>
    dplyr::pull(fantasy_points_ppr)
}
