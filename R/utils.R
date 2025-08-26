load_base_stats_act <- function(years){
  
  years_short <- stringr::str_sub(years,start= -2)

  pbp <- nflfastR::load_pbp(years)
  weekly <- nflfastR::calculate_stats(pbp, summary_level = "week")
  weekly_def <- nflfastR::calculate_stats_def(pbp, summary_level = "week")

  saveRDS(weekly, glue::glue("data/weekly_{years_short}.rds"))
  saveRDS(weekly_def, glue::glue("data/weekly_def_{years_short}.rds"))

  ### SNAPS #####
  year <- 2024
  get_snaps <- function(year){
    raw <- nflreadr::load_snap_counts(year)
    content <- raw |>
      dplyr::select(pfr_player_id,offense_snaps,defense_snaps,season,week) |>
      dplyr::left_join(nflfastR::fast_scraper_roster(year) |> dplyr::select(player_id=gsis_id,pfr_player_id=pfr_id))

    return(content)

  }

  all_snaps <- tidyr::crossing(year=years) |>
    purrr::pmap_dfr(get_snaps)

  saveRDS(all_snaps, glue::glue("data/all_snaps_{years_short}.rds"))

  ep_act_pre <- ffopportunity::ep_build()
  ep_act <- ep_act_pre$ep_weekly

  ep <- readRDS("data/ep_past.rds") |>
    dplyr::bind_rows(ep_act)

  ### JOINS #####
  all_snaps <- dplyr::bind_rows(
    readRDS("data/all_snaps_12-22.rds"),
    readRDS("data/all_snaps_23.rds"),
    readRDS("data/all_snaps_24.rds")
  )
  weekly <- dplyr::bind_rows(
    readRDS("data/weekly_99-22.rds"),
    readRDS("data/weekly_23.rds"),
    readRDS("data/weekly_24.rds")
  )
  weekly_def <- dplyr::bind_rows(
    readRDS("data/weekly_def_99-22.rds"),
    readRDS("data/weekly_def_23.rds"),
    readRDS("data/weekly_def_24.rds")
  )

  weekly_join <- weekly |>
    dplyr::left_join(nflfastR::fast_scraper_roster(1999:max(years)) |> dplyr::select(player_id=gsis_id,season,position,sleeper_id) |> dplyr::distinct()) |>
    dplyr::left_join(all_snaps |>
                       dplyr::group_by(player_id,season,week) |>
                       dplyr::summarise(snaps_off = sum(offense_snaps,na.rm=TRUE),
                          snaps_def = sum(defense_snaps,na.rm=TRUE))
              ) |>
    dplyr::left_join(
      ep |>
        dplyr::mutate(
          season = as.integer(season)
        ) |>
        dplyr::select(
          player_id,
          season,
          week,
          pass_completions_exp,
          pass_yards_gained_exp,
          rec_yards_gained_exp,
          rush_yards_gained_exp,
          pass_touchdown_exp,
          rec_touchdown_exp,
          rush_touchdown_exp,
          pass_first_down_exp,
          rec_first_down_exp,
          rush_first_down_exp,
          pass_interception_exp,
          total_fantasy_points_exp
        )
      ) |>
    dplyr::filter(position %in% c("QB","RB","WR","TE")) |>
    dplyr::mutate(touches = attempts + targets + carries,
           yps  = (passing_yards + rushing_yards + receiving_yards) / snaps_off,
           ops  = (attempts + targets + carries)/snaps_off,
           epps = total_fantasy_points_exp/snaps_off,
           fpps = fantasy_points_ppr      /snaps_off,
           stat = as.factor(ifelse(position=="QB","Passing","Rushing/Receiving")),
           player_id = as.factor(as.character(player_id)),
           season = as.factor(season),
           week = as.factor(week),
           position = as.factor(as.character(position)),
           recent_team = as.factor(as.character(recent_team))
    )

  saveRDS(weekly_join, "data/weekly_stats.rds")

  ### DEFENSE ###

  weekly_join_def <- weekly_def |>
    dplyr::left_join(nflfastR::fast_scraper_roster(1999:max(years)) |> dplyr::select(player_id=gsis_id,season,pff_id,sleeper_id,espn_id) |> dplyr::distinct()) |>
    dplyr::left_join(ffscrapr::espn_players() |> dplyr::mutate(espn_id=as.character(player_id)) |> dplyr::select(espn_id,pos) |> dplyr::distinct()) |>
    dplyr::mutate(position = pos) |>
    dplyr::filter(position %in% c("DT","DE","LB","CB","S")) |>
    dplyr::mutate(player_id = as.factor(as.character(player_id)),
           season = as.factor(season),
           week = as.factor(week),
           position = as.factor(as.character(position)),
           recent_team = as.factor(as.character(team)),
           fantasy_points_ppr =
             -4 * (def_fumbles)+-0.5 * (def_sacks)+-0.2 * (def_penalty_yards) +
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
               position == "DT" ~ 2.5    * def_tackles_solo + 1.5 * def_tackles_with_assist + 1.5 * def_tackle_assists,
               position %in% c("DE") ~ 2 * def_tackles_solo + 1   * def_tackles_with_assist + 1   * def_tackle_assists,
               position %in% c("LB") ~ 1 * def_tackles_solo + 0.5 * def_tackles_with_assist + 0.5 * def_tackle_assists,
               position == "CB" ~ 1      * def_tackles_solo + 1   * def_tackles_with_assist + 1   * def_tackle_assists,
               position %in% c("S") ~ 1  * def_tackles_solo + 0.5 * def_tackles_with_assist + 0.5 * def_tackle_assists,
               TRUE ~ 0
             )
    )

  saveRDS(weekly_join_def, "data/weekly_stats_def.rds")
}
