#' Compute WAR (Wins Above Replacement) for Fantasy Football
#' 
#' This function calculates Wins Above Replacement (WAR) for fantasy football players
#' across all positions including offense (QB, RB, WR, TE) and Individual Defensive
#' Players (DT, DE, LB, CB, S). The calculation uses statistical methods with pnorm()
#' for win probabilities based on team score expectations.
#' 
#' @param f_years The season year to analyze
#' @param f_week The week(s) to include (or "0" for all weeks 1-17)
#' @param weekly_join_war Offensive player data
#' @param weekly_join_def_war Defensive player data
#' @param weekly_join Combined player data for league lookups
#' @param input Shiny input object containing league settings
#' 
#' @return A data frame with WAR calculations for all qualifying players
#' @export

compute_war <- function(f_years, f_week, weekly_join_war, weekly_join_def_war, weekly_join, input){

  f_week <- if (f_week=="0"){
    c(1:17)
    } else {f_week}

  war_raw <- weekly_join_war %>%
    mutate(position_off = as.character(position),
           fantasy_points_ppr_off = fantasy_points_ppr) %>%
    select(-position) %>%
    full_join(weekly_join_def_war %>%
                mutate(position_def = as.character(position),
                       fantasy_points_ppr_def = fantasy_points_ppr) %>%
                select(-position),
              by = c("player_id", "player_name", "recent_team", "season", "week")) %>%
    mutate(position = as.factor(ifelse(is.na(position_off),position_def,position_off)),
           fantasy_points_ppr = ifelse(is.na(fantasy_points_ppr_off),fantasy_points_ppr_def,fantasy_points_ppr_off)) %>%
    {
      if(input$war_league == "PPR") {
        mutate(.,fantasy_points_league=fantasy_points_ppr)
      } else if(input$war_league == "mPPR") {
        left_join(.,
                  ff_playerscores(ffscrapr::mfl_connect(max(input$war_season), league_id = 60206),
                                  season=max(input$war_season),
                                  week=1:max(weekly_join %>% filter(season==max(input$war_season)) %>% mutate(week=as.numeric(week)) %>% select(week))) %>%
                    mutate(season = as.factor(season),
                           week = as.factor(week)) %>%
                    group_by(mfl_id=player_id,season,week) %>%
                    summarise(
                      fantasy_points_league=mean(points)
                    ) %>%
                    ungroup() %>%
                    left_join(
                      ffscrapr::dp_playerids() %>% filter(!is.na(mfl_id),!is.na(gsis_id)) %>% mutate(player_id = as.factor(gsis_id)) %>% select(mfl_id,player_id) |> distinct()
                    ) %>%
                    filter(!is.na(player_id)) %>%
                    select(-mfl_id)
        )
      } else if(grepl("mfl", input$war_league, fixed = TRUE)) {
        left_join(.,
                  ffscrapr::ff_playerscores(ffscrapr::mfl_connect(max(input$war_season), league_id = gsub("mfl", "", input$war_league)),
                                            season=max(input$war_season),
                                            week=1:max(weekly_join %>% filter(season==max(input$war_season)) %>% mutate(week=as.numeric(week)) %>% select(week))) %>%
                    mutate(season = as.factor(season),
                           week = as.factor(week)) %>%
                    group_by(mfl_id=player_id,season,week) %>%
                    summarise(
                      fantasy_points_league=mean(points)
                    ) %>%
                    ungroup() %>%
                    left_join(
                      ffscrapr::dp_playerids() %>% filter(!is.na(mfl_id),!is.na(gsis_id)) %>% mutate(player_id = as.factor(gsis_id)) %>% select(mfl_id,player_id) |> distinct()
                    ) %>%
                    filter(!is.na(player_id)) %>%
                    select(-mfl_id)
        )
      } else {
        left_join(.,
                  ffscrapr::ff_scoringhistory(ffscrapr::sleeper_connect(season = (max(as.numeric(input$war_season))), league_id = as.character(input$war_league)),
                                    season = (max(as.numeric(input$war_season)))) %>%
                    mutate(season = as.factor(season),
                           week = as.factor(week),
                           player_id = gsis_id) %>%
                    summarise(
                      fantasy_points_league=mean(points),
                      .by = c(player_id,season,week)
                    ) |>
                    filter(!is.na(player_id))
        )
      }
    }

                   qb_mppr <- war_raw %>%
                     filter(position=="QB",season == f_years, week %in% f_week)

                   rb_mppr <- war_raw %>%
                     filter(position=="RB",season == f_years, week %in% f_week)

                   wr_mppr <- war_raw %>%
                     filter(position=="WR",season == f_years, week %in% f_week)

                   TE_mppr <- war_raw %>%
                     filter(position=="TE",season == f_years, week %in% f_week)

                   di_mppr <- war_raw %>%
                     filter(position=="DT",season == f_years, week %in% f_week)

                   de_mppr <- war_raw %>%
                     filter(position=="DE",season == f_years, week %in% f_week)

                   lb_mppr <- war_raw %>%
                     filter(position=="LB",season == f_years, week %in% f_week)

                   cb_mppr <- war_raw %>%
                     filter(position=="CB",season == f_years, week %in% f_week)

                   s_mppr <- war_raw %>%
                     filter(position=="S",season  == f_years, week %in% f_week)

                   # Offense #####

                   #QB
                   qb_top_mppr <- qb_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(n = as.numeric(input$war_teams)*as.numeric(input$war_qb), wt = total_pts)

                   qb_top_mppr <- qb_top_mppr$player_id

                   qb_ave_metrics_mppr <- qb_mppr %>%
                     filter(player_id %in% qb_top_mppr)

                   #RB
                   rb_top_mppr <- rb_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(n = as.numeric(input$war_teams)*as.numeric(input$war_rb), wt = total_pts)

                   rb_top_mppr <- rb_top_mppr$player_id

                   rb_ave_metrics_mppr <- rb_mppr %>%
                     filter(player_id %in% rb_top_mppr)

                   #WR

                   wr_top_mppr <- wr_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_wr), wt = total_pts)

                   wr_top_player_ids_mppr <- wr_top_mppr$player_id

                   wr_ave_metrics_mppr <- wr_mppr %>%
                     filter(player_id %in% wr_top_player_ids_mppr)

                   #TE1

                   te_top_mppr <- TE_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_te), wt = total_pts)

                   te_player_ids_mppr <- te_top_mppr$player_id

                   te_ave_metrics_mppr <- TE_mppr %>%
                     filter(player_id %in% te_player_ids_mppr)

                   # RECEIVER FLEX

                   #WR qualifying flex pool
                   wr_flex_mppr <- wr_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_wr)+as.numeric(input$war_teams), wt = total_pts) %>%
                     top_n(-(as.numeric(input$war_teams)*as.numeric(input$war_wr)), wt = total_pts)

                   wr_flex_player_ids_mppr <- wr_flex_mppr$player_id

                   wr_flex_mppr <- wr_mppr %>%
                     filter(player_id %in% wr_flex_player_ids_mppr)

                   #TE qualifying flex pool
                   te_flex_mppr <- TE_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_te)+as.numeric(input$war_teams), wt = total_pts) %>%
                     top_n(-(as.numeric(input$war_teams)*as.numeric(input$war_te)), wt = total_pts)

                   te_flex_player_ids_mppr <- te_flex_mppr$player_id

                   te_flex_mppr <- TE_mppr %>%
                     filter(player_id %in% te_flex_player_ids_mppr)


                   #calculate average of the best 12 flex qualifiers
                   rec_mppr <- bind_rows(wr_flex_mppr,te_flex_mppr)

                   rec_player_ids_mppr <- rec_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_rec), wt = total_pts)

                   rec_player_ids_mppr <- rec_player_ids_mppr$player_id

                   rec_ave_metrics_mppr <- rec_mppr %>%
                     filter(player_id %in% rec_player_ids_mppr)

                   #FLEX

                   #RB qualifying flex pool
                   rb_flex_mppr <- rb_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_rb)+as.numeric(input$war_teams), wt = total_pts) %>%
                     top_n(-(as.numeric(input$war_teams)*as.numeric(input$war_rb)), wt = total_pts)

                   rb_flex_player_ids_mppr <- rb_flex_mppr$player_id

                   rb_flex_mppr <- rb_mppr %>%
                     filter(player_id %in% rb_flex_player_ids_mppr)

                   #calculate average of the best 12 flex qualifiers
                   flex_mppr <- bind_rows(rb_flex_mppr,wr_flex_mppr,te_flex_mppr)

                   flex_player_ids_mppr <- flex_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_flx), wt = total_pts)

                   flex_player_ids_mppr <- flex_player_ids_mppr$player_id

                   flx_ave_metrics_mppr <- flex_mppr %>%
                     filter(player_id %in% flex_player_ids_mppr)

                   #####

                   # Indivisual Defense Player ######

                   # DI
                   di_top_mppr <- di_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(n = as.numeric(input$war_teams)*as.numeric(input$war_di), wt = total_pts)

                   di_top_mppr <- di_top_mppr$player_id

                   di_ave_metrics_mppr <- di_mppr %>%
                     filter(player_id %in% di_top_mppr)

                   #DE
                   de_top_mppr <- de_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(n = as.numeric(input$war_teams)*as.numeric(input$war_de), wt = total_pts)

                   de_top_mppr <- de_top_mppr$player_id

                   de_ave_metrics_mppr <- de_mppr %>%
                     filter(player_id %in% de_top_mppr)

                   #LB

                   lb_top_mppr <- lb_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_lb), wt = total_pts)

                   lb_top_player_ids_mppr <- lb_top_mppr$player_id

                   lb_ave_metrics_mppr <- lb_mppr %>%
                     filter(player_id %in% lb_top_player_ids_mppr)

                   #CB

                   cb_top_mppr <- cb_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_cb), wt = total_pts)

                   cb_player_ids_mppr <- cb_top_mppr$player_id

                   cb_ave_metrics_mppr <- cb_mppr %>%
                     filter(player_id %in% cb_player_ids_mppr)

                   #S

                   s_top_mppr <- s_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_ss), wt = total_pts)

                   s_player_ids_mppr <- s_top_mppr$player_id

                   s_ave_metrics_mppr <- s_mppr %>%
                     filter(player_id %in% s_player_ids_mppr)


                   #FLEX

                   #DI qualifying flex pool
                   di_flex_mppr <- di_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_di)+as.numeric(input$war_teams), wt = total_pts) %>%
                     top_n(-(as.numeric(input$war_teams)*as.numeric(input$war_di)), wt = total_pts)

                   di_flex_player_ids_mppr <- di_flex_mppr$player_id

                   di_flex_mppr <- di_mppr %>%
                     filter(player_id %in% di_flex_player_ids_mppr)

                   #DE qualifying flex pool
                   de_flex_mppr <- de_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_de)+as.numeric(input$war_teams), wt = total_pts) %>%
                     top_n(-(as.numeric(input$war_teams)*as.numeric(input$war_de)), wt = total_pts)

                   de_flex_player_ids_mppr <- de_flex_mppr$player_id

                   de_flex_mppr <- de_mppr %>%
                     filter(player_id %in% de_flex_player_ids_mppr)

                   #LB qualifying flex pool
                   lb_flex_mppr <- lb_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_lb)+as.numeric(input$war_teams), wt = total_pts) %>%
                     top_n(-(as.numeric(input$war_teams)*as.numeric(input$war_lb)), wt = total_pts)

                   lb_flex_player_ids_mppr <- lb_flex_mppr$player_id

                   lb_flex_mppr <- lb_mppr %>%
                     filter(player_id %in% lb_flex_player_ids_mppr)

                   #CB qualifying flex pool
                   cb_flex_mppr <- cb_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_cb)+as.numeric(input$war_teams), wt = total_pts) %>%
                     top_n(-(as.numeric(input$war_teams)*as.numeric(input$war_cb)), wt = total_pts)

                   cb_flex_player_ids_mppr <- cb_flex_mppr$player_id

                   cb_flex_mppr <- cb_mppr %>%
                     filter(player_id %in% cb_flex_player_ids_mppr)

                   #S qualifying flex pool
                   s_flex_mppr <- s_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_ss)+as.numeric(input$war_teams), wt = total_pts) %>%
                     top_n(-(as.numeric(input$war_teams)*as.numeric(input$war_ss)), wt = total_pts)

                   s_flex_player_ids_mppr <- s_flex_mppr$player_id

                   s_flex_mppr <- s_mppr %>%
                     filter(player_id %in% s_flex_player_ids_mppr)

                   #calculate average of the best 12 flex qualifiers
                   flex_mppr_def <- bind_rows(di_flex_mppr,de_flex_mppr,lb_flex_mppr,cb_flex_mppr,s_flex_mppr)

                   flex_player_ids_mppr_def <- flex_mppr_def %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_idp), wt = total_pts)

                   flex_player_ids_mppr_def <- flex_player_ids_mppr_def$player_id

                   flx_ave_metrics_mppr_def <- flex_mppr_def %>%
                     filter(player_id %in% flex_player_ids_mppr_def)
                   #####

                   #fantasy team average weekly score (weekfp)
                   ({
                     if (as.numeric(input$war_qb) > 0) {
                       (describe(qb_ave_metrics_mppr$fantasy_points_league)  %>% select(mean)) * as.numeric(input$war_qb)
                     } else {
                       0
                     }
                   } +
                     {
                       if (as.numeric(input$war_rb) > 0) {
                         (describe(rb_ave_metrics_mppr$fantasy_points_league)  %>% select(mean)) * as.numeric(input$war_rb)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_wr) > 0) {
                         (describe(wr_ave_metrics_mppr$fantasy_points_league)  %>% select(mean)) * as.numeric(input$war_wr)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_te) > 0) {
                         (describe(te_ave_metrics_mppr$fantasy_points_league)  %>% select(mean)) * as.numeric(input$war_te)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_rec) > 0) {
                         (describe(flx_ave_metrics_mppr$fantasy_points_league) %>% select(mean)) * as.numeric(input$war_rec)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_flx) > 0) {
                         (describe(flx_ave_metrics_mppr$fantasy_points_league) %>% select(mean)) * as.numeric(input$war_flx)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_di) > 0) {
                         (describe(di_ave_metrics_mppr$fantasy_points_league) %>% select(mean)) * as.numeric(input$war_di)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_de) > 0) {
                         (describe(de_ave_metrics_mppr$fantasy_points_league) %>% select(mean)) * as.numeric(input$war_de)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_lb) > 0) {
                         (describe(lb_ave_metrics_mppr$fantasy_points_league) %>% select(mean)) * as.numeric(input$war_lb)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_cb) > 0) {
                         (describe(cb_ave_metrics_mppr$fantasy_points_league) %>% select(mean)) * as.numeric(input$war_cb)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_ss) > 0) {
                         (describe(s_ave_metrics_mppr$fantasy_points_league) %>% select(mean)) * as.numeric(input$war_ss)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_idp) > 0) {
                         (
                           describe(flx_ave_metrics_mppr_def$fantasy_points_league) %>% select(mean)
                         ) * as.numeric(input$war_idp)
                       } else {
                         0
                       }
                     } +
                     0) %>% as_vector() -> weekfp

                   #fantasy team average weekly score standard deviation (weeksd)
                   (sqrt({
                     if (as.numeric(input$war_qb) > 0) {
                       ((
                         describe(qb_ave_metrics_mppr$fantasy_points_league)  %>% select(sd)
                       ) ^ 2) * as.numeric(input$war_qb)
                     } else {
                       0
                     }
                   } +
                     {
                       if (as.numeric(input$war_rb) > 0) {
                         ((
                           describe(rb_ave_metrics_mppr$fantasy_points_league)  %>% select(sd)
                         ) ^ 2) * as.numeric(input$war_rb)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_wr) > 0) {
                         ((
                           describe(wr_ave_metrics_mppr$fantasy_points_league)  %>% select(sd)
                         ) ^ 2) * as.numeric(input$war_wr)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_te) > 0) {
                         ((
                           describe(te_ave_metrics_mppr$fantasy_points_league)  %>% select(sd)
                         ) ^ 2) * as.numeric(input$war_te)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_rec) > 0) {
                         ((
                           describe(flx_ave_metrics_mppr$fantasy_points_league) %>% select(sd)
                         ) ^ 2) * as.numeric(input$war_rec)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_flx) > 0) {
                         ((
                           describe(flx_ave_metrics_mppr$fantasy_points_league) %>% select(sd)
                         ) ^ 2) * as.numeric(input$war_flx)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_di) > 0) {
                         ((
                           describe(di_ave_metrics_mppr$fantasy_points_league) %>% select(sd)
                         ) ^ 2) * as.numeric(input$war_di)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_de) > 0) {
                         ((
                           describe(de_ave_metrics_mppr$fantasy_points_league) %>% select(sd)
                         ) ^ 2) * as.numeric(input$war_de)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_lb) > 0) {
                         ((
                           describe(lb_ave_metrics_mppr$fantasy_points_league) %>% select(sd)
                         ) ^ 2) * as.numeric(input$war_lb)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_cb) > 0) {
                         ((
                           describe(cb_ave_metrics_mppr$fantasy_points_league) %>% select(sd)
                         ) ^ 2) * as.numeric(input$war_cb)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_ss) > 0) {
                         ((
                           describe(s_ave_metrics_mppr$fantasy_points_league) %>% select(sd)
                         ) ^ 2) * as.numeric(input$war_ss)
                       } else {
                         0
                       }
                     } +
                     {
                       if (as.numeric(input$war_idp) > 0) {
                         ((
                           describe(flx_ave_metrics_mppr_def$fantasy_points_league) %>% select(sd)
                         ) ^ 2) * as.numeric(input$war_idp)
                       } else {
                         0
                       }
                     } +
                     0)) %>% as_vector() -> weeksd

                   ### QB #####

                   qb_top_mppr <- qb_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_qb), wt = total_pts)
                   qb_names_mppr <- qb_top_mppr$player_id

                   qb_top_mppr <- qb_mppr %>%
                     filter(player_id %in% qb_names_mppr)

                   #find replacement level player_id
                   replace_qb <- qb_top_mppr %>%
                     group_by(player_id) %>%
                     summarize(pts = sum(fantasy_points_league)) %>%
                     arrange(pts) %>%
                     top_n(-1,wt = pts) %>% top_n(-1,wt = player_id) %>% select(player_id) %>% as_vector()


                   qb_mppr <- mutate(qb_mppr, exp_team_score = (fantasy_points_league - (describe(qb_ave_metrics_mppr$fantasy_points_league) %>% select(mean) %>% as_vector()) + weekfp))

                   #find all rb player_ids' win percentage using pnorm function
                   for (i in 1:nrow(qb_mppr)) {
                     qb_mppr$win_prob[i] <- pnorm(qb_mppr$exp_team_score[i], weekfp, weeksd)
                   }

                   {
                     if(!is_empty(replace_qb)){
                       #find replacement level player_id average
                       replace_wp_qb <- qb_mppr %>%
                         filter(player_id == replace_qb) %>%
                         group_by(player_id) %>%
                         summarise(games = n(), pts = sum(fantasy_points_league), mean = pts/games, per = mean(win_prob)) %>% select(per) %>% as_vector()

                       #find average (WAA)
                       replace_waa_qb <- qb_mppr %>%
                         group_by(player_id) %>%
                         summarize(win_per = mean(win_prob)) %>%
                         top_n(as.numeric(input$war_teams)*as.numeric(input$war_qb)) %>%
                         top_n(-1, wt = win_per) %>%
                         summarise(mean = mean(win_per)) %>% as_vector()

                       war_qb_mppr <- qb_mppr %>%
                         mutate(war_pre = win_prob - replace_wp_qb,
                                sd_war_pre = war_pre+abs(min(war_pre))) |>
                         summarize(
                           Position = "QB",
                           season = f_years,
                           # week = f_week,
                           Avr_Win_Percent = mean(win_prob),
                           Games = n(),
                           Wins_exp = Avr_Win_Percent * as.numeric(Games),
                           WAA = round(Wins_exp - (
                             replace_waa_qb * as.numeric(Games)
                           ), 2),
                           repl_per = replace_wp_qb,
                           repl_wins = replace_wp_qb * as.numeric(Games),
                           WAR = round(Wins_exp - repl_wins, 2),
                           consistency_temp= sd(sd_war_pre),
                           Ave_Week_Points = round(mean(fantasy_points_league), 2),
                           .by = player_id
                         ) %>%
                         arrange(desc(WAR)) %>% filter(Games >= as.numeric(input$war_games)) %>%
                         select(player_id, season, Position, Ave_Week_Points, Avr_Win_Percent, WAR, WAA, Games, consistency_temp)
                     } else {
                       war_qb_mppr <- tibble::tibble()
                     }
                   }
                   #####

                   ### RB #####

                   rb_top_mppr <- rb_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_rb), wt = total_pts)
                   rb_names_mppr <- rb_top_mppr$player_id

                   rb_top_mppr <- rb_mppr %>%
                     filter(player_id %in% rb_names_mppr)

                   # describe(rb_top_mppr$fantasy_points_league)

                   #find replacement level player_id
                   replace_rb <- rb_top_mppr %>%
                     group_by(player_id) %>%
                     summarize(pts = sum(fantasy_points_league)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_rb),wt = pts) %>%
                     top_n(-1,wt = pts) %>% top_n(-1,wt = player_id) %>% select(player_id) %>% as_vector()

                   #Now to compute a fantasy teams expected weekly score based on individual player_id performance
                   rb_mppr <- mutate(rb_mppr, exp_team_score = (fantasy_points_league - (describe(rb_ave_metrics_mppr$fantasy_points_league) %>% select(mean) %>% as_vector()) + weekfp))

                   #find all rb player_ids' win percentage using pnorm function
                   for (i in 1:nrow(rb_mppr)) {
                     rb_mppr$win_prob[i] <- pnorm(rb_mppr$exp_team_score[i], weekfp, weeksd)
                   }

                   {
                     if(!is_empty(replace_rb)){
                       #find replacement level player_id average
                       replace_wp_rb <- rb_mppr %>%
                         filter(player_id == replace_rb) %>%
                         group_by(player_id) %>%
                         summarise(games = n(), pts = sum(fantasy_points_league), mean = pts/games, per = mean(win_prob)) %>% select(per) %>% as_vector()

                       #find average (WAA)
                       replace_waa_rb <- rb_mppr %>%
                         group_by(player_id) %>%
                         summarize(win_per = mean(win_prob)) %>%
                         top_n(as.numeric(input$war_teams)*as.numeric(input$war_rb)) %>%
                         top_n(-1, wt = win_per) %>%
                         summarise(mean = mean(win_per)) %>% as_vector()

                       war_rb_mppr <- rb_mppr %>%
                         filter(!is.na(fantasy_points_league)) |>
                         mutate(war_pre = win_prob - replace_wp_rb,
                                sd_war_pre = war_pre-min(war_pre)) |>
                         summarize(
                           Position = "RB",
                           season = f_years,
                           # week = f_week,
                           Avr_Win_Percent = mean(win_prob),
                           Games = n(),
                           Wins_exp = Avr_Win_Percent * as.numeric(Games),
                           WAA = round(Wins_exp - (
                             replace_waa_rb * as.numeric(Games)
                           ), 2),
                           repl_per = replace_wp_rb,
                           repl_wins = replace_wp_rb * as.numeric(Games),
                           WAR = round(Wins_exp - repl_wins, 2),
                           consistency_temp= sd(sd_war_pre),
                           Ave_Week_Points = round(mean(fantasy_points_league), 2),
                           .by = player_id
                         ) %>%
                         arrange(desc(WAR)) %>% filter(Games >= as.numeric(input$war_games)) %>%
                         select(player_id, season, Position, Ave_Week_Points, Avr_Win_Percent, WAR, WAA, Games, consistency_temp)
                     } else {
                       war_rb_mppr <- tibble::tibble()
                     }
                   }
                   #####

                   ### WR #####

                   wr_top_mppr <- wr_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_wr), wt = total_pts)
                   wr_names_mppr <- wr_top_mppr$player_id

                   wr_top_mppr <- wr_mppr %>%
                     filter(player_id %in% wr_names_mppr)

                   # describe(wr_top_mppr$fantasy_points_league)

                   #find replacement level player_id
                   replace_wr <- wr_top_mppr %>%
                     group_by(player_id) %>%
                     summarize(pts = sum(fantasy_points_league)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_wr),wt = pts) %>%
                     top_n(-1,wt = pts) %>% top_n(-1,wt = player_id) %>% select(player_id) %>% as_vector()

                   #Now to compute a fantasy input$war_teams expected weekly score based on individual player_id performance
                   wr_mppr <- mutate(wr_mppr, exp_team_score = (fantasy_points_league - (describe(wr_ave_metrics_mppr$fantasy_points_league) %>% select(mean) %>% as_vector())) + weekfp)

                   #find all rb player_ids' win percentage using pnorm function
                   for (i in 1:nrow(wr_mppr)) {
                     wr_mppr$win_prob[i] <- pnorm(wr_mppr$exp_team_score[i], weekfp, weeksd)
                   }

                   {
                     if(!is_empty(replace_wr)){
                       #find replacement level player_id average
                       replace_wp_wr <- wr_mppr %>%
                         filter(player_id == replace_wr) %>%
                         group_by(player_id) %>%
                         summarise(games = n(), pts = sum(fantasy_points_league), mean = pts/games, per = mean(win_prob)) %>% select(per) %>% as_vector()

                       #find average (WAA)
                       replace_waa_wr <- wr_mppr %>%
                         group_by(player_id) %>%
                         summarize(win_per = mean(win_prob)) %>%
                         top_n(as.numeric(input$war_teams)*as.numeric(input$war_wr)) %>%
                         top_n(-1, wt = win_per) %>%
                         summarise(mean = mean(win_per)) %>%  as_vector()

                       war_wr_mppr <- wr_mppr %>%
                         filter(!is.na(fantasy_points_league)) |>
                         mutate(war_pre = win_prob - replace_wp_wr,
                                sd_war_pre = war_pre+abs(min(war_pre))) |>
                         summarize(
                           Position = "WR",
                           season = f_years,
                           # week = f_week,
                           Avr_Win_Percent = mean(win_prob),
                           Games = n(),
                           Wins_exp = Avr_Win_Percent * as.numeric(Games),
                           WAA = round(Wins_exp - (
                             replace_waa_wr * as.numeric(Games)
                           ), 2),
                           repl_per = replace_wp_wr,
                           repl_wins = replace_wp_wr * as.numeric(Games),
                           WAR = round(Wins_exp - repl_wins, 2),
                           consistency_temp= sd(sd_war_pre),
                           Ave_Week_Points = round(mean(fantasy_points_league), 2),
                           .by = player_id
                         ) %>%
                         arrange(desc(WAR)) %>% filter(Games >= as.numeric(input$war_games)) %>%
                         select(player_id, season, Position, Ave_Week_Points, Avr_Win_Percent, WAR, WAA, Games, consistency_temp)
                     } else {
                       war_wr_mppr <- tibble::tibble()
                     }
                   }
                   #####

                   ### TE #####

                   te_top_mppr <- TE_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league,na.rm = TRUE)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_te), wt = total_pts)
                   te_names_mppr <- te_top_mppr$player_id

                   te_top_mppr <- TE_mppr %>%
                     filter(player_id %in% te_names_mppr)

                   #find replacement level player_id
                   replace_te <- te_top_mppr %>%
                     group_by(player_id) %>%
                     summarize(pts = sum(fantasy_points_league)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_te),wt = pts) %>%
                     top_n(-1,wt = pts) %>% top_n(-1,wt = player_id) %>% select(player_id) %>% as_vector()

                   #Now to compute a fantasy input$war_teams expected weekly score based on individual player_id performance
                   te_mppr <- mutate(TE_mppr, exp_team_score = (fantasy_points_league - (describe(te_ave_metrics_mppr$fantasy_points_league) %>% select(mean) %>% as_vector())) + weekfp)

                   #find all rb player_ids' win percentage using pnorm function
                   for (i in 1:nrow(te_mppr)) {
                     te_mppr$win_prob[i] <- pnorm(te_mppr$exp_team_score[i], weekfp, weeksd)
                   }

                   {
                     if(!is_empty(replace_te)){
                       #find replacement level player_id average
                       replace_wp_te <- te_mppr %>%
                         filter(player_id == replace_te) %>%
                         group_by(player_id) %>%
                         summarise(games = n(), pts = sum(fantasy_points_league), mean = pts/games, per = mean(win_prob)) %>% select(per) %>% as_vector()

                       #find average (WAA)
                       replace_waa_te <- te_mppr %>%
                         group_by(player_id) %>%
                         summarize(win_per = mean(win_prob)) %>%
                         top_n(as.numeric(input$war_teams)*as.numeric(input$war_te)) %>%
                         top_n(-1, wt = win_per) %>%
                         summarise(mean = mean(win_per)) %>% as_vector()

                       war_te_mppr <- te_mppr %>%
                         filter(!is.na(fantasy_points_league)) |>
                         mutate(war_pre = win_prob - replace_wp_te,
                                sd_war_pre = war_pre+abs(min(war_pre))) |>
                         summarize(
                           Position = "TE",
                           season = f_years,
                           # week = f_week,
                           Avr_Win_Percent = mean(win_prob),
                           Games = n(),
                           Wins_exp = Avr_Win_Percent * as.numeric(Games),
                           WAA = round(Wins_exp - (
                             replace_waa_te * as.numeric(Games)
                           ), 2),
                           repl_per = replace_wp_te,
                           repl_wins = replace_wp_te * as.numeric(Games),
                           WAR = round(Wins_exp - repl_wins, 2),
                           consistency_temp= sd(sd_war_pre),
                           Ave_Week_Points = round(mean(fantasy_points_league), 2),
                           .by = player_id
                         ) %>%
                         arrange(desc(WAR)) %>% filter(Games >= as.numeric(input$war_games)) %>%
                         select(player_id, season, Position, Ave_Week_Points, Avr_Win_Percent, WAR, WAA, Games, consistency_temp)
                     } else {
                       war_te_mppr <- tibble::tibble()
                     }
                   }
                   #####


                   ### Put IDP in here #####

                   # DI

                   di_top_mppr <- di_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_di), wt = total_pts)
                   di_names_mppr <- di_top_mppr$player_id

                   di_top_mppr <- di_mppr %>%
                     filter(player_id %in% di_names_mppr)

                   #find replacement level player_id
                   replace_di <- di_top_mppr %>%
                     group_by(player_id) %>%
                     summarize(pts = sum(fantasy_points_league)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_di),wt = pts) %>%
                     top_n(-1,wt = pts) %>% top_n(-1,wt = player_id) %>% select(player_id) %>% as_vector()

                   #Now to compute a fantasy teams expected weekly score based on individual player_id performance
                   di_mppr <- mutate(di_mppr, exp_team_score = (fantasy_points_league - (describe(di_ave_metrics_mppr$fantasy_points_league) %>% select(mean) %>% as_vector())) + weekfp)

                   #find all rb player_ids' win percentage using pnorm function
                   for (i in 1:nrow(di_mppr)) {
                     di_mppr$win_prob[i] <- pnorm(di_mppr$exp_team_score[i], weekfp, weeksd)
                   }

                   {
                     if(!is_empty(replace_di)){
                       #find replacement level player_id average
                       replace_wp_di <- di_mppr %>%
                         filter(player_id == replace_di) %>%
                         summarise(games = n(), pts = sum(fantasy_points_league), mean = pts/games, per = mean(win_prob)) %>% ungroup() %>% select(per) %>% as_vector()

                       #find average (WAA)
                       replace_waa_di <- di_mppr %>%
                         group_by(player_id) %>%
                         summarize(win_per = mean(win_prob)) %>%
                         top_n(as.numeric(input$war_teams)*as.numeric(input$war_di)) %>%
                         top_n(-1) %>%
                         summarise(mean = mean(win_per)) %>%  as_vector()

                       war_di_mppr <- di_mppr %>%
                         filter(!is.na(fantasy_points_league)) |>
                         mutate(war_pre = win_prob - replace_wp_di,
                                sd_war_pre = war_pre+abs(min(war_pre))) |>
                         summarize(
                           Position = "DI",
                           season = f_years,
                           # week = f_week,
                           Avr_Win_Percent = mean(win_prob),
                           Games = n(),
                           Wins_exp = Avr_Win_Percent * as.numeric(Games),
                           WAA = round(Wins_exp - (
                             replace_waa_di * as.numeric(Games)
                           ), 2),
                           repl_per = replace_wp_di,
                           repl_wins = replace_wp_di * as.numeric(Games),
                           WAR = round(Wins_exp - repl_wins, 2),
                           consistency_temp= sd(sd_war_pre),
                           Ave_Week_Points = round(mean(fantasy_points_league), 2),
                           .by = player_id
                         ) %>%
                         arrange(desc(WAR)) %>% filter(Games >= as.numeric(input$war_games)) %>%
                         select(player_id, season, Position, Ave_Week_Points, Avr_Win_Percent, WAR, WAA, Games, consistency_temp)
                     } else {
                       war_di_mppr <- tibble::tibble()
                     }
                   }
                   # DE

                   de_top_mppr <- de_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_de), wt = total_pts)
                   de_names_mppr <- de_top_mppr$player_id

                   de_top_mppr <- de_mppr %>%
                     filter(player_id %in% de_names_mppr)

                   # describe(de_top_mppr$fantasy_points_league)

                   #find replacement level player_id
                   replace_de <- de_top_mppr %>%
                     group_by(player_id) %>%
                     summarize(pts = sum(fantasy_points_league)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_de),wt = pts) %>%
                     top_n(-1,wt = pts) %>% top_n(-1,wt = player_id) %>% select(player_id) %>% as_vector()

                   #Now to compute a fantasy teams expected weekly score based on individual player_id performance
                   de_mppr <- mutate(de_mppr, exp_team_score = (fantasy_points_league - (describe(de_ave_metrics_mppr$fantasy_points_league) %>% select(mean) %>% as_vector())) + weekfp)

                   #find all rb player_ids' win percentage using pnorm function
                   for (i in 1:nrow(de_mppr)) {
                     de_mppr$win_prob[i] <- pnorm(de_mppr$exp_team_score[i], weekfp, weeksd)
                   }

                   {
                     if(!is_empty(replace_de)){
                       #find replacement level player_id average
                       replace_wp_de <- de_mppr %>%
                         filter(player_id == replace_de) %>%
                         summarise(games = n(), pts = sum(fantasy_points_league), mean = pts/games, per = mean(win_prob)) %>% ungroup() %>% select(per) %>% as_vector()

                       #find average (WAA)
                       replace_waa_de <- de_mppr %>%
                         group_by(player_id) %>%
                         summarize(win_per = mean(win_prob)) %>%
                         top_n(as.numeric(input$war_teams)*as.numeric(input$war_de)) %>%
                         top_n(-1) %>%
                         summarise(mean = mean(win_per)) %>%  as_vector()

                       war_de_mppr <- de_mppr %>%
                         filter(!is.na(fantasy_points_league)) |>
                         mutate(war_pre = win_prob - replace_wp_de,
                                sd_war_pre = war_pre+abs(min(war_pre))) |>
                         summarize(
                           Position = "DE",
                           season = f_years,
                           # week = f_week,
                           Avr_Win_Percent = mean(win_prob),
                           Games = n(),
                           Wins_exp = Avr_Win_Percent * as.numeric(Games),
                           WAA = round(Wins_exp - (
                             replace_waa_de * as.numeric(Games)
                           ), 2),
                           repl_per = replace_wp_de,
                           repl_wins = replace_wp_de * as.numeric(Games),
                           WAR = round(Wins_exp - repl_wins, 2),
                           consistency_temp= sd(sd_war_pre),
                           Ave_Week_Points = round(mean(fantasy_points_league), 2),
                           .by = player_id
                         ) %>%
                         arrange(desc(WAR)) %>% filter(Games >= as.numeric(input$war_games)) %>%
                         select(player_id, season, Position, Ave_Week_Points, Avr_Win_Percent, WAR, WAA, Games, consistency_temp)
                     } else {
                       war_de_mppr <- tibble::tibble()
                     }
                   }
                   # LB

                   lb_top_mppr <- lb_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_lb), wt = total_pts)
                   lb_names_mppr <- lb_top_mppr$player_id

                   lb_top_mppr <- lb_mppr %>%
                     filter(player_id %in% lb_names_mppr)

                   #find replacement level player_id
                   replace_lb <- lb_top_mppr %>%
                     group_by(player_id) %>%
                     summarize(pts = sum(fantasy_points_league)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_lb),wt = pts) %>%
                     top_n(-1,wt = pts) %>% top_n(-1,wt = player_id) %>% select(player_id) %>% as_vector()

                   #Now to compute a fantasy teams expected weekly score based on individual player_id performance
                   lb_mppr <- mutate(lb_mppr, exp_team_score = (fantasy_points_league - (describe(lb_ave_metrics_mppr$fantasy_points_league) %>% select(mean) %>% as_vector())) + weekfp)

                   #find all rb player_ids' win percentage using pnorm function
                   for (i in 1:nrow(lb_mppr)) {
                     lb_mppr$win_prob[i] <- pnorm(lb_mppr$exp_team_score[i], weekfp, weeksd)
                   }

                   {
                     if(!is_empty(replace_lb)){
                       #find replacement level player_id average
                       replace_wp_lb <- lb_mppr %>%
                         filter(player_id == replace_lb) %>%
                         summarise(games = n(), pts = sum(fantasy_points_league), mean = pts/games, per = mean(win_prob)) %>% ungroup() %>% select(per) %>% as_vector()

                       #find average (WAA)
                       replace_waa_lb <- lb_mppr %>%
                         group_by(player_id) %>%
                         summarize(win_per = mean(win_prob)) %>%
                         top_n(as.numeric(input$war_teams)*as.numeric(input$war_lb)) %>%
                         top_n(-1) %>%
                         summarise(mean = mean(win_per)) %>%  as_vector()

                       war_lb_mppr <- lb_mppr %>%
                         filter(!is.na(fantasy_points_league)) |>
                         mutate(war_pre = win_prob - replace_wp_lb,
                                sd_war_pre = war_pre+abs(min(war_pre))) |>
                         summarize(
                           Position = "LB",
                           season = f_years,
                           # week = f_week,
                           Avr_Win_Percent = mean(win_prob),
                           Games = n(),
                           Wins_exp = Avr_Win_Percent * as.numeric(Games),
                           WAA = round(Wins_exp - (
                             replace_waa_lb * as.numeric(Games)
                           ), 2),
                           repl_per = replace_wp_lb,
                           repl_wins = replace_wp_lb * as.numeric(Games),
                           WAR = round(Wins_exp - repl_wins, 2),
                           consistency_temp= sd(sd_war_pre),
                           Ave_Week_Points = round(mean(fantasy_points_league), 2),
                           .by = player_id
                         ) %>%
                         arrange(desc(WAR)) %>% filter(Games >= as.numeric(input$war_games)) %>%
                         select(player_id, season, Position, Ave_Week_Points, Avr_Win_Percent, WAR, WAA, Games, consistency_temp)
                     } else {
                       war_lb_mppr <- tibble::tibble()
                     }
                   }
                   # CB

                   cb_top_mppr <- cb_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_cb), wt = total_pts)
                   cb_names_mppr <- cb_top_mppr$player_id

                   cb_top_mppr <- cb_mppr %>%
                     filter(player_id %in% cb_names_mppr)

                   #find replacement level player_id
                   replace_cb <- cb_top_mppr %>%
                     group_by(player_id) %>%
                     summarize(pts = sum(fantasy_points_league)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_cb),wt = pts) %>%
                     top_n(-1,wt = pts) %>% top_n(-1,wt = player_id) %>%select(player_id) %>% as_vector()

                   #Now to compute a fantasy teams expected weekly score based on individual player_id performance
                   cb_mppr <- mutate(cb_mppr, exp_team_score = (fantasy_points_league - (describe(cb_ave_metrics_mppr$fantasy_points_league) %>% select(mean) %>% as_vector())) + weekfp)

                   #find all rb player_ids' win percentage using pnorm function
                   for (i in 1:nrow(cb_mppr)) {
                     cb_mppr$win_prob[i] <- pnorm(cb_mppr$exp_team_score[i], weekfp, weeksd)
                   }

                   {
                     if(!is_empty(replace_cb)){
                       #find replacement level player_id average
                       replace_wp_cb <- cb_mppr %>%
                         filter(player_id == replace_cb) %>%
                         summarise(games = n(), pts = sum(fantasy_points_league), mean = pts/games, per = mean(win_prob)) %>% ungroup() %>% select(per) %>% as_vector()

                       #find average (WAA)
                       replace_waa_cb <- cb_mppr %>%
                         group_by(player_id) %>%
                         summarize(win_per = mean(win_prob)) %>%
                         top_n(as.numeric(input$war_teams)*as.numeric(input$war_cb)) %>%
                         top_n(-1) %>%
                         summarise(mean = mean(win_per)) %>%  as_vector()

                       war_cb_mppr <- cb_mppr %>%
                         filter(!is.na(fantasy_points_league)) |>
                         mutate(war_pre = win_prob - replace_wp_cb,
                                sd_war_pre = war_pre+abs(min(war_pre))) |>
                         summarize(
                           Position = "CB",
                           season = f_years,
                           # week = f_week,
                           Avr_Win_Percent = mean(win_prob),
                           Games = n(),
                           Wins_exp = Avr_Win_Percent * as.numeric(Games),
                           WAA = round(Wins_exp - (
                             replace_waa_cb * as.numeric(Games)
                           ), 2),
                           repl_per = replace_wp_cb,
                           repl_wins = replace_wp_cb * as.numeric(Games),
                           WAR = round(Wins_exp - repl_wins, 2),
                           consistency_temp= sd(sd_war_pre),
                           Ave_Week_Points = round(mean(fantasy_points_league), 2),
                           .by = player_id
                         ) %>%
                         arrange(desc(WAR)) %>% filter(Games >= as.numeric(input$war_games)) %>%
                         select(player_id, season, Position, Ave_Week_Points, Avr_Win_Percent, WAR, WAA, Games, consistency_temp)
                     } else {
                       war_cb_mppr <- tibble::tibble()
                     }
                   }
                   # S

                   s_top_mppr <- s_mppr %>%
                     group_by(player_id) %>%
                     summarise(total_pts = sum(fantasy_points_league)) %>%
                     arrange(desc(total_pts)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_ss), wt = total_pts)
                   s_names_mppr <- s_top_mppr$player_id

                   s_top_mppr <- s_mppr %>%
                     filter(player_id %in% s_names_mppr)

                   # describe(s_top_mppr$fantasy_points_league)

                   #find replacement level player_id
                   replace_s <- s_top_mppr %>%
                     group_by(player_id) %>%
                     summarize(pts = sum(fantasy_points_league)) %>%
                     top_n(as.numeric(input$war_teams)*as.numeric(input$war_ss),wt = pts) %>%
                     top_n(-1,wt = pts) %>%
                     top_n(-1,wt = player_id) %>% select(player_id) %>% as_vector()

                   #Now to compute a fantasy teams expected weekly score based on individual player_id performance
                   s_mppr <- mutate(s_mppr, exp_team_score = (fantasy_points_league - (describe(s_ave_metrics_mppr$fantasy_points_league) %>% select(mean) %>% as_vector())) + weekfp)

                   #find all rb player_ids' win percentage using pnorm function
                   for (i in 1:nrow(s_mppr)) {
                     s_mppr$win_prob[i] <- pnorm(s_mppr$exp_team_score[i], weekfp, weeksd)
                   }

                   {
                     if(!is_empty(replace_s)){
                       #find replacement level player_id average
                       replace_wp_s <- s_mppr %>%
                         filter(player_id == replace_s) %>%
                         summarise(games = n(), pts = sum(fantasy_points_league), mean = pts/games, per = mean(win_prob)) %>% ungroup() %>% select(per) %>% as_vector()

                       #find average (WAA)
                       replace_waa_s <- s_mppr %>%
                         group_by(player_id) %>%
                         summarize(win_per = mean(win_prob)) %>%
                         top_n(as.numeric(input$war_teams)*as.numeric(input$war_ss)) %>%
                         top_n(-1) %>%
                         summarise(mean = mean(win_per)) %>%  as_vector()

                       war_s_mppr <- s_mppr %>%
                         filter(!is.na(fantasy_points_league)) |>
                         mutate(war_pre = win_prob - replace_wp_s,
                                sd_war_pre = war_pre+abs(min(war_pre))) |>
                         summarize(
                           Position = "S",
                           season = f_years,
                           # week = f_week,
                           Avr_Win_Percent = mean(win_prob),
                           Games = n(),
                           Wins_exp = Avr_Win_Percent * as.numeric(Games),
                           WAA = round(Wins_exp - (
                             replace_waa_s * as.numeric(Games)
                           ), 2),
                           repl_per = replace_wp_s,
                           repl_wins = replace_wp_s * as.numeric(Games),
                           WAR = round(Wins_exp - repl_wins, 2),
                           consistency_temp= sd(sd_war_pre),
                           Ave_Week_Points = round(mean(fantasy_points_league), 2),
                           .by = player_id
                         ) %>%
                         arrange(desc(WAR)) %>% filter(Games >= as.numeric(input$war_games)) %>%
                         select(player_id, season, Position, Ave_Week_Points, Avr_Win_Percent, WAR, WAA, Games, consistency_temp)
                     } else {
                       war_s_mppr <- tibble::tibble()
                     }
                   }

                   #####


                   #put it all together for requested top 300 WAR of 2018
                   total_WAR <- bind_rows(
                     war_qb_mppr,
                     war_wr_mppr,
                     war_rb_mppr,
                     war_te_mppr,
                     war_di_mppr,
                     war_de_mppr,
                     war_lb_mppr,
                     war_cb_mppr,
                     war_s_mppr
                   ) |>
                     mutate(
                       season = as.numeric(as.character(season))
                     ) |>
                     ungroup() |>
                     filter(!is.na(WAR)) |>
                     summarise(
                       Position        = first(Position),
                       Ave_Week_Points = mean(Ave_Week_Points,na.rm=TRUE),
                       Avr_Win_Percent = mean(Avr_Win_Percent,na.rm=TRUE),
                       WAR             = sum(WAR,na.rm=TRUE),
                       WAA             = sum(WAA,na.rm=TRUE),
                       Games           = mean(Games,na.rm=TRUE),
                       consistency_temp= mean(consistency_temp),
                       .by = c(player_id, season)
                       ) |>
                     left_join(nflfastR::fast_scraper_roster(as.numeric(f_years)) %>% group_by(player_id=gsis_id) %>% summarise(player=first(full_name),season = first(season)))

}