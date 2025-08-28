library(tidyverse)
library(powerjoin)

league_data <- weekly_join |> 
          select("headshot_url","player_id","sleeper_id","player_name"="player_display_name","recent_team","position","season","week","fantasy_points_ppr","attempts","completions","sacks","carries","receptions",
                                                                        "sack_fumbles","rushing_fumbles","receiving_fumbles","sack_fumbles_lost","rushing_fumbles_lost","receiving_fumbles_lost",
                                                                        "wopr","racr","pacr","touches","yps","ops","epps","fpps","snaps_off","snaps_def","target_share","air_yards_share",
                                                                        "passing_2pt_conversions","rushing_2pt_conversions","receiving_2pt_conversions",ends_with("_exp"),"stat") %>%
          left_join(.,
                ffscrapr::ff_scoringhistory(ffscrapr::sleeper_connect(season = "2024", league_id = "1260312306642845696"),
                season = 2024) %>%
                  mutate(season = as.factor(season),
                        week = as.factor(week)) %>%
                  select(player_id=gsis_id,sleeper_id,season,week,fantasy_points_league=points)
              )|> 
          mutate(
            pass_att          = 0,
            pass_2pt          = 0,
            pass_int          = 0,
            rec_yd            = 0,
            pass_fd           = 0,
            pass_sack         = 0,
            pass_cmp          = 0,
            rush_fd           = 0,
            pass_inc          = 0,
            rec_2pt           = 0,
            rec               = 0,
            rush_2pt          = 0,
            rush_att          = 0,
            rec_td            = 0,
            rush_yd           = 0,
            pass_yd           = 0,
            pass_td           = 0,
            rush_td           = 0,
            fum_lost          = 0,
            fum               = 0,
            rec_fd            = 0,
            bonus_pass_yd_300 = 0,
            bonus_pass_yd_400 = 0,
            bonus_rush_yd_100 = 0,
            bonus_rush_yd_200 = 0,
            bonus_rec_yd_100  = 0,
            bonus_rec_yd_200  = 0,
            xpmiss            = 0,
            xpm               = 0,
            fgmiss            = 0,
            fgm_50p           = 0,
            fgm_40_49         = 0,
            fgm_30_39         = 0,
            fgm_20_29         = 0,
            fgm_0_19          = 0,
            st_fum_rec        = 0,
            st_ff             = 0,
            st_td             = 0,
            fum_rec           = 0,
            fum_rec_td        = 0
          ) %>% 
          powerjoin::power_left_join(
            ffscrapr::ff_scoring(ffscrapr::sleeper_connect(season = 2024, league_id = "1260312306642845696")) %>%
                  #filter(pos %in% c("QB","RB","WR","TE")) %>%
                  pivot_wider(
                    names_from = event,
                    values_from = points,
                    values_fill = 0
                  ) %>%
                  mutate(position = as.factor(pos)),
                by = "position",
                conflict = coalesce_yx
              ) %>%
          mutate(
            incompletions = attempts-completions,
            ep_league =
              ifelse(
                pass_yards_gained_exp >=300 & pass_yards_gained_exp < 400,
                bonus_pass_yd_300,
                0) +
                # ifelse(
                #   pass_yards_gained_exp >= 400,
                #   bonus_pass_yd_400,
                #   0) +
                #     pass_yards_gained_exp      * pass_yd      +
                #     pass_first_down_exp        * pass_fd      +
                #     pass_touchdown_exp         * pass_td      +
                #     attempts                   * pass_att     +
                #     pass_completions_exp       * pass_cmp     +
                #     incompletions              * pass_inc     +
                #     passing_2pt_conversions    * pass_2pt     +
                #     pass_interception_exp      * pass_int     +
                #     sacks                      * pass_sack    +
                #     ifelse(
                #       rush_yards_gained_exp >=100 & rush_yards_gained_exp < 200,
                #       bonus_rush_yd_100,
                #       0) +
                # ifelse(
                #   rush_yards_gained_exp >= 200,
                #   bonus_rush_yd_200,
                #   0) +
                #     rush_yards_gained_exp      * rush_yd      +
                #     rush_first_down_exp        * rush_fd      +
                #     rush_touchdown_exp         * rush_td      +
                #     carries                    * rush_att     +
                #     rushing_2pt_conversions    * rush_2pt     +
                #     ifelse(
                #       rec_yards_gained_exp >=100 & rec_yards_gained_exp < 200,
                #       bonus_rec_yd_100,
                #       0) +
                # ifelse(
                #   rec_yards_gained_exp >= 200,
                #   bonus_rec_yd_200,
                #   0) +
                    rec_yards_gained_exp       * rec_yd       +
                    rec_first_down_exp         * rec_fd       +
                    rec_touchdown_exp          * rec_td       +
                    receptions                 * rec          +
                    receiving_2pt_conversions  * rec_2pt      +
                    (sack_fumbles+rushing_fumbles+receiving_fumbles) * fum +
                    (sack_fumbles_lost+rushing_fumbles_lost+receiving_fumbles_lost)* fum_lost
                )|>filter(season==2024)

league_stats <- league_data |>filter(season==2024) |> 
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  mutate(
                          diff_league = fantasy_points_league - ep_league,
                          diff_ppr    = fantasy_points_ppr    - total_fantasy_points_exp
                        ) %>%
      group_by(recent_team,player_id,sleeper_id,player_name,position,season) %>%
      summarise(
                                  FPTS_League         = sum(fantasy_points_league,na.rm=TRUE),
                                  FPTSpG_League       = mean(fantasy_points_league,na.rm=TRUE),
                                  EP_League           = sum(ep_league,na.rm=TRUE),
                                  EPpG_League         = mean(ep_league,na.rm=TRUE),
                                  Diff_League         = sum(diff_league, na.rm=TRUE),
                                  Diff_PG_League      = mean(diff_league,na.rm=TRUE),
                                  FPTS_PPR            = sum(fantasy_points_ppr),
                                  FPTSpG_PPR          = mean(fantasy_points_ppr),
                                  EP_PPR              = sum(total_fantasy_points_exp,na.rm=TRUE),
                                  EPpG_PPR            = mean(total_fantasy_points_exp,na.rm=TRUE),
                                  Diff_PPR            = sum(diff_ppr, na.rm=TRUE),
                                  Diff_PG_PPR         = mean(diff_ppr,na.rm=TRUE),
                                  wopr                = mean(wopr,na.rm=TRUE),
                                  racr                = mean(racr,na.rm=TRUE),
                                  pacr                = mean(pacr,na.rm=TRUE),
                                  touches             = sum(touches,na.rm=TRUE),
                                  snaps               = sum(snaps_off,na.rm=TRUE),
                                  yps                 = sum(yps,na.rm=TRUE),
                                  opps                = sum(touches,na.rm=TRUE)/sum(snaps_off,na.rm=TRUE),
                                  epps                = sum(epps,na.rm=TRUE),
                                  fpps                = sum(fpps,na.rm=TRUE),
                                  # routes              = sum(routes,na.rm=TRUE),
                                  # trr                 = mean(trr,na.rm=TRUE),
                                  # yprr                = mean(yprr,na.rm=TRUE),
                                  target_share        = mean(target_share,na.rm=TRUE)*100,
                                  air_yards_share     = mean(air_yards_share,na.rm=TRUE)*100
                                  ) %>%
                        filter(
                          touches>=input$threshold_tbl_off,
                          snaps  >=input$threshold_tbl_off_snap
                          ) %>%
                        {
                          if(input$league_off %in% c("Bitte ID eintragen","","DFS")) {
                            mutate(.,franchise_name="Keine Liga ausgewählt")
                          } else {
                            left_join(.,
                              ffscrapr::ff_rosters(ffscrapr::sleeper_connect(season = max(input$season_tbl_off), league_id = input$league_off)) %>%
                                select(sleeper_id=player_id,franchise_name) |>
                                left_join(
                                  ffscrapr::dp_playerids() %>% filter(!is.na(sleeper_id),!is.na(gsis_id)) %>% mutate(player_id = as.factor(gsis_id)) %>% select(sleeper_id,player_id) |> distinct()
                                )
                              )
                          }
                        } %>%
                        mutate(franchise_name = ifelse(is.na(franchise_name),"Free Agent",franchise_name)) %>%
                        ungroup()  %>%
                        arrange(-wopr) %>%
                        mutate(rank = row_number()) %>%
                        relocate(rank) %>%
                        select(-c(sleeper_id,player_id))

t <- ffscrapr::ff_scoring(ffscrapr::sleeper_connect(season = 2024, league_id = "1260312306642845696")) %>%
                  filter(pos %in% c("QB","RB","WR","TE")) 
t2 <- t |> pivot_wider(
                    names_from = event,
                    values_from = points,
                    values_fill = 0
                  ) |> 
  mutate(position = as.factor(pos))
t3 <- weekly_join |> left_join(t2)
