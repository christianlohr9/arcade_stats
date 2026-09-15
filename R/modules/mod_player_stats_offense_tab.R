#' Player Stats Offense Tab Module
#'
#' @description A shiny module for the Player Stats Offense tab
#'
#' @param id The module namespace id
#'
#' @name mod_player_stats_offense_tab

#' @describeIn mod_player_stats_offense_tab UI function
#' @export
mod_player_stats_offense_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Player Stats Offense",
    fluidRow(column(3,
      pickerInput(ns("stat_tbl_off"),
        label = "Choose a Stat",
        choices = levels(as.factor(c("Passing","Rushing/Receiving"))),
        selected = "Rushing/Receiving",
        options = list(`actions-box` = TRUE),
        multiple = FALSE)
    ),
    column(3,pickerInput(ns("team_tbl_off"),
      label = "Choose a Team",
      choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LA", "LAC", "LV", "MIA", "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS"), 
      selected = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LA", "LAC", "LV", "MIA", "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS"),  
      options = list(`actions-box` = TRUE),
      multiple = TRUE)
    ),
    column(3,pickerInput(ns("week_tbl_off"),
      label = "Choose a Week",
      choices = 1:17,
      selected = 1:17,
      options = list(`actions-box` = TRUE),
      multiple = TRUE)
    ),
    column(3,pickerInput(ns("season_tbl_off"),
      label = "Choose a Season",
      choices = 1999:lubridate::year(lubridate::today()),
      selected = ifelse(yday(lubridate::today())>=250,lubridate::year(lubridate::today()),lubridate::year(lubridate::today())-1),
      multiple = TRUE)
    )),
    fluidRow(column(3,
      pickerInput(ns("pos_tbl_off"),
        label = "Choose a Position",
        choices = c("QB","RB","WR","TE"),  
        selected = c("QB","RB","WR","TE"),  
        multiple = TRUE)
    ),
    column(3,numericInput(ns("threshold_tbl_off"),
      label = "Minimum Touches",
      value = 1)
    ),
    column(3,numericInput(ns("threshold_tbl_off_snap"),
      label = "Minimum Snaps",
      value = 0)
    ),
    column(3,textInput(ns("league_off"),
      label = "Sleeper League ID",
      value = "Bitte ID eintragen")
    )),
    fluidRow(
      column(2,
        useWaiter(),
        actionButton(ns("league_stats_calc"),
          label = "Calculate"
        )
      ),
      column(2,
        downloadButton(ns("tbl_off_download"),
          label = "Download CSV",
          class = "btn-primary"
        )
      )),
    DT::dataTableOutput(ns("Stats_League_Offense"))
  )
}

#' @describeIn mod_player_stats_offense_tab Server function
#' @export
mod_player_stats_offense_tab_server <- function(id, weekly_join, show_no_data_message, waiting_screen) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store league stats
    league_stats_reactive <- reactiveVal(NULL)
    
    # Update choices dynamically when data is available
    observe({
      if(!is.null(weekly_join) && nrow(weekly_join) > 0) {
        updatePickerInput(session, "team_tbl_off",
          choices = levels(as.factor(weekly_join$team)),
          selected = levels(as.factor(weekly_join$team))
        )
        
        updatePickerInput(session, "pos_tbl_off",
          choices = levels(weekly_join$position),
          selected = levels(weekly_join$position)
        )
      }
    })
    
    # League stats calculation observeEvent
    observeEvent(input$league_stats_calc, {
      if(nrow(weekly_join) == 0) {
        show_no_data_message()
        return()
      }

      waiter_show(html = waiting_screen, color = "black")

      league_data <- reactive({
        weekly_join |> 
          select("headshot_url","player_id","sleeper_id","player_name"="player_display_name","team","position","season","week","fantasy_points_ppr","attempts","completions","sacks_suffered","carries","receptions",
                                                                        "sack_fumbles","rushing_fumbles","receiving_fumbles","sack_fumbles_lost","rushing_fumbles_lost","receiving_fumbles_lost",
                                                                        "wopr","racr","pacr","touches","yps","ops","epps","fpps","snaps_off","snaps_def","target_share","air_yards_share",
                                                                        "passing_2pt_conversions","rushing_2pt_conversions","receiving_2pt_conversions",ends_with("_exp"),"stat") %>%
          {
            if(input$league_off %in% c("Bitte ID eintragen","","DFS")) {
              mutate(.,fantasy_points_league=0)
            } else {
              left_join(.,
                ffscrapr::ff_scoringhistory(ffscrapr::sleeper_connect(season = max(input$season_tbl_off), league_id = input$league_off),
                season = max(as.numeric(input$season_tbl_off))) %>%
                  mutate(season = as.factor(season),
                week = as.factor(week)) %>%
                  select(player_id=gsis_id,sleeper_id,season,week,fantasy_points_league=points)
              )
            }
          } |> 
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
          {
            if(input$league_off %in% c("Bitte ID eintragen","","DFS")) {
              ungroup(.,)
            } else {
              powerjoin::power_left_join(.,
                ffscrapr::ff_scoring(ffscrapr::sleeper_connect(season = max(input$season_tbl_off), league_id = input$league_off)) %>%
                  filter(pos %in% c("QB","RB","WR","TE")) %>%
                  pivot_wider(
                    names_from = event,
                    values_from = points,
                    values_fill = 0
                  ) %>%
                  mutate(position = as.factor(pos)),
                by = "position",
                conflict = powerjoin::coalesce_yx
              )
            }
          } %>%
          mutate(
            incompletions = attempts-completions,
            ep_league =
              ifelse(
                pass_yards_gained_exp >=300 & pass_yards_gained_exp < 400,
                bonus_pass_yd_300,
                0) +
                ifelse(
                  pass_yards_gained_exp >= 400,
                  bonus_pass_yd_400,
                  0) +
                    pass_yards_gained_exp      * pass_yd      +
                    pass_first_down_exp        * pass_fd      +
                    pass_touchdown_exp         * pass_td      +
                    attempts                   * pass_att     +
                    pass_completions_exp       * pass_cmp     +
                    incompletions              * pass_inc     +
                    passing_2pt_conversions    * pass_2pt     +
                    pass_interception_exp      * pass_int     +
                    sacks_suffered             * pass_sack    +
                    ifelse(
                      rush_yards_gained_exp >=100 & rush_yards_gained_exp < 200,
                      bonus_rush_yd_100,
                      0) +
                ifelse(
                  rush_yards_gained_exp >= 200,
                  bonus_rush_yd_200,
                  0) +
                    rush_yards_gained_exp      * rush_yd      +
                    rush_first_down_exp        * rush_fd      +
                    rush_touchdown_exp         * rush_td      +
                    carries                    * rush_att     +
                    rushing_2pt_conversions    * rush_2pt     +
                    ifelse(
                      rec_yards_gained_exp >=100 & rec_yards_gained_exp < 200,
                      bonus_rec_yd_100,
                      0) +
                ifelse(
                  rec_yards_gained_exp >= 200,
                  bonus_rec_yd_200,
                  0) +
                    rec_yards_gained_exp       * rec_yd       +
                    rec_first_down_exp         * rec_fd       +
                    rec_touchdown_exp          * rec_td       +
                    receptions                 * rec          +
                    receiving_2pt_conversions  * rec_2pt      +
                    (sack_fumbles+rushing_fumbles+receiving_fumbles) * fum +
                    (sack_fumbles_lost+rushing_fumbles_lost+receiving_fumbles_lost)* fum_lost
                )
              })
            
      # Store the result for processing
      league_stats <- league_data() |>
                        dplyr::filter(
                          stat        %in% input$stat_tbl_off,
                          team %in% input$team_tbl_off,
                          week        %in% input$week_tbl_off,
                          season      %in% input$season_tbl_off,
                          position    %in% input$pos_tbl_off
                          ) %>%
                        mutate_if(is.numeric , replace_na, replace = 0) %>%
                        mutate(
                          diff_league = fantasy_points_league - ep_league,
                          diff_ppr    = fantasy_points_ppr    - total_fantasy_points_exp
                        ) %>%
      group_by(team,player_id,sleeper_id,player_name,position,season) %>%
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

      # Store in reactive value
      league_stats_reactive(league_stats)
                      
      waiter_hide()
    })

    # Store the result for output
    output$Stats_League_Offense <- DT::renderDataTable({
      league_stats <- league_stats_reactive()
      
      if(is.null(league_stats)) {
        return(DT::datatable(data.frame(Message = "Bitte Calculate drücken")))
      }
      
      DT::datatable(league_stats,
      rownames = FALSE,
      colnames = c(
        "Rank",
        "Team",
        "Name",
        "Position",
        "Season",
        "FP League",
        "PPG League",
        "EP League",
        "EPG League",
        "Diff League",
        "Diff PG League",
        "FP PPR",
        "PPG PPR",
        "EP PPR",
        "EPG PPR",
        "Diff PPR",
        "Diff PG PPR",
        "WOPR",
        "RACR",
        "PACR",
        "Touches",
        "Snaps",
        "YPS",
        "OPPS",
        "EPPS",
        "FPPS",
        # "Routes",
        # "TRR",
        # "YRR",
        "Tgt-Share",
        "AY-Share",
        "GM"),
      options = list(headerCallback = JS(
        "function(thead, data, start, end, display){
                        var tooltips = [
                        'Total Points in der ausgewahlten Liga.',
                        'Points per Game in der ausgewahlten Liga.',
                        'Expected Fantasy Points in der ausgewahlten Liga.',
                        'Expected Fantasy Points per Game in der ausgewahlten Liga.',
                        'Differenz der Fantasy Points mit den Expected Points in der ausgewahlten Liga.',
                        'Differenz der Fantasy Points per Game mit den Expected Points in der ausgewahlten Liga.',
                        'Total Points nach PPR.',
                        'Points per Game nach PPR.',
                        'Expected Fantasy Points nach PPR.',
                        'Expected Fantasy Points per Game nach PPR.',
                        'Differenz der Fantasy Points mit den Expected Points nach PPR.',
                        'Differenz der Fantasy Points per Game mit den Expected Points nach PPR.',
                        'Weighted Opportunity Rating: Hier mit altem WOPR: 1.5 * Target Share + 0.7 * Share of Team Air Yards.',
                        'Receiver Air Conversion Ratio: Receiving Yards / Total Air Yards.',
                        'Passer Air Conversion Ratio: Passing Yards / Total Air Yards.',
                        'Anzahl an Receptions und Carries / Passes.',
                        'Anzahl der Snaps.',
                        'Yards per Snap.',
                        'Opportunity (Opportunity meint immer Touches, klingt aber fetziger) per Snap.',
                        'Expected Fantasy Points per Snap.',
                        'Fantasy Points per Snap.',
                        'Anteil der Targets an den gesamten Targets des jeweiligen Teams',
                        'Anteil der Airyards an den gesamten Airyards des jeweiligen Teams',
                        'Der GM in dessen Kader der Spieler in der ausgewählten Liga steht.',
                        ];
                      var start = 4;
                      for(var i=start; i<(start+tooltips.length); i++){
                      $('th:eq('+i+')',thead).attr('title', tooltips[i-start]);
                      }
    }"
      ),
    pageLength = 100),
      style = "bootstrap4",
      filter = "top"
    ) %>%
      formatRound(columns = c(18:20,23:28), digits = 2)%>%
      formatRound(columns = c(6:17), digits = 1)%>%
      formatRound(columns = c(21:22), digits = 0)
      })

  # Download
  output$tbl_off_download <- downloadHandler(
    filename = function(){paste0(Sys.Date(),"_arcade_ep-stats.csv")},
    content = function(fname){
      league_stats <- league_stats_reactive()
      if(!is.null(league_stats)) {
        write.csv2(league_stats, fname)
      }
    }
  )
  })
}