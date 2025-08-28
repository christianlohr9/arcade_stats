library(shiny)
library(shinyjs)
library(shinytoastr)
library(shinyWidgets)
library(shinymanager)
library(shinycssloaders)
library(waiter)
library(bslib)
library(tidyverse)
library(DT)
library(RCurl)
library(ggimage)
library(nflfastR)
library(ffscrapr)
library(ggimage)
library(googlesheets4)
library(shinydisconnect)
library(powerjoin)
library(psych)
library(qs)
library(jsonlite)

# Source external functions and modules
source("R/functions/compute_war.R")
source("R/modules/mod_war_tab.R")
source("R/modules/mod_nflfastr_offense_tab.R")
source("R/modules/mod_nflfastr_defense_tab.R")
source("R/modules/mod_player_stats_offense_tab.R")
source("R/modules/mod_help_league_ids_tab.R")

options(digits = 2,scipen = 9999,nflreadr.prefer = "qs")
pal <- c(rev(ggsci::rgb_material("deep-purple")), ggsci::rgb_material("green"))
light <- bs_theme(version = 4)
dark <- bs_theme(version = 4,
  bg = "#222222",
  fg = "#D4D3D3",
  primary = "#375A7F",
  secondary = "#635D5D",
  warning = "orange",
  error = "darkred",
  base_font = font_google("Roboto")
  )

gif <- "https://i.scdn.co/image/ab6765630000ba8aed1285ce5d62661172e43877"
waiting_screen <- tagList(
  h3(glue::glue("Je nach Auswahl der Methode kann die \nArcade Fantasy Magie etwas dauern {emo::ji('crystal')}"), style = "color:white;"),
  img(src = gif, height = "200px")
)

##### Data Input #####
years <-2024
weeks <- 1:17

### Past Data #####
weekly_join <- tryCatch({
  readRDS("data/weekly_stats.rds")
}, error = function(e) {
  data.frame()
})
weekly_join_war <- weekly_join |> select("player_id","player_name","recent_team","position","season","week","fantasy_points_ppr")
weekly_join_league_stats <- weekly_join |>  select("player_id","sleeper_id","player_name","recent_team","position","season","week","fantasy_points_ppr","attempts","completions","sacks","carries","receptions",
                                                                        "sack_fumbles","rushing_fumbles","receiving_fumbles","sack_fumbles_lost","rushing_fumbles_lost","receiving_fumbles_lost",
                                                                        "wopr","racr","pacr","touches","yps","ops","epps","fpps","snaps_off","snaps_def","target_share","air_yards_share",
                                                                        "passing_2pt_conversions","rushing_2pt_conversions","receiving_2pt_conversions",ends_with("_exp"),"stat")
weekly_join_def <- tryCatch({
  readRDS("data/weekly_stats_def.rds")
}, error = function(e) {
  data.frame()
})
weekly_join_def_war <- weekly_join_def |>  select("player_id","player_name","recent_team","position","season","week","fantasy_points_ppr")
war_df_global <- reactiveVal(NULL)
league_stats_df_global <- reactiveVal(NULL)

##### Funktionen #####
# Funktion für gradient Columns
color_gradient <- function(dt, column_name, gradient_colors = c(rev(ggsci::rgb_material("green")), ggsci::rgb_material("deep-purple"))) {
  col_func <- colorRampPalette(gradient_colors)
  unique_values <- sort(unique(dt$x$data[[column_name]]), decreasing = TRUE)
  col_values <- col_func(length(unique_values))
  dt %>%
    formatStyle(column_name,
                backgroundColor = styleEqual(
                  levels = unique_values,
                  values = col_values
                )
    )
}

 # Define UI for application #####
ui <- fluidPage(
  #######
  # Favicon and Icons
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
    tags$link(rel = "apple-touch-icon", href = "apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "192x192", href = "android-chrome-192x192.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "android-chrome-512x512.png"),
    tags$meta(name = "theme-color", content = "#000000")
  ),
  autoWaiter(),
  useToastr(),
      disconnectMessage(
        text = "Game Over! Du warst zu lange inaktiv. Bitte lade die Seite neu.",
        refresh = "Ardace Fantasy Rocks!",
        background = "#000000",
        colour = "#FFFFFF",
        refreshColour = "#337AB7",
        overlayColour = "#000000",
        overlayOpacity = 1,
        width = 450,
        top = "center",
        size = 20,
        css = ""
      ),
      actionButton('disconnect', 'Disconnect the app'),
    theme = dark,
    div(
      class = "custom-control custom-switch",
      tags$input(
        id = "light_mode", type = "checkbox", class = "custom-control-input",
        onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('light_mode').value);")
      ),
      tags$label(
        "Light mode", `for` = "light_mode", class = "custom-control-label"
      )
    ),
    # Navigation Bar:
    navbarPage(
      theme = dark,
      title = "Arcade Fantasy Stats Lab",

      # Reiter für die nflfastR Offense Stats (Module) #####
      mod_nflfastr_offense_tab_ui("nflfastr_offense"),
      #####
      #####
      # Reiter für die nflfastR Defense Stats (Module) #####
      mod_nflfastr_defense_tab_ui("nflfastr_defense"),
      ),
      #####
      # Reiter für die League Import Stats (Module) #####
      mod_player_stats_offense_tab_ui("player_stats_offense"),
      #####
      # Reiter für die Wins Above Replacement Stats (Module) #####
      mod_war_tab_ui("war"),
      #####
      # Reiter für die Values Stats #####
      tabPanel("Values",
               fluidRow(column(width=12,
                               p("ACHTUNG: In diesem Tab richtet sich alles nach der Auswahl, die im Tab 'Wins Above Replacement' getroffen wurde."),
                               br()
               )),
               fluidRow(
                 column(2,
                        actionButton("value_calc",
                                     label = "Calculate"
                        )
                 )),
               DT::dataTableOutput("values")
      ),
      #####
      # Reiter für den League ID Helper (Module) #####
      mod_help_league_ids_tab_ui("help_league_ids")
      #####
    )

# Define server logic
server <- function(input, output, session) {

  observe({
    session$setCurrentTheme(
      if (isTRUE(input$light_mode)) light else dark
    )
  })

  options(shiny.maxRequestSize=50*1024^2)

  observeEvent(input$disconnect, {
    session$close()
  })

  # nflfastR Stats Offense #####

  # nflfastR Offense module server call
  mod_nflfastr_offense_tab_server("nflfastr_offense", weekly_join)
  #####

  # nflfastR Stats Defense #####

  # nflfastR Defense module server call
  mod_nflfastr_defense_tab_server("nflfastr_defense", weekly_join_def)
  #####
  
  # Player Stats Offense module server call
  mod_player_stats_offense_tab_server("player_stats_offense", weekly_join, show_no_data_message, waiting_screen)
  #####
  
  # Help - Get League IDs module server call
  mod_help_league_ids_tab_server("help_league_ids")

  # Placeholder message for other tabs requiring data
  show_no_data_message <- function() {
    shinytoastr::toastr_warning("Keine Daten verfügbar. Bitte zuerst update_data.R ausführen.", 
                                title = "ACHTUNG!", closeButton = TRUE,
                                position = "top-full-width")
  }

  # League Stats - full functionality restored
  compute_data <-  function(f_years){
    weekly_join_league_stats %>%
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
                        } %>%
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
                            power_left_join(.,
                                           ffscrapr::ff_scoring(ffscrapr::sleeper_connect(season = max(input$season_tbl_off), league_id = input$league_off)) %>%
                                             filter(pos %in% c("QB","RB","WR","TE")) %>%
                                             pivot_wider(
                                               names_from = event,
                                               values_from = points,
                                               values_fill = 0
                                             ) %>%
                                             mutate(position = as.factor(pos)),
                                           by = "position",
                                           conflict = coalesce_yx
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
                            sacks                      * pass_sack    +
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
  }

  observeEvent(input$league_stats_calc, {
    if(nrow(weekly_join_league_stats) == 0) {
      show_no_data_message()
      return()
    }

    waiter_show(html = waiting_screen, color = "black")

    thedata <- tidyr::crossing(f_years = input$season_tbl_off) |>
      purrr::pmap_dfr(compute_data)

                      league_stats_df_global(thedata)

                      league_stats <- league_stats_df_global() |>
                        filter(
                          stat        %in% input$stat_tbl_off,
                          recent_team %in% input$team_tbl_off,
                          week        %in% input$week_tbl_off,
                          season      %in% input$season_tbl_off,
                          position    %in% input$pos_tbl_off
                          ) %>%
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
                                  target_share        = mean(target_share,na.rm=TRUE)*100,
                                  air_yards_share     = mean(air_yards_share,na.rm=TRUE)*100,
                                  .groups = "drop"
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

                      waiter_hide()

  output$Stats_League_Offense <- DT::renderDataTable({
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
  })

  # WAR function and modules already sourced at top of file

  # WAR module server call
  mod_war_tab_server("war", war_df_global, weekly_join_war, weekly_join_def_war, weekly_join, show_no_data_message, waiting_screen, compute_war)

  # Values functionality
  output$values <- DT::renderDataTable({
    war_df <- war_df_global()

    if (!is.null(war_df) && nrow(war_df) > 0) {

      values_team <- bind_rows(
        war_df %>%
          filter(Position == "QB",
                 franchise_name != "Free Agent") %>%
          group_by(
            franchise_name
          ) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_qb)) %>%
          group_by(
            Team = franchise_name
          ) %>%
          summarise(
            pos = "QB",
            war = sum(WAR)
          ),
        war_df %>%
          filter(Position == "RB",
                 franchise_name != "Free Agent") %>%
          group_by(
            franchise_name
          ) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_rb)) %>%
          group_by(
            Team = franchise_name
          ) %>%
          summarise(
            pos = "RB",
            war = sum(WAR)
          ),
        war_df %>%
          filter(Position == "WR",
                 franchise_name != "Free Agent") %>%
          group_by(
            franchise_name
          ) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_wr)) %>%
          group_by(
            Team = franchise_name
          ) %>%
          summarise(
            pos = "WR",
            war = sum(WAR)
          ),
        war_df %>%
          filter(Position == "TE",
                 franchise_name != "Free Agent") %>%
          group_by(
            franchise_name
          ) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_te)) %>%
          group_by(
            Team = franchise_name
          ) %>%
          summarise(
            pos = "TE",
            war = sum(WAR)
          ),
        war_df %>%
          filter(Position == "DI",
                 franchise_name != "Free Agent") %>%
          group_by(
            franchise_name
          ) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_di)) %>%
          group_by(
            Team = franchise_name
          ) %>%
          summarise(
            pos = "DI",
            war = sum(WAR)
          ),
        war_df %>%
          filter(Position == "DE",
                 franchise_name != "Free Agent") %>%
          group_by(
            franchise_name
          ) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_de)) %>%
          group_by(
            Team = franchise_name
          ) %>%
          summarise(
            pos = "DE",
            war = sum(WAR)
          ),
        war_df %>%
          filter(Position == "LB",
                 franchise_name != "Free Agent") %>%
          group_by(
            franchise_name
          ) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_lb)) %>%
          group_by(
            Team = franchise_name
          ) %>%
          summarise(
            pos = "LB",
            war = sum(WAR)
          ),
        war_df %>%
          filter(Position == "CB",
                 franchise_name != "Free Agent") %>%
          group_by(
            franchise_name
          ) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_cb)) %>%
          group_by(
            Team = franchise_name
          ) %>%
          summarise(
            pos = "CB",
            war = sum(WAR)
          ),
        war_df %>%
          filter(Position == "S",
                 franchise_name != "Free Agent") %>%
          group_by(
            franchise_name
          ) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_ss)) %>%
          group_by(
            Team = franchise_name
          ) %>%
          summarise(
            pos = "S",
            war = sum(WAR)
          )
      ) 
      
      if(nrow(bind_rows(war_df %>% filter(Position == "QB", franchise_name != "Free Agent"),
                        war_df %>% filter(Position == "RB", franchise_name != "Free Agent"),
                        war_df %>% filter(Position == "WR", franchise_name != "Free Agent"),
                        war_df %>% filter(Position == "TE", franchise_name != "Free Agent"),
                        war_df %>% filter(Position == "DI", franchise_name != "Free Agent"),
                        war_df %>% filter(Position == "DE", franchise_name != "Free Agent"),
                        war_df %>% filter(Position == "LB", franchise_name != "Free Agent"),
                        war_df %>% filter(Position == "CB", franchise_name != "Free Agent"),
                        war_df %>% filter(Position == "S", franchise_name != "Free Agent"))) == 0) {
        return(DT::datatable(data.frame(Message = "Keine Teams mit Spielern gefunden. WAR zuerst berechnen.")))
      }
      
      values_team <- values_team %>%
        pivot_wider(
          names_from = pos,
          values_from = war,
          values_fill = 0
        )
      
      if(ncol(values_team) > 1) {
        values_team <- values_team %>%
          mutate(
            OVR = rowSums(select(., where(is.numeric)), na.rm = TRUE)
          ) %>%
          relocate(OVR) %>%
          arrange(-OVR)
      } else {
        values_team <- values_team %>%
          mutate(OVR = 0) %>%
          relocate(OVR) %>%
          arrange(-OVR)
      }

      if(nrow(values_team) == 0) {
        return(DT::datatable(data.frame(Message = "Keine Teams mit Spielern gefunden. WAR zuerst berechnen.")))
      }

      # Prepare numeric columns for formatting
      numeric_cols <- names(values_team)[sapply(values_team, is.numeric)]
      
      dt <- DT::datatable(values_team,
                    rownames = FALSE,
                    options = list(pageLength = 100,
                                   columnDefs = list(list(className = 'dt-left', targets = "_all"))
                                   ),
                    style = "bootstrap4"
                    )
      
      # Only apply formatting if there are numeric columns
      if(length(numeric_cols) > 0) {
        dt <- dt %>% DT::formatRound(columns = numeric_cols, digits = 1)
        
        # Only apply color styling if we have more than just the Team column
        if(ncol(values_team) > 1 && nrow(values_team) > 0) {
          numeric_data <- values_team[, numeric_cols, drop = FALSE]
          if(ncol(numeric_data) > 0 && nrow(numeric_data) > 0) {
            data_range <- range(as.matrix(numeric_data), na.rm = TRUE)
            if(!is.infinite(data_range[1]) && !is.infinite(data_range[2])) {
              dt <- dt %>% 
                DT::formatStyle(names(values_team),
                                background = if (isTRUE(input$light_mode)) styleColorBar(data_range, '#FFC010', angle = -90) else styleColorBar(data_range, '#915191', angle = -90),
                                backgroundSize = '98% 88%',
                                backgroundRepeat = 'no-repeat',
                                backgroundPosition = 'center'
                                )
            }
          }
        }
      }
      
      dt
    } else {
      return(
        DT::datatable(data.frame(Message = "Keine Daten verfügbar. Bitte zuerst Wins Above Replacement kalkulieren."))
      )
    }
  })

  # League helper functionality
  league_data <- reactive({
    req(input$league_user)
    req(input$league_season)
    req(input$league_type_ovr)
    
    tryCatch({
      tidyr::crossing(leagues = sleeper_userleagues(input$league_user, input$league_season) %>%
                        select(league_id) %>%
                        as_vector()) %>%
        purrr::pmap_dfr(function(leagues){
          raw <- sleeper_connect(season = input$league_season, league_id = leagues) %>%
            ff_league() %>%
            select(league_type, best_ball, league_name, league_id) %>%
            mutate(
              league_type_new =
                case_when(
                  league_type == "redraft" & best_ball == "TRUE"  ~ "Redraft Bestball",
                  league_type == "redraft" & best_ball == "FALSE" ~ "Redraft",
                  league_type == "dynasty" & best_ball == "TRUE"  ~ "Dynasty Bestball",
                  league_type == "dynasty" & best_ball == "FALSE" ~ "Dynasty",
                  league_type == "keeper"  & best_ball == "TRUE"  ~ "Keeper Bestball",
                  league_type == "keeper"  & best_ball == "FALSE" ~ "Keeper"
                )
            ) %>%
            filter(league_type_new %in% input$league_type_ovr) %>%
            select(league_name, league_id) %>%
            mutate(url = paste0('<a href="https://sleeper.com/leagues/',league_id,'">League on Sleeper</a>'))

          return(raw)
        }) %>%
        arrange(league_name)
    }, error = function(e) {
      data.frame(
        league_name = "Error loading leagues",
        league_id = paste("Error:", e$message),
        url = "N/A"
      )
    })
  })

  output$Leagues <- DT::renderDataTable({
    DT::datatable(league_data(),
                  rownames = FALSE,
                  colnames = c(
                    "League Name",
                    "League ID",
                    "URL"),
                  options = list(pageLength = 100),
                  style = "bootstrap4",
                  filter = "top",
                  escape = FALSE
    )
  })

  # Download handlers
  output$tbl_off_download <- downloadHandler(
    filename = function(){paste0(Sys.Date(),"_arcade_ep-stats.csv")},
    content = function(fname){
      league_data <- league_stats_df_global()
      if (!is.null(league_data)) {
        write.csv2(league_data, fname)
      } else {
        write.csv2(data.frame(Message="No data available"), fname)
      }
    }
  )

  output$war_download <- downloadHandler(
    filename = function(){paste0(Sys.Date(),"_arcade_war_stats.csv")},
    content = function(fname){
      war_data <- war_df_global()
      if (!is.null(war_data)) {
        write.csv2(war_data, fname)
      } else {
        write.csv2(data.frame(Message="No data available"), fname)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)