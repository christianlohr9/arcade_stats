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

      # Reiter für die nflfastR Offense Stats #####
      tabPanel("nflfastR Stats Offense",
               fluidRow(column(2,
                               downloadButton('nflfastR_off_download',
                                              "Download")
                               )
                        ),
               fluidRow(column(3,
                               pickerInput("stat_tbl_nflfastR_off",
                                           label = "Choose a Stat",
                                           choices = levels(as.factor(
                                             c("Passing","Rushing/Receiving")
                                           )),
                                           selected = "Rushing/Receiving",
                                           options = list(`actions-box` = TRUE),
                                           multiple = FALSE)
                               ),
                        column(3,pickerInput("team_tbl_nflfastR_off",
                                             label = "Choose a Team",
                                             choices = if(nrow(weekly_join) > 0) levels(weekly_join$recent_team) else c("ARI", "ATL", "BAL", "BUF"),
                                             selected = if(nrow(weekly_join) > 0) levels(weekly_join$recent_team) else c("ARI", "ATL", "BAL", "BUF"),
                                             options = list(`actions-box` = TRUE),
                                             multiple = TRUE)
                        ),
                        column(3,pickerInput("week_tbl_nflfastR_off",
                                             label = "Choose a Week",
                                             choices = 1:17,
                                             selected = 1:17,
                                             options = list(`actions-box` = TRUE),
                                             multiple = TRUE)
                        ),
                        column(3,pickerInput("season_tbl_nflfastR_off",
                                             label = "Choose a Season",
                                             choices = 1999:lubridate::year(lubridate::today()),
                                             selected = ifelse(yday(lubridate::today())>=240,lubridate::year(lubridate::today()),lubridate::year(lubridate::today())-1),
                                             # options = list(`actions-box` = TRUE),
                                             multiple = TRUE)
                        )),
               fluidRow(column(3,
                               pickerInput("pos_tbl_nflfastR_off",
                                           label = "Choose a Position",
                                           choices = levels(as.factor(
                                             c(
                                               "QB","RB","WR","TE"
                                             )
                                           )),
                                           selected = as.factor(
                                             c(
                                               "QB","RB","WR","TE"
                                             )
                                           ),
                                           # options = list(`actions-box` = TRUE),
                                           multiple = TRUE)
                               ),
                        column(3,numericInput("threshold_tbl_nflfastR_off",
                                              label = "Minimum Touches",
                                              value = 1)
                        )
               ),
               DT::dataTableOutput("Stats_nflfastR_Offense")
      ),
      #####
      # Reiter für die nflfastR Defense Stats #####
      tabPanel("nflfastR Stats Defense",
               fluidRow(column(2,
                               downloadButton('nflfastR_def_download',
                                              "Download")
               )
               ),
               fluidRow(
                 column(3,pickerInput("team_tbl_nflfastR_def",
                                    label = "Choose a Team",
                                    choices = if(nrow(weekly_join_def) > 0) levels(weekly_join_def$recent_team) else c("ARI", "ATL", "BAL", "BUF"),
                                    selected = if(nrow(weekly_join_def) > 0) levels(weekly_join_def$recent_team) else c("ARI", "ATL", "BAL", "BUF"),
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE)
               ),
               column(3,pickerInput("week_tbl_nflfastR_def",
                                    label = "Choose a Week",
                                    choices = 1:17,
                                    selected = 1:17,
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE)
               ),
               column(3,pickerInput("season_tbl_nflfastR_def",
                                    label = "Choose a Season",
                                    choices = 1999:lubridate::year(lubridate::today()),
                                    selected = ifelse(yday(lubridate::today())>=240,lubridate::year(lubridate::today()),lubridate::year(lubridate::today())-1),
                                    # options = list(`actions-box` = TRUE),
                                    multiple = TRUE)
               )),
               fluidRow(column(3,
                               pickerInput("pos_tbl_nflfastR_def",
                                           label = "Choose a Position",
                                           choices = if(nrow(weekly_join_def) > 0) levels(weekly_join_def$position) else c("DT", "DE", "LB", "CB", "S"),
                                           selected = if(nrow(weekly_join_def) > 0) levels(weekly_join_def$position) else c("DT", "DE", "LB", "CB", "S"),
                                           # options = list(`actions-box` = TRUE),
                                           multiple = TRUE)
               ),
               column(3,numericInput("threshold_tbl_nflfastR_def_tkl",
                                     label = "Minimum Tackles",
                                     value = 0)
               ),
               column(3,numericInput("threshold_tbl_nflfastR_def_sk",
                                     label = "Minimum Sacks",
                                     value = 0)
               ),
               column(3,numericInput("threshold_tbl_nflfastR_def_pd",
                                     label = "Minimum Passes Defended",
                                     value = 0)
               )
               ),
               DT::dataTableOutput("Stats_nflfastR_Defense")
      ),
      #####
      # Reiter für die League Import Stats #####
      tabPanel("Player Stats Offense",
               fluidRow(column(2,
                               downloadButton('tbl_off_download',
                                              "Download")
               )
               ),
               fluidRow(column(3,
                               pickerInput("stat_tbl_off",
                                           label = "Choose a Stat",
                                           choices = if(nrow(weekly_join) > 0) levels(weekly_join$stat) else c("Passing", "Rushing/Receiving"),
                                           selected = "Rushing/Receiving",
                                           options = list(`actions-box` = TRUE),
                                           multiple = T)
                               ),
                        column(3,pickerInput("team_tbl_off",
                                             label = "Choose a Team",
                                             choices = if(nrow(weekly_join) > 0) levels(weekly_join$recent_team) else c("ARI", "ATL", "BAL", "BUF"),
                                             selected = if(nrow(weekly_join) > 0) levels(weekly_join$recent_team) else c("ARI", "ATL", "BAL", "BUF"),
                                             options = list(`actions-box` = TRUE),
                                             multiple = T)
                        ),
                        column(3,pickerInput("week_tbl_off",
                                             label = "Choose a Week",
                                             choices = if(nrow(weekly_join) > 0) levels(weekly_join$week) else 1:17,
                                             selected = if(nrow(weekly_join) > 0) 1:max(as.numeric(weekly_join$week)) else 1:17,
                                             options = list(`actions-box` = TRUE),
                                             multiple = T)
                        ),
                        column(3,pickerInput("season_tbl_off",
                                             label = "Choose a Season",
                                             choices = if(nrow(weekly_join) > 0) levels(weekly_join$season) else c("2024"),
                                             selected = ifelse(yday(lubridate::today())>=240,lubridate::year(lubridate::today()),lubridate::year(lubridate::today())-1),
                                             # options = list(`actions-box` = TRUE),
                                             multiple = T)
                        )),
               fluidRow(column(3,
                               pickerInput("pos_tbl_off",
                                           label = "Choose a Position",
                                           choices = if(nrow(weekly_join) > 0) levels(weekly_join$position) else c("QB","RB","WR","TE"),
                                           selected = if(nrow(weekly_join) > 0) levels(weekly_join$position) else c("QB","RB","WR","TE"),
                                           # options = list(`actions-box` = TRUE),
                                           multiple = T)
                               ),
                        column(3,numericInput("threshold_tbl_off",
                                              label = "Minimum Touches",
                                              value = 1)
                               ),
                        column(3,numericInput("threshold_tbl_off_snap",
                                              label = "Minimum Snaps",
                                              value = 0)
                        ),
                        column(3,textInput("league_off",
                                           label = "Sleeper League ID",
                                           value = "Bitte ID eintragen")
                               )
                        ),
               fluidRow(
                 column(2,
                        useWaiter(),
                        actionButton("league_stats_calc",
                                     label = "Calculate"
                        )
                 )),
               DT::dataTableOutput("Stats_League_Offense")
               ),
      #####
      # Reiter für die Wins Above Replacement Stats #####
      tabPanel("Wins Above Replacement",
               fluidRow(column(width=12,
                               p('Hinweis: Bei der Auswahl "mPPR" kann die Ladezeit etwas länger dauern als bei PPR oder einer Sleeper/MFL League ID. Bei einer MFL ID bitte "mfl" voranstellen, bspw. "mfl22686"'),
                               br()
               )),
               fluidRow(
                 column(2,textInput("war_league",
                                    label = "Sleeper League ID / 'PPR' / 'mPPR'",
                                    value = "PPR")
                 ),
                 column(2,
                        pickerInput("war_season",
                                    label = "Choose a Season",
                                    choices = if(nrow(weekly_join) > 0) levels(weekly_join$season) else c("2024"),
                                    selected = if(nrow(weekly_join) > 0) levels(as.factor(ifelse(lubridate::year(lubridate::today()) %in% levels(weekly_join$season),lubridate::year(lubridate::today()),max(levels(weekly_join$season))))) else "2024",
                                    # options = list(`actions-box` = TRUE),
                                    multiple = FALSE)
                 ),
                 column(2,
                        pickerInput("war_week",
                                      label = 'Choose a Week ("0" for whole season)',
                                      choices = if(nrow(weekly_join) > 0) levels(as.factor(c(0,as.numeric(weekly_join$week)))) else c("0", "1", "2"),
                                      selected = 0,
                                      options = list(`actions-box` = TRUE),
                                      multiple = FALSE)
                        ),
               column(2,
                      pickerInput("war_games",
                                  label = "Min. Games Played",
                                  choices = 1:17,
                                  selected = 1,
                                  options = list(`actions-box` = TRUE),
                                  multiple = FALSE)
                      ),
               column(2,
                        pickerInput("war_teams",
                                  label = "Teams",
                                  choices = as.numeric(2:20),
                                  selected = 12,
                                  options = list(`actions-box` = TRUE),
                                  multiple = FALSE)
                        ),
               ),
               fluidRow(
                 column(2,
                      pickerInput("war_qb",
                                  label = "QB",
                                  choices = 0:2,
                                  selected = 1,
                                  options = list(`actions-box` = TRUE),
                                  multiple = FALSE)
                      ),
                column(2,
                      pickerInput("war_rb",
                                  label = "RB",
                                  choices = 0:5,
                                  selected = 2,
                                  options = list(`actions-box` = TRUE),
                                  multiple = FALSE)
                      ),
                column(2,
                        pickerInput("war_wr",
                                    label = "WR",
                                    choices = 0:5,
                                    selected = 3,
                                    options = list(`actions-box` = TRUE),
                                    multiple = FALSE)
                 ),
                 column(2,
                      pickerInput("war_te",
                                  label = "TE",
                                  choices = 0:2,
                                  selected = 1,
                                  options = list(`actions-box` = TRUE),
                                  multiple = FALSE)
                      ),
                 column(2,
                        pickerInput("war_rec",
                                    label = "WR/TE",
                                    choices = 0:5,
                                    selected = 0,
                                    options = list(`actions-box` = TRUE),
                                    multiple = FALSE)
                 ),
                 column(2,
                        pickerInput("war_flx",
                                    label = "RB/WR/TE",
                                    choices = 0:5,
                                    selected = 1,
                                    options = list(`actions-box` = TRUE),
                                    multiple = FALSE)
                 ),
               ),
               fluidRow(
                 column(2,
                        pickerInput("war_di",
                                    label = "DI",
                                    choices = 0:5,
                                    selected = 0,
                                    options = list(`actions-box` = TRUE),
                                    multiple = FALSE)
                 ),
                 column(2,
                        pickerInput("war_de",
                                    label = "DE",
                                    choices = 0:5,
                                    selected = 0,
                                    options = list(`actions-box` = TRUE),
                                    multiple = FALSE)
                 ),
                 column(2,
                        pickerInput("war_lb",
                                    label = "LB",
                                    choices = 0:5,
                                    selected = 0,
                                    options = list(`actions-box` = TRUE),
                                    multiple = FALSE)
                 ),
                 column(2,
                        pickerInput("war_cb",
                                    label = "CB",
                                    choices = 0:5,
                                    selected = 0,
                                    options = list(`actions-box` = TRUE),
                                    multiple = FALSE)
                 ),
                 column(2,
                        pickerInput("war_ss",
                                    label = "S",
                                    choices = 0:5,
                                    selected = 0,
                                    options = list(`actions-box` = TRUE),
                                    multiple = FALSE)
                 ),
                 column(2,
                        pickerInput("war_idp",
                                    label = "IDP",
                                    choices = 0:5,
                                    selected = 0,
                                    options = list(`actions-box` = TRUE),
                                    multiple = FALSE)
                 ),
               ),
               fluidRow(
                 column(2,
                        useWaiter(),
                        actionButton("war_calc",
                                    label = "Calculate"
                                    )
                 ),
                 column(2,
                        downloadButton('war_download',
                                       "Download")
                 )
               ),
               DT::dataTableOutput("WAR") #|> withSpinner(type = 5, color = "#ffc010", size = 2)
      ),
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
      # Reiter für den League ID Helper #####
      tabPanel("Help - Get League IDs",
               fluidRow(column(width=12,
                               p("Achtung: Das Laden der League IDs kann (je nach Anzahl eurer Ligen) etwas dauern."),
                               br()
               )),
               fluidRow(column(3,textInput("league_user",
                                           label = "Sleeper User Name",
                                           value = "solarpool")
               ),
               column(3,pickerInput("league_season",
                                    label = "Choose a Season",
                                    choices = 2014:lubridate::year(lubridate::today()),
                                    selected = ifelse(yday(lubridate::today())>=240,lubridate::year(lubridate::today()),lubridate::year(lubridate::today())-1),
                                    # options = list(`actions-box` = TRUE),
                                    multiple = TRUE)
               ),
               column(6,
                      pickerInput("league_type_ovr",
                                  label = "Choose a League Type",
                                  choices = levels(
                                    as.factor(c("Redraft Bestball",
                                                "Redraft",
                                                "Dynasty Bestball",
                                                "Dynasty",
                                                "Keeper Bestball",
                                                "Keeper"))),
                                  selected = levels(
                                    as.factor(c("Redraft Bestball",
                                                "Redraft",
                                                "Dynasty Bestball",
                                                "Dynasty",
                                                "Keeper Bestball",
                                                "Keeper"))),
                                  # options = list(`actions-box` = TRUE),
                                  multiple = T))
               ),
               DT::dataTableOutput("Leagues")
      )
      #####
      )
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

  nflfastR_data <- reactive({
    if(nrow(weekly_join) == 0) {
      return(data.frame(Message = "No data available. Please run update_data.R first."))
    }
    
    nflfastR::load_player_stats(as.numeric(input$season_tbl_nflfastR_off)) %>%
      mutate(recent_team = as.factor(as.character(recent_team))
             ) %>%
      filter(
        season %in% input$season_tbl_nflfastR_off,
        week %in% input$week_tbl_nflfastR_off,
        recent_team %in% input$team_tbl_nflfastR_off
      ) %>%
      left_join(nflfastR::fast_scraper_roster(as.numeric(input$season_tbl_nflfastR_off)) %>%
                  select(player_id=gsis_id,season,position)) %>%
      mutate(
        position = as.factor(position),
        touches = attempts + carries + receptions,
        rushrec_fumbles = rushing_fumbles + receiving_fumbles,
        rushrec_fumbles_lost = rushing_fumbles_lost + receiving_fumbles_lost,
        rushrec_2pt = rushing_2pt_conversions + receiving_2pt_conversions
      ) %>%
      filter(
        position %in% input$pos_tbl_nflfastR_off
      ) %>%
      group_by(player_id,Player=player_display_name,Position=position,Team=recent_team) %>%
      ###
      {
        if(input$stat_tbl_nflfastR_off == "Passing") {
          summarise(.,
            Att = sum(attempts,na.rm=TRUE),
            Comp = sum(completions,na.rm=TRUE),
            Yds = sum(passing_yards,na.rm=TRUE),
            `Air Yds` = sum(passing_air_yards,na.rm=TRUE),
            YAC = sum(passing_yards_after_catch,na.rm=TRUE),
            TD = sum(passing_tds,na.rm=TRUE),
            `1stD` = sum(passing_first_downs,na.rm=TRUE),
            `2Pt` = sum(passing_2pt_conversions,na.rm=TRUE),
            Int = sum(interceptions,na.rm=TRUE),
            Sk = sum(sacks,na.rm=TRUE),
            `Sk Yds` = sum(sack_yards,na.rm=TRUE),
            Fm = sum(sack_fumbles,na.rm=TRUE),
            Fml = sum(sack_fumbles_lost,na.rm=TRUE),
            PACR = round(mean(pacr,na.rm=TRUE),2),
            EPA = round(mean(passing_epa,na.rm=TRUE),2),
            DAKOTA = round(mean(dakota,na.rm=TRUE),2),
            touches = sum(touches,na.rm=TRUE),
            .groups = "drop"
          )
        } else {
          summarise(.,
                    Att = sum(carries,na.rm=TRUE),
                    `Rush Yds` = sum(rushing_yards,na.rm=TRUE),
                    `Rush TD` = sum(rushing_tds,na.rm=TRUE),
                    `Rush 1stD` = sum(rushing_first_downs,na.rm=TRUE),
                    Tgt = sum(targets,na.rm=TRUE),
                    Rec = sum(receptions,na.rm=TRUE),
                    `Rec Yds` = sum(receiving_yards,na.rm=TRUE),
                    `Air Yds` = sum(receiving_air_yards,na.rm=TRUE),
                    `YAC` = sum(receiving_yards_after_catch,na.rm=TRUE),
                    `Rec TD` = sum(receiving_tds,na.rm=TRUE),
                    `Rec 1stD` = sum(receiving_first_downs,na.rm=TRUE),
                    `2Pt` = sum(rushrec_2pt,na.rm=TRUE),
                    Fm = sum(rushrec_fumbles,na.rm=TRUE),
                    Fml = sum(rushrec_fumbles_lost,na.rm=TRUE),
                    WOPR = round(mean(wopr,na.rm=TRUE),2),
                    RACR = round(mean(racr,na.rm=TRUE),2),
                    `Tgt-Share` = round(mean(target_share,na.rm=TRUE),2),
                    `Air Yds-Share` = round(mean(air_yards_share,na.rm=TRUE),2),
                    `Rush EPA` = round(mean(rushing_epa,na.rm=TRUE),2),
                    `Rec EPA` = round(mean(receiving_epa,na.rm=TRUE),2),
                    touches = sum(touches,na.rm=TRUE),
                    .groups = "drop"
          )
        }
      } %>%
      filter(
        touches >= input$threshold_tbl_nflfastR_off
      ) %>%
      ungroup() %>%
      select(-c(player_id, touches))
  })

  output$Stats_nflfastR_Offense <- DT::renderDataTable({
    DT::datatable(nflfastR_data(),
                  rownames = FALSE,
                  options = list(pageLength = 100),
                  style = "bootstrap4",
                  filter = "top"
    )
  })

  # Download
  output$nflfastR_off_download <- downloadHandler(
    filename = function(){paste0(Sys.Date(),"_nflfastR_stats.csv")},
    content = function(fname){
      write.csv2(nflfastR_data(), fname)
    }
  )

  #####

  # nflfastR Stats Defense #####

  nflfastR_data_def <- reactive({
    if(nrow(weekly_join_def) == 0) {
      return(data.frame(Message = "No data available. Please run update_data.R first."))
    }
    
    weekly_join_def %>%
      filter(
        season %in% input$season_tbl_nflfastR_def,
        week %in% input$week_tbl_nflfastR_def,
        recent_team %in% input$team_tbl_nflfastR_def,
        position %in% input$pos_tbl_nflfastR_def
      ) %>%
      group_by(player_id,Player=player_display_name,Position=position,Team=recent_team) %>%
      summarise(
        TKL                       = sum(def_tackles,na.rm=TRUE),
        TKL_Solo                  = sum(def_tackles_solo,na.rm=TRUE),
        TKL_w_Ass                 = sum(def_tackles_with_assist,na.rm=TRUE),
        TKL_Ass                   = sum(def_tackle_assists,na.rm=TRUE),
        TFL                       = sum(def_tackles_for_loss,na.rm=TRUE),
        TFL_Yds                   = sum(def_tackles_for_loss_yards,na.rm=TRUE),
        FF                        = sum(def_fumbles_forced,na.rm=TRUE),
        SK                        = sum(def_sacks,na.rm=TRUE),
        SK_Yds                    = sum(def_sack_yards,na.rm=TRUE),
        QB_Hit                    = sum(def_qb_hits,na.rm=TRUE),
        INT                       = sum(def_interceptions,na.rm=TRUE),
        INT_Yds                   = sum(def_interception_yards,na.rm=TRUE),
        PD                        = sum(def_pass_defended,na.rm=TRUE),
        TD                        = sum(def_tds,na.rm=TRUE),
        FM                        = sum(def_fumbles,na.rm=TRUE),
        FMR_Own                   = sum(def_fumble_recovery_own,na.rm=TRUE),
        FMR_Own_Yds               = sum(def_fumble_recovery_yards_own,na.rm=TRUE),
        FMR_Opp                   = sum(def_fumble_recovery_opp,na.rm=TRUE),
        FMR_Opp_Yds               = sum(def_fumble_recovery_yards_opp,na.rm=TRUE),
        Safety                    = sum(def_safety,na.rm=TRUE),
        Penalty                   = sum(def_penalty,na.rm=TRUE),
        Penalty_Yds               = sum(def_penalty_yards,na.rm=TRUE),
        .groups = "drop"
      ) %>%
      filter(
        TKL >= input$threshold_tbl_nflfastR_def_tkl,
        SK  >= input$threshold_tbl_nflfastR_def_sk,
        PD  >= input$threshold_tbl_nflfastR_def_pd
      ) %>%
      ungroup() %>%
      select(-c(player_id))
  })

  output$Stats_nflfastR_Defense <- DT::renderDataTable({
    DT::datatable(nflfastR_data_def(),
                  rownames = FALSE,
                  options = list(pageLength = 100),
                  style = "bootstrap4",
                  filter = "top"
    )
  })

  # Download
  output$nflfastR_def_download <- downloadHandler(
    filename = function(){paste0(Sys.Date(),"_nflfastR_stats_def.csv")},
    content = function(fname){
      write.csv2(nflfastR_data_def(), fname)
    }
  )

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

  # WAR Function from shiny_base_stats.R
  compute_war <- function(f_years,f_week){

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
                                    week=1:max(weekly_join %>% filter(season==max(input$war_season)) %>% mutate(week=as.numeric(week)) %>% select(week) %>% max())) %>%
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
                                              week=1:max(weekly_join %>% filter(season==max(input$war_season)) %>% mutate(week=as.numeric(week)) %>% select(week) %>% max())) %>%
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

  # WAR calculation observeEvent
  observeEvent(input$war_calc, {
    if(nrow(weekly_join_war) == 0) {
      show_no_data_message()
      return()
    }

    waiter_show(html = waiting_screen, color = "black")

    war_df_pre <- tidyr::crossing(f_years = input$war_season, f_week = input$war_week) |>
      purrr::pmap_dfr(compute_war)|>
      mutate(
        consistency = consistency_temp*WAR
      )

    war_df <- war_df_pre %>%
      {
        if(input$war_league %in% c("PPR","mPPR")) {
          mutate(.,franchise_name="Keine Liga ausgewählt")
        } else if(grepl("mfl", input$war_league, fixed = TRUE)) {
          left_join(.,
                    ffscrapr::ff_starters(ffscrapr::mfl_connect(max(input$war_season), league_id = gsub("mfl", "", input$war_league))) %>%
                      left_join(ffscrapr::ff_franchises(ffscrapr::mfl_connect(max(input$war_season), league_id = gsub("mfl", "", input$war_league))) %>% select(franchise_id,conference)) %>%
                      filter(conference=="01") %>%
                      group_by(mfl_id=player_id) %>%
                      summarise(
                        franchise_name = first(franchise_name)
                      ) %>%
                      left_join(
                        ffscrapr::dp_playerids() %>% filter(!is.na(mfl_id),!is.na(gsis_id)) %>% mutate(player_id = as.factor(gsis_id)) %>% select(mfl_id,player_id)
                      )
          )
        }  else {
          left_join(.,
                    ffscrapr::ff_rosters(ffscrapr::sleeper_connect(season = max(as.numeric(input$war_season)), league_id = input$war_league)) %>%
                      select(sleeper_id=player_id,franchise_name) %>%
                      left_join(
                        ffscrapr::dp_playerids() %>% filter(!is.na(sleeper_id),!is.na(gsis_id)) %>% mutate(player_id = as.factor(gsis_id)) %>% select(sleeper_id,player_id)
                      )
          )
        }
      } %>%
      mutate(franchise_name = ifelse(is.na(franchise_name),"Free Agent",franchise_name)) %>%
      ungroup()  %>%
      arrange(-WAR) %>%
      mutate(rank = row_number(),
             Avr_Win_Percent = paste0(round(Avr_Win_Percent*100,0),"%"),
             player = as.factor(player)) %>%
      relocate(rank)

    war_df_global(war_df)
    waiter_hide()
  })

  output$WAR <- DT::renderDataTable({
    war_df <- war_df_global()
    
    if (!is.null(war_df) && nrow(war_df) > 0) {
      DT::datatable(war_df %>%
                      select(
                        rank,
                        position=Position,
                        player,
                        games = Games,
                        war = WAR,
                        waa = WAA,
                        ppg = Ave_Week_Points,
                        wpg = Avr_Win_Percent,
                        vor = consistency,
                        franchise_name
                      ),
                    rownames = FALSE,
                    colnames = c(
                      "Rank",
                      "Position",
                      "Name",
                      "Games",
                      "WAR",
                      "WAA",
                      "PPG",
                      "WPG",
                      "VOR",
                      "GM"),
                    options = list(
                      headerCallback = JS(
                        "function(thead, data, start, end, display){
                        var tooltips = [
                        'Anzahl der absolvierten Spiele',
                        'Wins Above Replacement (FAQ folgt).',
                        'Wins Above Average (FAQ folgt).',
                        'Points per Game.',
                        'Durchschnittliche Win Percentage mit diesem Spieler per Game (FAQ folgt).',
                        'Value over Replacement: Standardabweichung der wöchentlichen WAR multipliziert mit dem aggregierten WAR.',
                        'Der GM in dessen Kader der Spieler in der ausgewählten Liga steht.',
                        ];
                      var start = 3;
                      for(var i=start; i<(start+tooltips.length); i++){
                      $('th:eq('+i+')',thead).attr('title', tooltips[i-start]);
                      }
    }"
                      ),
    pageLength = 100),
    style = "bootstrap4",
    filter = "top"
      ) %>%
        formatRound(columns = c(5:7,9), digits = 1)
    } else {
      DT::datatable(data.frame(Message = "No WAR data available. Click Calculate first."))
    }
  })

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