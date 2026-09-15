# nflfastR Stats Offense tab --------------------------------------------------------

OFFENSE_POSITIONS <- c("QB", "RB", "WR", "TE")

mod_offense_stats_ui <- function(id, offense) {
  ns <- shiny::NS(id)
  season <- latest_season(offense)

  shiny::tabPanel("nflfastR Stats Offense",
    shiny::fluidRow(shiny::column(2, shiny::downloadButton(ns("download"), "Download"))),
    shiny::fluidRow(
      shiny::column(3, single_picker(ns("stat"), "Choose a Stat", c("Passing", "Rushing/Receiving"), "Rushing/Receiving")),
      shiny::column(3, multi_picker(ns("team"), "Choose a Team", team_choices(offense))),
      shiny::column(3, multi_picker(ns("week"), "Choose a Week", week_choices(offense, season))),
      shiny::column(3, multi_picker(ns("season"), "Choose a Season", season_choices(offense), season))
    ),
    shiny::fluidRow(
      shiny::column(3, multi_picker(ns("position"), "Choose a Position", OFFENSE_POSITIONS)),
      shiny::column(3, shiny::numericInput(ns("min_touches"), "Minimum Touches", value = 1))
    ),
    DT::dataTableOutput(ns("table"))
  )
}

mod_offense_stats_server <- function(id, offense) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$season, {
      weeks <- week_choices(offense, input$season)
      shinyWidgets::updatePickerInput(session, "week", choices = weeks, selected = weeks)
    }, ignoreInit = TRUE)

    table_data <- shiny::reactive({
      shiny::req(input$season, input$week, input$team, input$position, input$stat)
      summarise_offense(offense, input$season, input$week, input$team, input$position,
                        input$stat, input$min_touches)
    })

    output$table <- DT::renderDataTable(stats_table(table_data()))
    output$download <- csv_download(table_data, "nflfastR_stats")
  })
}

summarise_offense <- function(offense, seasons, weeks, teams, positions, stat, min_touches = 0) {
  filtered <- offense |>
    dplyr::filter(.data$season %in% seasons, .data$week %in% weeks,
                  .data$team %in% teams, .data$position %in% positions) |>
    dplyr::mutate(touches = .data$attempts + .data$carries + .data$receptions)

  by <- c("player_id", "player_display_name", "position", "team")

  summary <- if (stat == "Passing") {
    dplyr::summarise(filtered,
      Att = sum(.data$attempts, na.rm = TRUE),
      Comp = sum(.data$completions, na.rm = TRUE),
      Yds = sum(.data$passing_yards, na.rm = TRUE),
      `Air Yds` = sum(.data$passing_air_yards, na.rm = TRUE),
      YAC = sum(.data$passing_yards_after_catch, na.rm = TRUE),
      TD = sum(.data$passing_tds, na.rm = TRUE),
      `1stD` = sum(.data$passing_first_downs, na.rm = TRUE),
      `2Pt` = sum(.data$passing_2pt_conversions, na.rm = TRUE),
      Int = sum(.data$passing_interceptions, na.rm = TRUE),
      Sk = sum(.data$sacks_suffered, na.rm = TRUE),
      `Sk Yds` = sum(.data$sack_yards_lost, na.rm = TRUE),
      Fm = sum(.data$sack_fumbles, na.rm = TRUE),
      Fml = sum(.data$sack_fumbles_lost, na.rm = TRUE),
      PACR = round(mean(.data$pacr, na.rm = TRUE), 2),
      EPA = round(mean(.data$passing_epa, na.rm = TRUE), 2),
      touches = sum(.data$touches, na.rm = TRUE),
      .by = dplyr::all_of(by)
    )
  } else {
    dplyr::summarise(filtered,
      Att = sum(.data$carries, na.rm = TRUE),
      `Rush Yds` = sum(.data$rushing_yards, na.rm = TRUE),
      `Rush TD` = sum(.data$rushing_tds, na.rm = TRUE),
      `Rush 1stD` = sum(.data$rushing_first_downs, na.rm = TRUE),
      Tgt = sum(.data$targets, na.rm = TRUE),
      Rec = sum(.data$receptions, na.rm = TRUE),
      `Rec Yds` = sum(.data$receiving_yards, na.rm = TRUE),
      `Air Yds` = sum(.data$receiving_air_yards, na.rm = TRUE),
      YAC = sum(.data$receiving_yards_after_catch, na.rm = TRUE),
      `Rec TD` = sum(.data$receiving_tds, na.rm = TRUE),
      `Rec 1stD` = sum(.data$receiving_first_downs, na.rm = TRUE),
      `2Pt` = sum(.data$rushing_2pt_conversions + .data$receiving_2pt_conversions, na.rm = TRUE),
      Fm = sum(.data$rushing_fumbles + .data$receiving_fumbles, na.rm = TRUE),
      Fml = sum(.data$rushing_fumbles_lost + .data$receiving_fumbles_lost, na.rm = TRUE),
      WOPR = round(mean(.data$wopr, na.rm = TRUE), 2),
      RACR = round(mean(.data$racr, na.rm = TRUE), 2),
      `Tgt-Share` = round(mean(.data$target_share, na.rm = TRUE), 2),
      `Air Yds-Share` = round(mean(.data$air_yards_share, na.rm = TRUE), 2),
      `Rush EPA` = round(mean(.data$rushing_epa, na.rm = TRUE), 2),
      `Rec EPA` = round(mean(.data$receiving_epa, na.rm = TRUE), 2),
      touches = sum(.data$touches, na.rm = TRUE),
      .by = dplyr::all_of(by)
    )
  }

  summary |>
    dplyr::filter(.data$touches >= (min_touches %||% 0)) |>
    dplyr::select(Player = "player_display_name", Position = "position", Team = "team",
                  dplyr::everything(), -"player_id")
}
