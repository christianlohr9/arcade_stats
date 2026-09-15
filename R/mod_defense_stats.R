# nflfastR Stats Defense tab --------------------------------------------------------

IDP_POSITIONS <- c("DT", "DE", "LB", "CB", "S")

mod_defense_stats_ui <- function(id, defense) {
  ns <- shiny::NS(id)
  season <- latest_season(defense)

  shiny::tabPanel("nflfastR Stats Defense",
    shiny::fluidRow(shiny::column(2, shiny::downloadButton(ns("download"), "Download"))),
    shiny::fluidRow(
      shiny::column(3, multi_picker(ns("team"), "Choose a Team", team_choices(defense))),
      shiny::column(3, multi_picker(ns("week"), "Choose a Week", week_choices(defense, season))),
      shiny::column(3, multi_picker(ns("season"), "Choose a Season", season_choices(defense), season))
    ),
    shiny::fluidRow(
      shiny::column(3, multi_picker(ns("position"), "Choose a Position", IDP_POSITIONS)),
      shiny::column(3, shiny::numericInput(ns("min_tackles"), "Minimum Tackles", value = 0)),
      shiny::column(3, shiny::numericInput(ns("min_sacks"), "Minimum Sacks", value = 0)),
      shiny::column(3, shiny::numericInput(ns("min_pd"), "Minimum Passes Defended", value = 0))
    ),
    DT::dataTableOutput(ns("table"))
  )
}

mod_defense_stats_server <- function(id, defense) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$season, {
      weeks <- week_choices(defense, input$season)
      shinyWidgets::updatePickerInput(session, "week", choices = weeks, selected = weeks)
    }, ignoreInit = TRUE)

    table_data <- shiny::reactive({
      shiny::req(input$season, input$week, input$team, input$position)
      summarise_defense(defense, input$season, input$week, input$team, input$position,
                        min_tackles = input$min_tackles %||% 0,
                        min_sacks = input$min_sacks %||% 0,
                        min_pd = input$min_pd %||% 0)
    })

    output$table <- DT::renderDataTable(stats_table(table_data()))
    output$download <- csv_download(table_data, "nflfastR_defense_stats")
  })
}

summarise_defense <- function(defense, seasons, weeks, teams, positions,
                              min_tackles = 0, min_sacks = 0, min_pd = 0) {
  defense |>
    dplyr::filter(.data$season %in% seasons, .data$week %in% weeks,
                  .data$team %in% teams, .data$position %in% positions) |>
    dplyr::summarise(
      TKL = sum(.data$def_tackles_solo + .data$def_tackle_assists, na.rm = TRUE),
      TKL_Solo = sum(.data$def_tackles_solo, na.rm = TRUE),
      TKL_w_Ass = sum(.data$def_tackles_with_assist, na.rm = TRUE),
      TKL_Ass = sum(.data$def_tackle_assists, na.rm = TRUE),
      TFL = sum(.data$def_tackles_for_loss, na.rm = TRUE),
      TFL_Yds = sum(.data$def_tackles_for_loss_yards, na.rm = TRUE),
      FF = sum(.data$def_fumbles_forced, na.rm = TRUE),
      SK = sum(.data$def_sacks, na.rm = TRUE),
      SK_Yds = sum(.data$def_sack_yards, na.rm = TRUE),
      QB_Hit = sum(.data$def_qb_hits, na.rm = TRUE),
      INT = sum(.data$def_interceptions, na.rm = TRUE),
      INT_Yds = sum(.data$def_interception_yards, na.rm = TRUE),
      PD = sum(.data$def_pass_defended, na.rm = TRUE),
      TD = sum(.data$def_tds, na.rm = TRUE),
      FM = sum(.data$def_fumbles, na.rm = TRUE),
      FMR_Own = sum(.data$fumble_recovery_own, na.rm = TRUE),
      FMR_Own_Yds = sum(.data$fumble_recovery_yards_own, na.rm = TRUE),
      FMR_Opp = sum(.data$fumble_recovery_opp, na.rm = TRUE),
      FMR_Opp_Yds = sum(.data$fumble_recovery_yards_opp, na.rm = TRUE),
      Safety = sum(.data$def_safeties, na.rm = TRUE),
      Penalty = sum(.data$penalties, na.rm = TRUE),
      Penalty_Yds = sum(.data$penalty_yards, na.rm = TRUE),
      .by = c("player_id", "player_display_name", "position", "team")
    ) |>
    dplyr::filter(.data$TKL >= min_tackles, .data$SK >= min_sacks, .data$PD >= min_pd) |>
    dplyr::select(Player = "player_display_name", Position = "position", Team = "team",
                  dplyr::everything(), -"player_id")
}
