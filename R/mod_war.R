# Wins Above Replacement and Values tabs -------------------------------------------

mod_war_ui <- function(id, offense) {
  ns <- shiny::NS(id)
  season <- latest_season(offense)
  defaults <- default_war_settings()$slots

  slot_picker <- function(slot, label, max) {
    shiny::column(2, single_picker(ns(paste0("slot_", slot)), label, 0:max, defaults[[slot]]))
  }

  shiny::tabPanel("Wins Above Replacement",
    shiny::fluidRow(shiny::column(12,
      shiny::p('Liga: "PPR", "mPPR", eine Sleeper League ID oder eine MFL ID mit "mfl" davor, z. B. "mfl22686". ',
               "Bei mPPR und Liga-IDs werden die Punkte live von der Plattform geladen, das kann etwas dauern.")
    )),
    shiny::fluidRow(
      shiny::column(2, shiny::textInput(ns("league"), "Liga", value = "PPR")),
      shiny::column(2, single_picker(ns("season"), "Choose a Season", season_choices(offense), season)),
      shiny::column(2, single_picker(ns("week"), 'Choose a Week ("0" = ganze Saison)',
                                     c(0, week_choices(offense, season)), 0)),
      shiny::column(2, single_picker(ns("min_games"), "Min. Games Played", 1:18, 1)),
      shiny::column(2, single_picker(ns("teams"), "Teams", 2:20, 12))
    ),
    shiny::fluidRow(
      slot_picker("QB", "QB", 2), slot_picker("RB", "RB", 5), slot_picker("WR", "WR", 5),
      slot_picker("TE", "TE", 2), slot_picker("REC", "WR/TE", 5), slot_picker("FLEX", "RB/WR/TE", 5)
    ),
    shiny::fluidRow(
      slot_picker("DT", "DI", 5), slot_picker("DE", "DE", 5), slot_picker("LB", "LB", 5),
      slot_picker("CB", "CB", 5), slot_picker("S", "S", 5), slot_picker("IDP", "IDP", 5)
    ),
    shiny::fluidRow(
      shiny::column(2, shiny::actionButton(ns("calculate"), "Calculate")),
      shiny::column(2, shiny::downloadButton(ns("download"), "Download"))
    ),
    DT::dataTableOutput(ns("table"))
  )
}

mod_values_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabPanel("Values",
    shiny::p("Die Werte basieren auf der letzten Berechnung im Tab 'Wins Above Replacement' (Liga, Saison und Roster-Slots)."),
    DT::dataTableOutput(ns("values"))
  )
}

mod_war_server <- function(id, offense, defense) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$season, {
      shinyWidgets::updatePickerInput(session, "week",
                                      choices = c(0, week_choices(offense, as.integer(input$season))),
                                      selected = 0)
    }, ignoreInit = TRUE)

    settings <- shiny::reactive({
      slots <- vapply(names(default_war_settings()$slots),
                      function(s) as.numeric(input[[paste0("slot_", s)]] %||% 0), numeric(1))
      list(teams = as.numeric(input$teams), min_games = as.numeric(input$min_games), slots = slots)
    })

    shiny::observeEvent(input$calculate, {
      current_settings <- settings()
      war <- run_with_feedback(function() {
        league <- parse_league(input$league)
        season <- as.integer(input$season)
        week <- as.integer(input$week)
        weeks <- if (week == 0) FANTASY_SEASON_WEEKS else week

        stats <- war_input(offense, defense, league, season, weeks)
        compute_war(stats, season, weeks, current_settings) |>
          attach_franchises(league, season) |>
          dplyr::mutate(rank = dplyr::row_number(), .before = 1)
      })
      if (!is.null(war)) result(list(war = war, settings = current_settings))
    })

    output$table <- DT::renderDataTable({
      war <- result()$war
      if (is.null(war)) return(message_table("Bitte Calculate drücken."))
      if (nrow(war) == 0) return(message_table("Keine Spieler für diese Auswahl gefunden."))

      war |>
        dplyr::transmute(.data$rank, .data$player, .data$Position,
                         `FP/W` = .data$Ave_Week_Points,
                         `Win%` = paste0(round(.data$Avr_Win_Percent * 100), "%"),
                         .data$WAR, .data$WAA, .data$Games, .data$consistency,
                         Franchise = .data$franchise_name) |>
        DT::datatable(
          extensions = "Buttons", rownames = FALSE, style = "bootstrap4", filter = "top",
          options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"), scrollX = TRUE,
                         pageLength = 100, columnDefs = list(list(className = "dt-center", targets = "_all")))
        ) |>
        DT::formatRound(columns = c("FP/W", "WAR", "WAA", "consistency"), digits = 2)
    })

    output$download <- shiny::downloadHandler(
      filename = function() paste0("war_data_", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(result()$war %||% data.frame(), file, row.names = FALSE)
    )

    result
  })
}

mod_values_server <- function(id, war_result) {
  shiny::moduleServer(id, function(input, output, session) {
    output$values <- DT::renderDataTable({
      res <- war_result()
      values <- if (!is.null(res)) compute_team_values(res$war, res$settings)
      if (is.null(values)) {
        return(message_table("Keine Werte verfügbar. Bitte zuerst WAR mit einer Liga-ID berechnen."))
      }

      value_cols <- c("OVR", "QB", "RB", "WR", "TE")
      DT::datatable(values, rownames = FALSE, style = "bootstrap4",
                    options = list(pageLength = 100, columnDefs = list(list(className = "dt-left", targets = "_all")))) |>
        DT::formatRound(columns = value_cols, digits = 1) |>
        DT::formatStyle(value_cols,
          background = DT::styleColorBar(range(unlist(values[value_cols])), "#915191", angle = -90),
          backgroundSize = "98% 88%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
    })
  })
}

# Weekly points per player under the chosen league's scoring.
war_input <- function(offense, defense, league, season, weeks) {
  cols <- c("player_id", "player_name", "position", "season", "week", "fantasy_points_ppr")
  players <- dplyr::bind_rows(
    offense[offense$season == season & offense$week %in% weeks, cols],
    defense[defense$season == season & defense$week %in% weeks, cols]
  )

  if (league$type == "ppr") {
    return(dplyr::rename(players, fantasy_points_league = "fantasy_points_ppr"))
  }

  players |>
    dplyr::select(-"fantasy_points_ppr") |>
    dplyr::inner_join(league_points(league, season, weeks), by = c("player_id", "season", "week"))
}
