# Help - Get League IDs tab ---------------------------------------------------------

LEAGUE_TYPES <- c("Redraft", "Redraft Bestball", "Dynasty", "Dynasty Bestball", "Keeper", "Keeper Bestball")

mod_league_ids_ui <- function(id, current_season) {
  ns <- shiny::NS(id)
  shiny::tabPanel("Help - Get League IDs",
    shiny::p("Achtung: Das Laden der League IDs kann (je nach Anzahl eurer Ligen) etwas dauern."),
    shiny::fluidRow(
      shiny::column(3, shiny::textInput(ns("user"), "Sleeper User Name", value = "solarpool")),
      shiny::column(3, single_picker(ns("season"), "Choose a Season", current_season:2017, current_season)),
      shiny::column(4, multi_picker(ns("types"), "Choose a League Type", LEAGUE_TYPES)),
      shiny::column(2, shiny::actionButton(ns("load"), "Ligen laden", style = "margin-top: 25px;"))
    ),
    DT::dataTableOutput(ns("leagues"))
  )
}

mod_league_ids_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    leagues <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$load, {
      shiny::req(input$user, input$season)
      leagues(run_with_feedback(function() sleeper_user_leagues(input$user, as.integer(input$season))))
    })

    output$leagues <- DT::renderDataTable({
      df <- leagues()
      if (is.null(df)) return(message_table("Sleeper User Name eintragen und 'Ligen laden' drücken."))
      df <- df[df$league_type %in% input$types, c("league_name", "league_id", "url")]
      if (nrow(df) == 0) return(message_table("Keine Ligen des gewählten Typs gefunden."))
      DT::datatable(df, rownames = FALSE, escape = FALSE, style = "bootstrap4", filter = "top",
                    options = list(pageLength = 100, scrollX = TRUE))
    })
  })
}

# Uses the Sleeper API directly: one request for all leagues of a user, and it
# also covers leagues where the user has no roster (ffscrapr fails on those).
sleeper_user_leagues <- function(user, season) {
  api <- "https://api.sleeper.app/v1"
  account <- jsonlite::fromJSON(sprintf("%s/user/%s", api, utils::URLencode(user, reserved = TRUE)))
  if (is.null(account$user_id)) stop("Sleeper User '", user, "' nicht gefunden.")

  leagues <- jsonlite::fromJSON(sprintf("%s/user/%s/leagues/nfl/%s", api, account$user_id, season))
  if (length(leagues) == 0) stop("Keine Ligen für diesen User und diese Saison gefunden.")

  type <- c("0" = "Redraft", "1" = "Keeper", "2" = "Dynasty")[as.character(leagues$settings$type)]
  best_ball <- dplyr::coalesce(leagues$settings$best_ball, 0L) == 1

  tibble::tibble(
    league_name = leagues$name,
    league_id = leagues$league_id,
    league_type = paste0(dplyr::coalesce(unname(type), "Redraft"), ifelse(best_ball, " Bestball", "")),
    url = sprintf('<a href="https://sleeper.com/leagues/%s" target="_blank">League on Sleeper</a>', leagues$league_id)
  ) |>
    dplyr::arrange(.data$league_name)
}
