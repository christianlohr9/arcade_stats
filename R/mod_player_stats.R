# Player Stats Offense tab: fantasy points vs. expected points --------------------

SCORING_EVENTS <- c(
  "pass_att", "pass_cmp", "pass_inc", "pass_yd", "pass_td", "pass_fd", "pass_2pt", "pass_int", "pass_sack",
  "rush_att", "rush_yd", "rush_td", "rush_fd", "rush_2pt",
  "rec", "rec_yd", "rec_td", "rec_fd", "rec_2pt", "fum", "fum_lost",
  "bonus_pass_yd_300", "bonus_pass_yd_400", "bonus_rush_yd_100", "bonus_rush_yd_200",
  "bonus_rec_yd_100", "bonus_rec_yd_200"
)

PLAYER_STATS_COLNAMES <- c(
  "Rank", "Team", "Name", "Position", "Season",
  "FP League", "PPG League", "EP League", "EPG League", "Diff League", "Diff PG League",
  "FP PPR", "PPG PPR", "EP PPR", "EPG PPR", "Diff PPR", "Diff PG PPR",
  "WOPR", "RACR", "PACR", "Touches", "Snaps", "YPS", "OPPS", "EPPS", "FPPS",
  "Tgt-Share", "AY-Share", "GM"
)

PLAYER_STATS_TOOLTIPS <- c(
  "Total Points in der ausgewählten Liga.",
  "Points per Game in der ausgewählten Liga.",
  "Expected Fantasy Points in der ausgewählten Liga.",
  "Expected Fantasy Points per Game in der ausgewählten Liga.",
  "Differenz der Fantasy Points mit den Expected Points in der ausgewählten Liga.",
  "Differenz der Fantasy Points per Game mit den Expected Points in der ausgewählten Liga.",
  "Total Points nach PPR.",
  "Points per Game nach PPR.",
  "Expected Fantasy Points nach PPR.",
  "Expected Fantasy Points per Game nach PPR.",
  "Differenz der Fantasy Points mit den Expected Points nach PPR.",
  "Differenz der Fantasy Points per Game mit den Expected Points nach PPR.",
  "Weighted Opportunity Rating: 1.5 * Target Share + 0.7 * Share of Team Air Yards.",
  "Receiver Air Conversion Ratio: Receiving Yards / Total Air Yards.",
  "Passer Air Conversion Ratio: Passing Yards / Total Air Yards.",
  "Anzahl an Receptions und Carries / Passes.",
  "Anzahl der Snaps.",
  "Yards per Snap.",
  "Opportunity (Touches) per Snap.",
  "Expected Fantasy Points per Snap.",
  "Fantasy Points per Snap.",
  "Anteil der Targets an den gesamten Targets des jeweiligen Teams.",
  "Anteil der Air Yards an den gesamten Air Yards des jeweiligen Teams.",
  "Der GM, in dessen Kader der Spieler in der ausgewählten Liga steht."
)

mod_player_stats_ui <- function(id, offense) {
  ns <- shiny::NS(id)
  season <- latest_season(offense)

  shiny::tabPanel("Player Stats Offense",
    shiny::fluidRow(
      shiny::column(3, single_picker(ns("stat"), "Choose a Stat", c("Passing", "Rushing/Receiving"), "Rushing/Receiving")),
      shiny::column(3, multi_picker(ns("team"), "Choose a Team", team_choices(offense))),
      shiny::column(3, multi_picker(ns("week"), "Choose a Week", week_choices(offense, season))),
      shiny::column(3, multi_picker(ns("season"), "Choose a Season", season_choices(offense), season))
    ),
    shiny::fluidRow(
      shiny::column(3, multi_picker(ns("position"), "Choose a Position", OFFENSE_POSITIONS)),
      shiny::column(3, shiny::numericInput(ns("min_touches"), "Minimum Touches", value = 1)),
      shiny::column(3, shiny::numericInput(ns("min_snaps"), "Minimum Snaps", value = 0)),
      shiny::column(3, shiny::textInput(ns("league"), "Sleeper League ID", placeholder = "leer = ohne Liga"))
    ),
    shiny::fluidRow(
      shiny::column(2, shiny::actionButton(ns("calculate"), "Calculate")),
      shiny::column(2, shiny::downloadButton(ns("download"), "Download CSV", class = "btn-primary"))
    ),
    DT::dataTableOutput(ns("table"))
  )
}

mod_player_stats_server <- function(id, offense) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$season, {
      weeks <- week_choices(offense, input$season)
      shinyWidgets::updatePickerInput(session, "week", choices = weeks, selected = weeks)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$calculate, {
      shiny::req(input$season, input$week, input$team, input$position)
      result(run_with_feedback(function() {
        player_league_stats(
          offense, seasons = as.integer(input$season), weeks = as.integer(input$week),
          teams = input$team, positions = input$position, stat = input$stat,
          min_touches = input$min_touches %||% 0, min_snaps = input$min_snaps %||% 0,
          league_id = trimws(input$league %||% "")
        )
      }))
    })

    output$table <- DT::renderDataTable({
      stats <- result()
      if (is.null(stats)) return(message_table("Bitte Calculate drücken."))

      tooltips <- jsonlite::toJSON(PLAYER_STATS_TOOLTIPS)
      DT::datatable(
        stats, rownames = FALSE, colnames = PLAYER_STATS_COLNAMES, style = "bootstrap4", filter = "top",
        options = list(
          pageLength = 100, scrollX = TRUE,
          headerCallback = DT::JS(sprintf(
            "function(thead) { var tips = %s; for (var i = 0; i < tips.length; i++) { $('th:eq(' + (i + 5) + ')', thead).attr('title', tips[i]); } }",
            tooltips
          ))
        )
      ) |>
        DT::formatRound(columns = c(18:20, 23:28), digits = 2) |>
        DT::formatRound(columns = 6:17, digits = 1) |>
        DT::formatRound(columns = 21:22, digits = 0)
    })

    output$download <- csv_download(function() result() %||% data.frame(), "arcade_ep-stats")
  })
}

player_league_stats <- function(offense, seasons, weeks, teams, positions, stat,
                                min_touches = 0, min_snaps = 0, league_id = "") {
  with_league <- nzchar(league_id)
  league_season <- max(seasons)

  data <- offense |>
    dplyr::filter(.data$stat == !!stat, .data$team %in% teams, .data$week %in% weeks,
                  .data$season %in% seasons, .data$position %in% positions)

  if (with_league) {
    league <- list(type = "sleeper", id = league_id)
    data <- data |>
      dplyr::left_join(league_points(league, league_season, weeks), by = c("player_id", "season", "week")) |>
      dplyr::left_join(sleeper_scoring_rules(league_id, league_season), by = "position")
  } else {
    data$fantasy_points_league <- 0
  }
  for (event in setdiff(SCORING_EVENTS, names(data))) data[[event]] <- 0

  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) dplyr::coalesce(x, 0))) |>
    dplyr::mutate(
      ep_league = expected_league_points(dplyr::pick(dplyr::everything())),
      diff_league = .data$fantasy_points_league - .data$ep_league,
      diff_ppr = .data$fantasy_points_ppr - .data$total_fantasy_points_exp
    )

  stats <- data |>
    dplyr::summarise(
      FPTS_League = sum(.data$fantasy_points_league),
      FPTSpG_League = mean(.data$fantasy_points_league),
      EP_League = sum(.data$ep_league),
      EPpG_League = mean(.data$ep_league),
      Diff_League = sum(.data$diff_league),
      Diff_PG_League = mean(.data$diff_league),
      FPTS_PPR = sum(.data$fantasy_points_ppr),
      FPTSpG_PPR = mean(.data$fantasy_points_ppr),
      EP_PPR = sum(.data$total_fantasy_points_exp),
      EPpG_PPR = mean(.data$total_fantasy_points_exp),
      Diff_PPR = sum(.data$diff_ppr),
      Diff_PG_PPR = mean(.data$diff_ppr),
      wopr = mean(.data$wopr),
      racr = mean(.data$racr),
      pacr = mean(.data$pacr),
      touches = sum(.data$touches),
      snaps = sum(.data$snaps_off),
      yps = sum(.data$yps),
      opps = sum(.data$touches) / sum(.data$snaps_off),
      epps = sum(.data$epps),
      fpps = sum(.data$fpps),
      target_share = mean(.data$target_share) * 100,
      air_yards_share = mean(.data$air_yards_share) * 100,
      .by = c("team", "player_id", "player_display_name", "position", "season")
    ) |>
    dplyr::filter(.data$touches >= min_touches, .data$snaps >= min_snaps)

  stats <- if (with_league) {
    attach_franchises(stats, list(type = "sleeper", id = league_id), league_season)
  } else {
    dplyr::mutate(stats, franchise_name = "Keine Liga ausgewählt")
  }

  stats |>
    dplyr::arrange(dplyr::desc(.data$wopr)) |>
    dplyr::mutate(rank = dplyr::row_number(), .before = 1) |>
    dplyr::select("rank", "team", player_name = "player_display_name", dplyr::everything(), -"player_id")
}

# Expected fantasy points under a league's scoring rules, one value per row.
expected_league_points <- function(d) {
  bonus <- function(yards, low, high) ifelse(yards >= low & yards < high, 1, 0)

  bonus(d$pass_yards_gained_exp, 300, 400) * d$bonus_pass_yd_300 +
    bonus(d$pass_yards_gained_exp, 400, Inf) * d$bonus_pass_yd_400 +
    d$pass_yards_gained_exp * d$pass_yd +
    d$pass_first_down_exp * d$pass_fd +
    d$pass_touchdown_exp * d$pass_td +
    d$attempts * d$pass_att +
    d$pass_completions_exp * d$pass_cmp +
    (d$attempts - d$completions) * d$pass_inc +
    d$passing_2pt_conversions * d$pass_2pt +
    d$pass_interception_exp * d$pass_int +
    d$sacks_suffered * d$pass_sack +
    bonus(d$rush_yards_gained_exp, 100, 200) * d$bonus_rush_yd_100 +
    bonus(d$rush_yards_gained_exp, 200, Inf) * d$bonus_rush_yd_200 +
    d$rush_yards_gained_exp * d$rush_yd +
    d$rush_first_down_exp * d$rush_fd +
    d$rush_touchdown_exp * d$rush_td +
    d$carries * d$rush_att +
    d$rushing_2pt_conversions * d$rush_2pt +
    bonus(d$rec_yards_gained_exp, 100, 200) * d$bonus_rec_yd_100 +
    bonus(d$rec_yards_gained_exp, 200, Inf) * d$bonus_rec_yd_200 +
    d$rec_yards_gained_exp * d$rec_yd +
    d$rec_first_down_exp * d$rec_fd +
    d$rec_touchdown_exp * d$rec_td +
    d$receptions * d$rec +
    d$receiving_2pt_conversions * d$rec_2pt +
    (d$sack_fumbles + d$rushing_fumbles + d$receiving_fumbles) * d$fum +
    (d$sack_fumbles_lost + d$rushing_fumbles_lost + d$receiving_fumbles_lost) * d$fum_lost
}
