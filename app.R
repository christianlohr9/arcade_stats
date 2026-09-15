# Arcade Fantasy Stats Lab
#
# Shiny sources every file in R/ before this one. Data comes from the "data"
# release built by .github/workflows/data.yml (see R/data_store.R).

library(shiny)

theme <- bslib::bs_theme(
  version = 4,
  bg = "#222222", fg = "#D4D3D3", primary = "#375A7F",
  "table-color" = "#D4D3D3"
)

head_tags <- tags$head(
  tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
  tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
  tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.png"),
  tags$link(rel = "stylesheet",
            href = "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;700&display=swap"),
  tags$style(HTML("
    * { font-family: 'JetBrains Mono', 'Courier New', monospace !important; }
    .data-status { position: absolute; top: 14px; right: 20px; z-index: 1000; font-size: 0.8em; opacity: 0.7; }
  "))
)

ui <- function(req) {
  data <- tryCatch(get_datasets(), error = function(e) e)

  if (inherits(data, "error")) {
    return(fluidPage(theme = theme, head_tags,
      h2("Arcade Fantasy Stats Lab"),
      p("Die Daten konnten gerade nicht geladen werden. Bitte später erneut versuchen."),
      tags$pre(conditionMessage(data))
    ))
  }

  fluidPage(
    theme = theme,
    waiter::useWaiter(),
    head_tags,
    div(class = "data-status", data_status_text(data$manifest)),
    navbarPage(
      title = "Arcade Fantasy Stats Lab",
      mod_offense_stats_ui("offense", data$offense),
      mod_defense_stats_ui("defense", data$defense),
      mod_player_stats_ui("player_stats", data$offense),
      mod_war_ui("war", data$offense),
      mod_values_ui("values"),
      mod_league_ids_ui("league_ids", data$manifest$current_season)
    )
  )
}

server <- function(input, output, session) {
  data <- tryCatch(get_datasets(), error = function(e) NULL)
  if (is.null(data)) return()

  mod_offense_stats_server("offense", data$offense)
  mod_defense_stats_server("defense", data$defense)
  mod_player_stats_server("player_stats", data$offense)
  war_result <- mod_war_server("war", data$offense, data$defense)
  mod_values_server("values", war_result)
  mod_league_ids_server("league_ids")
}

shinyApp(ui, server)
