# Shared UI and server helpers -----------------------------------------------------

# Most fantasy leagues end their season in week 17; "whole season" in the WAR
# tab means these weeks.
FANTASY_SEASON_WEEKS <- 1:17

waiting_screen <- function() {
  shiny::tagList(
    shiny::h3("Je nach Auswahl der Methode kann die Arcade Fantasy Magie etwas dauern 🔮",
              style = "color:white;"),
    shiny::img(src = "https://i.scdn.co/image/ab6765630000ba8aed1285ce5d62661172e43877", height = "200px")
  )
}

season_choices <- function(df) sort(unique(df$season), decreasing = TRUE)

latest_season <- function(df) max(df$season)

week_choices <- function(df, season = latest_season(df)) {
  weeks <- sort(unique(df$week[df$season %in% season]))
  if (length(weeks) == 0) FANTASY_SEASON_WEEKS else weeks
}

team_choices <- function(df, season = NULL) {
  if (!is.null(season)) df <- df[df$season %in% season, ]
  sort(unique(stats::na.omit(df$team)))
}

multi_picker <- function(id, label, choices, selected = choices) {
  shinyWidgets::pickerInput(id, label = label, choices = choices, selected = selected,
                            multiple = TRUE, options = list(`actions-box` = TRUE))
}

single_picker <- function(id, label, choices, selected = choices[1]) {
  shinyWidgets::pickerInput(id, label = label, choices = choices, selected = selected, multiple = FALSE)
}

stats_table <- function(df, ...) {
  DT::datatable(df, rownames = FALSE, style = "bootstrap4", filter = "top",
                options = list(pageLength = 100, scrollX = TRUE, ...))
}

message_table <- function(text) {
  DT::datatable(data.frame(Hinweis = text), rownames = FALSE, style = "bootstrap4",
                options = list(dom = "t"))
}

csv_download <- function(data_fn, name) {
  shiny::downloadHandler(
    filename = function() paste0(Sys.Date(), "_", name, ".csv"),
    content = function(file) utils::write.csv2(data_fn(), file, row.names = FALSE)
  )
}

# Runs a (slow, remote) calculation behind the loading screen. Errors, e.g. a
# mistyped league ID or an unreachable platform API, are shown to the user
# instead of ending the session.
run_with_feedback <- function(expr_fn) {
  waiter::waiter_show(html = waiting_screen(), color = "black")
  on.exit(waiter::waiter_hide(), add = TRUE)
  tryCatch(expr_fn(), error = function(e) {
    shiny::showNotification(paste("Fehler:", conditionMessage(e)), type = "error", duration = 15)
    NULL
  })
}
