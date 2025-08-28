#' Help - Get League IDs Tab Module
#'
#' @description A shiny module for the Help - Get League IDs tab
#'
#' @param id The module namespace id
#'
#' @name mod_help_league_ids_tab

#' @describeIn mod_help_league_ids_tab UI function
#' @export
mod_help_league_ids_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Help - Get League IDs",
    fluidRow(column(width=12,
      p("Achtung: Das Laden der League IDs kann (je nach Anzahl eurer Ligen) etwas dauern."),
      br()
    )),
    fluidRow(column(3,textInput(ns("league_user"),
      label = "Sleeper User Name",
      value = "solarpool")
    ),
    column(3,pickerInput(ns("league_season"),
      label = "Choose a Season",
      choices = 2014:lubridate::year(lubridate::today()),
      selected = ifelse(yday(lubridate::today())>=240,lubridate::year(lubridate::today()),lubridate::year(lubridate::today())-1),
      multiple = TRUE)
    ),
    column(6,
      pickerInput(ns("league_type_ovr"),
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
        multiple = TRUE))
    ),
    DT::dataTableOutput(ns("Leagues"))
  )
}

#' @describeIn mod_help_league_ids_tab Server function
#' @export
mod_help_league_ids_tab_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Original reactive logic from shiny_base_stats.R
    league_data <- reactive({
      req(input$league_user, input$league_season, input$league_type_ovr)
      
      tryCatch({
        tidyr::crossing(
          leagues = ffscrapr::sleeper_userleagues(input$league_user, input$league_season) %>%
            select(league_id) %>%
            as_vector()
        ) %>%
          purrr::pmap_dfr(function(leagues) {
            raw <- ffscrapr::sleeper_connect(season = input$league_season, league_id = leagues) %>%
              ffscrapr::ff_league() %>%
              select(league_type, best_ball, league_name, league_id) %>%
              mutate(
                league_type_new = case_when(
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
              mutate(url = paste0('<a href="https://sleeper.com/leagues/', league_id, '">League on Sleeper</a>'))
            
            return(raw)
          }) %>%
          arrange(league_name)
        
      }, error = function(e) {
        return(data.frame(Message = paste("Fehler beim Laden der Ligen:", e$message)))
      })
    })
    
    # Leagues output
    output$Leagues <- DT::renderDataTable({
      DT::datatable(league_data(),
        rownames = FALSE,
        options = list(pageLength = 100, scrollX = TRUE),
        style = "bootstrap4",
        filter = "top",
        escape = FALSE  # For HTML links
      )
    })
  })
}