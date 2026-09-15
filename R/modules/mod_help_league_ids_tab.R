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
    
    # Simplified reactive logic with better error handling
    league_data <- reactive({
      req(input$league_user, input$league_season, input$league_type_ovr)
      
      tryCatch({
        # Get user leagues directly without crossing
        user_leagues <- ffscrapr::sleeper_userleagues(input$league_user, input$league_season)
        
        if(is.null(user_leagues) || nrow(user_leagues) == 0) {
          return(data.frame(Message = "Keine Ligen für diesen User und Season gefunden."))
        }
        
        # Process each league
        result_list <- list()
        for(i in 1:nrow(user_leagues)) {
          league_id <- user_leagues$league_id[i]
          
          tryCatch({
            league_info <- ffscrapr::sleeper_connect(season = input$league_season, league_id = league_id) %>%
              ffscrapr::ff_league()
            
            if(nrow(league_info) > 0) {
              league_processed <- league_info %>%
                select(league_type, best_ball, league_name, league_id) %>%
                mutate(
                  league_type_new = case_when(
                    league_type == "redraft" & best_ball == TRUE  ~ "Redraft Bestball",
                    league_type == "redraft" & best_ball == FALSE ~ "Redraft",
                    league_type == "dynasty" & best_ball == TRUE  ~ "Dynasty Bestball",
                    league_type == "dynasty" & best_ball == FALSE ~ "Dynasty",
                    league_type == "keeper"  & best_ball == TRUE  ~ "Keeper Bestball",
                    league_type == "keeper"  & best_ball == FALSE ~ "Keeper",
                    TRUE ~ paste(league_type, ifelse(best_ball, "Bestball", ""))
                  )
                ) %>%
                filter(league_type_new %in% input$league_type_ovr) %>%
                select(league_name, league_id) %>%
                mutate(url = paste0('<a href="https://sleeper.com/leagues/', league_id, '">League on Sleeper</a>'))
              
              result_list[[i]] <- league_processed
            }
          }, error = function(e) {
            # Skip leagues that cause errors
            message(paste("Skipping league", league_id, "due to error:", e$message))
          })
        }
        
        # Combine all results
        if(length(result_list) > 0) {
          final_result <- do.call(rbind, result_list[!sapply(result_list, is.null)])
          if(nrow(final_result) > 0) {
            return(final_result %>% arrange(league_name))
          }
        }
        
        return(data.frame(Message = "Keine Ligen des gewählten Typs gefunden."))
        
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