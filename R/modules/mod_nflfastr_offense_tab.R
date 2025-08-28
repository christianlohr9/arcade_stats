#' nflfastR Stats Offense Tab Module
#'
#' @description A shiny module for the nflfastR offensive statistics tab
#'
#' @param id The module namespace id
#'
#' @name mod_nflfastr_offense_tab

#' @describeIn mod_nflfastr_offense_tab UI function
#' @export
mod_nflfastr_offense_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("nflfastR Stats Offense",
    fluidRow(column(2,
      downloadButton(ns('nflfastR_off_download'),
        "Download")
    )),
    fluidRow(column(3,
      pickerInput(ns("stat_tbl_nflfastR_off"),
        label = "Choose a Stat",
        choices = levels(as.factor(
          c("Passing","Rushing/Receiving")
        )),
        selected = "Rushing/Receiving",
        options = list(`actions-box` = TRUE),
        multiple = FALSE)
    ),
    column(3,pickerInput(ns("team_tbl_nflfastR_off"),
      label = "Choose a Team",
      choices = levels(weekly_join$recent_team),  
      selected = levels(weekly_join$recent_team), 
      options = list(`actions-box` = TRUE),
      multiple = TRUE)
    ),
    column(3,pickerInput(ns("week_tbl_nflfastR_off"),
      label = "Choose a Week",
      choices = 1:17,
      selected = 1:17,
      options = list(`actions-box` = TRUE),
      multiple = TRUE)
    ),
    column(3,pickerInput(ns("season_tbl_nflfastR_off"),
      label = "Choose a Season",
      choices = 1999:lubridate::year(lubridate::today()),
      selected = ifelse(yday(lubridate::today())>=240,lubridate::year(lubridate::today()),lubridate::year(lubridate::today())-1),
      multiple = TRUE)
    )),
    fluidRow(column(3,
      pickerInput(ns("pos_tbl_nflfastR_off"),
        label = "Choose a Position",
        choices = levels(as.factor(
          c("QB","RB","WR","TE")
        )),
        selected = as.factor(
          c("QB","RB","WR","TE")
        ),
        multiple = TRUE)
    ),
    column(3,numericInput(ns("threshold_tbl_nflfastR_off"),
      label = "Minimum Touches",
      value = 1)
    )),
    DT::dataTableOutput(ns("Stats_nflfastR_Offense"))
  )
}

#' @describeIn mod_nflfastr_offense_tab Server function
#' @export
mod_nflfastr_offense_tab_server <- function(id, weekly_join) {
  moduleServer(id, function(input, output, session) {
    
    # Update team choices dynamically when data is available
    observe({
      if(!is.null(weekly_join) && nrow(weekly_join) > 0) {
        updatePickerInput(session, "team_tbl_nflfastR_off",
          choices = levels(weekly_join$recent_team),
          selected = levels(weekly_join$recent_team)
        )
      }
    })
    
    # Reactive data function (replicating original nflfastR_data logic)
    nflfastR_data <- reactive({
      if(is.null(weekly_join) || nrow(weekly_join) == 0) {
        return(data.frame(Message = "No data available. Please run update_data.R first."))
      }
      
      t <- nflfastR::load_player_stats(as.numeric(input$season_tbl_nflfastR_off)) |> 
        dplyr::mutate(recent_team = as.factor(as.character(recent_team))) |> 
        dplyr::filter(
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
        {
          if(input$stat_tbl_nflfastR_off == "Passing") {
            filter(., attempts >= input$threshold_tbl_nflfastR_off) %>%
              select(
                player_display_name, recent_team, position, season, week,
                attempts, completions, passing_yards, passing_tds, 
                interceptions, sacks, sack_yards, fantasy_points_ppr
              )
          } else {
            filter(., touches >= input$threshold_tbl_nflfastR_off) %>%
              select(
                player_display_name, recent_team, position, season, week,
                carries, rushing_yards, rushing_tds,
                targets, receptions, receiving_yards, receiving_tds,
                touches, rushrec_fumbles, rushrec_fumbles_lost, rushrec_2pt,
                fantasy_points_ppr
              )
          }
        }
    })
    
    # nflfastR Stats Offense output
    output$Stats_nflfastR_Offense <- DT::renderDataTable({
      DT::datatable(nflfastR_data(),
        rownames = FALSE,
        options = list(pageLength = 100),
        style = "bootstrap4",
        filter = "top"
      )
    })
    
    # Download handler
    output$nflfastR_off_download <- downloadHandler(
      filename = function(){paste0(Sys.Date(),"_nflfastR_stats.csv")},
      content = function(fname){
        write.csv2(nflfastR_data(), fname)
      }
    )
  })
}