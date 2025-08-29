#' nflfastR Stats Defense Tab Module
#'
#' @description A shiny module for the nflfastR defensive statistics tab
#'
#' @param id The module namespace id
#'
#' @name mod_nflfastr_defense_tab

#' @describeIn mod_nflfastr_defense_tab UI function
#' @export
mod_nflfastr_defense_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("nflfastR Stats Defense",
    fluidRow(column(2,
      downloadButton(ns('nflfastR_def_download'),
        "Download")
    )),
    fluidRow(
      column(3,pickerInput(ns("team_tbl_nflfastR_def"),
        label = "Choose a Team",
        choices = c("ARI", "ATL", "BAL", "BUF"),  # Will be updated dynamically
        selected = c("ARI", "ATL", "BAL", "BUF"),  # Will be updated dynamically
        options = list(`actions-box` = TRUE),
        multiple = TRUE)
      ),
      column(3,pickerInput(ns("week_tbl_nflfastR_def"),
        label = "Choose a Week",
        choices = 1:17,
        selected = 1:17,
        options = list(`actions-box` = TRUE),
        multiple = TRUE)
      ),
      column(3,pickerInput(ns("season_tbl_nflfastR_def"),
        label = "Choose a Season",
        choices = 1999:lubridate::year(lubridate::today()),
        selected = ifelse(yday(lubridate::today())>=250,lubridate::year(lubridate::today()),lubridate::year(lubridate::today())-1),
        multiple = TRUE)
      )
    ),
    fluidRow(column(3,
      pickerInput(ns("pos_tbl_nflfastR_def"),
        label = "Choose a Position",
        choices = c("DT", "DE", "LB", "CB", "S"),  # Will be updated dynamically
        selected = c("DT", "DE", "LB", "CB", "S"),  # Will be updated dynamically
        multiple = TRUE)
    ),
    column(3,numericInput(ns("threshold_tbl_nflfastR_def_tkl"),
      label = "Minimum Tackles",
      value = 0)
    ),
    column(3,numericInput(ns("threshold_tbl_nflfastR_def_sk"),
      label = "Minimum Sacks",
      value = 0)
    ),
    column(3,numericInput(ns("threshold_tbl_nflfastR_def_pd"),
      label = "Minimum Passes Defended",
      value = 0)
    )),
    DT::dataTableOutput(ns("Stats_nflfastR_Defense"))
  )
}

#' @describeIn mod_nflfastr_defense_tab Server function
#' @export
mod_nflfastr_defense_tab_server <- function(id, weekly_join_def) {
  moduleServer(id, function(input, output, session) {
    
    # Update choices dynamically when data is available
    observe({
      if(!is.null(weekly_join_def) && nrow(weekly_join_def) > 0) {
        updatePickerInput(session, "team_tbl_nflfastR_def",
          choices = levels(weekly_join_def$team),
          selected = levels(weekly_join_def$team)
        )
        
        updatePickerInput(session, "pos_tbl_nflfastR_def",
          choices = levels(weekly_join_def$position),
          selected = levels(weekly_join_def$position)
        )
      }
    })
    
    # Reactive data function (replicating original nflfastR_data_def logic)
    nflfastR_data_def <- reactive({
      if(is.null(weekly_join_def) || nrow(weekly_join_def) == 0) {
        return(data.frame(Message = "No data available. Please run update_data.R first."))
      }
      
      weekly_join_def %>%
        mutate(def_tackles = def_tackles_solo + def_tackle_assists) |> 
        filter(
          season %in% input$season_tbl_nflfastR_def,
          week %in% input$week_tbl_nflfastR_def,
          team %in% input$team_tbl_nflfastR_def,
          position %in% input$pos_tbl_nflfastR_def,
        ) %>%
        group_by(player_id,Player=player_display_name,Position=position,Team=team) %>%
        summarise(
          TKL                       = sum(def_tackles,na.rm=TRUE),
          TKL_Solo                  = sum(def_tackles_solo,na.rm=TRUE),
          TKL_w_Ass                 = sum(def_tackles_with_assist,na.rm=TRUE),
          TKL_Ass                   = sum(def_tackle_assists,na.rm=TRUE),
          TFL                       = sum(def_tackles_for_loss,na.rm=TRUE),
          TFL_Yds                   = sum(def_tackles_for_loss_yards,na.rm=TRUE),
          FF                        = sum(def_fumbles_forced,na.rm=TRUE),
          SK                        = sum(def_sacks,na.rm=TRUE),
          SK_Yds                    = sum(def_sack_yards,na.rm=TRUE),
          QB_Hit                    = sum(def_qb_hits,na.rm=TRUE),
          INT                       = sum(def_interceptions,na.rm=TRUE),
          INT_Yds                   = sum(def_interception_yards,na.rm=TRUE),
          PD                        = sum(def_pass_defended,na.rm=TRUE),
          TD                        = sum(def_tds,na.rm=TRUE),
          FM                        = sum(def_fumbles,na.rm=TRUE),
          FMR_Own                   = sum(fumble_recovery_own,na.rm=TRUE),
          FMR_Own_Yds               = sum(fumble_recovery_yards_own,na.rm=TRUE),
          FMR_Opp                   = sum(fumble_recovery_opp,na.rm=TRUE),
          FMR_Opp_Yds               = sum(fumble_recovery_yards_opp,na.rm=TRUE),
          Safety                    = sum(def_safeties,na.rm=TRUE),
          Penalty                   = sum(penalties,na.rm=TRUE),
          Penalty_Yds               = sum(penalty_yards,na.rm=TRUE)
          ) %>%
        filter(
          TKL >= input$threshold_tbl_nflfastR_def_tkl,
          SK  >= input$threshold_tbl_nflfastR_def_sk,
          PD  >= input$threshold_tbl_nflfastR_def_pd
        ) %>%
        ungroup() |> 
        select(-player_id)
    })
    
    # nflfastR Stats Defense output
    output$Stats_nflfastR_Defense <- DT::renderDataTable({
      DT::datatable(nflfastR_data_def(),
        rownames = FALSE,
        options = list(pageLength = 100),
        style = "bootstrap4",
        filter = "top"
      )
    })
    
    # Download handler
    output$nflfastR_def_download <- downloadHandler(
      filename = function(){paste0(Sys.Date(),"_nflfastR_defense_stats.csv")},
      content = function(fname){
        write.csv2(nflfastR_data_def(), fname)
      }
    )
  })
}