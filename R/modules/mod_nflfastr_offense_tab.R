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
      choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LA", "LAC", "LV", "MIA", "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS"),  
      selected = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC", "LA", "LAC", "LV", "MIA", "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS"), 
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
      selected = ifelse(yday(lubridate::today())>=250,lubridate::year(lubridate::today()),lubridate::year(lubridate::today())-1),
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
          choices = levels(as.factor(weekly_join$team)),
          selected = levels(as.factor(weekly_join$team))
        )
      }
    })
    
    # Reactive data function (replicating original nflfastR_data logic)
    nflfastR_data <- reactive({
      if(is.null(weekly_join) || nrow(weekly_join) == 0) {
        return(data.frame(Message = "No data available. Please run update_data.R first."))
      }
      
      nflfastR::load_player_stats(as.numeric(input$season_tbl_nflfastR_off)) |> 
        dplyr::mutate(team = as.factor(as.character(team))) |> 
        dplyr::filter(
          season %in% input$season_tbl_nflfastR_off,
          week %in% input$week_tbl_nflfastR_off,
          team %in% input$team_tbl_nflfastR_off
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
        group_by(player_id,Player=player_display_name,Position=position,Team=team) %>%
        ###
      {
        if(input$stat_tbl_nflfastR_off == "Passing") {
          summarise(.,
            Att = sum(attempts,na.rm=TRUE),
            Comp = sum(completions,na.rm=TRUE),
            Yds = sum(passing_yards,na.rm=TRUE),
            `Air Yds` = sum(passing_air_yards,na.rm=TRUE),
            YAC = sum(passing_yards_after_catch,na.rm=TRUE),
            TD = sum(passing_tds,na.rm=TRUE),
            `1stD` = sum(passing_first_downs,na.rm=TRUE),
            `2Pt` = sum(passing_2pt_conversions,na.rm=TRUE),
            Int = sum(passing_interceptions,na.rm=TRUE),
            Sk = sum(sacks_suffered,na.rm=TRUE),
            `Sk Yds` = sum(sack_yards_lost,na.rm=TRUE),
            Fm = sum(sack_fumbles,na.rm=TRUE),
            Fml = sum(sack_fumbles_lost,na.rm=TRUE),
            PACR = round(mean(pacr,na.rm=TRUE),2),
            EPA = round(mean(passing_epa,na.rm=TRUE),2),
            touches = sum(touches,na.rm=TRUE)
          )
        } else {
          summarise(.,
                    Att = sum(carries,na.rm=TRUE),
                    `Rush Yds` = sum(rushing_yards,na.rm=TRUE),
                    `Rush TD` = sum(rushing_tds,na.rm=TRUE),
                    `Rush 1stD` = sum(rushing_first_downs,na.rm=TRUE),
                    Tgt = sum(targets,na.rm=TRUE),
                    Rec = sum(receptions,na.rm=TRUE),
                    `Rec Yds` = sum(receiving_yards,na.rm=TRUE),
                    `Air Yds` = sum(receiving_air_yards,na.rm=TRUE),
                    `YAC` = sum(receiving_yards_after_catch,na.rm=TRUE),
                    `Rec TD` = sum(receiving_tds,na.rm=TRUE),
                    `Rec 1stD` = sum(receiving_first_downs,na.rm=TRUE),
                    `2Pt` = sum(rushrec_2pt,na.rm=TRUE),
                    Fm = sum(rushrec_fumbles,na.rm=TRUE),
                    Fml = sum(rushrec_fumbles_lost,na.rm=TRUE),
                    WOPR = round(mean(wopr,na.rm=TRUE),2),
                    RACR = round(mean(racr,na.rm=TRUE),2),
                    `Tgt-Share` = round(mean(target_share,na.rm=TRUE),2),
                    `Air Yds-Share` = round(mean(air_yards_share,na.rm=TRUE),2),
                    `Rush EPA` = round(mean(rushing_epa,na.rm=TRUE),2),
                    `Rec EPA` = round(mean(receiving_epa,na.rm=TRUE),2),
                    touches = sum(touches,na.rm=TRUE)
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