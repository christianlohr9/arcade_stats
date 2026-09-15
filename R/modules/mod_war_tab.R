#' WAR (Wins Above Replacement) Tab Module
#'
#' @description A shiny module for the Wins Above Replacement calculation tab
#'
#' @param id The module namespace id
#'
#' @name mod_war_tab

#' @describeIn mod_war_tab UI function
#' @export
mod_war_tab_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Wins Above Replacement",
    fluidRow(column(width=12,
      p('Hinweis: Bei der Auswahl "mPPR" kann die Ladezeit etwas länger dauern als bei PPR oder einer Sleeper/MFL League ID. Bei einer MFL ID bitte "mfl" voranstellen, bspw. "mfl22686"'),
      br()
    )),
    fluidRow(
      column(2,textInput(ns("war_league"),
        label = "Sleeper League ID / 'PPR' / 'mPPR'",
        value = "PPR")
      ),
      column(2,
        pickerInput(ns("war_season"),
          label = "Choose a Season",
          choices = c("2024"),  # Will be updated dynamically
          selected = "2024",
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_week"),
          label = 'Choose a Week ("0" for whole season)',
          choices = c("0", "1", "2"),  # Will be updated dynamically
          selected = 0,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_games"),
          label = "Min. Games Played",
          choices = 1:17,
          selected = 1,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_teams"),
          label = "Teams",
          choices = as.numeric(2:20),
          selected = 12,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
    ),
    fluidRow(
      column(2,
        pickerInput(ns("war_qb"),
          label = "QB",
          choices = 0:2,
          selected = 1,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_rb"),
          label = "RB",
          choices = 0:5,
          selected = 2,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_wr"),
          label = "WR",
          choices = 0:5,
          selected = 3,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_te"),
          label = "TE",
          choices = 0:2,
          selected = 1,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_rec"),
          label = "WR/TE",
          choices = 0:5,
          selected = 0,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_flx"),
          label = "RB/WR/TE",
          choices = 0:5,
          selected = 1,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
    ),
    fluidRow(
      column(2,
        pickerInput(ns("war_di"),
          label = "DI",
          choices = 0:5,
          selected = 0,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_de"),
          label = "DE",
          choices = 0:5,
          selected = 0,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_lb"),
          label = "LB",
          choices = 0:5,
          selected = 0,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_cb"),
          label = "CB",
          choices = 0:5,
          selected = 0,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_ss"),
          label = "S",
          choices = 0:5,
          selected = 0,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
      column(2,
        pickerInput(ns("war_idp"),
          label = "IDP",
          choices = 0:5,
          selected = 0,
          options = list(`actions-box` = TRUE),
          multiple = FALSE)
      ),
    ),
    fluidRow(
      column(2,
        useWaiter(),
        actionButton(ns("war_calc"),
          label = "Calculate"
        )
      ),
      column(2,
        downloadButton(ns('war_download'),
          "Download")
      )
    ),
    DT::dataTableOutput(ns("WAR"))
  )
}

#' @describeIn mod_war_tab Server function
#' @export
mod_war_tab_server <- function(id, war_df_global, weekly_join_war, weekly_join_def_war, weekly_join, show_no_data_message, waiting_screen, compute_war) {
  moduleServer(id, function(input, output, session) {
    
    # Update picker choices dynamically when data is available
    observe({
      if(!is.null(weekly_join) && nrow(weekly_join) > 0) {
        updatePickerInput(session, "war_season",
          choices = levels(weekly_join$season),
          selected = levels(as.factor(ifelse(lubridate::year(lubridate::today()) %in% levels(weekly_join$season),
                                           lubridate::year(lubridate::today()),
                                           max(levels(weekly_join$season)))))
        )
        
        updatePickerInput(session, "war_week",
          choices = levels(as.factor(c(0,as.numeric(weekly_join$week))))
        )
      }
    })
    
    # WAR calculation observeEvent
    observeEvent(input$war_calc, {
      if(nrow(weekly_join_war) == 0) {
        show_no_data_message()
        return()
      }

      waiter_show(html = waiting_screen, color = "black")

      war_df_pre <- tidyr::crossing(f_years = input$war_season, f_week = input$war_week) |>
        purrr::pmap_dfr(function(f_years, f_week) {
          compute_war(f_years, f_week, weekly_join_war, weekly_join_def_war, weekly_join, input)
        })|>
        mutate(
          consistency = consistency_temp*WAR
        )

      war_df <- war_df_pre %>%
        {
          if(input$war_league %in% c("PPR","mPPR")) {
            mutate(.,franchise_name="Keine Liga ausgewählt")
          } else if(grepl("mfl", input$war_league, fixed = TRUE)) {
            left_join(.,
                      ffscrapr::ff_starters(ffscrapr::mfl_connect(max(input$war_season), league_id = gsub("mfl", "", input$war_league))) %>%
                        left_join(ffscrapr::ff_franchises(ffscrapr::mfl_connect(max(input$war_season), league_id = gsub("mfl", "", input$war_league))) %>% select(franchise_id,conference)) %>%
                        filter(conference=="01") %>%
                        group_by(mfl_id=player_id) %>%
                        summarise(
                          franchise_name = first(franchise_name)
                        ) %>%
                        left_join(
                          ffscrapr::dp_playerids() %>% filter(!is.na(mfl_id),!is.na(gsis_id)) %>% mutate(player_id = as.factor(gsis_id)) %>% select(mfl_id,player_id)
                        )
            )
          }  else {
            left_join(.,
                      ffscrapr::ff_rosters(ffscrapr::sleeper_connect(season = max(as.numeric(input$war_season)), league_id = input$war_league)) %>%
                        select(sleeper_id=player_id,franchise_name) %>%
                        left_join(
                          ffscrapr::dp_playerids() %>% filter(!is.na(sleeper_id),!is.na(gsis_id)) %>% mutate(player_id = as.factor(gsis_id)) %>% select(sleeper_id,player_id)
                        )
            )
          }
        } %>%
        mutate(franchise_name = ifelse(is.na(franchise_name),"Free Agent",franchise_name)) %>%
        ungroup()  %>%
        arrange(-WAR) %>%
        mutate(rank = row_number(),
               Avr_Win_Percent = paste0(round(Avr_Win_Percent*100,0),"%"),
               player = as.factor(player)) %>%
        relocate(rank)

      war_df_global(war_df)
      
      waiter_hide()
    })

    # WAR output
    output$WAR <- DT::renderDataTable({
      war_df <- war_df_global()
      
      if (!is.null(war_df) && nrow(war_df) > 0) {
        DT::datatable(war_df %>%
            select(rank, player, Position, Ave_Week_Points, Avr_Win_Percent, WAR, WAA, Games, consistency, franchise_name) %>%
            rename(`FP/W` = Ave_Week_Points) %>%
            rename(`Win%` = Avr_Win_Percent) %>%
            rename(`Franchise` = franchise_name),
          extensions = c('Buttons'),
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            scrollX = TRUE,
            scrollY = '600px',
            fixedColumns = list(leftColumns = 3),
            scroller = TRUE,
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            pageLength = 100
          ),
          rownames = FALSE,
          style = "bootstrap4",
          filter = "top"
        ) %>%
        formatRound(columns = c('FP/W', 'WAR', 'WAA'), digits = 2) %>%
        formatRound(columns = "Games", digits = 1) %>%
        formatStyle(columns = c('rank', 'player', 'Position', 'FP/W', 'Win%', 'WAR', 'WAA', 'Games', 'Franchise'), 
                   backgroundColor = 'transparent', color = 'inherit')
      } else {
        DT::datatable(data.frame(Message = "Keine Daten verfügbar. Bitte laden Sie zuerst Daten."))
      }
    })

    # Download handler
    output$war_download <- downloadHandler(
      filename = function() {
        paste("war_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        war_df <- war_df_global()
        if (!is.null(war_df) && nrow(war_df) > 0) {
          write.csv(war_df, file, row.names = FALSE)
        }
      }
    )
  })
}