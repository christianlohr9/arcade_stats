library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinytoastr)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(nflfastR)

# Conditional ffscrapr loading for deployment compatibility
if (requireNamespace("ffscrapr", quietly = TRUE)) {
  library(ffscrapr)
  .ffscrapr_available <- TRUE
  message("✓ ffscrapr loaded - Liga integration available")
} else {
  .ffscrapr_available <- FALSE
  message("⚠ ffscrapr not available - Liga integration disabled")
}

library(RCurl)
library(powerjoin)
library(psych)
library(qs)
library(jsonlite)
library(glue)
library(purrr)

# Source external functions and modules
source("R/functions/compute_war.R")
source("R/modules/mod_war_tab.R")
source("R/modules/mod_nflfastr_offense_tab.R")
source("R/modules/mod_nflfastr_defense_tab.R")
source("R/modules/mod_player_stats_offense_tab.R")
source("R/modules/mod_help_league_ids_tab.R")

options(digits = 2,scipen = 9999,nflreadr.prefer = "qs")
pal <- c(rev(ggsci::rgb_material("deep-purple")), ggsci::rgb_material("green"))
# Create themes with SAME version but different configs
light <- bs_theme(version = 4, 
  bg = "#ffffff", 
  fg = "#000000",
  primary = "#007bff"
)
dark <- bs_theme(version = 4,
  bg = "#222222",
  fg = "#D4D3D3",
  primary = "#375A7F",
  base_font = font_google("Roboto"),
  "body-bg" = "#222222",
  "body-color" = "#D4D3D3",
  "navbar-dark-bg" = "#222222",
  "table-bg" = "#222222", 
  "table-color" = "#D4D3D3"
)

# Load data
weekly_join <- tryCatch({
  readRDS("data/weekly_stats.rds")
}, error = function(e) {
  data.frame()
}) |> 
  dplyr::mutate(team = factor(coalesce(as.character(team), recent_team)))

weekly_join_def <- tryCatch({
  readRDS("data/weekly_stats_def.rds")  
}, error = function(e) {
  data.frame()
}) |> 
  dplyr::mutate(team = factor(coalesce(as.character(team), recent_team)))

weekly_join_war <- weekly_join |>  select("player_id","player_name","recent_team","position","season","week","fantasy_points_ppr")
weekly_join_def_war <- weekly_join_def |>  select("player_id","player_name","recent_team","position","season","week","fantasy_points_ppr")

# Global reactive values
war_df_global <- reactiveVal(NULL)

# Original waiting screen from shiny_base_stats.R
gif <- "https://i.scdn.co/image/ab6765630000ba8aed1285ce5d62661172e43877"
waiting_screen <- tagList(
  h3(glue::glue("Je nach Auswahl der Methode kann die \nArcade Fantasy Magie etwas dauern 🔮"), style = "color:white;"),
  img(src = gif, height = "200px")
)

# Define UI with dark theme
ui <- fluidPage(
  theme = dark,
  useShinyjs(),
  useToastr(),
  useWaiter(),
  
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "192x192", href = "android-chrome-192x192.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "android-chrome-512x512.png"),
    
    # Import JetBrains Mono font
    tags$link(
      rel = "preconnect",
      href = "https://fonts.googleapis.com"
    ),
    tags$link(
      rel = "preconnect", 
      href = "https://fonts.gstatic.com",
      crossorigin = NA
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=JetBrains+Mono:ital,wght@0,100..800;1,100..800&display=swap"
    ),
    
    # Only font styling, let Bootstrap handle the rest
    tags$style(HTML("
      * {
        font-family: 'JetBrains Mono', 'Courier New', monospace !important;
      }
    "))
  ),
  
  # Theme toggle
  div(
    class = "form-check form-switch",
    style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
    tags$input(
      type = "checkbox",
      class = "form-check-input",
      id = "light_mode"
    ),
    tags$label(
      "Light mode", `for` = "light_mode", class = "custom-control-label"
    )
  ),
  
  # Navigation Bar:
  navbarPage(
    title = "Arcade Fantasy Stats Lab",

    # Module tabs
    mod_nflfastr_offense_tab_ui("nflfastr_offense"),
    mod_nflfastr_defense_tab_ui("nflfastr_defense"), 
    mod_player_stats_offense_tab_ui("player_stats_offense"),
    mod_war_tab_ui("war"),
    
    # Values Stats
    tabPanel("Values",
      fluidRow(column(width=12,
        p("ACHTUNG: In diesem Tab richtet sich alles nach der Auswahl, die im Tab 'Wins Above Replacement' getroffen wurde."),
        br()
      )),
      fluidRow(
        column(2,
          actionButton("value_calc",
            label = "Calculate"
          )
        )),
      DT::dataTableOutput("values")
    ),
    
    mod_help_league_ids_tab_ui("help_league_ids")
  )
)

# Define server logic
server <- function(input, output, session) {

  # No theme switching - accept the limitation
  # observeEvent(input$light_mode, {
  #   # Theme switching disabled due to Bootstrap version conflicts
  # }, ignoreInit = TRUE)

  options(shiny.maxRequestSize=50*1024^2)

  observeEvent(input$disconnect, {
    session$close()
  })

  # Module server calls
  mod_nflfastr_offense_tab_server("nflfastr_offense", weekly_join)
  mod_nflfastr_defense_tab_server("nflfastr_defense", weekly_join_def)
  mod_player_stats_offense_tab_server("player_stats_offense", weekly_join, show_no_data_message, waiting_screen)
  mod_war_tab_server("war", war_df_global, weekly_join_war, weekly_join_def_war, weekly_join, show_no_data_message, waiting_screen, compute_war)
  mod_help_league_ids_tab_server("help_league_ids")

  # Placeholder message for other tabs requiring data
  show_no_data_message <- function() {
    shinytoastr::toastr_warning("Keine Daten verfügbar. Bitte zuerst update_data.R ausführen.", 
                                title = "ACHTUNG!", closeButton = TRUE,
                                position = "top-full-width")
  }

  # Values functionality - original complex version
  output$values <- DT::renderDataTable({
    war_df <- war_df_global()

    if (!is.null(war_df) && nrow(war_df) > 0) {
      values_team <- bind_rows(
        war_df %>%
          filter(Position == "QB",
                 franchise_name != "Free Agent") %>%
          group_by(franchise_name) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_qb %||% 1)) %>%
          group_by(Team = franchise_name) %>%
          summarise(pos = "QB", war = sum(WAR), .groups = "drop"),
        war_df %>%
          filter(Position == "RB",
                 franchise_name != "Free Agent") %>%
          group_by(franchise_name) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_rb %||% 2)) %>%
          group_by(Team = franchise_name) %>%
          summarise(pos = "RB", war = sum(WAR), .groups = "drop"),
        war_df %>%
          filter(Position == "WR",
                 franchise_name != "Free Agent") %>%
          group_by(franchise_name) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_wr %||% 3)) %>%
          group_by(Team = franchise_name) %>%
          summarise(pos = "WR", war = sum(WAR), .groups = "drop"),
        war_df %>%
          filter(Position == "TE",
                 franchise_name != "Free Agent") %>%
          group_by(franchise_name) %>%
          arrange(-WAR) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= as.numeric(input$war_te %||% 1)) %>%
          group_by(Team = franchise_name) %>%
          summarise(pos = "TE", war = sum(WAR), .groups = "drop")
      ) %>%
        pivot_wider(names_from = pos, values_from = war, values_fill = 0) %>%
        mutate(OVR = QB + RB + WR + TE, .before = QB) %>%
        arrange(-OVR)

      DT::datatable(values_team,
        rownames = FALSE,
        options = list(
          pageLength = 100,
          columnDefs = list(list(className = 'dt-left', targets = "_all"))
        ),
        style = "bootstrap4"
      ) %>%
        DT::formatRound(columns = names(values_team %>% select(-Team)), digits = 1) %>%
        DT::formatStyle(names(values_team %>% select(-Team)),
          background = if (isTRUE(input$light_mode)) 
            styleColorBar(range(c(values_team$OVR, values_team$QB, values_team$RB, values_team$WR, values_team$TE)), '#FFC010', angle = -90) 
          else 
            styleColorBar(range(c(values_team$OVR, values_team$QB, values_team$RB, values_team$WR, values_team$TE)), '#915191', angle = -90),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    } else {
      DT::datatable(data.frame(Message = "No Values data available. Calculate WAR first."))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)