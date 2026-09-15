#' Global Configuration for Arcade Stats Shiny App
#' 
#' This file contains shared configurations, themes, and global variables
#' used across the Shiny application modules.

# Load required libraries
library(shiny)
library(shinyjs)
library(shinytoastr)
library(shinyWidgets)
library(shinymanager)
library(shinycssloaders)
library(waiter)
library(bslib)
library(tidyverse)
library(DT)
library(RCurl)
library(ggimage)
library(nflfastR)

# Conditional ffscrapr loading for deployment compatibility
if (requireNamespace("ffscrapr", quietly = TRUE)) {
  library(ffscrapr)
  if (!exists(".ffscrapr_available")) .ffscrapr_available <- TRUE
} else {
  if (!exists(".ffscrapr_available")) .ffscrapr_available <- FALSE
}
library(googlesheets4)
library(shinydisconnect)
library(powerjoin)
library(psych)
library(qs)
library(jsonlite)

# Global options
options(
  digits = 2,
  scipen = 9999,
  nflreadr.prefer = "qs",
  shiny.maxRequestSize = 50 * 1024^2
)

# Color palette
pal <- c(rev(ggsci::rgb_material("deep-purple")), ggsci::rgb_material("green"))

# Themes
light <- bs_theme(version = 4)

dark <- bs_theme(
  version = 4,
  bg = "#222222",
  fg = "#D4D3D3",
  primary = "#375A7F",
  secondary = "#635D5D",
  warning = "orange",
  error = "darkred",
  base_font = font_google("Roboto")
)

# Loading screen configuration
gif <- "https://i.scdn.co/image/ab6765630000ba8aed1285ce5d62661172e43877"

waiting_screen <- tagList(
  h3(glue::glue("Je nach Auswahl der Methode kann die \nArcade Fantasy Magie etwas dauern 🔮"), 
     style = "color:white;"),
  img(src = gif, height = "200px")
)

# Data loading functions
load_weekly_stats <- function() {
  if (file.exists("data/weekly_stats.rds")) {
    readRDS("data/weekly_stats.rds")
  } else {
    stop("weekly_stats.rds not found. Please run update_data.R first.")
  }
}

load_weekly_stats_def <- function() {
  if (file.exists("data/weekly_stats_def.rds")) {
    readRDS("data/weekly_stats_def.rds")
  } else {
    stop("weekly_stats_def.rds not found. Please run update_data.R first.")
  }
}

# Utility functions for data processing
get_team_choices <- function() {
  c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
    "DET", "GB", "HOU", "IND", "JAX", "KC", "LV", "LAC", "LAR", "MIA", 
    "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", "SF", "TB", 
    "TEN", "WAS")
}

get_position_choices_offense <- function() {
  c("QB", "RB", "WR", "TE")
}

get_position_choices_defense <- function() {
  c("DT", "DE", "LB", "CB", "S")
}

get_stat_type_choices <- function() {
  c("Passing", "Rushing/Receiving")
}

# Common UI helper functions
create_download_button <- function(outputId, label = "Download as CSV", class = "btn-primary") {
  downloadButton(outputId, label, class = class)
}

create_season_input <- function(inputId, label = "Season", value = 2024, min = 1999) {
  numericInput(inputId, label, value = value, min = min, max = as.integer(format(Sys.Date(), "%Y")))
}

create_week_input <- function(inputId, label = "Week", value = 1:18, choices = 1:22) {
  checkboxGroupInput(inputId, label, choices = choices, selected = value, inline = TRUE)
}

create_team_input <- function(inputId, label = "Team", selected = get_team_choices()) {
  checkboxGroupInput(inputId, label, choices = get_team_choices(), selected = selected, inline = TRUE)
}

create_position_input <- function(inputId, label = "Position", choices = get_position_choices_offense(), selected = choices) {
  checkboxGroupInput(inputId, label, choices = choices, selected = selected, inline = TRUE)
}

# Common server helper functions
create_reactive_nflfastr_data <- function(input) {
  reactive({
    nflfastR::load_player_stats(as.numeric(input$season)) %>%
      mutate(recent_team = as.factor(as.character(recent_team))) %>%
      filter(
        season %in% input$season,
        week %in% input$week,
        recent_team %in% input$team
      ) %>%
      left_join(
        nflfastR::fast_scraper_roster(as.numeric(input$season)) %>%
          select(player_id = gsis_id, season, position)
      ) %>%
      mutate(
        position = as.factor(position),
        touches = attempts + carries + receptions,
        rushrec_fumbles = rushing_fumbles + receiving_fumbles,
        rushrec_fumbles_lost = rushing_fumbles_lost + receiving_fumbles_lost,
        rushrec_2pt = rushing_2pt_conversions + receiving_2pt_conversions
      ) %>%
      filter(position %in% input$position)
  })
}

# Common DT options
get_dt_options <- function() {
  list(
    pageLength = 25,
    lengthMenu = c(10, 25, 50, 100),
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  )
}