root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

app_env <- new.env()
for (f in list.files(file.path(root, "R"), full.names = TRUE)) sys.source(f, envir = app_env)
pipeline_env <- new.env()
sys.source(file.path(root, "pipeline", "data.R"), envir = pipeline_env)

# Small synthetic datasets with the columns the pipeline produces.
fake_datasets <- function(seasons = 2024:2025, weeks = 1:17, seed = 1) {
  set.seed(seed)
  positions <- c(QB = 30, RB = 60, WR = 80, TE = 30)
  players <- data.frame(
    player_id = sprintf("00-%07d", seq_len(sum(positions))),
    position = rep(names(positions), positions),
    skill = stats::runif(sum(positions), 0.5, 1.5)
  )
  players$player_name <- paste("Player", players$player_id)
  base <- c(QB = 18, RB = 11, WR = 10, TE = 7)

  grid <- merge(players, expand.grid(season = seasons, week = weeks))
  n <- nrow(grid)
  offense <- data.frame(
    player_id = grid$player_id, player_name = grid$player_name,
    player_display_name = grid$player_name, headshot_url = NA_character_,
    position = grid$position, team = sample(c("KC", "BUF", "SF", "DET"), n, TRUE),
    season = as.integer(grid$season), week = as.integer(grid$week)
  )
  numeric_cols <- setdiff(c(pipeline_env$OFFENSE_COLUMNS, pipeline_env$EP_COLUMNS),
                          c(names(offense), "fantasy_points_ppr"))
  for (col in numeric_cols) offense[[col]] <- stats::rpois(n, 3)
  offense$fantasy_points_ppr <- pmax(0, stats::rnorm(n, base[grid$position] * grid$skill, 5))
  offense$sleeper_id <- as.character(seq_len(n))
  offense$snaps_off <- stats::rpois(n, 40)
  offense$snaps_def <- 0
  offense$touches <- offense$attempts + offense$targets + offense$carries
  offense$yps <- offense$passing_yards / offense$snaps_off
  offense$ops <- offense$touches / offense$snaps_off
  offense$epps <- offense$total_fantasy_points_exp / offense$snaps_off
  offense$fpps <- offense$fantasy_points_ppr / offense$snaps_off
  offense$stat <- ifelse(offense$position == "QB", "Passing", "Rushing/Receiving")

  def_players <- data.frame(
    player_id = sprintf("00-9%06d", 1:100),
    position = rep(c("DT", "DE", "LB", "CB", "S"), each = 20)
  )
  dgrid <- merge(def_players, expand.grid(season = seasons, week = weeks))
  defense <- data.frame(
    player_id = dgrid$player_id, player_name = dgrid$player_id, player_display_name = dgrid$player_id,
    position = dgrid$position, team = "KC", season = as.integer(dgrid$season), week = as.integer(dgrid$week)
  )
  for (col in setdiff(pipeline_env$DEFENSE_COLUMNS, names(defense))) defense[[col]] <- stats::rpois(nrow(dgrid), 2)
  defense$fantasy_points_ppr <- pipeline_env$idp_points(defense)

  list(offense = tibble::as_tibble(offense), defense = tibble::as_tibble(defense))
}

write_fake_release <- function(dir, datasets = fake_datasets(), updated_at = "2025-10-01T10:00:00Z") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  nanoparquet::write_parquet(datasets$offense, file.path(dir, "offense.parquet"))
  nanoparquet::write_parquet(datasets$defense, file.path(dir, "defense.parquet"))
  manifest <- list(updated_at = updated_at, current_season = max(datasets$offense$season),
                   current_week = 18, latest_week = max(datasets$offense$week),
                   seasons = sort(unique(datasets$offense$season)),
                   rows = lapply(datasets, nrow))
  jsonlite::write_json(manifest, file.path(dir, "manifest.json"), auto_unbox = TRUE)
  dir
}
