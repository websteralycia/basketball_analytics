## ============================================================
## paths.R — where the tidy layer reads and writes
## ------------------------------------------------------------
## One place that knows the on-disk layout, so the fetch function and
## the cache layer can never disagree about where a file lives.
##
## Root resolution, in order:
##   1. GAMEPREP_ROOT             explicit override
##   2. <BBALL_HOME>/11_gameprep_project
##   3. ~/Desktop                 last-resort fallback
## ============================================================

#' Project root on this machine
gameprep_root <- function() {
  explicit <- Sys.getenv("GAMEPREP_ROOT", unset = "")
  if (nzchar(explicit)) return(explicit)
  bball_home <- Sys.getenv("BBALL_HOME", unset = "")
  if (nzchar(bball_home)) return(file.path(bball_home, "11_gameprep_project"))
  "~/Desktop"
}

#' Folder holding per-game tidy files for one league
#'
#' Namespaced by league because ESPN game ids are only unique within a
#' sport — a wbb and an mbb game could otherwise collide on filename.
tidy_games_dir <- function(league) {
  file.path(gameprep_root(), "data", "tidy", tolower(league), "games")
}

#' Folder holding the card renderer's editable config
#'
#' Lives under consumers/, not data/, because these files are authored by
#' a coach rather than produced by a build — the phrasing on a card is an
#' editorial choice, not a derived number.
card_config_dir <- function() {
  file.path(gameprep_root(), "consumers", "gameprep_cards", "config")
}

#' Cache path for one game's lineup stints
stint_cache_path <- function(game_id, league) {
  file.path(tidy_games_dir(league), paste0(as.character(game_id), "_lineups.csv"))
}

#' Column types for reading a cached stint file back
#'
#' Passed to readr so a round-trip through CSV returns the same types the
#' fetch produced — without this, player ids read back as numeric and
#' stop matching ids fetched fresh.
stint_col_types <- function() {
  readr::cols(
    game_id       = readr::col_character(),
    team          = readr::col_character(),
    opponent      = readr::col_character(),
    date          = readr::col_date(),
    stint_id      = readr::col_integer(),
    period        = readr::col_integer(),
    clock_start   = readr::col_character(),
    clock_end     = readr::col_character(),
    player_id_1   = readr::col_character(),
    player_id_2   = readr::col_character(),
    player_id_3   = readr::col_character(),
    player_id_4   = readr::col_character(),
    player_id_5   = readr::col_character(),
    team_pts      = readr::col_integer(),
    opp_pts       = readr::col_integer(),
    stint_seconds = readr::col_double(),
    lineup_valid  = readr::col_logical()
  )
}

#' Cached play-profile path for one population and season
#'
#' Profiles are namespaced by league for the same reason games are: the
#' population name alone does not identify a sport.
profile_cache_path <- function(population, season, league) {
  slug <- tolower(gsub("[^A-Za-z0-9]+", "_", population))
  slug <- gsub("^_|_$", "", slug)
  file.path(gameprep_root(), "data", "tidy", league, "profiles",
            paste0(slug, "_", season, "_play_profile.csv"))
}

#' Where rendered cards are written
card_output_dir <- function() {
  file.path(gameprep_root(), "consumers", "gameprep_cards", "output")
}

#' Cache path for one game's possessions
possession_cache_path <- function(game_id, league) {
  file.path(tidy_games_dir(league), paste0(as.character(game_id), "_possessions.csv"))
}

#' Column types for reading a cached possession file back
#'
#' Same reason as stint_col_types(): a CSV round trip must return what
#' the fetch produced. `game_id` in particular reads back as numeric
#' without this and stops matching ids resolved from the schedule.
possession_col_types <- function() {
  readr::cols(
    game_id      = readr::col_character(),
    period       = readr::col_integer(),
    poss_id      = readr::col_integer(),
    off_team     = readr::col_character(),
    def_team     = readr::col_character(),
    # Clocks are CHARACTER ("9:49"), not seconds. Forcing integer here
    # parsed every clock to NA and readr reported it only as a warning --
    # the cache read "succeeded" with the column blank. See also the
    # ESPN two-clock-format trap: under a minute the feed sends "58.9".
    clock_start  = readr::col_character(),
    clock_end    = readr::col_character(),
    seconds      = readr::col_double(),
    points       = readr::col_integer(),
    context      = readr::col_character(),
    start_reason = readr::col_character(),
    end_reason   = readr::col_character(),
    had_oreb     = readr::col_logical(),
    start_idx    = readr::col_integer(),
    end_idx      = readr::col_integer()
  )
}
