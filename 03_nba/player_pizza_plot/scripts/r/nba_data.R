# nba_data.R ------------------------------------------------------------------
#
# The fetch layer. Everything that touches the network or disk lives here, so
# that nba_metrics.R can stay pure and testable.
#
# WHY hoopR's load_* AND NOT stats.nba.com
#   stats.nba.com blocks libcurl at the TLS layer. Verified with identical
#   headers on the same machine, same minute:
#       Python nba_api (urllib3)  -> OK, 0.3s
#       R httr::GET (libcurl)     -> timeout, 30s
#       command-line curl         -> timeout, 25s
#   So every hoopR `nba_*` function fails here, surfacing as the unhelpful
#   error `object 'df_list' not found`. The ESPN-backed `load_nba_*` releases
#   work fine and are fast (team box 0.7s; a 625k-row season of pbp 9.6s).
#
#   If you genuinely need a stats.nba.com-only endpoint (synergy play types,
#   tracking, hustle), pull it in Python and write parquet, then read it here
#   with arrow::read_parquet().
#
# CAVEAT
#   load_* serves a periodic data release, not live ESPN. Recent games can lag.
#   Irrelevant for completed historical seasons; check before using it in-season.
#
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(hoopR)
})


#' Cache an expensive expression to parquet
#'
#' Play-by-play is the only call slow enough to be worth caching (~10s/season).
#' Box scores are sub-second, so just re-fetch those.
#'
#' @param key Cache filename stem.
#' @param expr Expression producing a data frame.
#' @param cache_dir Directory for parquet files; created if absent.
#' @param refresh If TRUE, ignore any existing cache entry.
nba_cached <- function(key, expr, cache_dir = "data/_cache", refresh = FALSE) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(force(expr))

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cache_dir, paste0(key, ".parquet"))

  if (!refresh && file.exists(path)) return(arrow::read_parquet(path))

  out <- force(expr)
  arrow::write_parquet(out, path)
  out
}


#' Drop games that ESPN tags as regular season but the NBA excludes from totals
#'
#' Two distinct quirks, both verified on 2024-25:
#'
#'   All-Star.  The 2025 All-Star mini-tournament (Team Chuck / Shaq / Kenny /
#'   Candace, 2025-02-16) carries season_type == 2. Those clubs get synthetic
#'   team_ids far above the 1-30 franchise range, so an id threshold removes
#'   them cleanly. Their target-score results (41-25 etc.) would badly distort
#'   any efficiency figure.
#'
#'   NBA Cup final.  Played between two real franchises and tagged regular
#'   season, but it does NOT count toward the 82-game standings or NBA.com's
#'   season totals. In 2024-25 that was Bucks 97 - Thunder 81 on 2024-12-17,
#'   leaving both clubs on 83 games. Detected generically as the game shared by
#'   the teams that exceed 82 games, so it works for any season since the Cup
#'   began in 2023-24 and no-ops for earlier ones.
#'
#' Excluding both makes team point totals reconcile EXACTLY with NBA.com for all
#' 30 teams (residual FGA 0.03% / TOV 0.1% are ordinary scorekeeping
#' corrections between the two sources).
#'
#' @param drop_cup_final Set FALSE to keep the NBA Cup championship game.
#'
#' @details
#' ESPN labels these games in the schedule's `notes_headline`, which is the
#' cleanest available discriminator:
#'
#'   "NBA Cup Championship"                 1 game  -- does NOT count
#'   "NBA All-Star - Championship/Semis"    3 games -- do NOT count
#'   "NBA Cup - Group Play/QF/Semifinals"  66 games -- DO count
#'   "NBA Paris Games", "NBA Mexico City"   3 games -- DO count
#'
#' Note that `neutral_site` alone is NOT a valid filter: the Paris, Mexico City
#' and Cup semifinal games are all neutral-site but count normally.
nba_drop_nonstandard <- function(tb, drop_cup_final = TRUE) {
  # Cheap safety net that works even if the schedule fetch fails: All-Star
  # clubs get synthetic team_ids far above the 1-30 franchise range.
  tb <- dplyr::filter(tb, team_id < 100000)

  sch <- try(hoopR::load_nba_schedule(seasons = unique(tb$season)), silent = TRUE)
  if (inherits(sch, "try-error")) {
    warning("Could not load schedule; All-Star removed by team_id but the ",
            "NBA Cup final may remain.")
    return(tb)
  }

  drop_pattern <- if (drop_cup_final) {
    "^NBA All-Star|^NBA Cup Championship"
  } else {
    "^NBA All-Star"
  }

  excluded <- sch %>%
    dplyr::filter(grepl(drop_pattern, notes_headline)) %>%
    dplyr::distinct(game_id)

  dplyr::anti_join(tb, excluded, by = "game_id")
}


#' Team box scores for one or more seasons
#'
#' @param seasons Season end-years, e.g. 2025 for 2024-25.
#' @param season_type One of "regular", "postseason", "play_in", or "all".
#' @param standard_only For the regular season, drop All-Star and NBA Cup final
#'   games so totals match NBA.com. See nba_drop_nonstandard().
nba_team_box <- function(seasons = 2025, season_type = "regular",
                         standard_only = TRUE) {
  tb <- hoopR::load_nba_team_box(seasons = seasons)

  if (!identical(season_type, "all")) {
    code <- NBA_SEASON_TYPE[[season_type]]
    if (is.null(code)) {
      stop("season_type must be one of: ",
           paste(c(names(NBA_SEASON_TYPE), "all"), collapse = ", "))
    }
    tb <- dplyr::filter(tb, season_type == code)
  }

  if (standard_only && identical(season_type, "regular")) {
    tb <- nba_drop_nonstandard(tb)
  }
  tb
}


#' Pair each team-game with its opponent's box score
#'
#' hoopR's team box carries the opponent's identity and score but not the
#' opponent's shooting or rebounding lines, so we self-join on game_id.
#'
#' Renames hoopR's verbose columns to the short names nba_add_metrics() expects,
#' and uses `total_turnovers` rather than `turnovers` -- see the note in
#' nba_metrics.R for the validation behind that choice.
#'
#' @return One row per team-game, with team and opp_ columns for both sides.
nba_team_game_pairs <- function(team_box) {
  side <- team_box %>%
    transmute(
      game_id, season, season_type, game_date,
      team_id, team_display_name, team_abbreviation, team_home_away,
      pts  = team_score,
      fga  = field_goals_attempted,
      fgm  = field_goals_made,
      fg3m = three_point_field_goals_made,
      fta  = free_throws_attempted,
      oreb = offensive_rebounds,
      dreb = defensive_rebounds,
      tov  = total_turnovers          # NOT `turnovers` -- see nba_metrics.R
    )

  # Pairing itself is source-agnostic and lives in nba_metrics.R, so a CSV goes
  # through exactly the same code path this does.
  paired <- nba_pair_opponents(side)

  # Game length, for pace. Taken from the schedule's period count rather than
  # summed player minutes: ESPN rounds minutes to whole numbers, so team totals
  # land on 237-239 roughly 60% of the time, whereas period count is exact.
  sch <- try(hoopR::load_nba_schedule(seasons = unique(paired$season)),
             silent = TRUE)
  if (inherits(sch, "try-error")) {
    warning("Could not load schedule; pace will be unavailable.")
    return(paired)
  }

  lengths <- sch %>%
    transmute(game_id, game_minutes = nba_game_minutes(status_period)) %>%
    distinct(game_id, .keep_all = TRUE)

  paired %>% left_join(lengths, by = "game_id")
}


#' Convenience: paired per-game frame with all metrics attached
nba_team_games <- function(seasons = 2025, season_type = "regular",
                           standard_only = TRUE) {
  nba_team_box(seasons = seasons, season_type = season_type,
               standard_only = standard_only) %>%
    nba_team_game_pairs() %>%
    nba_add_metrics()
}


#' Play-by-play, cached to parquet
nba_pbp <- function(seasons = 2025, cache_dir = "data/_cache", refresh = FALSE) {
  nba_cached(
    key = paste0("nba_pbp_", paste(seasons, collapse = "_")),
    expr = hoopR::load_nba_pbp(seasons = seasons),
    cache_dir = cache_dir,
    refresh = refresh
  )
}
