## ============================================================
## league_config.R — per-league constants and data-source dispatch
## ------------------------------------------------------------
## Every league difference the tidy layer cares about lives HERE.
## Downstream functions take a `league` string and ask this file
## for period lengths and fetchers; they never branch on league
## themselves.
##
## Women's leagues come from wehoop, men's from hoopR. Both are
## sportsdataverse packages with matching signatures, so the
## dispatch is a lookup rather than a code fork.
## ============================================================

# Regulation period count, period length, and OT length per league.
# NCAA women's has used four 10-minute quarters since 2015-16, so wbb
# matches the WNBA. NCAA men's is the outlier: two 20-minute halves.
LEAGUE_CONFIG <- list(
  wnba = list(
    label          = "WNBA",
    reg_periods    = 4L,
    period_seconds = 600L,
    ot_seconds     = 300L,
    shot_clock     = 24L,
    pbp_fn = function(game_id) wehoop::espn_wnba_pbp(game_id = game_id),
    box_fn = function(game_id) wehoop::espn_wnba_player_box(game_id = game_id),
    sched_fn = function(seasons) wehoop::load_wnba_schedule(seasons = seasons),
    bulk_pbp_fn = function(seasons) wehoop::load_wnba_pbp(seasons = seasons),
    team_box_fn = function(seasons) wehoop::load_wnba_team_box(seasons = seasons)
  ),
  wbb = list(
    label          = "NCAA WBB",
    reg_periods    = 4L,
    period_seconds = 600L,
    ot_seconds     = 300L,
    shot_clock     = 30L,
    pbp_fn = function(game_id) wehoop::espn_wbb_pbp(game_id = game_id),
    box_fn = function(game_id) wehoop::espn_wbb_player_box(game_id = game_id),
    sched_fn = function(seasons) wehoop::load_wbb_schedule(seasons = seasons),
    bulk_pbp_fn = function(seasons) wehoop::load_wbb_pbp(seasons = seasons),
    team_box_fn = function(seasons) wehoop::load_wbb_team_box(seasons = seasons)
  ),
  mbb = list(
    label          = "NCAA MBB",
    reg_periods    = 2L,      # two halves
    period_seconds = 1200L,   # 20 minutes
    ot_seconds     = 300L,
    shot_clock     = 30L,
    pbp_fn = function(game_id) hoopR::espn_mbb_pbp(game_id = game_id),
    box_fn = function(game_id) hoopR::espn_mbb_player_box(game_id = game_id),
    sched_fn = function(seasons) hoopR::load_mbb_schedule(seasons = seasons),
    bulk_pbp_fn = function(seasons) hoopR::load_mbb_pbp(seasons = seasons),
    team_box_fn = function(seasons) hoopR::load_mbb_team_box(seasons = seasons)
  ),
  nba = list(
    label          = "NBA",
    reg_periods    = 4L,
    period_seconds = 720L,    # 12 minutes
    ot_seconds     = 300L,
    shot_clock     = 24L,
    pbp_fn = function(game_id) hoopR::espn_nba_pbp(game_id = game_id),
    box_fn = function(game_id) hoopR::espn_nba_player_box(game_id = game_id),
    sched_fn = function(seasons) hoopR::load_nba_schedule(seasons = seasons),
    bulk_pbp_fn = function(seasons) hoopR::load_nba_pbp(seasons = seasons),
    team_box_fn = function(seasons) hoopR::load_nba_team_box(seasons = seasons)
  )
)

#' Look up one league's config, with a clear error on a bad name
league_cfg <- function(league) {
  league <- tolower(as.character(league)[1])
  if (!league %in% names(LEAGUE_CONFIG)) {
    stop("Unknown league '", league, "'. Options: ",
         paste(names(LEAGUE_CONFIG), collapse = ", "), call. = FALSE)
  }
  LEAGUE_CONFIG[[league]]
}

#' Tempo bucket edges: the shot clock split into thirds.
#'
#' CURRENTLY UNUSED. The `tempo` dimension it fed was removed 2026-09-03:
#' bucketing by possession LENGTH meant 22% of "early offense" possessions
#' were turnovers and 8% were buzzer-truncated, so the label oversold what
#' was in the bucket. Kept, with `shot_clock`, because a corrected version
#' measuring time-to-first-SHOT would need exactly this.
#'
#' Early / middle / late offence has to scale with the clock -- 10s is
#' early on a 30-second clock and a third of the way through a 24-second
#' one. Returned as the two interior cut points.
league_tempo_edges <- function(league) {
  sc <- league_cfg(league)$shot_clock
  c(round(sc / 3), round(2 * sc / 3))
}

#' Full length in seconds of a given period number for a league.
#' Anything past regulation is an overtime period.
league_period_seconds <- function(league, period) {
  cfg <- league_cfg(league)
  period <- as.integer(period)
  ifelse(period > cfg$reg_periods, cfg$ot_seconds, cfg$period_seconds)
}

#' Leagues this layer currently supports
supported_leagues <- function() names(LEAGUE_CONFIG)
