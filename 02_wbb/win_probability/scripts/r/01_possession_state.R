## ============================================================
## 01_possession_state.R — Phase 1: score + clock on the possession spine
## ------------------------------------------------------------
## Builds ON TOP of 11_gameprep_project's possession layer. It does NOT
## re-detect possessions: classify_possessions() owns that definition,
## is validated against the 0.44 box estimate (mean |diff| 4.1% across
## 203 games), and a second definition in this codebase would drift.
##
## What this adds, per the Phase 1 brief:
##   * running score differential at that point in game time
##   * period / clock state (period, seconds remaining, OT flag)
##
## HOW THE JOIN WORKS. classify_possessions() emits `start_idx` /
## `end_idx`, row indices into order_pbp(pbp). Those are the only safe
## key: clock values tie constantly in this feed (a bucket and the subs
## after it share one), so a period+clock join silently mismatches.
## Everything below indexes; nothing matches on clock.
##
## SCORE IS READ FROM THE FEED, not accumulated. ESPN carries
## home_score/away_score on every event, and classify_possessions()
## itself reports an `uncredited_points` attribute for baskets it could
## not attribute — so summing its `points` column would inherit that
## drift. The feed's running score is authoritative.
##
## Score is taken AT possession start. The event that closes one
## possession opens the next at the same index, so a made basket is
## already reflected in the score the receiving team inherits — which is
## the correct state for a win-probability model.
##
## Depends on: 11_gameprep_project (source_all.R)
## ============================================================

suppressMessages({
  library(dplyr)
  library(tibble)
})

GAMEPREP <- Sys.getenv("GAMEPREP_ROOT", unset = "")
if (!nzchar(GAMEPREP)) {
  GAMEPREP <- file.path(Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics"),
                        "11_gameprep_project")
}
source(file.path(GAMEPREP, "source_all.R"))

WINPROB_ROOT <- Sys.getenv("WINPROB_ROOT", unset = "")
if (!nzchar(WINPROB_ROOT)) {
  WINPROB_ROOT <- file.path(Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics"),
                            "12_wbb_winprob_calc")
}

#' Seconds left in the whole game at a point in a period
#'
#' Regulation sums the remaining full periods; overtime does not. An OT
#' period is its own terminal game — if it ends level another begins, and
#' no model can know in advance that it will — so "time remaining" in OT
#' means time left in THIS period. `is_ot` is carried so a model can fit
#' overtime separately rather than pretending it is the 5th quarter.
seconds_remaining_game <- function(league, period, secs_left_in_period) {
  cfg    <- league_cfg(league)
  period <- as.integer(period)
  ifelse(
    period > cfg$reg_periods,
    secs_left_in_period,
    secs_left_in_period + (cfg$reg_periods - period) * cfg$period_seconds
  )
}

#' Possessions for one game with score and clock state attached
#'
#' @param pbp    Play-by-play for ONE game. Pure function over it — no
#'   network — so this is testable offline like the layer beneath it.
#' @param league One of supported_leagues().
#' @return the possession spine plus:
#'   secs_left_period, secs_left_game, is_ot,
#'   home_team, away_team, off_is_home,
#'   home_score, away_score, home_margin, off_margin,
#'   home_win (the Phase 4 label; NA while a game is in progress)
possession_state <- function(pbp, league = "wbb") {
  poss <- classify_possessions(pbp, league)
  if (nrow(poss) == 0) return(poss)

  ord <- chr_ids(order_pbp(pbp))

  home_ab <- as.character(ord$home_team_abbrev[1])
  away_ab <- as.character(ord$away_team_abbrev[1])

  # ESPN carries the running score on every row, but only updates it on
  # scoring plays; non-scoring rows can arrive empty depending on how the
  # feed was assembled. Carry the last known value forward so a possession
  # starting on a rebound still knows the score.
  fill_fwd <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (all(is.na(x))) return(rep(0, length(x)))
    idx <- cummax(ifelse(is.na(x), 0L, seq_along(x)))
    idx[idx == 0L] <- which(!is.na(x))[1]
    x[idx]
  }
  hs <- fill_fwd(ord$home_score)
  as_ <- fill_fwd(ord$away_score)

  # WHICH ROW HOLDS THE SCORE AT A POSSESSION'S START.
  #
  # ESPN's home_score/away_score are the score AFTER that row's event.
  # Usually the event at `start_idx` is the one that ENDED the previous
  # possession (a made basket, a defensive rebound) — the new offence
  # genuinely inherits that score, so `start_idx` is right.
  #
  # The exception is a possession the walker opened itself, with
  # start_reason "period_start". That happens when a period's first ball
  # event is not preceded by a change of possession — e.g. WBB 401827253,
  # where period 2 opens with four substitutions and the first ball event
  # IS a made jumper. The walker opens a possession on that basket and
  # closes it on the same row, so reading `start_idx` would hand the model
  # a starting score that already contains this possession's own points:
  # the outcome leaking into the state. Step back one row for those.
  score_idx <- ifelse(poss$start_reason == "period_start",
                      pmax(1L, poss$start_idx - 1L),
                      poss$start_idx)
  # A game's very first possession has nothing before it.
  opens_game <- poss$start_reason == "period_start" & poss$start_idx <= 1L

  secs_left <- round(clock_to_seconds(ord$clock_display_value[poss$start_idx]))
  # A possession opened by an event with an unparseable clock falls back
  # to the walker's own recorded start, which is never NA.
  secs_left[is.na(secs_left)] <- round(clock_to_seconds(poss$clock_start))[is.na(secs_left)]

  win <- if ("home_team_winner" %in% names(ord)) {
    w <- ord$home_team_winner[1]
    if (is.na(w)) NA else isTRUE(as.logical(w))
  } else NA

  poss |>
    mutate(
      home_team        = home_ab,
      away_team        = away_ab,
      off_is_home      = off_team == home_ab,

      secs_left_period = as.integer(secs_left),
      secs_left_game   = as.integer(seconds_remaining_game(league, period, secs_left)),
      is_ot            = period > league_cfg(league)$reg_periods,

      home_score       = as.integer(ifelse(opens_game, 0L, hs[score_idx])),
      away_score       = as.integer(ifelse(opens_game, 0L, as_[score_idx])),
      home_margin      = home_score - away_score,
      # Margin from the perspective of whoever has the ball — the framing
      # a win-probability model conditions on.
      off_margin       = ifelse(off_is_home, home_margin, -home_margin),

      home_win         = win
    )
}

#' Fetch, build and cache one game's possession state
possession_state_game <- function(game_id, league = "wbb", write = TRUE) {
  cfg <- league_cfg(league)
  pbp <- cfg$pbp_fn(as.character(game_id)[1])
  if (!is.data.frame(pbp)) pbp <- as.data.frame(pbp)
  if (nrow(pbp) == 0) stop("No play-by-play for game ", game_id, call. = FALSE)

  out <- possession_state(pbp, league)

  if (isTRUE(write)) {
    path <- file.path(WINPROB_ROOT, "data", "tidy", tolower(league),
                      paste0(as.character(game_id)[1], "_poss_state.csv"))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(out, path)
    message("Wrote ", path, "  (", nrow(out), " possessions)")
  }
  out
}
