## ============================================================
## 04_training_set.R — Phase 4: the historical training set
## ------------------------------------------------------------
## Runs Phases 1-2 over every season of play-by-play, attaches the final
## outcome as the label, and joins the Phase 3 rating differential as of
## each game's date.
##
## SEASON AT A TIME, WRITTEN AS IT GOES. One season of WBB play-by-play
## is ~2.8M events and ~1.1 GB in memory; ten at once will not fit. Each
## season is loaded, processed, written to its own parquet file, and
## dropped before the next. A crash costs one season, not the run.
##
## SPLIT THE PLAY-BY-PLAY ONCE. The obvious shape — `for (g in game_ids)
## pbp[pbp$game_id == g, ]` — filters a 2.8M-row frame once per game and
## costs more than every other step combined: 0.201 s/game against 0.071
## for the actual work. split() pays that once. Measured over 54,019
## games that is the difference between ~3 hours and ~65 minutes.
##
## WHAT A ROW IS. One game-state moment, from Phase 2 — a pure possession
## start or a single free-throw attempt. Phase 5 must filter to
## `is_pure` before fitting: inpredictable derives non-pure states from
## the base model rather than fitting them, so training on free-throw
## rows would change what the base model means. They are carried here so
## the derivation layer in Phase 6 has them.
##
## THE LABEL IS home_win, AND IT IS CONSTANT WITHIN A GAME. Every row of
## a game shares its outcome, so rows are NOT independent. Any
## cross-validation in Phase 5 has to split BY GAME, never by row, or the
## same game appears in both folds and the estimate is optimistic.
##
## Depends on: 01_possession_state.R, 02_game_states.R, 03_team_ratings.R
## ============================================================

suppressMessages({
  library(dplyr)
  library(tibble)
})

# 2020, not 2017 — ESPN's PBP coverage before 2020 is a BIASED 30% SAMPLE.
# Measured 2026-08-06 over 2017-2019 (15,890 games, 4,680 with play-by-play):
#
#                    n      mean margin   SD    |margin|   neutral%   total pts
#   has PBP        4,680       8.55      20.1     17.3       19.1       133.3
#   no PBP        11,210       3.48      16.7     13.7        6.5       128.8
#
# and coverage runs 20% in January against 56% in March. The early feed is
# postseason, neutral-site and televised nonconference — margins 26% wider
# and 3x the neutral rate. Detail per game is NOT the problem: pure states
# per game is flat at ~143 in every season, 2017 through 2024.
#
# That subset is disqualifying HERE specifically. A win-probability model
# estimates P(win | margin, time, spread) — a sample skewed toward blowouts
# on neutral courts biases the very distribution being fitted. 2020-2026 is
# 91-98% covered and still yields ~5.5M pure state rows.
#
# This does NOT affect Phase 3: box scores and schedules are complete back to
# 2017, the ratings are still built over all ten seasons, and the carryover
# prior means the first training season starts from a real rating, not cold.
#
# NARROWED TO FIVE SEASONS 2026-08-07, at Alycia's direction — the five most
# recent. That is a tighter window than the coverage evidence alone requires
# (2020 and 2021 are 91% and 94% covered, so they were not excluded for
# quality), and it happens to drop the COVID-shortened 2021 season, which ran
# 3,823 games against a normal ~5,500 and was played under abnormal
# conditions. Still 4.07M pure rows across 28,308 games.
#
# Widening it back is a one-line change and needs NO Phase 4 rebuild: the
# parquets are per-season files and load_training_set() derives its paths from
# this constant, so 2020 and 2021 are simply not read. They remain on disk,
# valid, built at lambda 3.
TRAIN_SEASONS <- 2022:2026

train_dir <- function() file.path(WINPROB_ROOT, "data", "tidy", "wbb", "training")

#' Per-game facts the state rows need but do not carry
#'
#' game_states() works in team ABBREVIATIONS (from the play-by-play) while
#' the Phase 3 ratings are keyed on team_id (from the box scores). This is
#' the bridge, built from the Phase 3 game frame, which has both plus the
#' pace and neutral-site flag the spread conversion needs.
game_key <- function(games) {
  games |>
    transmute(game_id = as.character(game_id), date, season,
              home_id = home, away_id = away, pace, neutral,
              home_final, away_final)
}

#' Rating lookup keyed on date + team, as of that date
rating_lookup <- function(ratings) {
  k <- paste(ratings$date, ratings$team)
  list(rating  = stats::setNames(ratings$rating, k),
       n_games = stats::setNames(ratings$n_games, k),
       home    = stats::setNames(ratings$home_term, k),
       neutral = stats::setNames(ratings$neutral_term, k))
}

#' Attach the spread-equivalent and the label to one season's state rows
#'
#' `spread_home` is the Phase 3 expected margin in points, positive when
#' the home team is favoured. `spread_ball` re-orients it to whoever has
#' the ball, which is the frame a win-probability model conditions on —
#' the same orientation as `ball_margin` from Phase 2.
attach_context <- function(states, gk, rl) {
  s <- states |> left_join(gk, by = "game_id")

  kh <- paste(s$date, s$home_id)
  ka <- paste(s$date, s$away_id)
  rh <- unname(rl$rating[kh]); ra <- unname(rl$rating[ka])
  nh <- unname(rl$n_games[kh]); na_ <- unname(rl$n_games[ka])
  ht <- ifelse(s$neutral, unname(rl$neutral[kh]), unname(rl$home[kh]))

  rh[is.na(rh)] <- 0; ra[is.na(ra)] <- 0; ht[is.na(ht)] <- 0
  nh[is.na(nh)] <- 0L; na_[is.na(na_)] <- 0L

  s |>
    mutate(
      rating_home = rh, rating_away = ra,
      # How informed the rating is. Carried, not filtered on: a November
      # row is a legitimate training row, it is just a weaker one, and
      # Phase 5 needs to be able to report calibration by this.
      rating_n_home = as.integer(nh), rating_n_away = as.integer(na_),
      rating_n_min  = as.integer(pmin(nh, na_)),
      spread_home = (rh - ra + ht) * pace / 100,
      spread_ball = ifelse(ball_is_home, spread_home, -spread_home),
      # THE LABEL. Derived from the final score, NOT from the
      # play-by-play's home_team_winner: that column exists in
      # espn_wbb_pbp() but not in the bulk load_wbb_pbp() this phase
      # uses, so reading it produces an entirely NA label without
      # erroring. Basketball has no ties, so the sign is unambiguous.
      home_won  = home_final > away_final,
      ball_win  = ifelse(ball_is_home, home_won, !home_won)
    ) |>
    select(-home_id, -away_id, -home_win)
}

#' Build and cache one season
#'
#' @param cores Forked workers. mclapply is used rather than a PSOCK
#'   cluster so the season's play-by-play is shared by copy-on-write
#'   instead of serialised to every worker.
build_season_training <- function(season, games, ratings, cores = 1L,
                                  out_dir = train_dir(), quiet = FALSE) {
  if (!quiet) message("season ", season, ": loading play-by-play ...")
  pbp <- suppressWarnings(league_cfg("wbb")$bulk_pbp_fn(season))
  if (nrow(pbp) == 0) { warning("no pbp for ", season); return(invisible(NULL)) }

  # Split ONCE — see the header.
  if (!quiet) message("  splitting ", format(nrow(pbp), big.mark = ","), " events ...")
  by_game <- split(pbp, pbp$game_id)
  rm(pbp); invisible(gc(FALSE))

  if (!quiet) message("  ", length(by_game), " games, ", cores, " core(s) ...")
  worker <- function(g) tryCatch(game_states(g, "wbb"), error = function(e) NULL)
  res <- if (cores > 1L) parallel::mclapply(by_game, worker, mc.cores = cores)
         else lapply(by_game, worker)

  failed <- vapply(res, is.null, logical(1))
  states <- bind_rows(res[!failed])
  rm(res, by_game); invisible(gc(FALSE))
  if (nrow(states) == 0) { warning("no states for ", season); return(invisible(NULL)) }

  out <- attach_context(states, game_key(games), rating_lookup(ratings))
  out$season <- season

  # A few games each season have play-by-play but NO team box score, so the
  # game frame has no row for them: no final score, hence no label — and no
  # pace either, so no spread. 2025 has five, all early-season neutral-site
  # tournament games (Paradise Jam, Fort Myers Tip-Off), 815 rows of 963,066.
  #
  # The schedule DOES carry their finals, so the label alone is recoverable.
  # They are still dropped, because pace is not: converting a rating to a
  # point spread needs it, and imputing it would slip a fabricated feature
  # into the training set to save 0.08% of rows. Dropped LOUDLY — silence
  # here is what the guard below exists to prevent.
  no_box <- is.na(out$home_final) | is.na(out$away_final)
  if (any(no_box)) {
    ids <- unique(out$game_id[no_box])
    message("  DROPPED ", sum(no_box), " rows from ", length(ids),
            " game(s) with no box score: ", paste(ids, collapse = ", "))
    out <- out[!no_box, ]
  }

  # A very few games carry a THIRD team_id on some events — an ESPN defect,
  # not a tie-breaking case. team_abbrev_map() knows only the two real teams,
  # so those events resolve to ball_team NA and thus ball_is_home NA.
  # Measured 2026-08-06: 2 games in 2026 (401824213, 401830077), 0 in 2025,
  # 0 in 2020-2024. Only 401830077 produces unresolvable STATE rows — two
  # free-throw attempts, one whole trip, on bogus team_id 108890.
  #
  # Checked before dropping: no PURE row is affected in either game, and no
  # affected row resolves to the WRONG team — they come out NA, so the guard
  # below catches them instead of a label quietly flipping. At two rows in
  # 1.07M, inferring the team from the shooter's other events is machinery
  # that would not pay for itself; revisit if this ever grows.
  bad_team <- is.na(out$ball_is_home)
  if (any(bad_team)) {
    ids <- unique(out$game_id[bad_team])
    message("  DROPPED ", sum(bad_team), " rows from ", length(ids),
            " game(s) with an unresolvable team: ", paste(ids, collapse = ", "))
    out <- out[!bad_team, ]
  }

  # Anything still unlabelled is a different problem and must not pass.
  # An unlabelled training set is worse than none — fail loudly.
  if (any(is.na(out$ball_win))) {
    stop(sum(is.na(out$ball_win)), " of ", nrow(out),
         " rows have no label in season ", season, call. = FALSE)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(out_dir, paste0("wbb_", season, "_training.parquet"))
  arrow::write_parquet(out, path)

  if (!quiet) message(sprintf("  wrote %s  (%s rows, %d pure, %d games failed)",
                              basename(path), format(nrow(out), big.mark = ","),
                              sum(out$is_pure), sum(failed)))
  invisible(list(path = path, rows = nrow(out), failed = sum(failed)))
}

#' Build every season
#'
#' @param skip_existing Resume an interrupted run. DEFAULT FALSE, and it has
#'   to stay that way: the parquets bake in `spread_home`/`spread_ball` from
#'   whatever RATING_LAMBDA/CARRYOVER_DECAY were current when they were
#'   written, so after a Phase 3 re-tune every existing file is stale and
#'   skipping it would silently mix two rating generations in one training
#'   set. Pass TRUE only to finish a run interrupted since the last re-tune.
build_training_set <- function(seasons = TRAIN_SEASONS, games, ratings,
                               cores = max(1L, parallel::detectCores() - 1L),
                               skip_existing = FALSE) {
  lapply(seasons, function(s) {
    path <- file.path(train_dir(), paste0("wbb_", s, "_training.parquet"))
    if (skip_existing && file.exists(path)) {
      message("season ", s, ": exists, skipping"); return(invisible(NULL))
    }
    build_season_training(s, games[games$season == s, ], ratings, cores)
  })
}

#' Read the training set back, optionally only the trainable rows
#'
#' `pure_only` is the default because it is what Phase 5 fits on; ask for
#' everything explicitly when building the Phase 6 derivation layer.
load_training_set <- function(seasons = TRAIN_SEASONS, pure_only = TRUE,
                              dir = train_dir()) {
  paths <- file.path(dir, paste0("wbb_", seasons, "_training.parquet"))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) stop("No training files under ", dir, call. = FALSE)
  d <- arrow::open_dataset(paths)
  if (pure_only) d <- dplyr::filter(d, is_pure)
  dplyr::collect(d)
}
