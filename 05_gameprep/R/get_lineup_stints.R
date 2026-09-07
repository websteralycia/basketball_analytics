## ============================================================
## get_lineup_stints.R — finest-grain lineup stints for one game
## ------------------------------------------------------------
## Refactored out of 02_wbb/scripts/r/7.22_02_build_stints.R, which
## reconstructed stints as one stage of the DPM/RAPM pipeline. This
## version is the shared tidy layer: raw stints only, no possessions,
## no ratings, no duos math. Downstream consumers aggregate.
##
## A stint is cut on EVERY substitution event and every period change,
## with no batching. Rapid substitutions at the same dead ball produce
## consecutive zero-second stints, which is intentional — this is meant
## to be the finest-grain source of truth, and any consumer can collapse
## adjacent identical-lineup stints itself.
##
## Depends on: league_config.R, utils_clock.R
## ============================================================

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
}

# Fraction of substitution events that may fail to resolve before the
# reconstruction is treated as broken rather than merely noisy.
MAX_UNRESOLVED_SUB_RATE <- 0.10

# Sort a set of athlete_ids into a stable length-5 vector, NA-padded.
# Sorted (not positional) so the same five players always produce the
# same column values — that makes the 5 columns usable as a lineup key.
# Returns the observed count too, so a broken lineup can be flagged
# rather than silently truncated.
pad5 <- function(ids) {
  ids <- sort(unique(ids[!is.na(ids)]))
  n <- length(ids)
  length(ids) <- 5L
  list(ids = ids, n = n)
}

# Point value when score_value is missing from the feed.
infer_points <- function(txt) {
  if (is.na(txt)) return(2)
  if (grepl("three", txt, ignore.case = TRUE))      return(3)
  if (grepl("free throw", txt, ignore.case = TRUE)) return(1)
  2
}

# espn_*_pbp / espn_*_player_box return a data frame in current
# versions, but have returned a named list historically. Accept either.
.as_df <- function(x, what) {
  if (is.data.frame(x)) return(x)
  if (is.list(x)) {
    dfs <- Filter(is.data.frame, x)
    if (length(dfs) > 0) return(dfs[[1]])
  }
  stop("Could not read ", what, " as a data frame.", call. = FALSE)
}

#' Reconstruct on-court lineup stints for a single game
#'
#' Pulls play-by-play and the player box for one game, rebuilds each
#' team's on-court five, and cuts a stint on every substitution and
#' period change.
#'
#' @param game_id ESPN game id (chr or numeric).
#' @param league  One of "wnba", "wbb", "mbb", "nba". A bare ESPN game
#'   id does not identify its league, so this is required in practice.
#' @param write   Write the CSV as well as returning the tibble.
#' @param out_dir Override the output folder. NULL resolves to
#'   <BBALL_HOME>/11_gameprep_project/data/tidy/<league>/games, falling
#'   back to the Desktop when BBALL_HOME is unset.
#'
#' @return A tibble, two rows per stint (one per team), columns:
#'   game_id, team, opponent, date, stint_id, period, clock_start,
#'   clock_end, player_id_1..5, team_pts, opp_pts, stint_seconds,
#'   lineup_valid
get_lineup_stints <- function(game_id,
                              league  = c("wnba", "wbb", "mbb", "nba"),
                              write   = TRUE,
                              out_dir = NULL) {

  league  <- match.arg(league)
  cfg     <- league_cfg(league)
  game_id <- as.character(game_id)[1]

  # --- 1. Fetch this game only -------------------------------------
  pbp <- .as_df(cfg$pbp_fn(game_id), "play-by-play")
  box <- .as_df(cfg$box_fn(game_id), "player box")

  if (nrow(pbp) == 0) stop("No play-by-play returned for game ", game_id, call. = FALSE)
  if (nrow(box) == 0) stop("No player box returned for game ", game_id, call. = FALSE)

  # ids as character throughout — avoids float-id join bugs
  pbp$team_id <- as.character(pbp$team_id)
  box$team_id <- as.character(box$team_id)
  box$athlete_id <- as.character(box$athlete_id)

  # Chronological order — shared with the possession walk so the two can
  # never disagree about event sequence. See order_pbp() in pbp_prep.R.
  pbp <- order_pbp(pbp)

  # --- 2. Team + game metadata (all available from the PBP) ---------
  home_id     <- as.character(pbp$home_team_id[1])
  away_id     <- as.character(pbp$away_team_id[1])
  home_abbrev <- as.character(pbp$home_team_abbrev[1])
  away_abbrev <- as.character(pbp$away_team_abbrev[1])
  game_date   <- as.Date(pbp$game_date[1])

  teams <- c(home_id, away_id)
  if (any(is.na(teams))) {
    stop("Could not resolve both team ids for game ", game_id, call. = FALSE)
  }

  # --- 3. Opening lineups from the box starter flags ----------------
  lineup <- list()
  for (t in teams) {
    st <- box$athlete_id[box$team_id == t & isTRUE_vec(box$starter)]
    st <- st[!is.na(st)]
    if (length(st) != 5) {
      warning(sprintf("Game %s team %s: %d starters (expected 5).",
                      game_id, t, length(st)), call. = FALSE)
    }
    lineup[[t]] <- st
  }

  # name -> id fallback for subs whose participant ids are missing
  name2id <- box[!is.na(box$athlete_id), c("team_id", "athlete_display_name", "athlete_id")]
  name2id$nm <- trimws(name2id$athlete_display_name)
  lookup_id <- function(team, nm) {
    hit <- name2id$athlete_id[name2id$team_id == team & name2id$nm == trimws(nm)]
    if (length(hit) >= 1) hit[1] else NA_character_
  }

  # --- 4. Pull event columns out as vectors (faster + clearer) ------
  v_period  <- suppressWarnings(as.integer(pbp$period_number))
  # Round to whole seconds once, here. Stint boundaries are SHARED (the
  # stint that closes and the one that opens read the same event), so
  # rounding a single time keeps the per-period durations telescoping to
  # exactly the period length, and makes clock_start/clock_end agree with
  # stint_seconds exactly. Sub-second stint precision is meaningless here.
  v_secs    <- round(clock_to_seconds(pbp$clock_display_value))
  v_team    <- pbp$team_id
  v_text    <- as.character(pbp$text)
  v_type    <- if ("type_text" %in% names(pbp)) as.character(pbp$type_text) else rep(NA_character_, nrow(pbp))
  v_scoring <- if ("scoring_play" %in% names(pbp)) pbp$scoring_play else rep(NA, nrow(pbp))
  v_sval    <- if ("score_value" %in% names(pbp)) suppressWarnings(as.numeric(pbp$score_value)) else rep(NA_real_, nrow(pbp))
  v_in      <- if ("athlete_id_1" %in% names(pbp)) as.character(pbp$athlete_id_1) else rep(NA_character_, nrow(pbp))
  v_out     <- if ("athlete_id_2" %in% names(pbp)) as.character(pbp$athlete_id_2) else rep(NA_character_, nrow(pbp))

  # --- Substitution grammar -----------------------------------------
  # ESPN uses TWO structures, and which one you get varies by feed, not
  # cleanly by league — so classify per event rather than per league:
  #
  #   PAIRED (WNBA): "X enters the game for Y"
  #     one event per swap; athlete_id_1 = in, athlete_id_2 = out
  #
  #   SINGLE (WBB):  "X subbing out for Grand Canyon"
  #     one event per PLAYER; athlete_id_2 is always NA, and the name
  #     after "for" is the TEAM, not the other player
  #
  # Single-player events carry no pairing information, so per-swap
  # granularity is impossible for those feeds. Consecutive sub events
  # sharing a period+clock are therefore treated as ONE substitution
  # group: the stint closes once before the group and opens once after,
  # which keeps every emitted lineup at a valid five.
  n_ev <- nrow(pbp)
  v_is_sub <- (!is.na(v_type) & grepl("substitution", v_type, ignore.case = TRUE)) |
              (!is.na(v_text) & grepl("subbing (in|out)|enters", v_text, ignore.case = TRUE))

  v_sub_dir <- rep(NA_character_, n_ev)
  v_sub_dir[v_is_sub & !is.na(v_in) & !is.na(v_out)] <- "paired"
  single <- v_is_sub & is.na(v_sub_dir) & !is.na(v_text)
  v_sub_dir[single & grepl("subbing in|enters|checks in", v_text, ignore.case = TRUE)]  <- "in"
  v_sub_dir[single & grepl("subbing out|leaves|checks out", v_text, ignore.case = TRUE)] <- "out"

  # Group id: consecutive sub events at the same period + clock.
  sub_key   <- ifelse(v_is_sub, paste(v_period, v_secs), NA_character_)
  prev_key  <- c(NA_character_, sub_key[-n_ev])
  grp_start <- v_is_sub & (is.na(prev_key) | prev_key != sub_key)
  next_key  <- c(sub_key[-1], NA_character_)
  grp_end   <- v_is_sub & (is.na(next_key) | next_key != sub_key)

  # A PAIRED event is a complete swap on its own, so it can cut its own
  # stint — that is the finest grain the feed supports, and it keeps the
  # original "cut on every substitution" contract for WNBA/NBA. Grouping
  # is only forced where events are single-player and a lone event would
  # leave four or six players on the floor.
  paired_ev <- v_is_sub & !is.na(v_sub_dir) & v_sub_dir == "paired"
  grp_start <- grp_start | paired_ev
  grp_end   <- grp_end   | paired_ev

  # ...but a paired event starting its own group mid-run has to also END the
  # run before it, and vice versa, or close_stint()/open_stint() stop pairing
  # up. WNBA 401857092 period 3 has three subs at 5:48 with MIXED grammar:
  #
  #   371  5:48  single  ("enters the game", no second athlete)
  #   372  5:48  paired
  #   373  5:48  paired
  #
  # 371 got grp_start (previous event was not a sub) and closed the stint, but
  # grp_end stayed FALSE because the same-clock run continued into 372 and 371
  # is not paired. `cur` was left NULL, so 372 fell into the is.null(cur)
  # branch and reopened at the PERIOD START -- inventing a 252-second stint
  # that re-covered 10:00-5:48 and made period 3 tile to 852s instead of 600s.
  #
  # Mirroring the override on both sides keeps every close matched by an open.
  next_paired <- c(paired_ev[-1], FALSE)
  prev_paired <- c(FALSE, paired_ev[-n_ev])
  grp_end   <- grp_end   | (v_is_sub & next_paired)
  grp_start <- grp_start | (v_is_sub & prev_paired)

  # --- 5. Walk the game, emitting stints ---------------------------
  stints    <- list()
  n_stint   <- 0L
  cur       <- NULL       # list(period, start_secs, pts = named numeric)
  n_unresolved_subs <- 0L
  n_phantom_subs    <- 0L # player subbed out who wasn't on the floor
  n_orphan_reopens  <- 0L # stint closed without a matching reopen
  last_period_opened <- NULL

  new_pts <- function() setNames(c(0, 0), teams)

  # Snapshot the CURRENT lineups and close the open stint.
  # MUST be called before mutating `lineup` on a substitution, so the
  # stint records the five who actually played it.
  close_stint <- function(end_secs) {
    if (is.null(cur)) return(invisible(NULL))
    end_secs <- if (is.na(end_secs)) cur$start_secs else end_secs
    lh <- pad5(lineup[[home_id]])
    la <- pad5(lineup[[away_id]])
    n_stint <<- n_stint + 1L
    stints[[length(stints) + 1L]] <<- list(
      stint_id   = n_stint,
      period     = cur$period,
      start_secs = cur$start_secs,
      end_secs   = end_secs,
      home_ids   = lh$ids, away_ids = la$ids,
      home_ok    = (lh$n == 5L), away_ok = (la$n == 5L),
      home_pts   = cur$pts[[home_id]],
      away_pts   = cur$pts[[away_id]]
    )
    cur <<- NULL
  }

  open_stint <- function(period, start_secs) {
    cur <<- list(period = period, start_secs = start_secs, pts = new_pts())
  }

  for (i in seq_len(nrow(pbp))) {
    period <- v_period[i]
    if (is.na(period)) next
    secs <- v_secs[i]
    t    <- v_team[i]

    # open the first stint, or roll over at a period boundary
    if (is.null(cur)) {
      # A stint may only start at the period length when this really IS the
      # first event of that period. If `cur` is NULL part-way through a period
      # something upstream closed a stint without reopening it, and starting
      # from 10:00 again re-covers time already tiled -- silently inflating
      # the period. Start from the current clock instead, and say so.
      first_in_period <- !identical(period, last_period_opened)
      if (first_in_period) {
        open_stint(period, league_period_seconds(league, period))
      } else {
        n_orphan_reopens <- n_orphan_reopens + 1L
        open_stint(period, secs)
      }
      last_period_opened <- period
    } else if (period != cur$period) {
      close_stint(0)                       # previous period ran to 0:00
      open_stint(period, league_period_seconds(league, period))
      last_period_opened <- period
    }

    # ---- substitution ----
    # Close once at the start of a substitution GROUP (snapshotting the
    # five who actually played the stint), apply every event in the
    # group, then reopen once at the end. See the grammar notes above.
    if (v_is_sub[i]) {

      if (grp_start[i]) close_stint(secs)   # <-- snapshot BEFORE any swap

      dir <- v_sub_dir[i]
      if (is.na(dir) || is.na(t) || !t %in% teams) {
        n_unresolved_subs <- n_unresolved_subs + 1L

      } else if (identical(dir, "paired")) {
        # Sanity check: the departing player should be on the floor. A
        # miss here usually means the in/out ids are reversed upstream.
        if (!v_out[i] %in% lineup[[t]]) n_phantom_subs <- n_phantom_subs + 1L
        lineup[[t]] <- union(setdiff(lineup[[t]], v_out[i]), v_in[i])

      } else if (identical(dir, "in")) {
        lineup[[t]] <- union(lineup[[t]], v_in[i])

      } else if (identical(dir, "out")) {
        if (!v_in[i] %in% lineup[[t]]) n_phantom_subs <- n_phantom_subs + 1L
        lineup[[t]] <- setdiff(lineup[[t]], v_in[i])
      }

      if (grp_end[i]) open_stint(period, secs)
      next
    }

    # ---- points ----
    if (is.na(t) || !t %in% teams) next

    is_scoring <- isTRUE(v_scoring[i])
    if (!is_scoring && !is.na(v_text[i]) && grepl("makes", v_text[i], ignore.case = TRUE)) {
      is_scoring <- TRUE
    }
    if (is_scoring) {
      add <- if (!is.na(v_sval[i]) && v_sval[i] > 0) v_sval[i] else infer_points(v_text[i])
      cur$pts[[t]] <- cur$pts[[t]] + add
    }
  }

  close_stint(0)                           # final period runs to 0:00

  # A high unresolved rate means the lineups are frozen at (or near) the
  # starters. That failure is INVISIBLE to lineup_valid, which only checks
  # that five players are present — and five frozen starters are five
  # players. So it has to be an error: returning plausible-looking but
  # wrong lineups is worse than returning nothing.
  n_sub_events <- sum(v_is_sub)
  if (n_sub_events > 0) {
    unresolved_rate <- n_unresolved_subs / n_sub_events
    if (unresolved_rate > MAX_UNRESOLVED_SUB_RATE) {
      stop(sprintf(paste0("Game %s (%s): %d of %d substitution events (%.0f%%) could not ",
                          "be resolved, so lineups would be frozen near the starters. ",
                          "This usually means an unrecognised substitution grammar."),
                   game_id, league, n_unresolved_subs, n_sub_events,
                   100 * unresolved_rate), call. = FALSE)
    }
    if (n_unresolved_subs > 0) {
      warning(sprintf("Game %s: %d of %d substitution(s) could not be resolved.",
                      game_id, n_unresolved_subs, n_sub_events), call. = FALSE)
    }
  }
  if (n_phantom_subs > 0) {
    warning(sprintf("Game %s: %d substitution(s) removed a player who was not on the floor.",
                    game_id, n_phantom_subs), call. = FALSE)
  }
  # An orphan reopen means a stint was closed and never reopened -- a grouping
  # bug, not a data quirk. The guard above stops it inflating the period, but
  # it should still be visible rather than silently absorbed.
  if (n_orphan_reopens > 0) {
    warning(sprintf(paste0("Game %s: %d stint(s) reopened mid-period after an ",
                           "unmatched close. Period totals are protected, but ",
                           "this indicates a substitution-grouping gap."),
                    game_id, n_orphan_reopens), call. = FALSE)
  }
  if (length(stints) == 0) stop("No stints reconstructed for game ", game_id, call. = FALSE)

  # --- 6. Reshape to two rows per stint (one per team) -------------
  one_side <- function(s, side) {
    is_home <- identical(side, "home")
    ids <- if (is_home) s$home_ids else s$away_ids
    tibble::tibble(
      game_id       = game_id,
      team          = if (is_home) home_abbrev else away_abbrev,
      opponent      = if (is_home) away_abbrev else home_abbrev,
      date          = game_date,
      stint_id      = as.integer(s$stint_id),
      period        = as.integer(s$period),
      clock_start   = seconds_to_clock(s$start_secs),
      clock_end     = seconds_to_clock(s$end_secs),
      player_id_1   = ids[1], player_id_2 = ids[2], player_id_3 = ids[3],
      player_id_4   = ids[4], player_id_5 = ids[5],
      team_pts      = as.integer(if (is_home) s$home_pts else s$away_pts),
      opp_pts       = as.integer(if (is_home) s$away_pts else s$home_pts),
      stint_seconds = max(0, s$start_secs - s$end_secs),
      lineup_valid  = if (is_home) s$home_ok else s$away_ok
    )
  }

  out <- dplyr::bind_rows(
    lapply(stints, one_side, side = "home"),
    lapply(stints, one_side, side = "away")
  )
  out <- out[order(out$stint_id, out$team), ]

  # --- 7. Write ---------------------------------------------------
  if (isTRUE(write)) {
    path <- if (is.null(out_dir)) {
      stint_cache_path(game_id, league)          # see paths.R
    } else {
      file.path(out_dir, paste0(game_id, "_lineups.csv"))
    }
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(out, path)
    message("Wrote ", path, "  (", nrow(out), " rows)")
  }

  out
}

# box$starter arrives as logical, but guard against 0/1 or "true"
isTRUE_vec <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.numeric(x)) return(!is.na(x) & x == 1)
  !is.na(x) & tolower(as.character(x)) %in% c("true", "t", "yes")
}
