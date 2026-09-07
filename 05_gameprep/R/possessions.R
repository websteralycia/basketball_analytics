## ============================================================
## possessions.R — possession reconstruction and context classification
## ------------------------------------------------------------
## The possession state machine is lifted from
## 02_wbb/scripts/r/7.22_02_build_stints.R, which is already validated
## against the 0.44 box estimate (mean |diff| 4.1% across 203 games).
## Reused rather than rewritten so the project has ONE definition of a
## possession; two would drift and produce numbers that disagree.
##
## Fixed on the way in:
##   * the clock parser (ESPN drops the colon under a minute)
##   * event ordering (see order_pbp() in pbp_prep.R)
##
## Deliberately NOT carried over: points-per-possession ratings and the
## RAPM direction split. This layer emits raw possessions; consumers
## aggregate.
## ============================================================

# A possession counts as transition when it begins on a live-ball change
# of possession AND the offence gets a shot up within this many seconds.
TRANSITION_SECONDS <- 7

# Possession start reasons that leave the defence unset. An inbound after
# a made basket is included: "transition" in scouting usage means early
# offence before the defence is set, not strictly a live-ball rebound.
LIVE_BALL_STARTS <- c("dreb", "steal", "made_fg", "made_ft")

#' Reconstruct possessions for one game and classify each by context
#'
#' Pure function over a play-by-play frame — no network, so it is
#' testable offline and reusable on cached play-by-play.
#'
#' A possession ends on: a made field goal, a made final free throw, a
#' defensive rebound, a turnover, or the end of a period.
#'
#' `context` is mutually exclusive, with second chance taking precedence
#' over transition (a possession extended by an offensive rebound is no
#' longer early offence, whatever it started as). That precedence is what
#' lets `freq` sum to 1 within the dimension.
#'
#' @param pbp    Play-by-play for ONE game.
#' @param league One of supported_leagues().
#' @return tibble, one row per possession: game_id, period, poss_id,
#'   off_team, def_team, clock_start, clock_end, seconds, points,
#'   context, start_reason, end_reason, had_oreb
classify_possessions <- function(pbp, league) {
  cfg <- league_cfg(league)
  pbp <- chr_ids(order_pbp(pbp))

  game_id <- as.character(pbp$game_id[1])
  home_id <- as.character(pbp$home_team_id[1])
  away_id <- as.character(pbp$away_team_id[1])
  abbrev  <- setNames(c(as.character(pbp$home_team_abbrev[1]),
                        as.character(pbp$away_team_abbrev[1])),
                      c(home_id, away_id))
  teams <- c(home_id, away_id)
  other <- function(t) if (identical(t, teams[1])) teams[2] else teams[1]

  n     <- nrow(pbp)
  v_per <- suppressWarnings(as.integer(pbp$period_number))
  v_sec <- round(clock_to_seconds(pbp$clock_display_value))
  v_tm  <- pbp$team_id
  v_txt <- as.character(pbp$text)
  v_typ <- if ("type_text" %in% names(pbp)) as.character(pbp$type_text) else rep(NA_character_, n)
  v_made <- is_made(pbp)
  v_sval <- if ("score_value" %in% names(pbp)) suppressWarnings(as.numeric(pbp$score_value)) else rep(NA_real_, n)

  blob   <- paste(v_typ, v_txt)
  v_ft   <- grepl("free throw", blob, ignore.case = TRUE)
  v_reb  <- grepl("rebound",    blob, ignore.case = TRUE)
  v_oreb <- grepl("offensive rebound", blob, ignore.case = TRUE)
  v_dreb <- v_reb & !v_oreb & !grepl("dead ball", blob, ignore.case = TRUE)
  v_stl  <- grepl("steal", blob, ignore.case = TRUE)
  # ESPN labels some violations without the word "turnover"
  v_tov  <- grepl("turnover|traveling|double dribble|palming|backcourt|shot clock",
                  blob, ignore.case = TRUE)
  v_shot <- grepl("shot|jumper|layup|dunk", blob, ignore.case = TRUE) & !v_reb & !v_ft
  # Events that actually indicate who has the ball. Substitutions, fouls
  # and timeouts carry a team_id but say nothing about possession — a
  # period opening "sub, sub, foul, free throw" would otherwise hand the
  # possession to whichever team happened to substitute first, and the
  # free throws that follow get credited to nobody.
  v_ball <- v_shot | v_reb | v_tov | v_ft

  # Last free throw of a trip. "free throw 2 of 2" is the reliable form,
  # but many feeds omit it entirely — WBB game 401827253 says only
  # "made Free Throw." — so fall back to a lookahead: a free throw is the
  # last of its trip when the next event is not another free throw by the
  # same team. Without the fallback, possessions never close on made free
  # throws in those games.
  lower_txt <- tolower(v_txt)
  has_of <- grepl("free throw\\s+\\d+\\s+of\\s+\\d+", lower_txt)
  ft_x <- suppressWarnings(as.integer(sub(".*free throw\\s+(\\d+)\\s+of\\s+(\\d+).*", "\\1", lower_txt)))
  ft_y <- suppressWarnings(as.integer(sub(".*free throw\\s+(\\d+)\\s+of\\s+(\\d+).*", "\\2", lower_txt)))

  # The lookahead must skip to the next BALL event, not the next row:
  # free throws within one trip are routinely separated by substitutions
  # (seen at 5:05 in WBB game 401817374, four subs between two attempts).
  # Looking only at the adjacent row declares the trip over early, closes
  # the possession, and orphans the remaining attempts.
  ball_idx <- which(v_ball)
  nxt_ball <- rep(NA_integer_, n)
  if (length(ball_idx) > 0) {
    pos <- findInterval(seq_len(n), ball_idx) + 1L
    nxt_ball <- ifelse(pos <= length(ball_idx), ball_idx[pmin(pos, length(ball_idx))], NA_integer_)
  }
  next_ft_same_team <- !is.na(nxt_ball) & v_ft[nxt_ball] &
                       !is.na(v_tm[nxt_ball]) & v_tm[nxt_ball] == v_tm
  v_last_ft <- ifelse(has_of,
                      !is.na(ft_x) & !is.na(ft_y) & ft_x == ft_y,
                      !next_ft_same_team)
  v_last_ft <- v_ft & !is.na(v_last_ft) & v_last_ft

  # Possession records accumulate into preallocated ATOMIC VECTORS, not a
  # list of one-row tibbles. Building a tibble per possession measured at
  # 93% of this function's runtime (tibble_quos, plus glue_data for its
  # error machinery) against ~5% for the actual basketball logic — 0.63
  # s/game, which is 9.4 hours over a 54,000-game training build. One
  # tibble at the end is ~10x faster and byte-identical.
  #
  # n is a safe upper bound: a game cannot have more possessions than
  # play-by-play events.
  cap <- max(n, 1L)
  o_period <- integer(cap); o_off <- character(cap); o_def <- character(cap)
  o_cstart <- character(cap); o_cend <- character(cap); o_secs <- numeric(cap)
  o_pts <- integer(cap); o_ctx <- character(cap); o_sreason <- character(cap)
  o_ereason <- character(cap); o_oreb <- logical(cap)
  o_sidx <- integer(cap); o_eidx <- integer(cap)

  poss_id <- 0L
  cur <- NULL
  # Team owed a possible and-1 free throw. A made field goal closes the
  # possession and hands the ball over, so a bonus free throw arrives
  # when the scoring team no longer owns the open possession. Without
  # this its point is silently dropped and team totals undercount.
  pending_and1 <- NA_character_
  uncredited <- 0
  uncredited_txt <- list()

  open_poss <- function(team, period, secs, reason, idx = NA_integer_) {
    cur <<- list(off = team, period = period, start = secs,
                 reason = reason, pts = 0, oreb = FALSE,
                 first_shot_secs = NA_real_, start_idx = as.integer(idx))
  }

  close_poss <- function(end_secs, reason, idx = NA_integer_) {
    if (is.null(cur)) return(invisible(NULL))
    end_secs <- if (is.na(end_secs)) cur$start else end_secs
    # transition: live-ball start AND a shot up inside the window
    ttf <- if (is.na(cur$first_shot_secs)) NA_real_ else cur$start - cur$first_shot_secs
    ctx <- if (isTRUE(cur$oreb)) {
      "second_chance"
    } else if (cur$reason %in% LIVE_BALL_STARTS &&
               !is.na(ttf) && ttf <= TRANSITION_SECONDS) {
      "transition"
    } else {
      "halfcourt"
    }
    poss_id <<- poss_id + 1L
    i0 <- poss_id
    o_period[i0]  <<- cur$period
    o_off[i0]     <<- unname(abbrev[cur$off])
    o_def[i0]     <<- unname(abbrev[other(cur$off)])
    o_cstart[i0]  <<- seconds_to_clock(cur$start)
    o_cend[i0]    <<- seconds_to_clock(end_secs)
    o_secs[i0]    <<- max(0, cur$start - end_secs)
    o_pts[i0]     <<- as.integer(cur$pts)
    o_ctx[i0]     <<- ctx
    o_sreason[i0] <<- cur$reason
    o_ereason[i0] <<- reason
    o_oreb[i0]    <<- isTRUE(cur$oreb)
    # Row indices into order_pbp(pbp), inclusive. The only safe key for
    # joining anything else off the feed onto a possession: clock values
    # tie constantly (a bucket and the subs after it share one), so a
    # period+clock join silently mismatches. Consumers that need score,
    # or the events INSIDE a possession, index with these.
    o_sidx[i0] <<- as.integer(cur$start_idx)
    o_eidx[i0] <<- as.integer(idx)
    cur <<- NULL
  }

  cur_period <- NA_integer_
  for (i in seq_len(n)) {
    per <- v_per[i]; if (is.na(per)) next
    secs <- v_sec[i]
    t    <- v_tm[i]

    if (!is.na(cur_period) && per != cur_period) {
      close_poss(0, "period_end", i - 1L)
      cur_period <- per
    } else if (is.na(cur_period)) {
      cur_period <- per
    }

    if (is.na(t) || !t %in% teams) next

    # The first team to genuinely touch the ball owns the possession.
    # Skip until a ball event arrives — see v_ball above.
    if (is.null(cur)) {
      if (!v_ball[i]) next
      open_poss(t, per, secs, "period_start", i)
    }

    # Points accrue to the offence of the open possession. The exception
    # is an and-1: the made basket already closed that possession, so the
    # bonus free throw is credited back to it rather than lost.
    if (v_made[i]) {
      add <- if (!is.na(v_sval[i]) && v_sval[i] > 0) v_sval[i] else if (v_ft[i]) 1 else 2
      if (identical(t, cur$off)) {
        cur$pts <- cur$pts + add
      } else if (v_ft[i] && identical(t, pending_and1) && poss_id > 0L) {
        o_pts[poss_id] <- o_pts[poss_id] + as.integer(add)
      } else {
        # Scored by a team that does not own the open possession and is
        # not owed an and-1. Recorded rather than silently dropped so
        # data-quality checks can see it.
        uncredited <- uncredited + add
        uncredited_txt[[length(uncredited_txt) + 1L]] <- v_txt[i]
      }
    }
    # The and-1 window survives the foul call that sits between the made
    # basket and the bonus free throw — clearing it on any intervening
    # event (as fouls are) drops the free throw's point entirely. Only a
    # resumption of live play closes the window.
    if (v_shot[i] || v_reb[i] || v_tov[i]) pending_and1 <- NA_character_

    if (v_shot[i] && identical(t, cur$off) && is.na(cur$first_shot_secs)) {
      cur$first_shot_secs <- secs
    }

    if (v_oreb[i]) {
      if (identical(t, cur$off)) cur$oreb <- TRUE
      next
    }

    # A defensive rebound hands the ball over — unless the rebounding
    # team already has it.
    #
    # That happens on a MISSED and-1, the mirror of the made-and-1 case
    # in the free-throw branch below. The basket has already transferred
    # possession; the bonus free throw is then missed, and ESPN labels
    # the carom a defensive rebound because it is defensive relative to
    # the SHOOTER. Without this guard the rebound closes the rebounding
    # team's own possession and opens them a second one, and the walker
    # is left with the wrong team on offence until something resyncs it —
    # so the error propagates past the rebound rather than staying local.
    # Seen at 4:19 in WBB 401851531.
    if (v_dreb[i]) {
      if (!identical(t, cur$off)) {
        close_poss(secs, "dreb", i)
        open_poss(t, per, secs, "dreb", i)
      }
      next
    }

    if (v_tov[i]) {
      if (is.null(cur)) open_poss(t, per, secs, "period_start", i)
      close_poss(secs, "turnover", i)
      open_poss(other(t), per, secs, if (any(v_stl[max(1, i - 1):min(n, i + 1)])) "steal" else "turnover", i)
      next
    }

    if (v_made[i] && !v_ft[i]) {
      close_poss(secs, "made_fg", i)
      pending_and1 <- t              # watch for the bonus free throw
      open_poss(other(t), per, secs, "made_fg", i)
      next
    }

    # A made final free throw ends the possession — the other team
    # inbounds, same as after a made basket. But ONLY when the shooter's
    # team is the one with the ball.
    #
    # The and-1 is why the team check is not optional. A made basket has
    # already closed the scorer's possession and handed the ball over, so
    # the bonus free throw is taken while the OPPONENT owns the open
    # possession. Without `identical(t, cur$off)` that free throw closes
    # the opponent's possession and opens a second one for the same team:
    # one trip down the floor becomes a 0-second phantom plus the real
    # possession. Measured at 2.3% of all possessions over five games
    # (787 -> 769), with total points unchanged — the phantoms carried
    # none. Seen at 6:45 in WBB 401851531, where USU's and-1 splits GCU's
    # possession in half before GCU has touched the ball.
    if (v_made[i] && v_last_ft[i] && identical(t, cur$off)) {
      close_poss(secs, "made_ft", i)
      open_poss(other(t), per, secs, "made_ft", i)
      next
    }
  }
  close_poss(0, "period_end", n)

  k <- seq_len(poss_id)
  res <- tibble::tibble(
    game_id = game_id, period = o_period[k], poss_id = k,
    off_team = o_off[k], def_team = o_def[k],
    clock_start = o_cstart[k], clock_end = o_cend[k], seconds = o_secs[k],
    points = o_pts[k], context = o_ctx[k],
    start_reason = o_sreason[k], end_reason = o_ereason[k],
    had_oreb = o_oreb[k], start_idx = o_sidx[k], end_idx = o_eidx[k]
  )
  attr(res, "uncredited_points") <- uncredited
  attr(res, "uncredited_text")   <- unlist(uncredited_txt)
  res
}

#' Possessions for one game, fetched and cached
#'
#' @inheritParams get_lineup_stints
get_possessions <- function(game_id,
                            league  = c("wnba", "wbb", "mbb", "nba"),
                            write   = TRUE,
                            out_dir = NULL) {
  league  <- match.arg(league)
  cfg     <- league_cfg(league)
  game_id <- as.character(game_id)[1]

  pbp <- .as_df(cfg$pbp_fn(game_id), "play-by-play")
  if (nrow(pbp) == 0) stop("No play-by-play returned for game ", game_id, call. = FALSE)

  out <- classify_possessions(pbp, league)

  if (isTRUE(write)) {
    path <- if (is.null(out_dir)) {
      file.path(tidy_games_dir(league), paste0(game_id, "_possessions.csv"))
    } else {
      file.path(out_dir, paste0(game_id, "_possessions.csv"))
    }
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(out, path)
    message("Wrote ", path, "  (", nrow(out), " possessions)")
  }
  out
}

#' The context levels, in display order
context_levels <- function() c("transition", "second_chance", "halfcourt")
