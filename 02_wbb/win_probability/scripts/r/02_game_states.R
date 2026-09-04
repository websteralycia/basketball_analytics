## ============================================================
## 02_game_states.R — Phase 2: fine-grained game-state moments
## ------------------------------------------------------------
## A layer ON TOP of Phase 1. It re-detects nothing: possessions come
## from classify_possessions(), and the events inside a possession are
## reached by slicing order_pbp(pbp) with the possession's
## start_idx/end_idx.
##
## WHY PURE AND NON-PURE STATES ARE SEPARATED.
##
## inpredictable does not fit a model per game state. From their
## writeup: situations that "don't qualify as 'pure' possession states,
## such as when a team has two free throws to shoot off of a personal
## foul" are, "rather than building separate regression models ...
## derived from the base 'pure' possession model with some simple
## assumptions."
##
## So this layer emits BOTH kinds of moment and marks them:
##
##   is_pure = TRUE   a team has the ball, live. These become the
##                    Phase 4 training rows and nothing else does.
##   is_pure = FALSE  a team is at the line with k shots left. Enumerated
##                    here so Phase 6 can derive them from the base
##                    model; they must NOT be trained on.
##
## Filtering on is_pure is what keeps the base model "pure". Training on
## everything would fit free-throw states as if they were possessions and
## quietly change what the base model means.
##
## THE FREE-THROW TRIP COUNTER IS NOT RELIABLE. ESPN's WBB feed uses two
## text conventions and they vary BY GAME, not within one: 401851531,
## 401865066 and 401856552 carry "free throw 1 of 2" on 100% of attempts;
## 401817374 and 401827253 carry "made Free Throw." on 100%, with no
## counter at all. So shot index and trip length are derived structurally
## — consecutive attempts by one team with no live-ball event between
## them — and the text counter is used only to CHECK that derivation
## where it happens to exist. See check_ft_trips().
##
## Depends on: 01_possession_state.R
## ============================================================

source(file.path(dirname(sys.frame(1)$ofile %||% "."), "01_possession_state.R"))

# Pure-possession start states, normalised from the walker's
# `start_reason`. inpredictable's examples of game state are "has
# possession, shooting two free throws, after missed shot" — so how a
# possession began is a covariate OF a pure state, not a separate state.
START_STATE <- c(
  made_fg      = "after_made_fg",
  made_ft      = "after_made_ft",
  dreb         = "after_dreb",        # i.e. after a missed shot
  steal        = "after_steal",       # live-ball turnover
  turnover     = "after_turnover",    # dead-ball turnover
  period_start = "period_start"
)

#' Free-throw attempts, grouped into trips
#'
#' A trip is a run of free throws by one team with no live-ball event
#' (shot, rebound, turnover) between them. Substitutions and fouls sit
#' inside trips routinely — 401817374 has four subs between two attempts
#' of one trip — so the grouping must skip them rather than treat them
#' as separators.
#'
#' @return tibble: idx (row in the ordered pbp), team_id, trip_id,
#'   shot_num, trip_size, made, plus txt_shot/txt_of where the feed
#'   happened to state them.
ft_trips <- function(ord) {
  blob <- paste(ord$type_text, ord$text)
  is_ft  <- grepl("free throw", blob, ignore.case = TRUE)
  if (!any(is_ft)) return(tibble::tibble())

  # Live-ball events that must end a trip.
  #
  # A DEAD-BALL / TEAM rebound must NOT count. ESPN emits one between the
  # attempts of almost every multi-shot trip ("Iowa Deadball Team
  # Rebound." at 2:39 of period 3 in 401856552), and treating it as live
  # splits one trip into a run of ft_1_of_1s. Note the feed spells it both
  # "Deadball" and "Dead Ball" — matching on the pasted type_text + text
  # blob catches both, which is also why the possession walker's
  # "dead ball" pattern is not the bug it looks like.
  is_dead_reb <- grepl("dead ?ball|team rebound", blob, ignore.case = TRUE)
  is_foul     <- grepl("foul", blob, ignore.case = TRUE) & !is_ft
  is_live <- (grepl("shot|jumper|layup|dunk", blob, ignore.case = TRUE) |
              (grepl("rebound", blob, ignore.case = TRUE) & !is_dead_reb) |
              grepl("turnover|traveling|double dribble|palming|backcourt|shot clock",
                    blob, ignore.case = TRUE)) & !is_ft

  idx  <- which(is_ft)
  team <- as.character(ord$team_id)
  who  <- if ("athlete_id_1" %in% names(ord)) as.character(ord$athlete_id_1) else rep(NA_character_, nrow(ord))
  low  <- tolower(as.character(ord$text))

  # A new trip starts on a live-ball event, a change of team, or a change
  # of SHOOTER. The shooter test is what separates back-to-back trips by
  # one team: at 1:01 of period 4 in 401851531, Sifa Joyeuse shoots 1-and-2,
  # a foul is called, and Norah Moo shoots 1-and-2 — same team, no live
  # ball between, but two trips. Without it they merge into a phantom
  # 4-shot trip and every shot index after the first is wrong.
  #
  # An intervening FOUL is the fourth boundary, and it catches what the
  # other three miss: one shooter fouled twice with no live ball between.
  # At 0:22 of the first OT in 401856552, Chazadi Wright shoots 1-and-2,
  # is fouled again at 0:18, and shoots 1-and-2 again — same shooter, same
  # team, only a deadball rebound, a foul and a timeout in between. A new
  # foul is what awards a new trip.
  #
  # A clock-change rule was tried here and REJECTED. It looks equivalent
  # (the clock is stopped during a trip) but the feed ticks it between
  # attempts in some games: it fixed 401856552 and broke 401865066,
  # 100% -> 70%. Measured over the three games that state "N of M",
  # the foul rule is 100% on all three where clock is 100/70/100 and a
  # 2-second clock tolerance is 100/90/100.
  trip_id <- integer(length(idx))
  t <- 0L
  for (k in seq_along(idx)) {
    if (k == 1L) { t <- 1L } else {
      gap_has_live <- any(is_live[(idx[k - 1L] + 1L):(idx[k] - 1L)])
      same_team    <- !is.na(team[idx[k]]) && !is.na(team[idx[k - 1L]]) &&
                       team[idx[k]] == team[idx[k - 1L]]
      same_shooter <- is.na(who[idx[k]]) || is.na(who[idx[k - 1L]]) ||
                       who[idx[k]] == who[idx[k - 1L]]
      gap_has_foul <- any(is_foul[(idx[k - 1L] + 1L):(idx[k] - 1L)])
      if (gap_has_live || !same_team || !same_shooter || gap_has_foul) t <- t + 1L
    }
    trip_id[k] <- t
  }

  has_of <- grepl("free throw\\s+\\d+\\s+of\\s+\\d+", low[idx])
  txt_shot <- suppressWarnings(as.integer(ifelse(has_of,
    sub(".*free throw\\s+(\\d+)\\s+of\\s+(\\d+).*", "\\1", low[idx]), NA)))
  txt_of <- suppressWarnings(as.integer(ifelse(has_of,
    sub(".*free throw\\s+(\\d+)\\s+of\\s+(\\d+).*", "\\2", low[idx]), NA)))

  tibble::tibble(
    idx = idx, team_id = team[idx], trip_id = trip_id,
    made = is_made(ord)[idx], txt_shot = txt_shot, txt_of = txt_of
  ) |>
    dplyr::group_by(trip_id) |>
    dplyr::mutate(shot_num = dplyr::row_number(), trip_size = dplyr::n()) |>
    dplyr::ungroup()
}

#' Agreement between the derived trip structure and the feed's counter
#'
#' Only meaningful on games that carry "N of M". Returns the share of
#' stated attempts the derivation reproduces — a regression in the
#' grouping shows up here as a drop below 1.
check_ft_trips <- function(trips) {
  stated <- trips[!is.na(trips$txt_shot) & !is.na(trips$txt_of), ]
  if (nrow(stated) == 0) return(NA_real_)
  mean(stated$shot_num == stated$txt_shot & stated$trip_size == stated$txt_of)
}

#' Abbreviation for each team_id the feed's EVENTS actually use
#'
#' The play-by-play and the box score do not always agree on a team's id.
#' In 401494314 (UL Monroe vs Louisiana College, 2022-11-07) the schedule
#' and box call the away team 2347 while every one of its event rows is
#' stamped 14314 — the id ESPN issued when the school became Louisiana
#' Christian. A lookup keyed only on home_team_id/away_team_id returns NA
#' for those rows, and since only the free-throw path is keyed on team_id
#' (Phase 1 works in abbreviations), the damage shows up as free-throw
#' states with no ball team — 17 unlabelled rows that stopped the Phase 4
#' build for the whole season.
#'
#' Resolved by ELIMINATION rather than by an id alias table, which would
#' need an entry per rename per season. A game has two teams: if the
#' events carry exactly one id that is neither the home nor the away id,
#' and the other side is present and accounted for, the odd id can only
#' be the missing side. Anything more ambiguous is left NA on purpose, so
#' the Phase 4 label guard still fails loudly rather than guessing.
team_abbrev_map <- function(ord) {
  hid <- as.character(ord$home_team_id[1]);    aid <- as.character(ord$away_team_id[1])
  hab <- as.character(ord$home_team_abbrev[1]); aab <- as.character(ord$away_team_abbrev[1])
  map <- stats::setNames(c(hab, aab), c(hid, aid))

  seen <- unique(as.character(ord$team_id))
  seen <- seen[!is.na(seen)]
  unknown <- setdiff(seen, c(hid, aid))
  known   <- intersect(seen, c(hid, aid))
  if (length(unknown) == 1L && length(known) == 1L) {
    map[unknown] <- if (identical(known, hid)) aab else hab
  }
  map
}

#' Every game-state moment in one game, pure and non-pure
#'
#' @return tibble ordered by game time. One row per pure possession start
#'   plus one row per free-throw attempt. Score columns are the state
#'   BEFORE the moment resolves, so nothing leaks its own outcome.
game_states <- function(pbp, league = "wbb") {
  poss <- possession_state(pbp, league)
  if (nrow(poss) == 0) return(poss)
  ord  <- chr_ids(order_pbp(pbp))

  home_ab <- as.character(ord$home_team_abbrev[1])
  abbrev  <- team_abbrev_map(ord)

  pure <- poss |>
    dplyr::transmute(
      game_id, poss_id, period, is_ot,
      secs_left_period, secs_left_game,
      state_type = "possession",
      state      = unname(START_STATE[start_reason]),
      is_pure    = TRUE,
      ball_team  = off_team,
      ball_is_home = off_is_home,
      home_score, away_score, home_margin,
      ball_margin = off_margin,
      ft_shot_num = NA_integer_, ft_trip_size = NA_integer_,
      ft_shots_remaining = NA_integer_,
      idx = start_idx, home_win
    )

  trips <- ft_trips(ord)
  if (nrow(trips) == 0) return(dplyr::arrange(pure, dplyr::desc(secs_left_game)))

  # Score BEFORE the attempt. ESPN's running score is the score AFTER a
  # row, so the shot's own point would otherwise be in its own state.
  hs <- suppressWarnings(as.numeric(ord$home_score))
  as_ <- suppressWarnings(as.numeric(ord$away_score))
  ffill <- function(x) { x[is.na(x)] <- 0; cummax(seq_along(x)) -> i; x }
  prev <- function(v, i) ifelse(i <= 1L, 0, v[pmax(1L, i - 1L)])

  secs <- round(clock_to_seconds(ord$clock_display_value))
  # Which possession each attempt sits inside, for the link back.
  owner <- vapply(trips$idx, function(i) {
    k <- which(poss$start_idx <= i & poss$end_idx >= i)
    if (length(k)) poss$poss_id[k[1]] else NA_integer_
  }, integer(1))

  ft_team <- unname(abbrev[trips$team_id])
  per     <- as.integer(ord$period_number[trips$idx])
  hsv     <- prev(hs,  trips$idx)
  asv     <- prev(as_, trips$idx)

  ft <- tibble::tibble(
    game_id = poss$game_id[1], poss_id = owner, period = per,
    is_ot = per > league_cfg(league)$reg_periods,
    secs_left_period = as.integer(secs[trips$idx]),
    secs_left_game = as.integer(seconds_remaining_game(league, per, secs[trips$idx])),
    state_type = "free_throw",
    state      = paste0("ft_", trips$shot_num, "_of_", trips$trip_size),
    is_pure    = FALSE,
    ball_team  = ft_team,
    ball_is_home = ft_team == home_ab,
    home_score = as.integer(hsv), away_score = as.integer(asv),
    home_margin = as.integer(hsv - asv),
    ball_margin = as.integer(ifelse(ft_team == home_ab, hsv - asv, asv - hsv)),
    ft_shot_num = trips$shot_num, ft_trip_size = trips$trip_size,
    ft_shots_remaining = trips$trip_size - trips$shot_num + 1L,
    idx = trips$idx, home_win = poss$home_win[1]
  )

  dplyr::bind_rows(pure, ft) |>
    dplyr::arrange(period, dplyr::desc(secs_left_period), idx)
}

#' Fetch, build and cache one game's state moments
game_states_game <- function(game_id, league = "wbb", write = TRUE) {
  pbp <- league_cfg(league)$pbp_fn(as.character(game_id)[1])
  if (!is.data.frame(pbp)) pbp <- as.data.frame(pbp)
  out <- game_states(pbp, league)
  if (isTRUE(write)) {
    path <- file.path(WINPROB_ROOT, "data", "tidy", tolower(league),
                      paste0(as.character(game_id)[1], "_game_states.csv"))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(out, path)
    message("Wrote ", path, "  (", nrow(out), " state moments)")
  }
  out
}
