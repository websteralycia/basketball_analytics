## ============================================================
## 06_winprob_calc.R — Phase 6: the calculator
## ------------------------------------------------------------
## Turns a game situation a person can describe — quarter, clock, score,
## who has the ball, who is playing — into a win probability, using the
## Phase 5 fit.
##
## WHAT THIS ADDS OVER inpredictable's CALCULATOR. Theirs takes Quarter,
## Time Remaining, Score Difference and Possession, and nothing else: it is
## team-agnostic because supplying a matchup would mean supplying a Vegas
## line. Ours takes an optional MATCHUP, because Phase 3 built the line
## substitute Vegas doesn't provide for NCAAW. Leave the spread at 0 and
## this reproduces their calculator exactly.
##
## A SPREAD OF 0 IS NOT "NO INFORMATION". It asserts the two teams are
## expected to finish level AT THIS VENUE. A typical game has the home team
## favoured by about 6, so 0 is a pick'em — an uncommon situation, not a
## neutral default. The spread carries home court, so two equally strong
## teams with one at home sit near +6, not 0. Hence `venue` below.
##
## FREE THROWS ARE DERIVED, NOT FITTED — Phase 5 trained on pure
## possessions only. inpredictable are explicit that non-pure states are
## "rather than building separate regression models ... derived from the
## base 'pure' possession model with some simple assumptions". Ours are
## below, and every constant in them is MEASURED, not guessed.
##
## Depends on: 05_winprob_model.R, and the cached fit + ratings table.
## ============================================================

suppressMessages({
  library(dplyr)
  library(locfit)
})

## ---- measured constants --------------------------------------------

# Free-throw conversion. Box scores 2022-2026: 673,250 of 948,952 = 0.7095.
# Play-by-play 2026 agrees at 0.7108 (142,433 of 200,389).
FT_MAKE <- 0.710

# P(shooting team keeps the ball | missed FINAL free throw). Measured on
# 7,322 missed final attempts across 1,500 games of 2026: 76.1% defensive
# rebound, 19.9% offensive, 2.6% dead-ball. Offensive rebounds go to the
# shooter's team in 1,457 of 1,459 cases, so the attribution is sound.
FT_OREB <- 0.20

# Default pace when a matchup is given without one. 2022-2026 mean 70.58,
# median 70.08, SD 6.24, 10th-90th percentile 63.1-78.8. Pace matters: a
# 10-per-100 rating edge is 6.5 points at pace 65 and 8.0 at pace 80, so a
# real pace estimate is worth supplying for a specific matchup.
DEFAULT_PACE <- 70.6

# NCAA women's basketball: four 10-minute quarters, 5-minute overtimes.
# NOT the NBA's 12-minute quarters — inpredictable's clock does not transfer.
QUARTER_SECS <- 600L
OT_SECS      <- 300L
REG_SECS     <- 4L * QUARTER_SECS

## ---- loading the fitted model --------------------------------------

#' Load the cached Phase 5 fit
load_winprob_fit <- function(path = file.path(WINPROB_ROOT,
                                              "data/tidy/wbb/winprob_fit.rds")) {
  if (!file.exists(path))
    stop("No fitted model at ", path, " — run scripts/r/run_05_fit.R", call. = FALSE)
  readRDS(path)
}

## ---- clock ----------------------------------------------------------

#' Seconds left in the game from a period and a clock reading
#'
#' @param period 1-4 for quarters, 5+ for overtimes
#' @param clock  seconds remaining IN THE PERIOD
#'
#' Overtime returns only the time left in the current OT: the game can end
#' there, so from the model's point of view that is all the time there is.
#' This is where inpredictable admit to "a bit of a cheat" — they map OT
#' onto fourth-quarter situations. We do not, because NCAAW overtime is 5
#' minutes against a 10-minute quarter, so the cheat would be twice as
#' wrong here as it is for them.
secs_left_from_clock <- function(period, clock) {
  period <- as.integer(period)
  ifelse(period <= 4L,
         (4L - period) * QUARTER_SECS + clock,
         clock)
}

## ---- the matchup ----------------------------------------------------

#' Team id -> display name, for the picker
#'
#' The ratings are keyed on ESPN `team_id`; nobody types a team id. Built
#' from the box scores of the most recent season, which covers every rated
#' team (663 of 663 on the 2026 table).
team_names <- function(season = 2026) {
  suppressWarnings(wehoop::load_wbb_team_box(seasons = season)) |>
    transmute(team_id = as.character(team_id),
              display = as.character(team_display_name),
              short   = as.character(team_short_display_name)) |>
    distinct(team_id, .keep_all = TRUE) |>
    arrange(display)
}

#' Ratings joined to names, most recent date — what the picker shows
rating_board <- function(ratings, names_tbl = team_names()) {
  latest <- ratings[ratings$date == max(ratings$date), ]
  latest |>
    mutate(team = as.character(team)) |>
    left_join(names_tbl, by = c("team" = "team_id")) |>
    filter(!is.na(display)) |>
    arrange(desc(rating))
}

#' Expected point spread for a matchup, from the cached ratings
#'
#' @param venue "home" (team1 at home), "away" (team1 on the road), or
#'   "neutral". This is not cosmetic — the spread carries home court.
#' @param as_of latest rating date at or before this; defaults to the most
#'   recent in the table.
#' @return points, positive when `team1` is favoured
matchup_spread <- function(ratings, team1, team2, venue = c("home", "away", "neutral"),
                           pace = DEFAULT_PACE, as_of = NULL) {
  venue <- match.arg(venue)
  if (is.null(as_of)) as_of <- max(ratings$date)
  r <- ratings[ratings$date <= as_of, ]
  r <- r[r$date == max(r$date), ]

  get <- function(tm) {
    v <- r$rating[as.character(r$team) == as.character(tm)]
    if (!length(v)) stop("team not in ratings: ", tm, call. = FALSE)
    v[1]
  }
  h_term <- r$home_term[1]; n_term <- r$neutral_term[1]

  # Orient to team1, then add whichever venue term applies. A neutral site
  # is not a neutral game — the nominal home team still wins there — which
  # is why Phase 3 fits two separate terms.
  diff <- get(team1) - get(team2)
  edge <- switch(venue, home = h_term, away = -h_term, neutral = 0)
  (diff + edge) * pace / 100
}

## ---- the model surface ----------------------------------------------

#' Win probability for the team WITH THE BALL, pure possession state
#'
#' The raw model. Everything else in this file is expressed in terms of it.
wp_pure <- function(fit, secs_left, ball_margin, spread_ball, eps = 1e-6) {
  nd <- data.frame(t = sqrt(pmax(secs_left, 0)),
                   ball_margin = ball_margin, spread_ball = spread_ball)
  p <- stats::predict(fit, newdata = nd)
  pmin(pmax(p, eps), 1 - eps)
}

#' Win probability for a team WITHOUT the ball
#'
#' Not a separate model — the same surface, viewed from the other bench.
#' Flip the margin and the spread, take the complement.
wp_no_ball <- function(fit, secs_left, margin, spread, eps = 1e-6) {
  1 - wp_pure(fit, secs_left, -margin, -spread, eps)
}

#' Win probability at a free-throw state, DERIVED from the pure model
#'
#' @param shots_remaining attempts still to be taken in this trip, 1-3
#' @param margin,spread from the SHOOTING team's perspective
#'
#' The derivation, and its simple assumptions, stated plainly:
#'   - each attempt is made with probability FT_MAKE, independently;
#'   - free throws take no clock time;
#'   - after the last attempt is MADE the opponent inbounds and has the ball;
#'   - after the last attempt is MISSED the shooting team retains with
#'     probability FT_OREB, otherwise the opponent has it.
#'
#' No 1-and-1 case: NCAA women's basketball has been on quarters with a
#' two-shot bonus since 2015-16, so trips are 1, 2 or 3 attempts and the
#' front end of a one-and-one does not arise.
wp_free_throw <- function(fit, secs_left, margin, spread, shots_remaining,
                          ft_make = FT_MAKE, ft_oreb = FT_OREB) {
  stopifnot(shots_remaining >= 1)
  if (shots_remaining > 1) {
    return(
      ft_make       * wp_free_throw(fit, secs_left, margin + 1, spread,
                                    shots_remaining - 1L, ft_make, ft_oreb) +
      (1 - ft_make) * wp_free_throw(fit, secs_left, margin, spread,
                                    shots_remaining - 1L, ft_make, ft_oreb))
  }
  made_branch   <- wp_no_ball(fit, secs_left, margin + 1, spread)
  missed_branch <- ft_oreb       * wp_pure(fit, secs_left, margin, spread) +
                   (1 - ft_oreb) * wp_no_ball(fit, secs_left, margin, spread)
  ft_make * made_branch + (1 - ft_make) * missed_branch
}

## ---- the calculator -------------------------------------------------

#' Win probability for a described game situation
#'
#' @param period 1-4, or 5+ for overtime
#' @param clock seconds remaining in the period
#' @param margin YOUR team's lead, negative when trailing
#' @param possession TRUE if your team has the ball
#' @param spread expected margin for YOUR team, positive when favoured.
#'   0 means pick'em at this venue — see the header.
#' @param free_throws attempts remaining if your team is at the line, else 0
#' @return probability that YOUR team wins
#'
#' AT ZERO SECONDS THE GAME IS DECIDED, and that is arithmetic, not
#' modelling — a smooth fit asked for t=0 will return something close to,
#' but not equal to, 1. inpredictable substitute a decision tree "for the
#' final few seconds"; we found the fit still beats a leader-wins rule in
#' every time bucket including 0-5s (0.1029 vs 0.1062), so the regression is
#' kept and only the terminal instant is handled exactly. A tied game at
#' 0:00 goes to overtime, which is why it returns the OT-start probability
#' rather than a flat 0.5.
win_probability <- function(fit, period, clock, margin, possession = TRUE,
                            spread = 0, free_throws = 0L) {
  secs <- secs_left_from_clock(period, clock)

  if (secs <= 0) {
    if (margin > 0) return(1)
    if (margin < 0) return(0)
    # tied at the horn: overtime, tip-off, nobody has the ball yet
    return(0.5 * (wp_pure(fit, OT_SECS, 0, spread) +
                  wp_no_ball(fit, OT_SECS, 0, spread)))
  }

  if (free_throws > 0) {
    if (!possession)
      stop("free_throws > 0 means your team is shooting, so possession must be TRUE",
           call. = FALSE)
    return(wp_free_throw(fit, secs, margin, spread, as.integer(free_throws)))
  }

  if (possession) wp_pure(fit, secs, margin, spread)
  else            wp_no_ball(fit, secs, margin, spread)
}
