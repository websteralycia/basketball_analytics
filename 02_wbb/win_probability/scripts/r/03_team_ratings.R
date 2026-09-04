## ============================================================
## 03_team_ratings.R — Phase 3: rolling team strength, as a spread
## ------------------------------------------------------------
## Vegas spreads do not exist for most NCAAW games, so this stands in
## for the pre-game line inpredictable's model consumes: an
## efficiency-margin rating per team per game date, computed from
## wehoop box scores, updating through the season so early games work
## off a less-informed rating and later games off a better one.
##
## STRICTLY AS-OF. A game's rating uses only games completed BEFORE its
## date. Using same-day or later games would train the model on
## knowledge of the outcome it is predicting. asof_ratings() takes a
## date and never looks at or past it.
##
## THE OUTPUT IS A SPREAD-EQUIVALENT, NOT A SPREAD. See
## NOTES_spread_substitute.md — measured on 2026, margins here have SD
## 22 against the NBA's ~13, home court is +8.56 rather than ~2.5, 9.5%
## of games are at neutral sites where the nominal home team still wins
## by +4.57, and the schedule graph is barely connected before January
## (median 11 distinct opponents per team, 666-team pool).
##
## TWO RATING METHODS, because the right one is an empirical question:
##
##   "margin"   the plain rolling efficiency margin — each team's own
##              net points per 100 possessions to date, shrunk toward
##              the league mean by games played. Simple, and what the
##              phase brief describes.
##
##   "adjusted" a ridge-penalised Massey fit: net margin per 100 is
##              modelled as rating[home] - rating[away] + home term,
##              solved over all prior games at once. In a 666-team pool
##              where a team has faced ~11 opponents by January, a plain
##              margin largely measures who a team played; this
##              separates that out. The ridge penalty does double duty —
##              opponent adjustment where the graph supports it,
##              regression to the mean where it does not — which is why
##              it is one penalised fit rather than a margin plus a
##              separate shrinkage step.
##
## rating_method_report() scores both against realised margins so the
## choice is made on evidence.
##
## MULTI-SEASON. Ratings are computed WITHIN a season — rosters turn over,
## so a rating cannot simply run across the summer. Dense wehoop coverage
## for WBB is 2017-2026 (~54,000 games; 2016 is partial at 1,792 and 2021
## is COVID-shortened at 3,823).
##
## CARRYOVER PRIOR. A team does not start each November knowing nothing,
## and neither does the market — a Vegas opener encodes last season plus
## roster change. So a season can be seeded with the previous season's
## final ratings, decayed toward the mean by CARRYOVER_DECAY. The ridge
## then shrinks toward THAT prior instead of toward zero, which is the
## one lever available against the early-season weakness documented in
## NOTES_spread_substitute.md. Set decay to 0 to disable.
##
## Depends on: dplyr, tibble, wehoop. Deliberately NOT on Phases 1-2 —
## this phase never opens a play-by-play, only box scores, so it stays
## loadable on its own.
## ============================================================

suppressMessages({
  library(dplyr)
  library(tibble)
})

if (!exists("WINPROB_ROOT")) {
  WINPROB_ROOT <- Sys.getenv("WINPROB_ROOT", unset = "")
  if (!nzchar(WINPROB_ROOT)) {
    WINPROB_ROOT <- file.path(
      Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics"),
      "12_wbb_winprob_calc")
  }
}

# Ridge penalty on team ratings. RE-TUNED 2026-08-06 over the full
# 2017-2026 range by run_03_tune.R — 35 (lambda, decay) cells, selected on
# 2017-2023 with 2024-2026 held out. Was 1, chosen in-sample on 2024-2026.
#
# LAMBDA AND CARRYOVER_DECAY ARE TWO LEVERS ON ONE AXIS. Raising lambda
# compresses predictions (calibration slope up); raising decay expands them
# (slope down). The good cells therefore form a RIDGE, not a peak, and
# tuning one without the other finds a false optimum — which is how the old
# lambda 1 was picked. Across all 35 cells RMSE spans only 12.39-12.78
# (<3%) while the slope spans 0.93-1.05, so the grid barely discriminates
# on accuracy and this is very nearly a pure calibration choice.
#
#   at decay 0.9    lambda    1      2      2.5     3      4
#   RMSE (all)               12.51  12.56  12.61  12.67  12.78
#   slope (tune block)        0.925  0.969  0.985  0.998  1.021
#
# WHY CALIBRATION AND NOT RMSE — the reason is inpredictable's INTERFACE,
# not the regression. Their wpCalc takes the pregame spread as a field the
# user types in, and Phase 6 reproduces that calculator. A hand-entered
# spread and a rating-derived spread land on the same axis, so the axis has
# to be real expected points: at slope 0.93 a user's "-7" and the rating
# system's "-7" mean different things and return different probabilities.
# (Were the spread only ever supplied internally, a locfit would absorb a
# uniform scale error and lambda 1.5 / decay 1.0 would win on information —
# RMSE 12.46, cor 0.7333. It is the manual input that rules that out.)
#
# lambda 3 gives slope 0.9983 on the tuning block, the closest to 1.0 of
# any cell, holding at 1.007 full-range and 1.027 on the holdout. It costs
# ~1.5% RMSE and 0.006 correlation against the flattest-error cells.
# lambda 4 / decay 1.0 is statistically indistinguishable; lambda 3 is
# preferred only because it leaves CARRYOVER_DECAY where it was.
#
# An earlier default of 60 was ~20x too strong and made the ADJUSTED
# method look worse than the plain rolling margin. It is not: at lambda 3
# it is RMSE 12.67 vs the margin method's 16.49, a 23% improvement.
RATING_LAMBDA <- 3

# A negligible ridge on the TWO HOME TERMS. They are conceptually
# unpenalised — a neutral site is not a neutral game and the coefficient
# should be whatever the data says — but at literally zero penalty the
# normal equations are SINGULAR whenever one of the two has seen no games
# yet, which is the case every season until its first neutral-site game.
#
# BUG FOUND 2026-08-07 by tests/testthat/test-team-ratings.R. solve() threw,
# the tryCatch swallowed it, and `beta` fell back to the carryover prior —
# so every rating before a season's first neutral game was the PRIOR rather
# than a fit, silently. 1,909 games, 3.5% of 2017-2026, and all of them in
# the opening days of November where the model is already weakest. Seasons
# 2019, 2020, 2022 and 2023 were worst (4-6 dates each); in the rest the
# first neutral game falls on opening day and only that one date was lost.
#
# 1e-8 is small enough to leave the fitted terms unchanged to eight decimals
# once any data exists, and large enough to make the matrix invertible.
# Do NOT set this to zero to "keep them unpenalised" — that is the bug.
HOME_TERM_EPS <- 1e-8

# Possessions per team-game, the standard box estimate. The same 0.44
# the possession layer is validated against, so the two agree on what a
# possession is even though this one never opens a play-by-play.
box_possessions <- function(fga, fta, tov, oreb) {
  fga + 0.44 * fta + tov - oreb
}

#' One row per team-game: pace, points for and against, net efficiency
#'
#' Net efficiency is per 100 possessions, using the GAME's pace (the mean
#' of the two teams' estimates) rather than each team's own. Both teams
#' face the same number of possessions; letting them differ would make a
#' team's rating depend on its own shot selection.
team_game_efficiency <- function(box) {
  b <- box |>
    filter(!is.na(team_score), !is.na(opponent_team_score)) |>
    transmute(
      game_id   = as.character(game_id),
      date      = as.Date(game_date),
      team      = as.character(team_id),
      opponent  = as.character(opponent_team_id),
      is_home   = team_home_away == "home",
      pts       = as.numeric(team_score),
      opp_pts   = as.numeric(opponent_team_score),
      poss_self = box_possessions(as.numeric(field_goals_attempted),
                                  as.numeric(free_throws_attempted),
                                  as.numeric(turnovers),
                                  as.numeric(offensive_rebounds))
    )

  b |>
    group_by(game_id) |>
    filter(n() == 2, all(!is.na(poss_self))) |>
    mutate(pace = mean(poss_self)) |>
    ungroup() |>
    filter(pace > 20) |>          # guard against malformed box rows
    mutate(net_per100 = 100 * (pts - opp_pts) / pace)
}

#' One row per GAME, oriented to the home team — the fitting frame
game_frame <- function(eff, schedule = NULL) {
  g <- eff |>
    select(game_id, date, team, opponent, is_home, pace, net_per100, pts, opp_pts) |>
    filter(is_home) |>
    transmute(game_id, date, home = team, away = opponent, pace,
              home_net = net_per100,
              # Final scores travel with the game because Phase 4's LABEL
              # comes from here, not from the play-by-play: the bulk
              # loader load_wbb_pbp() does not carry `home_team_winner`
              # even though the single-game espn_wbb_pbp() does. Reading
              # it from the feed yields an entirely unlabelled training
              # set, silently.
              home_final = pts, away_final = opp_pts)
  if (!is.null(schedule)) {
    ns <- schedule |>
      transmute(game_id = as.character(game_id),
                neutral = isTRUE(neutral_site) | neutral_site %in% c(TRUE, "TRUE"))
    g <- left_join(g, ns, by = "game_id")
  }
  g$neutral[is.na(g$neutral)] <- FALSE
  g
}

#' Ratings using only games strictly before `date`
#'
#' WARNING — NO CARRYOVER PRIOR. This function has no `prior0` argument, so
#' it shrinks toward zero, not toward last season's ratings. It is therefore
#' NOT the path that build_rating_table() takes and NOT what the cached CSV
#' contains. Scoring through it understates the shipped model badly in
#' November, where the prior does nearly all of its work (RMSE 17.7 without
#' it against 14.5 with it). The 2026-08-04 validation figures in
#' NOTES_spread_substitute.md were wrong for exactly this reason.
#'
#' Use it for a single ad-hoc date. To SCORE the model, score the output of
#' build_rating_table() — run_03_tune.R shows how, and it is also far
#' faster, since this refits from scratch for every date.
#'
#' @param method "adjusted" (ridge Massey) or "margin" (shrunk rolling mean)
#' @return list(rating = named numeric, home = points-per-100 home term,
#'   neutral = the same for neutral sites, n_games = named integer)
asof_ratings <- function(games, date, method = c("adjusted", "margin"),
                         lambda = RATING_LAMBDA) {
  method <- match.arg(method)
  prior  <- games[games$date < date, ]

  teams <- sort(unique(c(games$home, games$away)))
  empty <- stats::setNames(rep(0, length(teams)), teams)
  ng    <- stats::setNames(rep(0L, length(teams)), teams)
  if (nrow(prior) == 0) {
    return(list(rating = empty, home = 0, neutral = 0, n_games = ng))
  }

  played <- table(c(prior$home, prior$away))
  ng[names(played)] <- as.integer(played)

  if (method == "margin") {
    # Each team's own net efficiency to date, shrunk toward 0 (the league
    # mean) by games played: with k games the estimate gets weight
    # k / (k + lambda/10). The divisor mirrors the ridge scale so the two
    # methods shrink comparably and the comparison is about opponent
    # adjustment, not about how hard each regresses.
    long <- rbind(
      data.frame(team = prior$home, net =  prior$home_net),
      data.frame(team = prior$away, net = -prior$home_net))
    m <- tapply(long$net, long$team, mean)
    k <- ng[names(m)]
    shrunk <- as.numeric(m) * k / (k + lambda / 10)
    empty[names(m)] <- shrunk
    hm <- mean(prior$home_net[!prior$neutral])
    nt <- if (any(prior$neutral)) mean(prior$home_net[prior$neutral]) else 0
    return(list(rating = empty, home = hm, neutral = nt, n_games = ng))
  }

  # --- ridge Massey ------------------------------------------------
  # home_net = rating[home] - rating[away] + home_term, one equation per
  # game. The two home terms are NOT penalised: a neutral site is not a
  # neutral game (the nominal home team wins by ~4.6), so it gets its own
  # coefficient rather than being forced to zero.
  idx <- stats::setNames(seq_along(teams), teams)
  nT  <- length(teams)
  p   <- nT + 2L

  XtX <- matrix(0, p, p)
  Xty <- numeric(p)
  hi  <- idx[prior$home]; ai <- idx[prior$away]
  hcol <- ifelse(prior$neutral, nT + 2L, nT + 1L)
  y <- prior$home_net

  for (r in seq_len(nrow(prior))) {
    j <- c(hi[r], ai[r], hcol[r]); v <- c(1, -1, 1)
    XtX[j, j] <- XtX[j, j] + tcrossprod(v)
    Xty[j]    <- Xty[j] + v * y[r]
  }
  pen <- c(rep(lambda, nT), HOME_TERM_EPS, HOME_TERM_EPS)
  diag(XtX) <- diag(XtX) + pen
  # Ratings are only identified up to a constant; the ridge on the team
  # block pins the mean at zero, so no extra sum-to-zero row is needed.
  beta <- tryCatch(solve(XtX, Xty), error = function(e) rep(0, p))

  list(rating  = stats::setNames(beta[seq_len(nT)], teams),
       home    = beta[nT + 1L],
       neutral = beta[nT + 2L],
       n_games = ng)
}

#' Expected point spread for one matchup, from an as-of rating set
#'
#' The rating is per 100 possessions; a spread is points. Converting
#' needs the game's expected pace, which varies far more across college
#' teams than across NBA teams — so pace is an argument, not a constant.
#'
#' Positive = the home team is favoured, matching how a margin reads.
expected_spread <- function(r, home, away, pace, neutral = FALSE) {
  rh <- if (home %in% names(r$rating)) r$rating[[home]] else 0
  ra <- if (away %in% names(r$rating)) r$rating[[away]] else 0
  hterm <- if (isTRUE(neutral)) r$neutral else r$home
  (rh - ra + hterm) * pace / 100
}

#' Rating table for ONE season: one row per team per game date
#'
#' Walks dates in order and SOLVES BEFORE adding that day's games, which
#' is what makes every rating strictly as-of. The normal equations are
#' accumulated incrementally rather than rebuilt per date — refitting
#' from scratch on every one of ~150 dates across 10 seasons does not
#' finish in reasonable time.
#'
#' @param prior0 Named vector of preseason ratings (the previous season's,
#'   decayed). The ridge shrinks toward these instead of toward zero.
season_rating_table <- function(games, method = "adjusted", lambda = RATING_LAMBDA,
                                prior0 = NULL, quiet = TRUE) {
  teams <- sort(unique(c(games$home, games$away)))
  nT <- length(teams); p <- nT + 2L
  idx <- stats::setNames(seq_len(nT), teams)

  b0 <- numeric(p)
  if (!is.null(prior0)) {
    common <- intersect(names(prior0), teams)
    if (length(common)) b0[idx[common]] <- prior0[common]
  }

  XtX <- matrix(0, p, p); Xty <- numeric(p)
  ng  <- stats::setNames(integer(nT), teams)
  sum_net <- stats::setNames(numeric(nT), teams)
  hn <- hs <- nn <- ns <- 0            # home / neutral running counts and sums

  dates <- sort(unique(games$date))
  out <- vector("list", length(dates))

  for (i in seq_along(dates)) {
    d <- dates[i]

    ## --- SOLVE on everything strictly before d ---------------------
    if (method == "adjusted") {
      A <- XtX
      pen <- c(rep(lambda, nT), HOME_TERM_EPS, HOME_TERM_EPS)
      diag(A) <- diag(A) + pen
      rhs <- Xty + pen * b0                      # ridge toward the prior
      beta <- tryCatch(solve(A, rhs), error = function(e) b0)
      rating <- stats::setNames(beta[seq_len(nT)], teams)
      hterm  <- beta[nT + 1L]; nterm <- beta[nT + 2L]
    } else {
      k <- ng; w <- k / (k + lambda / 10)
      mean_net <- ifelse(k > 0, sum_net / pmax(k, 1), 0)
      rating <- stats::setNames(w * mean_net + (1 - w) * b0[seq_len(nT)], teams)
      hterm  <- if (hn > 0) hs / hn else 0
      nterm  <- if (nn > 0) ns / nn else 0
    }

    keep <- ng > 0 | (!is.null(prior0) & b0[seq_len(nT)] != 0)
    out[[i]] <- tibble(
      date = d, team = teams[keep], rating = unname(rating[keep]),
      n_games = unname(ng[keep]), home_term = hterm, neutral_term = nterm,
      method = method)

    ## --- THEN fold in the day's games ------------------------------
    day <- games[games$date == d, ]
    if (nrow(day)) {
      hi <- idx[day$home]; ai <- idx[day$away]
      hc <- ifelse(day$neutral, nT + 2L, nT + 1L)
      for (r in seq_len(nrow(day))) {
        if (is.na(hi[r]) || is.na(ai[r])) next
        j <- c(hi[r], ai[r], hc[r]); v <- c(1, -1, 1)
        XtX[j, j] <- XtX[j, j] + tcrossprod(v)
        Xty[j]    <- Xty[j] + v * day$home_net[r]
      }
      tb <- table(c(day$home, day$away)); ng[names(tb)] <- ng[names(tb)] + as.integer(tb)
      sh <- tapply(day$home_net, day$home, sum); sum_net[names(sh)] <- sum_net[names(sh)] + sh
      sa <- tapply(-day$home_net, day$away, sum); sum_net[names(sa)] <- sum_net[names(sa)] + sa
      hn <- hn + sum(!day$neutral); hs <- hs + sum(day$home_net[!day$neutral])
      nn <- nn + sum(day$neutral);  ns <- ns + sum(day$home_net[day$neutral])
    }
    if (!quiet && i %% 40 == 0) message("    ", i, "/", length(dates), " dates")
  }
  bind_rows(out)
}

# How much of last season's rating carries into the next. 0 disables the
# carryover entirely; 1 carries the previous rating forward undecayed.
#
# RE-TUNED 2026-08-06 across 2017-2026 and KEPT at 0.9. Read it with the
# lambda note above — the two constants trade off, so this value is only
# meaningful at lambda 3.
#
# The carryover is worth far more than the three-season tuning showed, and
# it is worth it ENTIRELY IN NOVEMBER. By month, lambda 1, ten seasons:
#
#   decay      Nov    Dec    Jan    Feb    Mar
#   0        17.68  14.02  12.31  11.79  11.77
#   0.9      14.35  13.01  12.02  11.70  11.72
#
# Turning it off costs 24% in November and ~1% by February. That is the
# season shape a market line has, and it confirms the original reading:
# this is only a PRIOR, and the ridge overwrites it as games accumulate.
#
# RMSE keeps improving past 0.9 — decay 1.0 and even 1.1 test better still
# (12.45, 12.39 at lambda 2). Both are rejected. 1.1 means AMPLIFYING last
# season's ratings, which has no story behind it and is pure ridge-fitting;
# and along this ridge more decay buys RMSE by giving up the calibrated
# scale the manual spread input needs. At lambda 3 the slope is 1.007 at
# decay 0.9 against 0.98 at 1.0.
CARRYOVER_DECAY <- 0.9

#' Rating tables across several seasons, with the carryover prior
#'
#' Seasons are fit in order so each can be seeded by the previous one.
build_rating_table <- function(games, method = "adjusted", lambda = RATING_LAMBDA,
                               decay = CARRYOVER_DECAY, quiet = FALSE) {
  seasons <- sort(unique(games$season))
  prior0 <- NULL
  out <- list()
  for (s in seasons) {
    if (!quiet) message("  season ", s, " ...")
    g <- games[games$season == s, ]
    tb <- season_rating_table(g, method, lambda, prior0, quiet = TRUE)
    tb$season <- s
    out[[as.character(s)]] <- tb
    if (decay > 0) {
      fin <- tb[tb$date == max(tb$date), ]
      prior0 <- stats::setNames(fin$rating * decay, fin$team)
    }
  }
  bind_rows(out)
}

#' Score both methods against realised margins
#'
#' The only test that matters for a spread substitute: does the number it
#' produces predict the margin?
#'
#' WARNING — this calls asof_ratings(), so it scores WITHOUT the carryover
#' prior and does not reflect what Phase 4 consumes. See that function's
#' note. It survives because the adjusted-vs-margin comparison it exists for
#' is a fair fight with the prior off on both sides. For tuning or for
#' headline accuracy figures, use run_03_tune.R instead.
rating_method_report <- function(games, methods = c("adjusted", "margin"),
                                 lambdas = RATING_LAMBDA, min_prior = 1) {
  dates <- sort(unique(games$date))
  res <- list()
  for (m in methods) for (lam in lambdas) {
    pred <- rep(NA_real_, nrow(games))
    for (d in dates) {
      r  <- asof_ratings(games, d, m, lam)
      ix <- which(games$date == d)
      if (!length(ix)) next
      ok <- r$n_games[games$home[ix]] >= min_prior & r$n_games[games$away[ix]] >= min_prior
      ok[is.na(ok)] <- FALSE
      if (any(ok)) pred[ix[ok]] <- vapply(ix[ok], function(j)
        expected_spread(r, games$home[j], games$away[j], games$pace[j], games$neutral[j]),
        numeric(1))
    }
    actual <- games$home_net * games$pace / 100      # realised margin, points
    keep <- !is.na(pred)
    fit  <- stats::lm(actual[keep] ~ pred[keep])
    res[[length(res) + 1]] <- tibble(
      method = m, lambda = lam, n = sum(keep),
      rmse = sqrt(mean((actual[keep] - pred[keep])^2)),
      cor  = stats::cor(actual[keep], pred[keep]),
      calib_slope = unname(coef(fit)[2])
    )
  }
  bind_rows(res)
}
