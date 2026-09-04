## ============================================================
## 05_winprob_model.R — Phase 5: the win-probability fit
## ------------------------------------------------------------
## Locally-weighted logistic regression over game state, via locfit,
## following inpredictable's method: one model over PURE possessions, from
## which the free-throw states are derived later (Phase 6) rather than fit
## separately.
##
## THREE PREDICTORS, all oriented to the team with the ball, matching the
## frame `ball_margin` and `spread_ball` already use:
##   t            sqrt(seconds remaining in the game)
##   ball_margin  points, positive when the team with the ball leads
##   spread_ball  the Phase 3 pre-game expectation, positive when favoured
##
## WHY sqrt(TIME) — this is the "smoothing window shrinks as time runs out"
## behaviour from inpredictable's writeup, obtained from the axis rather
## than from a schedule of bandwidths. A nearest-neighbour window covers a
## fixed FRACTION of the data, and game states are spread fairly evenly
## through clock time, so on a raw seconds axis the window would be about as
## wide at 0:10 as at 30:00 — hopeless, since win probability moves faster in
## the last ten seconds than in the first ten minutes. Under sqrt, one unit
## near t=0 is a fraction of a second while one unit near t=49 is a minute
## and a half, so the same window is automatically far tighter late. It is
## also the natural scale: the spread of a lead's future change grows like
## sqrt of the time left.
##
## NOT COVERED HERE. inpredictable replace the regression entirely "for the
## final few seconds of game time" with a decision tree, because at that
## point possession and foul situation dominate and the response is nearly a
## step function. That is a separate pass — see endgame_cutoff() below for
## where the boundary is measured, and NOTES_phase5.md.
##
## Depends on: 04_training_set.R (and hence 03), locfit, dplyr, arrow.
## ============================================================

suppressMessages({
  library(dplyr)
  library(locfit)
})

# locfit's adaptive tree runs out of vertices on this data at the default
# maxk of 100 — it fails with "newsplit: out of vertex space" rather than
# degrading, so it must be raised. 2000 is ample for three predictors;
# 20000 was measured to give an identical fit at the same cost.
LOCFIT_MAXK <- 2000

# Rows used to select the smoothing parameter. A full-data fit is ~13 min,
# so cross-validating a grid over 5.3M rows is hours; at 300k a fit is ~45s.
# Sampled BY GAME, never by row — see winprob_sample().
CV_ROWS   <- 300000L
CV_FOLDS  <- 5L
NN_GRID   <- c(0.05, 0.1, 0.2, 0.3, 0.5)

# The smoothing window, as a FRACTION of the data in each local fit.
#
# CHOSEN ON LATE-GAME BEHAVIOUR, NOT ON CV. Global CV picked 0.5 — the top of
# its grid — and that is wrong twice over. First, above nn 0.2 the CV curve is
# flat to within 0.0014 while the SD across folds is 0.019, so its argmin is
# noise. Second and more usefully:
#
#   NN IS A FRACTION, SO ITS BEST VALUE DEPENDS ON HOW MANY ROWS YOU FIT.
#   CV trains on ~240k-row folds, where nn 0.1 is 24k points per local fit.
#   The production fit uses 1.2M rows, where the same nn 0.1 is 120k points —
#   five times wider in absolute terms. A fraction tuned on small folds is
#   therefore systematically TOO LARGE for the full fit. Any nearest-neighbour
#   parameter cross-validated on a subsample has this problem.
#
# Measured on held-out games, all three fitted on the same 1.2M rows
# (run_05_window.R), log loss by time remaining:
#
#              0-5s     5-10s   10-20s   60-120s   300s+   overall
#   nn 0.1    0.1049   0.0885   0.0963    0.1349   0.3507   0.32468
#   nn 0.2    0.1131   0.0928   0.0973    0.1341   0.3509   0.32486
#   nn 0.5    0.1322   0.1070   0.1050    0.1346   0.3512   0.32531
#   naive     0.1050   0.1341   0.1493    0.2471   0.9336
#
# 0.1 wins everywhere that matters and is 26% better than 0.5 in the last five
# seconds. The overall column barely separates them — it is dominated by the
# 87% of states with 5+ minutes left, and its spread is smaller than the
# variation between two different held-out splits, so it decides nothing.
WINPROB_NN <- 0.1

#' Model frame: the three predictors plus the label
#'
#' @param d output of load_training_set(pure_only = TRUE)
winprob_frame <- function(d) {
  d |>
    filter(!is.na(secs_left_game), !is.na(ball_margin), !is.na(spread_ball)) |>
    mutate(
      t = sqrt(pmax(secs_left_game, 0)),
      y = as.numeric(ball_win),
      month = as.integer(format(date, "%m"))
    )
}

#' Sample rows BY GAME
#'
#' Rows within a game are heavily dependent — 143 states from one game share
#' its outcome entirely. Sampling rows would put the same game on both sides
#' of a CV split and make the estimate optimistic, which is the trap the
#' Phase 4 header already flags. Whole games move together.
winprob_sample <- function(d, n_rows, seed = 1) {
  set.seed(seed)
  # SORTED, not just unique. load_training_set() reads a multi-file arrow
  # dataset and its row order is not guaranteed stable between runs, so
  # unique() alone returns the games in a different ORDER each time and the
  # same seed then selects a different sample. Two runs that should have been
  # identical differed by ~600 rows before this was fixed, which is enough to
  # make model comparisons across runs meaningless.
  games <- sort(unique(d$game_id))
  # rows per game is near-constant, so a game count approximates a row count
  per <- nrow(d) / length(games)
  keep <- sample(games, min(length(games), ceiling(n_rows / per)))
  d[d$game_id %in% keep, ]
}

#' Assign each GAME to a fold
fold_by_game <- function(d, k = CV_FOLDS, seed = 2) {
  set.seed(seed)
  games <- sort(unique(d$game_id))   # sorted: see winprob_sample()
  f <- stats::setNames(sample(rep_len(seq_len(k), length(games))), games)
  unname(f[d$game_id])
}

#' Fit one model
fit_winprob <- function(d, nn = 0.1, maxk = LOCFIT_MAXK) {
  locfit(y ~ lp(t, ball_margin, spread_ball, nn = nn),
         data = d, family = "binomial", link = "logit", maxk = maxk)
}

#' Predict probabilities, clamped away from 0 and 1
#'
#' locfit extrapolates outside the fitted region and can return values a
#' shade outside [0,1]; log loss is infinite at exactly 0 or 1, so a
#' prediction that is merely confident must not become an infinite penalty.
predict_winprob <- function(fit, newdata, eps = 1e-6) {
  p <- stats::predict(fit, newdata = newdata)
  pmin(pmax(p, eps), 1 - eps)
}

log_loss <- function(y, p) -mean(y * log(p) + (1 - y) * log(1 - p))
brier    <- function(y, p) mean((y - p)^2)

#' Cross-validate the smoothing parameter, splitting by game
cv_winprob <- function(d, nn_grid = NN_GRID, k = CV_FOLDS, quiet = FALSE) {
  d$fold <- fold_by_game(d, k)
  out <- list()
  for (nn in nn_grid) {
    ll <- br <- numeric(0)
    for (i in seq_len(k)) {
      tr <- d[d$fold != i, ]; te <- d[d$fold == i, ]
      fit <- tryCatch(fit_winprob(tr, nn), error = function(e) NULL)
      if (is.null(fit)) next
      p <- predict_winprob(fit, te)
      ll <- c(ll, log_loss(te$y, p)); br <- c(br, brier(te$y, p))
    }
    out[[length(out) + 1]] <- tibble::tibble(
      nn = nn, folds = length(ll),
      log_loss = mean(ll), log_loss_sd = stats::sd(ll), brier = mean(br))
    if (!quiet) message(sprintf("  nn %-5s log loss %.5f  brier %.5f  (%d folds)",
                                nn, mean(ll), mean(br), length(ll)))
  }
  bind_rows(out)
}

#' Calibration in bins of predicted probability
#'
#' The headline number for a probability model: of the states we called 70%,
#' did 70% of them go on to win? Reported in bins rather than as a slope
#' because miscalibration here is not expected to be linear.
calibration_table <- function(y, p, bins = 10) {
  b <- cut(p, breaks = seq(0, 1, length.out = bins + 1), include.lowest = TRUE)
  tibble::tibble(bin = b, y = y, p = p) |>
    group_by(bin) |>
    summarise(n = n(), mean_pred = mean(p), actual = mean(y),
              gap = mean(y) - mean(p), .groups = "drop")
}

#' Where does the regression stop being the right tool?
#'
#' inpredictable swap to a decision tree "for the final few seconds". This
#' measures the boundary rather than guessing it: log loss by time bucket,
#' fitted vs the naive baseline of "whoever leads wins". Where the naive
#' rule catches the model, the smooth fit has stopped adding anything.
endgame_report <- function(d, fit, buckets = c(0, 5, 10, 20, 30, 60, 120, 300, Inf)) {
  p <- predict_winprob(fit, d)
  naive <- ifelse(d$ball_margin > 0, 0.99, ifelse(d$ball_margin < 0, 0.01, 0.5))
  tibble::tibble(secs = d$secs_left_game, y = d$y, p = p, naive = naive) |>
    mutate(bucket = cut(secs, buckets, include.lowest = TRUE, right = FALSE)) |>
    group_by(bucket) |>
    summarise(n = n(), log_loss = log_loss(y, p),
              naive_log_loss = log_loss(y, naive),
              brier = brier(y, p), .groups = "drop")
}
