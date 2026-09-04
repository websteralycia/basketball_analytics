## ============================================================
## run_03_tune.R — re-tune RATING_LAMBDA and CARRYOVER_DECAY
## ------------------------------------------------------------
## Both Phase 3 constants were selected in-sample on 2024-2026 and the
## full-range calibration slope then fell to 0.955, so 03_team_ratings.R
## flags them as unsettled. This sweeps a (lambda, decay) grid over the
## whole 2017-2026 range and scores every cell.
##
## THE EVALUATOR DOES NOT CALL asof_ratings(). That function refits from
## scratch for one date; scoring a grid with it means a fresh solve per
## date per cell and does not finish. build_rating_table() already emits
## a strictly as-of rating for every (season, date, team) — it solves
## BEFORE folding each date's games in — so scoring is a join, and one
## grid cell costs one incremental build.
##
## HOLDOUT. 2017-2023 is the tuning block and 2024-2026 the holdout, the
## same three seasons the original in-sample choice used. A constant that
## only looks good on the block it was picked on will show it here.
##
##   Rscript scripts/r/run_03_tune.R
## ============================================================

suppressMessages({library(dplyr); library(tibble)})

R_HOME_DIR <- Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics")
source(file.path(R_HOME_DIR, "12_wbb_winprob_calc/scripts/r/03_team_ratings.R"))

OUT_DIR   <- file.path(WINPROB_ROOT, "outputs")
CACHE     <- file.path(WINPROB_ROOT, "data/tidy/wbb/rating_games_2017_2026.rds")
HOLDOUT   <- 2024:2026
# Not TRAIN_SEASONS — that lives in 04_training_set.R, and this tuner
# deliberately depends on Phase 3 alone. Same range.
SEASONS   <- 2017:2026
# Overridable so a single cell can be re-run as a smoke test:
#   TUNE_LAMBDAS=1 TUNE_DECAYS=0.9 Rscript scripts/r/run_03_tune.R
env_nums <- function(nm, default) {
  v <- Sys.getenv(nm, unset = "")
  if (!nzchar(v)) default else as.numeric(strsplit(v, ",")[[1]])
}
LAMBDAS   <- env_nums("TUNE_LAMBDAS", c(0.4, 0.7, 1, 1.5, 2.5))
DECAYS    <- env_nums("TUNE_DECAYS",  c(0, 0.5, 0.7, 0.9, 1.0))
MIN_PRIOR <- 1L

## ---- the fitting frame, cached ------------------------------------
if (file.exists(CACHE)) {
  games <- readRDS(CACHE)
  message("games from cache: ", format(nrow(games), big.mark = ","))
} else {
  message("loading box scores + schedule for ", min(SEASONS), "-",
          max(SEASONS), " ...")
  box <- suppressWarnings(wehoop::load_wbb_team_box(seasons = SEASONS))
  sch <- suppressWarnings(wehoop::load_wbb_schedule(seasons = SEASONS))
  games <- game_frame(team_game_efficiency(box), sch)
  games$season <- box$season[match(games$game_id, as.character(box$game_id))]
  games <- games[!is.na(games$season) & !is.na(games$home_net) & !is.na(games$pace), ]
  rm(box, sch); invisible(gc(FALSE))
  saveRDS(games, CACHE)
  message("games built: ", format(nrow(games), big.mark = ","))
}

games$month  <- as.integer(format(games$date, "%m"))
games$actual <- games$home_net * games$pace / 100     # realised margin, points

## ---- score one rating table against realised margins ---------------
#' @param tb output of build_rating_table()
#' @return the games frame with `pred` attached (NA where either team has
#'   fewer than MIN_PRIOR completed games, which is the same exclusion
#'   rating_method_report() applies)
attach_pred <- function(games, tb) {
  tkey <- paste(tb$season, tb$date, tb$team)
  rat  <- stats::setNames(tb$rating,  tkey)
  ngm  <- stats::setNames(tb$n_games, tkey)

  # the two home terms are per (season, date), carried on every row
  dk   <- paste(tb$season, tb$date)
  first <- !duplicated(dk)
  hterm <- stats::setNames(tb$home_term[first],    dk[first])
  nterm <- stats::setNames(tb$neutral_term[first], dk[first])

  gd <- paste(games$season, games$date)
  gh <- paste(gd, games$home); ga <- paste(gd, games$away)

  rh <- unname(rat[gh]); ra <- unname(rat[ga])
  nh <- unname(ngm[gh]); na_ <- unname(ngm[ga])
  term <- ifelse(games$neutral, unname(nterm[gd]), unname(hterm[gd]))

  pred <- (rh - ra + term) * games$pace / 100
  ok <- !is.na(pred) & !is.na(nh) & !is.na(na_) & nh >= MIN_PRIOR & na_ >= MIN_PRIOR
  games$pred <- ifelse(ok, pred, NA_real_)
  games
}

score <- function(d) {
  k <- !is.na(d$pred)
  if (sum(k) < 50) return(tibble(n = sum(k), rmse = NA_real_, cor = NA_real_,
                                 calib_slope = NA_real_))
  fit <- stats::lm(d$actual[k] ~ d$pred[k])
  tibble(n = sum(k),
         rmse = sqrt(mean((d$actual[k] - d$pred[k])^2)),
         cor  = stats::cor(d$actual[k], d$pred[k]),
         calib_slope = unname(coef(fit)[2]))
}

## ---- the sweep -----------------------------------------------------
grid <- expand.grid(lambda = LAMBDAS, decay = DECAYS)
message("sweeping ", nrow(grid), " cells over ", format(nrow(games), big.mark = ","),
        " games ...")

rows <- list(); by_month <- list(); by_season <- list()
t0 <- Sys.time()

for (i in seq_len(nrow(grid))) {
  lam <- grid$lambda[i]; dec <- grid$decay[i]
  ti <- Sys.time()
  tb <- build_rating_table(games, method = "adjusted", lambda = lam,
                           decay = dec, quiet = TRUE)
  d <- attach_pred(games, tb)
  rm(tb); invisible(gc(FALSE))

  tune <- score(d[!d$season %in% HOLDOUT, ])
  hold <- score(d[ d$season %in% HOLDOUT, ])
  all  <- score(d)

  rows[[i]] <- tibble(
    lambda = lam, decay = dec,
    rmse_tune = tune$rmse, slope_tune = tune$calib_slope,
    rmse_hold = hold$rmse, slope_hold = hold$calib_slope,
    rmse_all  = all$rmse,  slope_all  = all$calib_slope,
    cor_all = all$cor, n = all$n)

  by_month[[i]] <- d |> group_by(month) |> group_modify(~score(.x)) |>
    ungroup() |> mutate(lambda = lam, decay = dec)
  by_season[[i]] <- d |> group_by(season) |> group_modify(~score(.x)) |>
    ungroup() |> mutate(lambda = lam, decay = dec)

  message(sprintf("  [%2d/%d] lambda %-4s decay %-4s | tune %.3f/%.3f  hold %.3f/%.3f  (%.0fs)",
                  i, nrow(grid), lam, dec, tune$rmse, tune$calib_slope,
                  hold$rmse, hold$calib_slope,
                  as.numeric(difftime(Sys.time(), ti, units = "secs"))))
}

res <- bind_rows(rows)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(res, file.path(OUT_DIR, "phase3_tuning_grid.csv"))
readr::write_csv(bind_rows(by_month),  file.path(OUT_DIR, "phase3_tuning_by_month.csv"))
readr::write_csv(bind_rows(by_season), file.path(OUT_DIR, "phase3_tuning_by_season.csv"))

message("\n=== full grid (sorted by holdout RMSE) ===")
print(as.data.frame(res |> arrange(rmse_hold)), digits = 4)

message("\n=== best-calibrated cells (|slope_hold - 1| smallest) ===")
print(as.data.frame(res |> arrange(abs(slope_hold - 1)) |> head(8)), digits = 4)

message("TOTAL ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
