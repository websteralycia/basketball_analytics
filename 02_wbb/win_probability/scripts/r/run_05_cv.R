## ============================================================
## run_05_cv.R — Phase 5 step 1: select the smoothing parameter
## ------------------------------------------------------------
## Cross-validates nn (the nearest-neighbour window) by game, on a
## game-sampled subset, and reports calibration and an endgame boundary for
## the winner. Writes outputs/phase5_cv.csv, phase5_calibration.csv,
## phase5_endgame.csv, and caches the chosen fit to data/tidy/wbb/.
##
##   Rscript scripts/r/run_05_cv.R
## ============================================================

suppressMessages({library(dplyr); library(arrow)})

R_HOME_DIR <- Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics")
for (f in c("03_team_ratings", "04_training_set", "05_winprob_model"))
  source(file.path(R_HOME_DIR, "12_wbb_winprob_calc/scripts/r", paste0(f, ".R")))

OUT_DIR <- file.path(WINPROB_ROOT, "outputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

message("loading pure training rows ...")
d <- winprob_frame(load_training_set(pure_only = TRUE))
message("  ", format(nrow(d), big.mark = ","), " rows, ",
        format(dplyr::n_distinct(d$game_id), big.mark = ","), " games")

## A held-out block of GAMES, never seen by CV or by the final fit, so the
## reported calibration is out-of-sample in the strict sense.
set.seed(99)
all_games <- sort(unique(d$game_id))   # sorted: arrow row order is not stable
test_games <- sample(all_games, floor(0.15 * length(all_games)))
test <- d[d$game_id %in% test_games, ]
pool <- d[!d$game_id %in% test_games, ]
message("  held out ", format(nrow(test), big.mark = ","), " rows / ",
        format(length(test_games), big.mark = ","), " games")

cv_d <- winprob_sample(pool, CV_ROWS)
message("CV on ", format(nrow(cv_d), big.mark = ","), " rows, ",
        CV_FOLDS, " folds by game ...")
t0 <- Sys.time()
cv <- cv_winprob(cv_d)
readr::write_csv(cv, file.path(OUT_DIR, "phase5_cv.csv"))
print(as.data.frame(cv), digits = 5)

best <- cv$nn[which.min(cv$log_loss)]
message("\nbest nn = ", best, "  (", round(difftime(Sys.time(), t0, units = "mins"), 1), " min)")

## Final fit on a larger sample than CV used — the window is a FRACTION of
## the data, so nn transfers across sample sizes, and more rows buy
## resolution in the sparse corners (big leads, last seconds).
fit_d <- winprob_sample(pool, 1200000L, seed = 7)
message("fitting on ", format(nrow(fit_d), big.mark = ","), " rows ...")
t1 <- Sys.time()
fit <- fit_winprob(fit_d, nn = WINPROB_NN)
message("  fitted in ", round(difftime(Sys.time(), t1, units = "mins"), 1), " min")
saveRDS(list(fit = fit, nn = WINPROB_NN, n = nrow(fit_d)),
        file.path(WINPROB_ROOT, "data/tidy/wbb/winprob_fit.rds"))

## --- out-of-sample scoring -------------------------------------------
p <- predict_winprob(fit, test)
message(sprintf("\nHELD-OUT: log loss %.5f | brier %.5f | base-rate log loss %.5f",
                log_loss(test$y, p), brier(test$y, p),
                log_loss(test$y, rep(mean(pool$y), nrow(test)))))

cal <- calibration_table(test$y, p)
readr::write_csv(cal, file.path(OUT_DIR, "phase5_calibration.csv"))
message("\n=== calibration (held out) ===")
print(as.data.frame(cal), digits = 4)

## By month — November ratings are the weakest input, so the question is
## whether that weakness reaches the probabilities.
by_month <- tibble::tibble(month = test$month, y = test$y, p = p) |>
  group_by(month) |>
  summarise(n = n(), log_loss = log_loss(y, p), brier = brier(y, p),
            mean_pred = mean(p), actual = mean(y), .groups = "drop")
readr::write_csv(by_month, file.path(OUT_DIR, "phase5_by_month.csv"))
message("\n=== by month (held out) ===")
print(as.data.frame(by_month), digits = 4)

eg <- endgame_report(test, fit)
readr::write_csv(eg, file.path(OUT_DIR, "phase5_endgame.csv"))
message("\n=== by time remaining (held out) ===")
print(as.data.frame(eg), digits = 4)
message("\nTOTAL ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
