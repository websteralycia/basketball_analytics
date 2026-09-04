## ============================================================
## run_05_fit.R — Phase 5: fit and cache the production model
## ------------------------------------------------------------
## Fits at WINPROB_NN on the standard split and writes the full held-out
## report. Separate from run_05_cv.R because CV no longer chooses anything:
## nn was settled on late-game behaviour (see 05_winprob_model.R), and
## re-running the grid costs 11 minutes to re-learn that it cannot decide.
##
## Run run_05_cv.R when the DATA changes enough to revisit nn. Run this
## whenever the model needs rebuilding.
##
## Writes data/tidy/wbb/winprob_fit.rds and outputs/phase5_*.csv.
##
##   Rscript scripts/r/run_05_fit.R
## ============================================================

suppressMessages({library(dplyr); library(arrow)})

R_HOME_DIR <- Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics")
for (f in c("03_team_ratings", "04_training_set", "05_winprob_model"))
  source(file.path(R_HOME_DIR, "12_wbb_winprob_calc/scripts/r", paste0(f, ".R")))

OUT_DIR <- file.path(WINPROB_ROOT, "outputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
FIT_ROWS <- 1200000L

message("loading pure training rows ...")
d <- winprob_frame(load_training_set(pure_only = TRUE))
message("  ", format(nrow(d), big.mark = ","), " rows, ",
        format(dplyr::n_distinct(d$game_id), big.mark = ","), " games, seasons ",
        paste(range(d$season), collapse = "-"))

## The standard split — same seed and same SORTED game list as run_05_cv.R
## and run_05_window.R, so every Phase 5 number is comparable.
set.seed(99)
all_games <- sort(unique(d$game_id))
test_games <- sample(all_games, floor(0.15 * length(all_games)))
test <- d[d$game_id %in% test_games, ]
pool <- d[!d$game_id %in% test_games, ]

fit_d <- winprob_sample(pool, FIT_ROWS, seed = 7)
message("fitting nn = ", WINPROB_NN, " on ", format(nrow(fit_d), big.mark = ","),
        " rows; holding out ", format(nrow(test), big.mark = ","), " ...")
t0 <- Sys.time()
fit <- fit_winprob(fit_d, nn = WINPROB_NN)
message("  fitted in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

saveRDS(list(fit = fit, nn = WINPROB_NN, n = nrow(fit_d),
             seasons = range(d$season), fitted_at = Sys.time()),
        file.path(WINPROB_ROOT, "data/tidy/wbb/winprob_fit.rds"))

p <- predict_winprob(fit, test)
message(sprintf("\nHELD-OUT: log loss %.5f | brier %.5f | base rate %.5f",
                log_loss(test$y, p), brier(test$y, p),
                log_loss(test$y, rep(mean(pool$y), nrow(test)))))

cal <- calibration_table(test$y, p)
readr::write_csv(cal, file.path(OUT_DIR, "phase5_calibration.csv"))
message("\n=== calibration (held out) ===")
print(as.data.frame(cal), digits = 4)

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

## A sanity table for eyeballing: does the surface behave the way a coach
## would expect? Even game, no pre-game edge, ball in hand.
grid <- expand.grid(secs = c(2400, 1200, 600, 300, 120, 60, 30, 10),
                    ball_margin = c(-10, -5, -2, 0, 2, 5, 10)) |>
  mutate(t = sqrt(secs), spread_ball = 0)
grid$wp <- round(predict_winprob(fit, grid), 3)
readr::write_csv(grid, file.path(OUT_DIR, "phase5_surface_check.csv"))
message("\n=== win prob, even matchup, team with the ball ===")
print(as.data.frame(
  grid |> select(secs, ball_margin, wp) |>
    tidyr::pivot_wider(names_from = ball_margin, values_from = wp,
                       names_prefix = "m")), digits = 3)
message("\nTOTAL ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
