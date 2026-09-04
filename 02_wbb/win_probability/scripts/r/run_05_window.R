## ============================================================
## run_05_window.R — choose nn on LATE-GAME accuracy, not global log loss
## ------------------------------------------------------------
## run_05_cv.R picks nn by log loss over all states, and that metric turned
## out not to discriminate: above nn 0.2 the curve is flat to within 0.0014
## while the fold-to-fold SD is 0.019, so its argmin (0.5, the top of the
## grid) is noise choosing a winner.
##
## It is also the wrong question. Global log loss is dominated by the ~87%
## of states with more than five minutes left, where every window looks
## alike. nn is the FRACTION of data in each local fit, so a large value
## smooths hardest exactly where the probability surface is sharpest — the
## last minute — and the global metric barely notices.
##
## So: fit the candidates on identical data and compare them BY TIME
## REMAINING on the held-out games. Also surfaces the warnings CV emitted.
##
##   Rscript scripts/r/run_05_window.R
## ============================================================

suppressMessages({library(dplyr); library(arrow)})

R_HOME_DIR <- Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics")
for (f in c("03_team_ratings", "04_training_set", "05_winprob_model"))
  source(file.path(R_HOME_DIR, "12_wbb_winprob_calc/scripts/r", paste0(f, ".R")))

OUT_DIR <- file.path(WINPROB_ROOT, "outputs")
CANDIDATES <- c(0.1, 0.2, 0.5)
FIT_ROWS <- 1200000L

d <- winprob_frame(load_training_set(pure_only = TRUE))

# The SAME split run_05_cv.R used, so this is comparable and still honest.
set.seed(99)
all_games <- sort(unique(d$game_id))   # sorted: arrow row order is not stable
test_games <- sample(all_games, floor(0.15 * length(all_games)))
test <- d[d$game_id %in% test_games, ]
pool <- d[!d$game_id %in% test_games, ]
fit_d <- winprob_sample(pool, FIT_ROWS, seed = 7)
message("fitting on ", format(nrow(fit_d), big.mark = ","), " rows; testing on ",
        format(nrow(test), big.mark = ","))

res <- list(); overall <- list()
for (nn in CANDIDATES) {
  message("  nn ", nn, " ...")
  w <- withCallingHandlers(
    {
      fit <- fit_winprob(fit_d, nn = nn)
      NULL
    },
    warning = function(cond) {
      assign("last_warn", conditionMessage(cond), envir = globalenv())
      invokeRestart("muffleWarning")
    })
  eg <- endgame_report(test, fit)
  eg$nn <- nn
  res[[length(res) + 1]] <- eg
  p <- predict_winprob(fit, test)
  overall[[length(overall) + 1]] <- tibble::tibble(
    nn = nn, log_loss = log_loss(test$y, p), brier = brier(test$y, p))
}

eg <- bind_rows(res)
readr::write_csv(eg, file.path(OUT_DIR, "phase5_window_by_time.csv"))

message("\n=== held-out log loss by time remaining ===")
wide <- eg |>
  select(bucket, n, nn, log_loss) |>
  tidyr::pivot_wider(names_from = nn, values_from = log_loss, names_prefix = "nn_")
naive <- eg |> filter(nn == CANDIDATES[1]) |> select(bucket, naive = naive_log_loss)
print(as.data.frame(left_join(wide, naive, by = "bucket")), digits = 4)

message("\n=== overall ===")
print(as.data.frame(bind_rows(overall)), digits = 5)

if (exists("last_warn")) message("\nexample warning from fitting: ", last_warn)
