## ============================================================
## run_03_build.R — build the cached Phase 3 rating table
## ------------------------------------------------------------
## Writes data/tidy/wbb/team_ratings_2017_2026.csv, the file Phase 4 reads
## to attach `spread_home`/`spread_ball` to every training row. Rerun this
## whenever RATING_LAMBDA or CARRYOVER_DECAY changes — and rerun Phase 4
## afterwards, because the spread is baked into the training parquets.
##
## Reuses the games frame cached by run_03_tune.R when it is present, so a
## rebuild after a re-tune does not re-download ten seasons of box scores.
##
##   Rscript scripts/r/run_03_build.R
## ============================================================

suppressMessages({library(dplyr)})

R_HOME_DIR <- Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics")
source(file.path(R_HOME_DIR, "12_wbb_winprob_calc/scripts/r/03_team_ratings.R"))

SEASONS <- 2017:2026
CACHE   <- file.path(WINPROB_ROOT, "data/tidy/wbb/rating_games_2017_2026.rds")
OUT     <- file.path(WINPROB_ROOT, "data/tidy/wbb/team_ratings_2017_2026.csv")

if (file.exists(CACHE)) {
  games <- readRDS(CACHE)
  message("games from cache: ", format(nrow(games), big.mark = ","))
} else {
  box <- suppressWarnings(wehoop::load_wbb_team_box(seasons = SEASONS))
  sch <- suppressWarnings(wehoop::load_wbb_schedule(seasons = SEASONS))
  games <- game_frame(team_game_efficiency(box), sch)
  games$season <- box$season[match(games$game_id, as.character(box$game_id))]
  games <- games[!is.na(games$season) & !is.na(games$home_net) & !is.na(games$pace), ]
  rm(box, sch); invisible(gc(FALSE))
  saveRDS(games, CACHE)
  message("games built: ", format(nrow(games), big.mark = ","))
}

message("building ratings at lambda ", RATING_LAMBDA, ", decay ", CARRYOVER_DECAY, " ...")
t0 <- Sys.time()
tb <- build_rating_table(games, method = "adjusted",
                         lambda = RATING_LAMBDA, decay = CARRYOVER_DECAY)
readr::write_csv(tb, OUT)
message(sprintf("wrote %s  (%s rows, %d teams, %d dates, %.1f min)",
                basename(OUT), format(nrow(tb), big.mark = ","),
                dplyr::n_distinct(tb$team), dplyr::n_distinct(tb$date),
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))

## ---- validate what was just written --------------------------------
tkey <- paste(tb$season, tb$date, tb$team)
rat  <- stats::setNames(tb$rating,  tkey)
ngm  <- stats::setNames(tb$n_games, tkey)
dk   <- paste(tb$season, tb$date); f <- !duplicated(dk)
ht   <- stats::setNames(tb$home_term[f], dk[f])
nt   <- stats::setNames(tb$neutral_term[f], dk[f])

gd <- paste(games$season, games$date)
rh <- unname(rat[paste(gd, games$home)]); ra <- unname(rat[paste(gd, games$away)])
nh <- unname(ngm[paste(gd, games$home)]); na_ <- unname(ngm[paste(gd, games$away)])
pred <- (rh - ra + ifelse(games$neutral, unname(nt[gd]), unname(ht[gd]))) *
  games$pace / 100
actual <- games$home_net * games$pace / 100
k <- !is.na(pred) & !is.na(nh) & !is.na(na_) & nh >= 1 & na_ >= 1

fit <- stats::lm(actual[k] ~ pred[k])
message(sprintf("\nscored %s games | RMSE %.2f (zero baseline %.2f) | cor %.4f | slope %.4f",
                format(sum(k), big.mark = ","),
                sqrt(mean((actual[k] - pred[k])^2)),
                sqrt(mean(actual[k]^2)),
                stats::cor(actual[k], pred[k]), unname(coef(fit)[2])))
message("spread percentiles (pts): ",
        paste(sprintf("%s=%.1f", c("1%","10%","50%","90%","99%"),
                      stats::quantile(pred[k], c(.01,.1,.5,.9,.99))), collapse = "  "))
