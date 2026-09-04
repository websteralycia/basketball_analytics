## ============================================================
## run_06_prep.R — assets the calculator app loads at startup
## ------------------------------------------------------------
## The full ratings table is 665,458 rows / 58MB because it carries a
## rating for every team on every date. The app needs one date and a name
## for each team — 663 rows. Writing that snapshot keeps app startup
## instant and means deploying the app does not mean shipping 58MB.
##
## Rerun after any Phase 3 rebuild, or when a new season's games land.
##
##   Rscript scripts/r/run_06_prep.R
## ============================================================

suppressMessages({library(dplyr)})

R_HOME_DIR <- Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics")
for (f in c("03_team_ratings", "04_training_set", "05_winprob_model", "06_winprob_calc"))
  source(file.path(R_HOME_DIR, "12_wbb_winprob_calc/scripts/r", paste0(f, ".R")))

ratings <- readr::read_csv(
  file.path(WINPROB_ROOT, "data/tidy/wbb/team_ratings_2017_2026.csv"),
  show_col_types = FALSE)

## Team pace, so a matchup can convert its rating edge at the tempo those
## two teams actually play at rather than at a league-average constant. The
## rating is per 100 possessions; the spread is points; pace is the bridge.
## Effect is modest — team pace runs 61 to 81 but the middle 80% sits
## between 67 and 75 — so this is a correctness fix, not a big mover.
PACE_SEASON  <- 2026
PACE_MIN_GAMES <- 15L

box  <- suppressWarnings(wehoop::load_wbb_team_box(seasons = PACE_SEASON))
pace <- team_game_efficiency(box) |>
  group_by(team) |>
  summarise(pace = mean(pace), pace_games = n(), .groups = "drop") |>
  filter(pace_games >= PACE_MIN_GAMES)

board <- rating_board(ratings) |>
  transmute(team_id = team, display, short, rating, n_games,
            home_term, neutral_term, as_of = date) |>
  left_join(pace, by = c("team_id" = "team")) |>
  # A team with too few games keeps the league default rather than a pace
  # estimated off five games.
  mutate(pace = ifelse(is.na(pace), DEFAULT_PACE, round(pace, 1)))

out <- file.path(WINPROB_ROOT, "data/tidy/wbb/rating_board.csv")
readr::write_csv(board, out)

message(sprintf("wrote %s — %d teams, as of %s",
                basename(out), nrow(board), format(board$as_of[1])))
message("top 5: ", paste(head(board$display, 5), collapse = ", "))
message(sprintf("pace: %d teams measured, %d on the default; range %.1f-%.1f",
                sum(!is.na(board$pace_games)), sum(is.na(board$pace_games)),
                min(board$pace), max(board$pace)))
