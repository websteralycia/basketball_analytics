## Runner for Phase 4. Season at a time, writes as it goes.
##   Rscript scripts/r/run_04_build.R
suppressMessages({library(dplyr)})
R <- "~/Desktop/Basketball Analytics"
source(file.path(R, "11_gameprep_project/source_all.R"))
for (f in c("01_possession_state","02_game_states","03_team_ratings","04_training_set"))
  source(file.path(R, "12_wbb_winprob_calc/scripts/r", paste0(f, ".R")))

message("building Phase 3 game frame ...")
box <- suppressWarnings(wehoop::load_wbb_team_box(seasons = TRAIN_SEASONS))
sch <- suppressWarnings(wehoop::load_wbb_schedule(seasons = TRAIN_SEASONS))
games <- game_frame(team_game_efficiency(box), sch)
games$season <- box$season[match(games$game_id, as.character(box$game_id))]
games <- games[!is.na(games$season) & !is.na(games$home_net) & !is.na(games$pace), ]
rm(box, sch); invisible(gc(FALSE))

ratings <- readr::read_csv(
  file.path(WINPROB_ROOT, "data/tidy/wbb/team_ratings_2017_2026.csv"),
  show_col_types = FALSE)
message("games ", nrow(games), " | ratings ", nrow(ratings))

## SKIP_EXISTING=1 resumes a run interrupted since the last Phase 3 re-tune.
## Do NOT set it after re-tuning — see build_training_set().
t0 <- Sys.time()
invisible(build_training_set(TRAIN_SEASONS, games, ratings, cores = 3L,
                             skip_existing = nzchar(Sys.getenv("SKIP_EXISTING"))))
message("TOTAL ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
