# example_csv_workflow.R ------------------------------------------------------
#
# A runnable tour of the pipeline. Start here.
#
#   Rscript 03_nba/scripts/r/example_csv_workflow.R
#
# It walks three paths, in the order you are likely to need them:
#
#   PART 1  pull from hoopR (what you'd do with no data supplied)
#   PART 2  write a CSV, then read it back and get identical numbers
#           -- this is the "they handed me a CSV" case
#   PART 3  a CSV with completely different column names
#
# Nothing here writes into your project except the demo CSVs, which land in
# 03_nba/data/ and are safe to delete.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# Locate our own files regardless of where this was launched from -- repo root,
# 03_nba/scripts/r, or a VS Code session with some other working directory.
find_script_dir <- function() {
  candidates <- c(".", "scripts/r", "../scripts/r", "../r",
                  "03_nba/metrics_layer/scripts/r", "03_nba/scripts/r")
  for (d in candidates) {
    if (file.exists(file.path(d, "nba_metrics.R"))) return(normalizePath(d))
  }
  stop("Cannot find nba_metrics.R. Working directory is: ", getwd(),
       "\nRun from the project root, or from 03_nba/scripts/r.", call. = FALSE)
}

script_dir <- find_script_dir()
data_dir   <- normalizePath(file.path(script_dir, "..", "..", "demo_data"),
                            mustWork = FALSE)

source(file.path(script_dir, "nba_metrics.R"))
source(file.path(script_dir, "nba_data.R"))

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

rule <- function(x) cat("\n", strrep("-", 78), "\n", x, "\n", sep = "")

cat("working directory :", getwd(), "\n")
cat("scripts           :", script_dir, "\n")
cat("data output       :", data_dir, "\n")


# PART 1 ----------------------------------------------------------------------
# The three-step shape of every analysis: fetch -> compute -> present.

rule("PART 1: straight from hoopR")

games <- nba_team_games(seasons = 2025, season_type = "regular")
cat("per-game rows:", nrow(games), " (30 teams x 82 games x 1 row each)\n")

season <- games %>% nba_aggregate(team_id, team_display_name)

season %>%
  arrange(desc(net_rtg)) %>%
  transmute(
    team    = team_display_name,
    g       = games,
    ortg    = round(ortg, 1),
    drtg    = round(drtg, 1),
    net     = round(net_rtg, 1),
    pace    = round(pace, 1),
    `efg%`  = round(100 * efg, 1),
    `tov%`  = round(100 * tov_pct, 1),
    `orb%`  = round(100 * orb_pct, 1),
    ftr     = round(100 * ft_rate, 1),
    `ts%`   = round(100 * ts_pct, 1)
  ) %>%
  head(10) %>%
  as.data.frame() %>%
  print(row.names = FALSE)


# PART 2 ----------------------------------------------------------------------
# The take-home case. We export a plain CSV, forget it came from hoopR, and run
# the identical pipeline over it. The point: nothing downstream knows or cares
# where the rows came from.

rule("PART 2: same analysis, but starting from a CSV")

csv_path <- file.path(data_dir, "example_team_games.csv")

# One row per team per game -- the shape almost any box-score export arrives in.
nba_team_box(seasons = 2025, season_type = "regular") %>%
  transmute(
    game_id, team_id, team_display_name, game_date,
    pts  = team_score,
    fga  = field_goals_attempted,
    fgm  = field_goals_made,
    fg3m = three_point_field_goals_made,
    fta  = free_throws_attempted,
    oreb = offensive_rebounds,
    dreb = defensive_rebounds,
    tov  = total_turnovers
  ) %>%
  write_csv(csv_path)

cat("wrote", csv_path, "\n\n")

from_csv <- read_csv(csv_path, show_col_types = FALSE) %>%
  nba_pair_opponents() %>%   # attach each team's opponent
  nba_add_metrics() %>%      # possessions, ratings, four factors
  nba_aggregate(team_id, team_display_name)

# Do the CSV numbers match the hoopR numbers? They must.
check <- season %>%
  select(team_id, ortg_api = ortg, efg_api = efg, net_api = net_rtg) %>%
  inner_join(
    from_csv %>% select(team_id, ortg_csv = ortg, efg_csv = efg, net_csv = net_rtg),
    by = "team_id"
  )

cat("teams compared      :", nrow(check), "\n")
cat("max ORtg difference :", max(abs(check$ortg_api - check$ortg_csv)), "\n")
cat("max eFG  difference :", max(abs(check$efg_api  - check$efg_csv)),  "\n")
cat("max Net  difference :", max(abs(check$net_api  - check$net_csv)),  "\n")
cat("\nNote: pace is absent here -- it needs game length, which this CSV\n")
cat("does not carry. Everything else is identical.\n")


# PART 3 ----------------------------------------------------------------------
# A CSV whose columns are named nothing like ours. This is the realistic case.

rule("PART 3: a CSV with unfamiliar column names")

odd_path <- file.path(data_dir, "example_odd_names.csv")

read_csv(csv_path, show_col_types = FALSE) %>%
  rename(
    Contest = game_id, Squad = team_id, Points = pts,
    ShotsTaken = fga, ShotsMade = fgm, Threes = fg3m,
    FreeThrowAtt = fta, OffBoards = oreb, DefBoards = dreb, Giveaways = tov
  ) %>%
  write_csv(odd_path)

odd <- read_csv(odd_path, show_col_types = FALSE)
cat("incoming columns:", paste(names(odd), collapse = ", "), "\n\n")

# Map canonical name = their name. Names are lower-cased first, so the mapping
# uses lower case on the right-hand side.
mapped <- odd %>%
  nba_standardize(c(
    game_id = "contest",   team_id = "squad",      pts  = "points",
    fga     = "shotstaken", fgm    = "shotsmade",  fg3m = "threes",
    fta     = "freethrowatt", oreb = "offboards",  dreb = "defboards",
    tov     = "giveaways"
  )) %>%
  nba_pair_opponents() %>%
  nba_add_metrics() %>%
  nba_aggregate(team_id)

cat("rows out:", nrow(mapped), "  league ORtg:", round(mean(mapped$ortg), 2), "\n")

# And what a missing column looks like -- it fails loudly, naming the problem.
rule("PART 4: what happens when a column is missing")
full_mapping <- c(
  game_id = "contest",   team_id = "squad",      pts  = "points",
  fga     = "shotstaken", fgm    = "shotsmade",  fg3m = "threes",
  fta     = "freethrowatt", oreb = "offboards",  dreb = "defboards",
  tov     = "giveaways"
)
broken <- odd %>% select(-Giveaways)   # turnovers absent from the file
res <- try(nba_standardize(broken, full_mapping), silent = TRUE)
cat(as.character(res))

rule("done")
