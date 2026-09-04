###############################################
# Runner: Midwest Conference (D3 women's), 2025-26
#
#   Rscript ncaa_d3_wbb_run_mwc.R smoke   # ~20 games
#   Rscript ncaa_d3_wbb_run_mwc.R full    # every MWC game
#
# Writes a wehoop-shaped player box CSV that
# "8.16_ncaa_d3_wbb_build_dataset.R" reads in step 1.
###############################################

# The pull script lives next to this one, so find it rather than hardcoding a
# path -- this pair has to run from wherever the folder was unzipped.
PULL <- "8.16_ncaa_d3_wbb_pull.R"
here <- NULL
for (d in c(".", "scripts/r", "02_wbb/d3_player_dataset/scripts/r"))
  if (file.exists(file.path(d, PULL))) { here <- normalizePath(d); break }
if (is.null(here))
  stop("Cannot find ", PULL, " from ", getwd(), call. = FALSE)
source(file.path(here, PULL))

mode <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "smoke"
message("=== MODE: ", mode, " ===")

SEASON <- 2026

MWC_TEAMS <- c("Beloit", "Cornell College", "Grinnell", "Illinois Col.",
               "Knox", "Lake Forest", "Lawrence", "Monmouth (IL)",
               "Ripon", "St. Norbert")

###############################################
# Stage 1: D3 team list -> MWC team ids
###############################################

all_d3 <- get_d3_teams(academic_year = SEASON, division = 3, sport_code = "WBB")
message("Stage 1: D3 women's teams found: ", nrow(all_d3))

mwc <- all_d3 %>% filter(team_display_name %in% MWC_TEAMS)
message("Stage 1: MWC teams matched: ", nrow(mwc), " / ", length(MWC_TEAMS))
print(as.data.frame(mwc))
if (nrow(mwc) != length(MWC_TEAMS)) {
  warning("not all MWC teams matched -- check names against all_d3")
}

session <- ncaa_session()

###############################################
# Stage 2: schedules -> game ids
###############################################

sched <- lapply(mwc$team_id, function(t) {
  ids <- tryCatch(get_team_game_ids(session, t), error = function(e) {
    message("  schedule FAILED ", t, " :: ", conditionMessage(e)); character(0)
  })
  message("  team ", t, ": ", length(ids), " games")
  tibble(team_id = t, game_id = ids)
})
sched <- bind_rows(sched)

game_ids <- unique(sched$game_id)
message("Stage 2: distinct MWC game ids: ", length(game_ids),
        "  (team-game rows: ", nrow(sched), ")")

if (mode == "smoke") {
  set.seed(1)
  game_ids <- head(game_ids, 20)
  message("Stage 2: SMOKE TEST -- limited to ", length(game_ids), " games")
}

###############################################
# Stage 3: box scores
###############################################

label <- paste0("mwc_", mode)
box <- scrape_games(game_ids, session = session, label = label, season = SEASON)
message("Stage 3: box score rows: ", nrow(box),
        " | games returned: ", n_distinct(box$game_id),
        " | teams seen: ", n_distinct(box$team_id))

###############################################
# Stage 4: rosters -> position + season games-started
#
# MWC teams only. The lab dataset covers MWC players, so
# non-conference opponents don't need positions -- their rows
# are kept in the CSV purely so steps 3-4 of the lab script can
# build correct opponent totals, which need no roster data.
###############################################

roster_teams <- intersect(unique(box$team_id[!is.na(box$team_id)]), mwc$team_id)
message("Stage 4: scraping rosters for ", length(roster_teams), " MWC teams")
rosters <- scrape_rosters(roster_teams, session = session, label = label)
message("Stage 4: roster rows: ", nrow(rosters),
        " | with a position: ", sum(!is.na(rosters$athlete_position_abbreviation)))

try(session$close(), silent = TRUE)

###############################################
# Stage 5: assemble wehoop-shaped player box
###############################################

wbb_player_box <- build_player_box(box, rosters)

mwc_rows <- wbb_player_box %>% filter(team_id %in% mwc$team_id)

message("Stage 5: player box rows: ", nrow(wbb_player_box),
        "  (MWC players: ", nrow(mwc_rows), ")")
message("  position missing (MWC): ",
        sum(is.na(mwc_rows$athlete_position_abbreviation)), " of ", nrow(mwc_rows))
message("  any '*' left in position: ",
        any(wbb_player_box$athlete_position_abbreviation == "*", na.rm = TRUE))
message("  games_started_season present (MWC): ",
        sum(!is.na(mwc_rows$games_started_season)), " of ", nrow(mwc_rows))
cat("\nPosition distribution (MWC players):\n")
print(table(mwc_rows$athlete_position_abbreviation, useNA = "ifany"))

# Reconciliation: per team, season games-started should be
# 5 x (games that team played). Anything else means the roster
# join or the schedule pull is off.
cat("\nGames-started reconciliation by MWC team:\n")
recon <- mwc_rows %>%
  group_by(team_id, team_display_name) %>%
  summarise(
    games = n_distinct(game_id),
    gs_total = sum(distinct(pick(athlete_id, games_started_season))$games_started_season,
                   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(expected = 5 * games, diff = gs_total - expected)
print(as.data.frame(recon))

out_file <- file.path(path.expand(data_dir),
                      paste0("ncaa_d3_wbb_player_box_", SEASON, "_", label, ".csv"))
write_csv(wbb_player_box, out_file)
message("\nWrote: ", out_file)
