# nba_cookbook.R --------------------------------------------------------------
#
# Worked examples for the two metric modules. Run it and read along:
#
#   Rscript 03_nba/scripts/r/nba_cookbook.R
#
# THE MENTAL MODEL
#
#   nba_data.R           fetching. Talks to hoopR. The only file that
#                        touches the network.
#
#   nba_metrics.R        TEAM metrics. Pure functions, one row per team-game.
#                        Possessions, ORtg/DRtg/Net, Four Factors, TS%, Pace.
#
#   nba_player_metrics.R PLAYER metrics. One row per player-game in, one row
#                        per player-season out. Usage, per-36, AST%/REB%,
#                        percentiles within position.
#
# The two metric files are INDEPENDENT -- nba_player_metrics.R does not use
# anything from nba_metrics.R. You can source either on its own.
#
# Which one you need is decided by your input:
#   a TEAM box score   (one row per team per game)   -> nba_metrics.R
#   a PLAYER box score (one row per player per game) -> nba_player_metrics.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(hoopR)
})

find_script_dir <- function() {
  for (d in c(".", "scripts/r", "../scripts/r", "../r",
              "03_nba/metrics_layer/scripts/r", "03_nba/scripts/r")) {
    if (file.exists(file.path(d, "nba_metrics.R"))) return(normalizePath(d))
  }
  stop("Cannot find nba_metrics.R from ", getwd(), call. = FALSE)
}
sd_ <- find_script_dir()
source(file.path(sd_, "nba_metrics.R"))
source(file.path(sd_, "nba_data.R"))
source(file.path(sd_, "nba_player_metrics.R"))

rule <- function(x) cat("\n", strrep("=", 76), "\n", x, "\n", strrep("=", 76), "\n", sep = "")
SEASON <- 2025


# =============================================================================
# PART A -- TEAM METRICS  (nba_metrics.R + nba_data.R)
# =============================================================================

rule("A1. The shortcut: everything in one call")
# nba_team_games() = fetch + filter All-Star/Cup + pair opponents + add metrics.
# Use this 90% of the time.
g <- nba_team_games(seasons = SEASON, season_type = "regular")
cat("rows:", nrow(g), " (one per team per game)\n")
cat("metric columns added:",
    paste(intersect(c("poss","ortg","drtg","net_rtg","efg","tov_pct",
                      "orb_pct","ft_rate","ts_pct","pace"), names(g)),
          collapse = ", "), "\n")

rule("A2. Season totals: nba_aggregate()")
# Sums the counting stats, THEN recomputes the rates. Never averages rates.
# Group by whatever you want -- it does not have to be team.
season <- g %>% nba_aggregate(team_id, team_display_name)
season %>% arrange(desc(net_rtg)) %>%
  transmute(team = team_display_name, g = games, ortg = round(ortg,1),
            drtg = round(drtg,1), net = round(net_rtg,1)) %>%
  head(5) %>% as.data.frame() %>% print(row.names = FALSE)

rule("A3. Group by anything: home vs away")
# The same aggregate, grouped differently. This is why nba_aggregate takes ...
g %>%
  nba_aggregate(team_home_away) %>%
  transmute(side = team_home_away, games,
            ortg = round(ortg,1), drtg = round(drtg,1), net = round(net_rtg,1)) %>%
  as.data.frame() %>% print(row.names = FALSE)

rule("A4. Filter first, aggregate second")
# Any dplyr filter works before aggregating -- last 20 games, a date window,
# one opponent, whatever the question needs.
g %>%
  group_by(team_id) %>% arrange(desc(game_date), .by_group = TRUE) %>%
  slice_head(n = 20) %>% ungroup() %>%
  nba_aggregate(team_id, team_display_name) %>%
  arrange(desc(net_rtg)) %>%
  transmute(team = team_display_name, last20_net = round(net_rtg,1)) %>%
  head(3) %>% as.data.frame() %>% print(row.names = FALSE)

rule("A5. The scalar functions work on plain numbers")
# Useful for a quick check, a unit test, or a hand calculation in a take-home.
cat("nba_possessions(fga=88, fta=20, tov=13, oreb=10) =",
    nba_possessions(88, 20, 13, 10), "\n")
cat("nba_efg(fgm=42, fg3m=14, fga=88)                 =",
    round(nba_efg(42, 14, 88), 4), "\n")
cat("nba_ts_pct(pts=110, fga=88, fta=20)              =",
    round(nba_ts_pct(110, 88, 20), 4), "\n")
cat("nba_orb_pct(oreb=10, opp_dreb=34)                =",
    round(nba_orb_pct(10, 34), 4), "\n")
cat("nba_pace(poss=100, opp_poss=100, game_minutes=48)=",
    nba_pace(100, 100, 48), "\n")

rule("A6. Starting from your own table (the take-home case)")
# The three verbs nba_team_games() calls for you. Use them when the data did
# not come from hoopR.
my_csv <- g %>%                       # pretend this was read_csv("theirs.csv")
  select(game_id, team_id, team_display_name,
         pts, fga, fgm, fg3m, fta, oreb, dreb, tov)

manual <- my_csv %>%
  nba_standardize() %>%      # lower-case names, check the contract, stop if short
  nba_pair_opponents() %>%   # attach each team's opponent as opp_* columns
  nba_add_metrics() %>%      # possessions, ratings, four factors
  nba_aggregate(team_id, team_display_name)

cat("matches the hoopR path exactly:",
    isTRUE(all.equal(sort(round(manual$ortg, 6)),
                     sort(round(season$ortg, 6)))), "\n")
cat("(pace is absent here -- it needs game length, which a bare CSV lacks)\n")

rule("A7. Renaming columns that do not match")
# canonical = "their name". Names are lower-cased first.
odd <- my_csv %>% rename(Giveaways = tov, Points = pts)
ok <- odd %>%
  nba_standardize(c(tov = "giveaways", pts = "points")) %>%
  nba_pair_opponents() %>% nba_add_metrics()
cat("rows out:", nrow(ok), " league ORtg:", round(mean(ok$ortg), 2), "\n")


# =============================================================================
# PART B -- PLAYER METRICS  (nba_player_metrics.R)
# =============================================================================

rule("B1. The shortcut: one call from a player box")
pb <- load_nba_player_box(seasons = SEASON) %>% filter(season_type == 2)
pb <- nba_drop_nonstandard(pb)        # All-Star / Cup final -- see nba_data.R

ps <- nba_player_season(pb)
cat("player-seasons:", nrow(ps), "\n")
cat("positions:", paste(names(table(ps$position)), table(ps$position),
                        collapse = "  "), "\n")

rule("B2. What you get back")
ps %>% filter(games_played >= 40) %>% arrange(desc(usage)) %>%
  transmute(player = athlete_display_name, pos = position,
            mpg = round(mpg,1), pts36 = round(pts_per_36,1),
            `usg%` = round(100*usage,1), `ts%` = round(100*ts_pct,1),
            `ast%` = round(100*ast_pct,1),
            usg_pctile = round(100*usage_pctile_pos)) %>%
  head(5) %>% as.data.frame() %>% print(row.names = FALSE)

rule("B3. Percentiles are WITHIN position, and weighted")
# Weighted by sample size, so four lucky threes do not outrank a season of them.
# The *_pctile_pos columns always compare a player to his own position group.
ps %>% filter(games_played >= 40) %>%
  group_by(position) %>%
  summarise(players = n(),
            median_ts = round(100*median(ts_pct, na.rm = TRUE), 1),
            median_usg = round(100*median(usage, na.rm = TRUE), 1),
            .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

rule("B4. Turning off the position collapse")
# ESPN also emits PG/SG/SF/PF but with only 7-10 players each -- percentiles
# inside those are noise. TRUE (the default) collapses to G/F/C.
raw_pos <- nba_player_season(pb, collapse_positions = FALSE,
                             include_pos_avgs = FALSE)
cat("collapsed :", paste(sort(unique(ps$position)), collapse = " "), "\n")
cat("raw       :", paste(sort(unique(raw_pos$position)), collapse = " "), "\n")
cat("group sizes raw:", paste(table(raw_pos$position), collapse = " "), "\n")

rule("B5. The helpers are usable on their own")
# weighted_percentile() and weighted_mean_safe() work on any vector pair.
x <- c(0.60, 0.55, 0.50, 0.45); w <- c(500, 500, 500, 5)
cat("unweighted rank of the 4-attempt player:",
    round(rank(x)[4] / length(x), 2), "\n")
cat("weighted percentile (w = attempts)     :",
    round(weighted_percentile(x, w)[4], 3), "  <- barely moves the distribution\n")
cat("weighted_mean_safe(x, w)               :",
    round(weighted_mean_safe(x, w), 4), "\n")

rule("B6. The intermediate steps, if you need them")
# nba_player_season() calls these in order. Reach for them when you want team
# totals derived from a player box, or the cleaned box itself.
clean <- nba_clean_player_box(pb)
tt    <- nba_team_totals_from_player_box(clean)
cat("cleaned player-game rows :", nrow(clean), "\n")
cat("team-game rows           :", nrow(tt$team_game), "\n")
cat("team-season rows         :", nrow(tt$team_season), "\n")
cat("names(tt):", paste(names(tt), collapse = ", "), "\n")

rule("Done")
cat("Team question  -> nba_metrics.R   (team box: one row per team per game)\n")
cat("Player question-> nba_player_metrics.R (player box: one row per player)\n")
cat("Fetching       -> nba_data.R\n")
