###############################################
# WNBA Player Pizza Plot (Dark Theme)
# Date: 2026 Season
# Source: wehoop (ESPN)
# Note: all "*_pct" and `usage` are decimals (0-1)
#       *_pctile_pos columns are weighted percentiles
#       within position groups.
#       *_pos_avg = weighted mean of raw metric at position
#       *_pos_avg_pctile = where that avg sits in the
#       weighted percentile distribution (reference ring)
#       Per-36 rates used throughout (WNBA standard).
###############################################

# 0. Install packages (run once if needed)
# install.packages("wehoop")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("cowplot")
# install.packages("magick")

library(wehoop)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(cowplot)
library(magick)

###############################################
# Helper: weighted percentile function
###############################################

weighted_percentile <- function(x, w) {
  if (length(x) == 0) return(numeric(0))

  w[is.na(w)] <- 0
  x_na <- is.na(x)

  if (all(x_na) || sum(w[!x_na]) <= 0) {
    return(rep(NA_real_, length(x)))
  }

  x2 <- x[!x_na]
  w2 <- w[!x_na]

  ord      <- order(x2)
  w_sorted <- w2[ord]
  cw       <- cumsum(w_sorted)
  total_w  <- sum(w_sorted)

  p_sorted        <- cw / total_w
  p2              <- numeric(length(x2))
  p2[ord]         <- p_sorted

  out         <- rep(NA_real_, length(x))
  out[!x_na]  <- p2
  out
}

# Helper: weighted mean (ignores NAs, weights NA -> 0)
weighted_mean_safe <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (sum(keep) == 0) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

###############################################
# Config: set player here (used throughout)
###############################################

main_player <- "Olivia Miles"
player_team <- "Minnesota Lynx"

# What to print after the team name in the title. This is derived from the data
# that actually went into the chart, so it can't go stale as the season runs on.
#   "date"   -> "Aug 30, 2026"                      (default; keeps the title short)
#   "games"  -> "39 Games"
#   "asof"   -> "As of Aug 30, 2026"
#   "both"   -> "39 Games | Through Aug 30, 2026"
#   "season" -> "2026 Season"
# Set PERIOD_TEXT to a string of your own to override it entirely, e.g.
# PERIOD_TEXT <- "First 10 Games" if you deliberately want a fixed window.
PERIOD_LABEL <- "date"
PERIOD_TEXT  <- NULL

###############################################
# 1. Pull ALL game-level player box data (2026)
###############################################

wnba_player_box_2026 <- load_wnba_player_box(seasons = 2026)

###############################################
# 2. Clean out DNPs / no-minutes rows
###############################################

wnba_player_box_2026_clean <- wnba_player_box_2026 %>%
  filter(
    !isTRUE(did_not_play),
    !is.na(minutes),
    minutes > 0
  )

# Uncomment to restrict to regular season only:
# wnba_player_box_2026_clean <- wnba_player_box_2026_clean %>%
#   filter(season_type == 2)

###############################################
# 3. Team-game totals (for team + opponent season stats)
###############################################

team_game_totals_2026 <- wnba_player_box_2026_clean %>%
  group_by(season, game_id, team_id) %>%
  summarise(
    team_minutes = sum(minutes, na.rm = TRUE),
    team_fgm     = sum(field_goals_made, na.rm = TRUE),
    team_fga     = sum(field_goals_attempted, na.rm = TRUE),
    team_fg3a    = sum(three_point_field_goals_attempted, na.rm = TRUE),
    team_ftm     = sum(free_throws_made, na.rm = TRUE),
    team_fta     = sum(free_throws_attempted, na.rm = TRUE),
    team_oreb    = sum(offensive_rebounds, na.rm = TRUE),
    team_dreb    = sum(defensive_rebounds, na.rm = TRUE),
    team_reb     = sum(rebounds, na.rm = TRUE),
    team_tov     = sum(turnovers, na.rm = TRUE),
    .groups = "drop"
  )

team_game_w_opp_2026 <- team_game_totals_2026 %>%
  left_join(
    team_game_totals_2026,
    by = c("season", "game_id"),
    suffix = c("", "_opp"),
    relationship = "many-to-many"
  ) %>%
  filter(team_id != team_id_opp)

###############################################
# 4. Team-season totals (team + opponent)
###############################################

team_season_totals_2026 <- team_game_w_opp_2026 %>%
  group_by(season, team_id) %>%
  summarise(
    team_minutes_total = sum(team_minutes, na.rm = TRUE),
    team_fgm_total     = sum(team_fgm, na.rm = TRUE),
    team_fga_total     = sum(team_fga, na.rm = TRUE),
    team_fg3a_total    = sum(team_fg3a, na.rm = TRUE),
    team_ftm_total     = sum(team_ftm, na.rm = TRUE),
    team_fta_total     = sum(team_fta, na.rm = TRUE),
    team_oreb_total    = sum(team_oreb, na.rm = TRUE),
    team_dreb_total    = sum(team_dreb, na.rm = TRUE),
    team_reb_total     = sum(team_reb, na.rm = TRUE),
    team_tov_total     = sum(team_tov, na.rm = TRUE),
    opp_oreb_total     = sum(team_oreb_opp, na.rm = TRUE),
    opp_dreb_total     = sum(team_dreb_opp, na.rm = TRUE),
    opp_reb_total      = sum(team_reb_opp, na.rm = TRUE),
    .groups = "drop"
  )

###############################################
# 5. Player-season totals + advanced metrics
###############################################

wnba_player_season_2026 <- wnba_player_box_2026_clean %>%
  arrange(game_date) %>%
  group_by(season, athlete_id) %>%
  summarise(
    athlete_display_name          = last(athlete_display_name),
    athlete_position_abbreviation = last(athlete_position_abbreviation),
    team_id                       = last(team_id),
    team_short_display_name       = last(team_short_display_name),
    games_played    = n_distinct(game_id),
    first_game_date = min(game_date, na.rm = TRUE),
    last_game_date  = max(game_date, na.rm = TRUE),
    games_started = sum(starter, na.rm = TRUE),
    minutes_total = sum(minutes, na.rm = TRUE),
    pts_total     = sum(points, na.rm = TRUE),
    fgm_total     = sum(field_goals_made, na.rm = TRUE),
    fga_total     = sum(field_goals_attempted, na.rm = TRUE),
    fg3m_total    = sum(three_point_field_goals_made, na.rm = TRUE),
    fg3a_total    = sum(three_point_field_goals_attempted, na.rm = TRUE),
    ftm_total     = sum(free_throws_made, na.rm = TRUE),
    fta_total     = sum(free_throws_attempted, na.rm = TRUE),
    oreb_total    = sum(offensive_rebounds, na.rm = TRUE),
    dreb_total    = sum(defensive_rebounds, na.rm = TRUE),
    reb_total     = sum(rebounds, na.rm = TRUE),
    ast_total     = sum(assists, na.rm = TRUE),
    stl_total     = sum(steals, na.rm = TRUE),
    blk_total     = sum(blocks, na.rm = TRUE),
    tov_total     = sum(turnovers, na.rm = TRUE),
    pf_total      = sum(fouls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(team_season_totals_2026, by = c("season", "team_id")) %>%
  mutate(
    # Per-game
    mpg     = minutes_total / games_played,
    ppg     = pts_total / games_played,
    rpg     = reb_total / games_played,
    apg     = ast_total / games_played,
    spg     = stl_total / games_played,
    bpg     = blk_total / games_played,
    tovpg   = tov_total / games_played,
    oreb_pg = oreb_total / games_played,
    dreb_pg = dreb_total / games_played,

    # Shooting percentages
    fg_pct  = if_else(fga_total  > 0, fgm_total  / fga_total,  NA_real_),
    fg3_pct = if_else(fg3a_total > 0, fg3m_total / fg3a_total, NA_real_),
    ft_pct  = if_else(fta_total  > 0, ftm_total  / fta_total,  NA_real_),

    # Advanced shooting
    efg_pct = if_else(
      fga_total > 0,
      (fgm_total + 0.5 * fg3m_total) / fga_total,
      NA_real_
    ),
    ts_pct = if_else(
      (fga_total + 0.44 * fta_total) > 0,
      pts_total / (2 * (fga_total + 0.44 * fta_total)),
      NA_real_
    ),

    # Per-36 rates
    pts_per_36  = if_else(minutes_total > 0, pts_total  * 36 / minutes_total, NA_real_),
    reb_per_36  = if_else(minutes_total > 0, reb_total  * 36 / minutes_total, NA_real_),
    ast_per_36  = if_else(minutes_total > 0, ast_total  * 36 / minutes_total, NA_real_),
    stl_per_36  = if_else(minutes_total > 0, stl_total  * 36 / minutes_total, NA_real_),
    blk_per_36  = if_else(minutes_total > 0, blk_total  * 36 / minutes_total, NA_real_),
    tov_per_36  = if_else(minutes_total > 0, tov_total  * 36 / minutes_total, NA_real_),
    oreb_per_36 = if_else(minutes_total > 0, oreb_total * 36 / minutes_total, NA_real_),
    dreb_per_36 = if_else(minutes_total > 0, dreb_total * 36 / minutes_total, NA_real_),

    # Usage
    usg_weight = pmax(fga_total + 0.44 * fta_total + tov_total, 0),
    usage = if_else(
      minutes_total > 0 &
        (team_fga_total + 0.44 * team_fta_total + team_tov_total) > 0,
      ((fga_total + 0.44 * fta_total + tov_total) * (team_minutes_total / 5)) /
        (minutes_total * (team_fga_total + 0.44 * team_fta_total + team_tov_total)),
      NA_real_
    ),

    # Assist %
    ast_denom = (
      (minutes_total / (team_minutes_total / 5)) * team_fgm_total - fgm_total
    ),
    ast_pct = if_else(
      minutes_total > 0 & team_minutes_total > 0 & ast_denom > 0,
      ast_total / ast_denom,
      NA_real_
    ),

    # Turnover %
    tov_denom = fga_total + 0.44 * fta_total + tov_total,
    tov_pct   = if_else(tov_denom > 0, tov_total / tov_denom, NA_real_),

    # Offensive Rebound %
    oreb_chances = (minutes_total / (team_minutes_total / 5)) *
      (team_oreb_total + opp_dreb_total),
    oreb_pct = if_else(
      minutes_total > 0 & team_minutes_total > 0 &
        (team_oreb_total + opp_dreb_total) > 0 & oreb_chances > 0,
      oreb_total / oreb_chances,
      NA_real_
    ),

    # Defensive Rebound %
    dreb_chances = (minutes_total / (team_minutes_total / 5)) *
      (team_dreb_total + opp_oreb_total),
    dreb_pct = if_else(
      minutes_total > 0 & team_minutes_total > 0 &
        (team_dreb_total + opp_oreb_total) > 0 & dreb_chances > 0,
      dreb_total / dreb_chances,
      NA_real_
    ),

    # 3PAr and FTA Rate
    threepar = if_else(fga_total > 0, fg3a_total / fga_total, NA_real_),
    fta_rate = if_else(fga_total > 0, fta_total  / fga_total, NA_real_),

    # Weights for weighted percentiles
    ts_weight       = pmax(fga_total + 0.44 * fta_total, 0),
    efg_weight      = pmax(fga_total, 0),
    ast_weight      = pmax(ast_denom, 0),
    stl_weight      = pmax(minutes_total, 0),
    tov_weight      = pmax(tov_denom, 0),
    oreb_weight     = pmax(oreb_chances, 0),
    dreb_weight     = pmax(dreb_chances, 0),
    fg3_weight      = pmax(fg3a_total, 0),
    ft_weight       = pmax(fta_total, 0),
    threepar_weight = pmax(fga_total, 0),
    fta_rate_weight = pmax(fga_total, 0)
  ) %>%

  # -------------------------------------------------
  # Weighted percentiles + position averages
  # -------------------------------------------------
  group_by(athlete_position_abbreviation) %>%
  mutate(
    # --- Percentiles ---
    usage_pctile_pos    = weighted_percentile(usage,    usg_weight),
    ts_pctile_pos       = weighted_percentile(ts_pct,   ts_weight),
    efg_pctile_pos      = weighted_percentile(efg_pct,  efg_weight),
    ast_pctile_pos      = weighted_percentile(ast_pct,  ast_weight),
    stl_pctile_pos      = weighted_percentile(spg,      stl_weight),
    tov_pctile_pos      = weighted_percentile(tov_pct,  tov_weight),
    oreb_pctile_pos     = weighted_percentile(oreb_pct, oreb_weight),
    dreb_pctile_pos     = weighted_percentile(dreb_pct, dreb_weight),
    fg3_pctile_pos      = weighted_percentile(fg3_pct,  fg3_weight),
    ft_pctile_pos       = weighted_percentile(ft_pct,   ft_weight),
    threepar_pctile_pos = weighted_percentile(threepar, threepar_weight),
    fta_rate_pctile_pos = weighted_percentile(fta_rate, fta_rate_weight),

    # --- Weighted position averages (raw metric) ---
    usage_pos_avg    = weighted_mean_safe(usage,    usg_weight),
    ts_pos_avg       = weighted_mean_safe(ts_pct,   ts_weight),
    efg_pos_avg      = weighted_mean_safe(efg_pct,  efg_weight),
    ast_pos_avg      = weighted_mean_safe(ast_pct,  ast_weight),
    stl_pos_avg      = weighted_mean_safe(spg,      stl_weight),
    tov_pos_avg      = weighted_mean_safe(tov_pct,  tov_weight),
    oreb_pos_avg     = weighted_mean_safe(oreb_pct, oreb_weight),
    dreb_pos_avg     = weighted_mean_safe(dreb_pct, dreb_weight),
    fg3_pos_avg      = weighted_mean_safe(fg3_pct,  fg3_weight),
    ft_pos_avg       = weighted_mean_safe(ft_pct,   ft_weight),
    threepar_pos_avg = weighted_mean_safe(threepar, threepar_weight),
    fta_rate_pos_avg = weighted_mean_safe(fta_rate, fta_rate_weight),

    # --- Where does the position average sit in the weighted distribution? ---
    usage_pos_avg_pctile    = weighted_percentile(
      c(usage,    usage_pos_avg[1]),    c(usg_weight,  mean(usg_weight,  na.rm = TRUE))
    )[length(usage) + 1],

    ts_pos_avg_pctile       = weighted_percentile(
      c(ts_pct,   ts_pos_avg[1]),       c(ts_weight,   mean(ts_weight,   na.rm = TRUE))
    )[length(ts_pct) + 1],

    efg_pos_avg_pctile      = weighted_percentile(
      c(efg_pct,  efg_pos_avg[1]),      c(efg_weight,  mean(efg_weight,  na.rm = TRUE))
    )[length(efg_pct) + 1],

    ast_pos_avg_pctile      = weighted_percentile(
      c(ast_pct,  ast_pos_avg[1]),      c(ast_weight,  mean(ast_weight,  na.rm = TRUE))
    )[length(ast_pct) + 1],

    stl_pos_avg_pctile      = weighted_percentile(
      c(spg,      stl_pos_avg[1]),      c(stl_weight,  mean(stl_weight,  na.rm = TRUE))
    )[length(spg) + 1],

    tov_pos_avg_pctile      = weighted_percentile(
      c(tov_pct,  tov_pos_avg[1]),      c(tov_weight,  mean(tov_weight,  na.rm = TRUE))
    )[length(tov_pct) + 1],

    oreb_pos_avg_pctile     = weighted_percentile(
      c(oreb_pct, oreb_pos_avg[1]),     c(oreb_weight, mean(oreb_weight, na.rm = TRUE))
    )[length(oreb_pct) + 1],

    dreb_pos_avg_pctile     = weighted_percentile(
      c(dreb_pct, dreb_pos_avg[1]),     c(dreb_weight, mean(dreb_weight, na.rm = TRUE))
    )[length(dreb_pct) + 1],

    fg3_pos_avg_pctile      = weighted_percentile(
      c(fg3_pct,  fg3_pos_avg[1]),      c(fg3_weight,  mean(fg3_weight,  na.rm = TRUE))
    )[length(fg3_pct) + 1],

    ft_pos_avg_pctile       = weighted_percentile(
      c(ft_pct,   ft_pos_avg[1]),       c(ft_weight,   mean(ft_weight,   na.rm = TRUE))
    )[length(ft_pct) + 1],

    threepar_pos_avg_pctile = weighted_percentile(
      c(threepar, threepar_pos_avg[1]), c(threepar_weight, mean(threepar_weight, na.rm = TRUE))
    )[length(threepar) + 1],

    fta_rate_pos_avg_pctile = weighted_percentile(
      c(fta_rate, fta_rate_pos_avg[1]), c(fta_rate_weight, mean(fta_rate_weight, na.rm = TRUE))
    )[length(fta_rate) + 1]
  ) %>%
  ungroup() %>%
  mutate(
    athlete_id   = as.character(athlete_id),
    headshot_url = paste0(
      "https://a.espncdn.com/i/headshots/wnba/players/full/",
      athlete_id,
      ".png"
    )
  )

###############################################
# 6. Trim to lab-friendly columns
###############################################

wnba_player_season_2026_lab <- wnba_player_season_2026 %>%
  select(
    season, athlete_id,
    player   = athlete_display_name,
    team     = team_short_display_name,
    position = athlete_position_abbreviation,
    headshot_url,
    games_played, first_game_date, last_game_date,
    games_started, minutes_total, mpg,
    pts_total,  ppg,  pts_per_36,
    reb_total,  rpg,  reb_per_36,
    oreb_total, oreb_pg, oreb_per_36,
    dreb_total, dreb_pg, dreb_per_36,
    ast_total,  apg,  ast_per_36,
    stl_total,  spg,  stl_per_36,
    blk_total,  bpg,  blk_per_36,
    tov_total,  tovpg, tov_per_36,
    fg_pct, fg3_pct, threepar, ft_pct, fta_rate, efg_pct, ts_pct,
    usage, ast_pct, tov_pct, oreb_pct, dreb_pct,

    # Weighted percentiles
    usage_pctile_pos, ts_pctile_pos, efg_pctile_pos,
    ast_pctile_pos, stl_pctile_pos, tov_pctile_pos, oreb_pctile_pos,
    dreb_pctile_pos, fg3_pctile_pos, ft_pctile_pos,
    threepar_pctile_pos, fta_rate_pctile_pos,

    # Position weighted averages (raw metric)
    usage_pos_avg, ts_pos_avg, efg_pos_avg, ast_pos_avg, stl_pos_avg,
    tov_pos_avg, oreb_pos_avg, dreb_pos_avg, fg3_pos_avg, ft_pos_avg,
    threepar_pos_avg, fta_rate_pos_avg,

    # Where the position average sits in the distribution
    usage_pos_avg_pctile, ts_pos_avg_pctile, efg_pos_avg_pctile,
    ast_pos_avg_pctile, stl_pos_avg_pctile, tov_pos_avg_pctile,
    oreb_pos_avg_pctile, dreb_pos_avg_pctile, fg3_pos_avg_pctile,
    ft_pos_avg_pctile, threepar_pos_avg_pctile, fta_rate_pos_avg_pctile
  )

###############################################
# 6.5 Print per-game stats to screen
###############################################

library(knitr)

# Define player_row here so it's available for both the table and section 8.3
player_row <- wnba_player_season_2026_lab %>%
  filter(player == main_player)

# Single row version (commented out)
# player_row %>%
#   select(player, team, position, games_played, ppg, rpg, apg, spg, bpg) %>%
#   kable(format = "simple", digits = 2) %>%
#   print()

###############################################
# 6.6 Title strings, derived from the data
###############################################
# Everything the title says about scope comes from player_row, so re-running
# the script later re-labels the chart automatically.

if (nrow(player_row) == 0) {
  stop("No rows for '", main_player, "'. Check the spelling against ",
       "wnba_player_season_2026_lab$player.", call. = FALSE)
}

n_games   <- player_row$games_played[1]
last_date <- as.Date(player_row$last_game_date[1])
season_yr <- player_row$season[1]

period_text <- if (!is.null(PERIOD_TEXT)) {
  PERIOD_TEXT
} else {
  switch(PERIOD_LABEL,
    date   = format(last_date, "%b %e, %Y"),
    games  = paste0(n_games, " Game", if (n_games == 1) "" else "s"),
    asof   = paste0("As of ", format(last_date, "%b %e, %Y")),
    season = paste0(season_yr, " Season"),
    both   = paste0(n_games, " Game", if (n_games == 1) "" else "s",
                    " | Through ", format(last_date, "%b %e, %Y")),
    stop("PERIOD_LABEL must be one of \"date\", \"games\", \"asof\", ",
         "\"both\" or \"season\".", call. = FALSE)
  )
}
period_text <- gsub("  +", " ", period_text)   # format(%e) pads single digits

# The comparison pool is the player's own position group, so name it from her
# position rather than assuming a guard.
pos_abbr  <- player_row$position[1]
pos_plural <- switch(as.character(pos_abbr),
                     G = "Guards", F = "Forwards", C = "Centers",
                     paste0(pos_abbr, "s"))
pos_single <- switch(as.character(pos_abbr),
                     G = "Guard", F = "Forward", C = "Center",
                     as.character(pos_abbr))

message("Title scope: ", period_text, "  |  pool: All WNBA ", pos_plural)

# Position average for per-game stats (unweighted mean across position group)
pos_avg <- wnba_player_season_2026_lab %>%
  filter(position == player_row$position[1]) %>%
  summarise(across(c(mpg, ppg, rpg, apg, spg, bpg, tovpg),
                   ~round(mean(., na.rm = TRUE), 2))) %>%
  mutate(player = paste0(player_row$position[1], " Avg"))

# Player row + position avg side by side
bind_rows(
  player_row %>%
    select(player, mpg, ppg, rpg, apg, spg, bpg, tovpg),
  pos_avg %>%
    select(player, mpg, ppg, rpg, apg, spg, bpg, tovpg)
) %>%
  kable(format = "simple", digits = 2) %>%
  print()

###############################################
# (Optional) Decimal place and number type
###############################################

wnba_player_season_2026_lab <- wnba_player_season_2026_lab %>%
  mutate(across(where(is.numeric), ~round(., 2)))

###############################################
# 7. (Optional) Write CSVs - comment out if not needed
###############################################

# Where everything this script writes is saved -- the chart and the CSV.
# Change this one line to send them somewhere else, e.g. "~/Documents/scouting".
save_dir <- "~/Desktop"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

output_dir <- save_dir   # charts
data_dir   <- save_dir   # CSVs

write_csv(
  wnba_player_season_2026,
  file = file.path(data_dir, "wnba_player_season_2026.csv")
)
write_csv(
  wnba_player_season_2026_lab,
  file = file.path(data_dir, "wnba_player_season_2026_lab.csv")
)

###############################################
# 8. Pizza Plot - Dark Theme
###############################################

# --- 8.1 Config: change player name here ---
# At the top now
# main_player <- "Olivia Miles"
# player_team <- "Minnesota Lynx"

key_metrics_pctile <- c(
  "ts_pctile_pos",
  "ast_pctile_pos",
  "dreb_pctile_pos",
  "oreb_pctile_pos",
  "usage_pctile_pos",
  "tov_pctile_pos",
  "fg3_pctile_pos",
  "ft_pctile_pos"
)

# Matching avg_pctile columns in the same order as key_metrics_pctile
key_metrics_avg_pctile <- c(
  "ts_pos_avg_pctile",
  "ast_pos_avg_pctile",
  "dreb_pos_avg_pctile",
  "oreb_pos_avg_pctile",
  "usage_pos_avg_pctile",
  "tov_pos_avg_pctile",
  "fg3_pos_avg_pctile",
  "ft_pos_avg_pctile"
)

# --- 8.1b Label geometry ---
# Radius the metric name and percentile badge are anchored at. Bars run 0-1,
# so this is the clearance above a 100th-percentile slice. The badge hangs
# inward from it, which is what a 96% bar used to collide with at 1.20.
# PLOT_MAX is the panel's radial extent and sets how big the wheel is drawn --
# labels can sit outside it because coord_polar runs with clip = "off", so
# push LABEL_RADIUS out without touching PLOT_MAX or the wheel shrinks.
LABEL_RADIUS <- 1.28
PLOT_MAX     <- 1.35

# --- 8.2 Theme colors ---
BACKGROUND_COLOR <- "#1a1a1a"
LINE_COLOR       <- "#78BE20"
LETTER_COLOR     <- "white"
AVG_RING_COLOR   <- "#FDD023"  # yellow dashed ring for position average (option 1 - commented out below)
AVG_BAR_COLOR    <- "white"    # ghost bar overlay color (option 2 - active)

# --- 8.3 Prepare player plot data ---
# Note: player_row is defined in section 6.5 above
plot_data <- player_row %>%
  select(player, all_of(key_metrics_pctile)) %>%
  mutate(across(all_of(key_metrics_pctile), ~replace_na(., 0.0))) %>%
  mutate(tov_pctile_pos = 1 - tov_pctile_pos) %>%
  pivot_longer(cols = -player, names_to = "Metric", values_to = "Percentile") %>%
  mutate(
    Metric = case_when(
      Metric == "ts_pctile_pos"    ~ "True Shooting",
      Metric == "ast_pctile_pos"   ~ "Assists",
      Metric == "dreb_pctile_pos"  ~ "Def. Rebound",
      Metric == "oreb_pctile_pos"  ~ "Off. Rebound",
      Metric == "usage_pctile_pos" ~ "Usage",
      Metric == "tov_pctile_pos"   ~ "Turnovers",
      Metric == "fg3_pctile_pos"   ~ "3PT",
      Metric == "ft_pctile_pos"    ~ "Free Throw",
      TRUE ~ Metric
    )
  )

# --- 8.3b Prepare position average ring data ---
avg_ring_data <- player_row %>%
  select(player, all_of(key_metrics_avg_pctile)) %>%
  mutate(tov_pos_avg_pctile = 1 - tov_pos_avg_pctile) %>%
  pivot_longer(cols = -player, names_to = "Metric", values_to = "AvgPctile") %>%
  mutate(
    Metric = case_when(
      Metric == "ts_pos_avg_pctile"    ~ "True Shooting",
      Metric == "ast_pos_avg_pctile"   ~ "Assists",
      Metric == "dreb_pos_avg_pctile"  ~ "Def. Rebound",
      Metric == "oreb_pos_avg_pctile"  ~ "Off. Rebound",
      Metric == "usage_pos_avg_pctile" ~ "Usage",
      Metric == "tov_pos_avg_pctile"   ~ "Turnovers",
      Metric == "fg3_pos_avg_pctile"   ~ "3PT",
      Metric == "ft_pos_avg_pctile"    ~ "Free Throw",
      TRUE ~ Metric
    )
  )

# Merge so the ring shares the same x-axis factor order as the bars
plot_data <- plot_data %>%
  left_join(avg_ring_data %>% select(Metric, AvgPctile), by = "Metric")

# Lock factor order so polar slices stay consistent
plot_data$Metric <- factor(plot_data$Metric, levels = unique(plot_data$Metric))

# --- 8.4 Build pizza plot ---
pizza_plot_dark <- ggplot(
  plot_data,
  aes(x = Metric, y = Percentile, fill = Percentile)
) +

  # Ghost bar (full-length background) TAKING OUT TEMP
  # geom_bar(aes(y = 1), width = 0.85, stat = "identity",
  #          fill = LETTER_COLOR, alpha = 0.35, color = NA) +

  # Player bar
  geom_bar(width = 0.85, stat = "identity",
           color = LINE_COLOR, linewidth = 1) +

  # --- Option 1: dashed reference ring (commented out) ---
  # geom_line(
  #   aes(y = AvgPctile, group = 1),
  #   color = AVG_RING_COLOR,
  #   linewidth = 0.8,
  #   linetype = "dashed"
  # ) +
  # geom_point(
  #   aes(y = AvgPctile),
  #   color = AVG_RING_COLOR,
  #   size = 2,
  #   shape = 21,
  #   fill = BACKGROUND_COLOR,
  #   stroke = 1.2
  # ) +

  # --- Option 2: ghost bar overlay (active) ---
  # Semi-transparent white bar drawn on top of the player bar.
  # Where the avg bar extends beyond the player bar, the white shows.
  # Where the player bar is taller, player color shows through above.
  geom_bar(
    aes(y = AvgPctile),
    width = 0.85,
    stat = "identity",
    fill = AVG_BAR_COLOR,
    alpha = 0.25,
    color = AVG_BAR_COLOR,
    linewidth = 1
  ) +

  # Metric name label
  # LABEL_RADIUS sits outside the bars, which top out at 1.0. The percentile
  # badge hangs inward from it, so this has to clear a 99th-percentile slice --
  # at the old 1.20 a 96% bar ran into the badge.
  geom_text(
    aes(y = LABEL_RADIUS, label = Metric),
    vjust = -0.5,
    color = LETTER_COLOR, size = 4, fontface = "bold"
  ) +

  # Percentile badge
  geom_label(
    aes(y = LABEL_RADIUS, label = sprintf("%.0f%%", Percentile * 100)),
    vjust = 1.35,
    color = LETTER_COLOR, size = 3.5, fontface = "bold",
    label.r = unit(0.3, "lines"),
    label.padding = unit(0.2, "lines"),
    label.size = 0
  ) +

  # Fixed gridlines at 25 / 50 / 75
  geom_hline(yintercept = c(0.25, 0.5, 0.75),
             color = LETTER_COLOR, linetype = "dashed", linewidth = 0.25) +

  coord_polar(clip = "off") +
  scale_y_continuous(limits = c(-0.3, PLOT_MAX),
                     breaks = c(0.25, 0.5, 0.75),
                     expand = c(0, 0)) +
  scale_fill_gradient(low = "#0C2340", high = "#0C2340") +
  theme_minimal() +
  theme(
    plot.title         = element_blank(),
    plot.subtitle      = element_blank(),
    legend.position    = "none",
    panel.background   = element_rect(fill = BACKGROUND_COLOR, color = NA),
    plot.background    = element_rect(fill = BACKGROUND_COLOR, color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x        = element_blank(),
    axis.text.y        = element_blank(),
    axis.title         = element_blank(),
    panel.border       = element_blank()
  ) +
  labs(title = NULL, subtitle = NULL)

# --- 8.5 Load headshot from ESPN CDN ---
headshot_url  <- player_row$headshot_url[1]

player_circle <- tryCatch(
  magick::image_read(headshot_url),
  error = function(e) {
    message("Headshot not found for ", main_player, " - skipping image.")
    NULL
  }
)

# --- 8.6 Composite: Old pizza + headshot + labels ---
# final_plot <- cowplot::ggdraw() +
#   cowplot::draw_plot(pizza_plot_dark)
#
# if (!is.null(player_circle)) {
#   final_plot <- final_plot +
#     cowplot::draw_image(player_circle,
#                         x = 0.5, y = 0.53,
#                         width = 0.09, height = 0.17,
#                         hjust = 0.5, vjust = 0.7)
# }
#
# final_plot <- final_plot +
#   cowplot::draw_label(
#     paste0(main_player, " | ", player_team, " | 2026 Season"),
#     x = 0.5, y = 0.96, hjust = 0.5, vjust = 1,
#     fontface = "bold", color = LETTER_COLOR, size = 14
#   ) +
#   cowplot::draw_label(
#     "Percentile Profile vs. All WNBA Guards | Per 36 Min",
#     x = 0.5, y = 0.935, hjust = 0.5, vjust = 1,
#     color = LETTER_COLOR, size = 11
#   ) +
#   # Position average legend note
#   cowplot::draw_label(
#     paste0("■ Guard Average (shaded)"),
#     x = 0.5, y = 0.07, hjust = 0.5, vjust = 1,
#     color = AVG_BAR_COLOR, size = 9, fontface = "italic"
#   )

# --- 8.6 Composite: New pizza + headshot + labels ---
final_plot <- cowplot::ggdraw() +
  cowplot::draw_plot(pizza_plot_dark)

if (!is.null(player_circle)) {
  final_plot <- final_plot +
    cowplot::draw_image(player_circle,
                        x = 0.5, y = 0.54,
                        width = 0.09, height = 0.17,
                        hjust = 0.5, vjust = 0.7)
}

final_plot <- final_plot +
  cowplot::draw_label(
    paste0(main_player, " | ", player_team, " | ", period_text),
    x = 0.5, y = 0.96, hjust = 0.5, vjust = 1,
    fontface = "bold", color = LETTER_COLOR, size = 14
  ) +
  cowplot::draw_label(
    paste0("Percentile Profile vs. All WNBA ", pos_plural, " | Per 36 Min"),
    x = 0.5, y = 0.932, hjust = 0.5, vjust = 1, # y = 0.935
    color = LETTER_COLOR, size = 11
  ) +
  # Legend: player swatch
  cowplot::draw_label(
    "\u25cf",
    x = 0.33, y = 0.075, hjust = 0.5, vjust = 1,
    color = LINE_COLOR, size = 14
  ) +
  cowplot::draw_label(
    main_player,
    x = 0.405, y = 0.07, hjust = 0.5, vjust = 1,
    color = LETTER_COLOR, size = 9, fontface = "italic"
  ) +
  # Legend: guard average swatch
  cowplot::draw_label(
    "\u25cf",
    x = 0.52, y = 0.075, hjust = 0.5, vjust = 1,
    color = AVG_BAR_COLOR, size = 14
  ) +
  cowplot::draw_label(
    paste0(pos_single, " Average (shaded)"),
    x = 0.635, y = 0.07, hjust = 0.5, vjust = 1,
    color = LETTER_COLOR, size = 9, fontface = "italic"
  )

# --- 8.7 Save ---
ggsave(
  filename = file.path(output_dir, paste0(
    "pizza_plot_",
    gsub(" ", "_", main_player),
    "_2025-26.png"
  )),
  plot   = final_plot,
  width  = 8,
  height = 8,
  dpi    = 300,
  bg     = BACKGROUND_COLOR
)