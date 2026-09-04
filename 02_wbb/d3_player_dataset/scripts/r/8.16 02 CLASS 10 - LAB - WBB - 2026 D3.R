###############################################
# Class 10 Lab Dataset: 2025-26 Wbb Players
# Source: stats.ncaa.org, Division III (Midwest Conference)
# Note: all "*_pct" and `usage` are decimals (0-1)
#       Additional *_pctile_pos columns are
#       weighted percentiles within position groups.
#
# ESPN (and therefore wehoop) does not cover D3, so the
# data pull now comes from stats.ncaa.org. Run
#   ncaa_d3_wbb_run_mwc.R
# first to produce the CSV read in step 1 below.
# Everything from step 2 onward is unchanged.
###############################################

# 0. Install packages (run once if needed)
# install.packages("hoopR")
# install.packages("dplyr")
# install.packages("readr")

library(dplyr)
library(readr)

###############################################
# Helper: weighted percentile function
###############################################

weighted_percentile <- function(x, w) {
  # Returns values in [0,1] representing the
  # weighted cumulative distribution position of each x
  if (length(x) == 0) return(numeric(0))
  
  w[is.na(w)] <- 0
  x_na <- is.na(x)
  
  if (all(x_na) || sum(w[!x_na]) <= 0) {
    return(rep(NA_real_, length(x)))
  }
  
  x2 <- x[!x_na]
  w2 <- w[!x_na]
  
  ord <- order(x2)
  x_sorted <- x2[ord]
  w_sorted <- w2[ord]
  
  cw <- cumsum(w_sorted)
  total_w <- sum(w_sorted)
  
  p_sorted <- cw / total_w
  
  p2 <- numeric(length(x2))
  p2[ord] <- p_sorted
  
  out <- rep(NA_real_, length(x))
  out[!x_na] <- p2
  out
}

###############################################
# 1. Load ALL game-level player box data (2025-26)
###############################################

# Produced by ncaa_d3_wbb_run_mwc.R -- scraped from
# stats.ncaa.org. Columns are named to match what
# wehoop::load_wbb_player_box() used to return, so
# steps 2-7 below did not have to change.
# The runner writes this; see the project README for the order to run in.
BOX_CSV <- file.path(path.expand("~/Desktop"),
                     "ncaa_d3_wbb_player_box_2026_mwc_full.csv")
wbb_player_box_2026 <- read_csv(BOX_CSV, show_col_types = FALSE)

# Drop games NCAA does not count toward D3 season stats.
# Ripon @ Milwaukee (12/14/2025) is a D3 team playing a D1
# program: it has a box score and shows on the schedule, but
# every Ripon player's official roster GP/GS counts 29 games,
# not 30, so NCAA excludes it. This is the only such game in
# the MWC season. Comment this out to include it -- but note
# games_played would then disagree with games_started, which
# is counted out of 29. # < -- EDITED
excluded_game_ids <- c("6391804")

wbb_player_box_2026 <- wbb_player_box_2026 %>%
  filter(!(as.character(game_id) %in% excluded_game_ids))

# (Optional) Save raw game-level data
# write_csv(
#   wbb_player_box_2026,
#   file = "~/Desktop/wbb_player_box_2026_raw.csv" # < -- EDITED
# )

###############################################
# 2. Clean out DNPs / no-minutes rows
###############################################

wbb_player_box_2026_clean <- wbb_player_box_2026 %>% # < -- EDITED
  filter(
    !isTRUE(did_not_play),    # drop DNP rows
    !is.na(minutes),
    minutes > 0
  )

# If you ONLY want regular season games, you can add:
# wbb_player_box_2026_clean <- wbb_player_box_2026_clean %>%
#   filter(season_type == 2)  # often 2 = regular season; check with count()

# Midwest Conference team ids. The lab dataset covers MWC
# players only -- non-conference opponents appear in only the
# games they played against MWC teams, so their season totals
# would be partial. Steps 3-4 below deliberately still use the
# UNfiltered data so opponent totals stay correct.
mwc_team_ids <- c(
  "611651",  # Beloit
  "611413",  # Cornell College
  "611719",  # Grinnell
  "611732",  # Illinois Col.
  "611746",  # Knox
  "611748",  # Lake Forest
  "611749",  # Lawrence
  "611695",  # Monmouth (IL)
  "611872",  # Ripon
  "611772"   # St. Norbert
)

wbb_player_box_2026_mwc <- wbb_player_box_2026_clean %>%
  filter(as.character(team_id) %in% mwc_team_ids)

###############################################
# 3. Team-game totals (for team + opponent season stats)
###############################################

team_game_totals_2026 <- wbb_player_box_2026_clean %>% # < -- EDITED
  group_by(
    season,
    game_id,
    team_id,
    team_display_name,
    team_short_display_name,
    team_abbreviation
  ) %>%
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

# Attach opponent team stats for each team-game
team_game_w_opp_2026 <- team_game_totals_2026 %>%
  left_join(
    team_game_totals_2026,
    by = c("season", "game_id"),
    suffix = c("", "_opp"),
    relationship = "many-to-many" # <--- ADD THIS LINE
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

wbb_player_season_2026 <- wbb_player_box_2026_mwc %>% # < -- EDITED (MWC players only)
  group_by(
    season,
    athlete_id,
    athlete_display_name,
    athlete_short_name,
    athlete_jersey,
    athlete_position_name,
    athlete_position_abbreviation,
    team_id,
    team_display_name,
    team_short_display_name,
    team_abbreviation
  ) %>%
  summarise(
    games_played   = n_distinct(game_id),
    # NCAA's box score "P" column carries EITHER a position OR
    # "*" for starters, never both, so there is no per-game
    # starter flag for feeds that report positions (all of the
    # MWC). games_started comes from the team roster page's GS
    # column instead -- a season total, and the official count.
    games_started  = dplyr::first(games_started_season), # < -- EDITED
    minutes_total  = sum(minutes, na.rm = TRUE),
    
    pts_total      = sum(points, na.rm = TRUE),
    fgm_total      = sum(field_goals_made, na.rm = TRUE),
    fga_total      = sum(field_goals_attempted, na.rm = TRUE),
    fg3m_total     = sum(three_point_field_goals_made, na.rm = TRUE),
    fg3a_total     = sum(three_point_field_goals_attempted, na.rm = TRUE),
    ftm_total      = sum(free_throws_made, na.rm = TRUE),
    fta_total      = sum(free_throws_attempted, na.rm = TRUE),
    
    oreb_total     = sum(offensive_rebounds, na.rm = TRUE),
    dreb_total     = sum(defensive_rebounds, na.rm = TRUE),
    reb_total      = sum(rebounds, na.rm = TRUE),
    ast_total      = sum(assists, na.rm = TRUE),
    stl_total      = sum(steals, na.rm = TRUE),
    blk_total      = sum(blocks, na.rm = TRUE),
    tov_total      = sum(turnovers, na.rm = TRUE),
    pf_total       = sum(fouls, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Attach team + opponent season totals (for usage, reb%, AST%)
  left_join(team_season_totals_2026, by = c("season", "team_id")) %>%
  mutate(
    # Per-game counting stats
    mpg   = minutes_total / games_played,
    ppg   = pts_total / games_played,
    rpg   = reb_total / games_played,
    apg   = ast_total / games_played,
    spg   = stl_total / games_played,
    bpg   = blk_total / games_played,
    tovpg = tov_total / games_played,
    
    oreb_pg = oreb_total / games_played,
    dreb_pg = dreb_total / games_played,
    
    # Shooting percentages (decimals)
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
    
    # Per-40 rates
    pts_per_40  = if_else(minutes_total > 0, pts_total  * 40 / minutes_total, NA_real_),
    reb_per_40  = if_else(minutes_total > 0, reb_total  * 40 / minutes_total, NA_real_),
    ast_per_40  = if_else(minutes_total > 0, ast_total  * 40 / minutes_total, NA_real_),
    stl_per_40  = if_else(minutes_total > 0, stl_total  * 40 / minutes_total, NA_real_),
    blk_per_40  = if_else(minutes_total > 0, blk_total  * 40 / minutes_total, NA_real_),
    tov_per_40  = if_else(minutes_total > 0, tov_total  * 40 / minutes_total, NA_real_),
    
    oreb_per_40 = if_else(minutes_total > 0, oreb_total * 40 / minutes_total, NA_real_),
    dreb_per_40 = if_else(minutes_total > 0, dreb_total * 40 / minutes_total, NA_real_),
    
    # Usage (decimal, not percent)
    # We'll also define usg_weight as the player's "possessions used":
    usg_weight = pmax(fga_total + 0.44 * fta_total + tov_total, 0),
    
    usage = if_else(
      minutes_total > 0 &
        (team_fga_total + 0.44 * team_fta_total + team_tov_total) > 0,
      ((fga_total + 0.44 * fta_total + tov_total) * (team_minutes_total / 5)) /
        (minutes_total * (team_fga_total + 0.44 * team_fta_total + team_tov_total)),
      NA_real_
    ),
    
    # Assist opportunities (denominator for AST%)
    ast_denom = (
      (minutes_total / (team_minutes_total / 5)) * team_fgm_total - fgm_total
    ),
    
    ast_pct = if_else(
      minutes_total > 0 &
        team_minutes_total > 0 &
        ast_denom > 0,
      ast_total / ast_denom,
      NA_real_
    ),
    
    # Turnover Percentage (decimal)
    tov_denom = fga_total + 0.44 * fta_total + tov_total,
    tov_pct = if_else(
      tov_denom > 0,
      tov_total / tov_denom,
      NA_real_
    ),
    
    # Offensive Rebound Percentage (decimal)
    oreb_chances = (minutes_total / (team_minutes_total / 5)) *
      (team_oreb_total + opp_dreb_total),
    
    oreb_pct = if_else(
      minutes_total > 0 &
        team_minutes_total > 0 &
        (team_oreb_total + opp_dreb_total) > 0 &
        oreb_chances > 0,
      oreb_total / oreb_chances,
      NA_real_
    ),
    
    # Defensive Rebound Percentage (decimal)
    dreb_chances = (minutes_total / (team_minutes_total / 5)) *
      (team_dreb_total + opp_oreb_total),
    
    dreb_pct = if_else(
      minutes_total > 0 &
        team_minutes_total > 0 &
        (team_dreb_total + opp_oreb_total) > 0 &
        dreb_chances > 0,
      dreb_total / dreb_chances,
      NA_real_
    ),
    
    # 3PA Rate (3PAr) and FTA Rate
    threepar = if_else(
      fga_total > 0,
      fg3a_total / fga_total,
      NA_real_
    ),
    fta_rate = if_else(
      fga_total > 0,
      fta_total / fga_total,
      NA_real_
    ),
    
    # Weights for weighted percentiles (sample-size proxies)
    ts_weight    = pmax(fga_total + 0.44 * fta_total, 0),
    efg_weight   = pmax(fga_total, 0),
    ast_weight   = pmax(ast_denom, 0),
    tov_weight   = pmax(tov_denom, 0),
    oreb_weight  = pmax(oreb_chances, 0),
    dreb_weight  = pmax(dreb_chances, 0),
    fg3_weight   = pmax(fg3a_total, 0),
    ft_weight    = pmax(fta_total, 0),
    threepar_weight = pmax(fga_total, 0),
    fta_rate_weight = pmax(fga_total, 0)
  ) %>%
  # Position-group weighted percentiles
  group_by(athlete_position_abbreviation) %>%
  mutate(
    usage_pctile_pos    = weighted_percentile(usage,    usg_weight),
    ts_pctile_pos       = weighted_percentile(ts_pct,   ts_weight),
    efg_pctile_pos      = weighted_percentile(efg_pct,  efg_weight),
    ast_pctile_pos      = weighted_percentile(ast_pct,  ast_weight),
    tov_pctile_pos      = weighted_percentile(tov_pct,  tov_weight),
    oreb_pctile_pos     = weighted_percentile(oreb_pct, oreb_weight),
    dreb_pctile_pos     = weighted_percentile(dreb_pct, dreb_weight),
    fg3_pctile_pos      = weighted_percentile(fg3_pct,  fg3_weight),
    ft_pctile_pos       = weighted_percentile(ft_pct,   ft_weight),
    threepar_pctile_pos = weighted_percentile(threepar, threepar_weight),
    fta_rate_pctile_pos = weighted_percentile(fta_rate, fta_rate_weight)
  ) %>%
  ungroup()

###############################################
# 6. Trim to a “lab-friendly” set of columns
###############################################

wbb_player_season_2026_lab <- wbb_player_season_2026 %>% # < -- EDITED
  select(
    season,
    athlete_id,
    player    = athlete_display_name,
    team      = team_short_display_name,
    position  = athlete_position_abbreviation,
    
    games_played,
    games_started,
    minutes_total,
    mpg,
    
    pts_total,
    ppg,
    pts_per_40,

    # How those points were scored -- makes and attempts sit
    # next to the rate they produce, so a 1-for-1 shooter is not
    # mistaken for a high-percentage one. # < -- EDITED
    fgm_total,
    fga_total,
    fg_pct,

    fg3m_total,
    fg3a_total,
    fg3_pct,
    threepar,

    ftm_total,
    fta_total,
    ft_pct,
    fta_rate,

    reb_total,
    rpg,
    reb_per_40,
    
    oreb_total,
    oreb_pg,
    oreb_per_40,
    
    dreb_total,
    dreb_pg,
    dreb_per_40,
    
    ast_total,
    apg,
    ast_per_40,
    
    stl_total,
    spg,
    stl_per_40,
    
    blk_total,
    bpg,
    blk_per_40,
    
    tov_total,
    tovpg,
    tov_per_40,
    
    efg_pct,
    ts_pct,
    usage,
    ast_pct,
    tov_pct,
    oreb_pct,
    dreb_pct,
    
    usage_pctile_pos,
    ts_pctile_pos,
    efg_pctile_pos,
    ast_pctile_pos,
    tov_pctile_pos,
    oreb_pctile_pos,
    dreb_pctile_pos,
    fg3_pctile_pos,
    ft_pctile_pos,
    threepar_pctile_pos,
    fta_rate_pctile_pos
  )

###############################################
# 6b. Round every numeric column to 3 decimals
#
# Raw values carry ~17 significant digits (0.7647058823529411)
# and very small percentiles land in scientific notation
# (3e-4), both of which are ugly in a spreadsheet. Rounding
# leaves the columns NUMERIC -- it changes the stored value,
# not the type -- so they still sort and compute as numbers.
# Counting stats (games, points) are integers already and are
# unaffected. IDs and the season are numeric too, but rounding
# a whole number is a no-op, so they pass through untouched.
###############################################

round_stats <- function(df, digits = 3) {
  df %>% mutate(across(where(is.numeric), ~ round(.x, digits)))
}

wbb_player_season_2026     <- round_stats(wbb_player_season_2026)     # < -- EDITED
wbb_player_season_2026_lab <- round_stats(wbb_player_season_2026_lab) # < -- EDITED

###############################################
# 7. Write CSVs to disk
#
# CSVs live in data/, not outputs/ -- outputs/ is for plots
# and finished visuals. # < -- EDITED
###############################################

# Where everything this pipeline reads and writes lives -- the four scripts
# hand files to each other through this one folder, so they all agree.
# Change this one line to point somewhere else.
save_dir <- "~/Desktop"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

data_dir   <- save_dir
output_dir <- save_dir

# Full season-level dataset (all columns)
write_csv(
  wbb_player_season_2026,
  file = file.path(data_dir, "wbb_player_season_2026_d3_full.csv") # < -- EDITED
)

# Trimmed “lab” dataset (cleaner for students)
write_csv(
  wbb_player_season_2026_lab,
  file = file.path(data_dir, "wbb_player_season_2026_d3_lab.csv") # < -- EDITED
)

###############################################
# End of script
###############################################
