###############################################
# Phase 1: Modeling the Four Factors (WNBA)
# Goal: Determine weights for "Optimized Lineup" Engine
# Scope: 5-Seasons (2021-2025)
###############################################

library(tidyverse)
library(wehoop)

###############################################
# 1. Pull Game Data (5 seasons for model stability)
###############################################
seasons <- 2021:2025
cat("Fetching data for seasons:", paste(seasons, collapse = ", "), "...\n")

raw_box_score <- load_wnba_player_box(seasons = seasons)

###############################################
# 2. Team-Game Totals & Possession Calculation
###############################################
# Filter for season_type == 2 (Regular Season) for consistent modeling
team_game_stats <- raw_box_score %>%
  filter(
    !is.na(minutes), 
    minutes > 0,
    season_type == 2 # <-- REGULAR SEASON
  ) %>%
  group_by(season, game_id, team_id, team_short_display_name) %>%
  summarise(
    pts   = sum(points, na.rm = TRUE),
    fga   = sum(field_goals_attempted, na.rm = TRUE),
    fgm   = sum(field_goals_made, na.rm = TRUE),
    fg3m  = sum(three_point_field_goals_made, na.rm = TRUE),
    fta   = sum(free_throws_attempted, na.rm = TRUE),
    tov   = sum(turnovers, na.rm = TRUE),
    oreb  = sum(offensive_rebounds, na.rm = TRUE),
    dreb  = sum(defensive_rebounds, na.rm = TRUE),
    .groups = "drop"
  )

###############################################
# 3. Calculate Opponent Stats
###############################################
team_game_w_opp <- team_game_stats %>%
  left_join(
    team_game_stats,
    by = c('season', 'game_id'),
    suffix = c("", "_opp"),
    relationship = "many-to-many"
  ) %>%
  filter(team_id != team_id_opp)

###############################################
# 4. The Four Factors
###############################################
model_ready_data <- team_game_w_opp %>%
  mutate(
    # Possession Formula: FGA + .44 * FTA + TOV - OREB
    possessions = fga + (0.44 * fta) + tov - oreb,
    
    # Dependent Variable: Net Rating
    ortg = 100 * (pts / possessions),
    drtg = 100 * (pts_opp / possessions),
    net_rating = ortg - drtg,
    
    # Independent Variables: The Four Factors (Decimals)
    efg_pct  = (fgm + 0.5 * fg3m) / fga,
    tov_pct  = tov / possessions,
    oreb_pct = oreb / (oreb + dreb_opp),
    ft_rate  = fta / fga
  ) %>%
  filter(!is.na(net_rating), is.finite(net_rating), possessions > 0)

###############################################
# 5. The Linear Regression Model
###############################################

# This model determines the WNBA-specific weights for my engine
four_factors_fit <- lm(net_rating ~ efg_pct + tov_pct + oreb_pct + ft_rate, 
                       data = model_ready_data)

# 6. Output Results
cat("\n--- WNBA REGRESSION RESULTS (5-YEAR DATA) ---\n")
print(summary(four_factors_fit))

# Extract the weights (coefficients)
weights <- coefficients(four_factors_fit)

cat("\n--- FINAL WNBA PREDICTION WEIGHTS ---\n")
print(weights)