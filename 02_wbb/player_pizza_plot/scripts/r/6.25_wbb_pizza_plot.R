###############################################
# NCAA WBB Conference Pizza Plot (Dark Theme)
# Compare a player to the rest of their conference,
# within their position group -- e.g. Utah State
# guard vs. all Mountain West guards.
#
# Template: 04_wnba/scripts/r/01_wnba_pizza_plot.R
# Data:     wehoop load_wbb_player_box (college stats)
#           + ESPN conference/standings API for membership
#
# Note: all "*_pct" and `usage` are decimals (0-1)
#       *_pctile_pos columns are weighted percentiles
#       computed WITHIN the selected conference and
#       position bucket (Guard / Forward / Center).
#       *_pos_avg = weighted mean of raw metric for that
#       position group inside the conference.
#       *_pos_avg_pctile = where that avg sits in the
#       weighted percentile distribution (reference ring).
#       Per-40 rates used throughout (college standard).
#
# HOW TO USE: edit the CONFIG block to swap
# the player, school, conference, and season. Nothing
# else needs to change.
###############################################

# 0. Install packages (run once if needed)
# install.packages(c("wehoop","dplyr","readr","ggplot2",
# "tidyr","cowplot","magick","httr","jsonlite"))

library(wehoop)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(cowplot)
library(magick)
library(httr)
library(jsonlite)

###############################################
# CONFIG: edit these to change the plot
###############################################

main_player     <- "Jamisyn Heaton"               #"Mila Holloway"             # player to feature, can be swapped 
school          <- "Utah State"                   #"University of Michigan"     # the player's school
conference_name <- "Mountain West Conference"     #"Big Ten"                    # comparison pool (partial match OK)
season          <- 2026                                                        # wehoop uses END year (2026 = 2025-26)

# Optional local headshot. If "", the script tries the ESPN
# CDN and silently skips the image when none is found.
headshot_local  <- ""

# Optional minimum-minutes filter for the comparison pool.
# Uncomment to drop low-minute players (end-of-bench noise)
# before percentiles are computed. Leave commented to keep everyone
# Uncomment section in 5b as well # filter(minutes_total >= min_minutes) %>%
# NOTE: the featured player must be above this too.
# min_minutes <- 100

# Tip: run get_wbb_conferences() (defined below) to print
# every conference name/ID you can pass to `conference_name`.

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
# Conference helpers (ESPN API)
###############################################

# Catalog of conferences: group_id + name (~33 conferences)
get_wbb_conferences <- function() {
  u <- paste0(
    "http://site.api.espn.com/apis/site/v2/sports/basketball/",
    "womens-college-basketball/scoreboard/conferences?seasontype=2"
  )
  j  <- jsonlite::fromJSON(httr::content(httr::RETRY("GET", u),
                                         as = "text", encoding = "UTF-8"))
  cf <- j[["conferences"]]
  tibble(
    group_id   = as.integer(cf$groupId),
    name       = cf$name,
    short_name = cf$shortName
  ) %>%
    filter(!group_id %in% c(0, 50)) %>%   # drop "All"/divisional roll-ups
    arrange(name)
}

# Team IDs that belong to a conference, via the standings endpoint
get_conference_teams <- function(group_id, season) {
  u <- paste0(
    "https://site.web.api.espn.com/apis/v2/sports/basketball/",
    "womens-college-basketball/standings?season=", season,
    "&group=", group_id
  )
  j <- jsonlite::fromJSON(httr::content(httr::RETRY("GET", u),
                                        as = "text", encoding = "UTF-8"),
                          flatten = TRUE)
  e <- j$standings$entries
  tibble(
    team_id       = as.character(e$team.id),
    team_name     = e$team.displayName,
    team_location = e$team.location
  )
}

# Resolve the configured conference name -> group_id (partial, case-insensitive)
conf_catalog <- get_wbb_conferences()
conf_match   <- conf_catalog %>%
  filter(grepl(conference_name, name, ignore.case = TRUE))

if (nrow(conf_match) == 0) {
  message("No conference matched '", conference_name,
          "'. Available conferences:")
  print(conf_catalog, n = nrow(conf_catalog))
  stop("Set `conference_name` to one of the names listed above.")
}
if (nrow(conf_match) > 1) {
  message("'", conference_name, "' matched multiple conferences; using the first:")
  print(conf_match)
}
conf_match <- conf_match[1, ]

conf_teams    <- get_conference_teams(conf_match$group_id, season)
conf_team_ids <- conf_teams$team_id

message("Conference: ", conf_match$name,
        " (group ", conf_match$group_id, ") - ",
        nrow(conf_teams), " teams")

###############################################
# 1. Pull ALL game-level player box data
###############################################

wbb_player_box <- load_wbb_player_box(seasons = season)

###############################################
# 2. Clean out DNPs / no-minutes rows
###############################################

wbb_player_box_clean <- wbb_player_box %>%
  filter(
    !isTRUE(did_not_play),
    !is.na(minutes),
    minutes > 0
  )

# Uncomment to restrict to regular season only:
# wbb_player_box_clean <- wbb_player_box_clean %>%
#   filter(season_type == 2)

###############################################
# 3. Team-game totals (for team + opponent season stats)
###############################################

team_game_totals <- wbb_player_box_clean %>%
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

team_game_w_opp <- team_game_totals %>%
  left_join(
    team_game_totals,
    by = c("season", "game_id"),
    suffix = c("", "_opp"),
    relationship = "many-to-many"
  ) %>%
  filter(team_id != team_id_opp)

###############################################
# 4. Team-season totals (team + opponent)
###############################################

team_season_totals <- team_game_w_opp %>%
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
#    (computed league-wide; raw metrics don't
#     depend on the comparison pool)
###############################################

wbb_player_season <- wbb_player_box_clean %>%
  arrange(game_date) %>%
  group_by(season, athlete_id) %>%
  summarise(
    athlete_display_name          = last(athlete_display_name),
    athlete_position_abbreviation = last(athlete_position_abbreviation),
    team_id                       = last(team_id),
    team_short_display_name       = last(team_short_display_name),
    team_display_name             = last(team_display_name),
    games_played  = n_distinct(game_id),
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
  left_join(team_season_totals, by = c("season", "team_id")) %>%
  mutate(
    team_id = as.character(team_id),

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

    # Per-40 rates (college standard)
    pts_per_40  = if_else(minutes_total > 0, pts_total  * 40 / minutes_total, NA_real_),
    reb_per_40  = if_else(minutes_total > 0, reb_total  * 40 / minutes_total, NA_real_),
    ast_per_40  = if_else(minutes_total > 0, ast_total  * 40 / minutes_total, NA_real_),
    stl_per_40  = if_else(minutes_total > 0, stl_total  * 40 / minutes_total, NA_real_),
    blk_per_40  = if_else(minutes_total > 0, blk_total  * 40 / minutes_total, NA_real_),
    tov_per_40  = if_else(minutes_total > 0, tov_total  * 40 / minutes_total, NA_real_),
    oreb_per_40 = if_else(minutes_total > 0, oreb_total * 40 / minutes_total, NA_real_),
    dreb_per_40 = if_else(minutes_total > 0, dreb_total * 40 / minutes_total, NA_real_),

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
    fta_rate_weight = pmax(fga_total, 0),

    # Position bucket: Guard / Forward / Center so "guards vs guards"
    # works even though ESPN tags a few PG/SG/SF/PF/ATH players.
    pos_group = case_when(
      athlete_position_abbreviation %in% c("G", "PG", "SG")        ~ "Guard",
      athlete_position_abbreviation %in% c("F", "SF", "PF", "F-C") ~ "Forward",
      athlete_position_abbreviation %in% c("C")                    ~ "Center",
      TRUE                                                         ~ "Guard"
    ),

    # ESPN women's-college headshot (often missing -> handled later)
    athlete_id   = as.character(athlete_id),
    headshot_url = paste0(
      "https://a.espncdn.com/i/headshots/womens-college-basketball/players/full/",
      athlete_id,
      ".png"
    )
  )

###############################################
# 5b. Restrict to the conference, then compute
#     weighted percentiles + position averages
#     WITHIN conference + position bucket
###############################################

wbb_conf_season <- wbb_player_season %>%
  filter(team_id %in% conf_team_ids) %>%

  # Optional: drop low-minute players from the comparison pool.
  # Uncomment alongside `min_minutes` in the CONFIG block above.
  # filter(minutes_total >= min_minutes) %>%

  group_by(pos_group) %>%
  mutate(
    # --- Percentiles (within conference + position) ---
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
  ungroup()

###############################################
# 6. Trim columns
###############################################

wbb_conf_season_simple<- wbb_conf_season %>%
  transmute(
    season, athlete_id,
    player   = athlete_display_name,
    team     = team_short_display_name,
    position = athlete_position_abbreviation,
    pos_group, headshot_url,
    games_played, games_started, minutes_total, mpg,
    pts_total,  ppg,  pts_per_40,
    reb_total,  rpg,  reb_per_40,
    oreb_total, oreb_pg, oreb_per_40,
    dreb_total, dreb_pg, dreb_per_40,
    ast_total,  apg,  ast_per_40,
    stl_total,  spg,  stl_per_40,
    blk_total,  bpg,  blk_per_40,
    tov_total,  tovpg, tov_per_40,
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
# 6.5 Print player vs conference position avg
###############################################

library(knitr)

# Find the featured player inside the conference pool
player_row <- wbb_conf_season_simple%>% filter(player == main_player)

if (nrow(player_row) == 0) {
  message("Player '", main_player, "' not found in ", conf_match$name,
          ". Players on ", school, ":")
  wbb_conf_season_simple%>%
    filter(grepl(school, team, ignore.case = TRUE)) %>%
    arrange(desc(minutes_total)) %>%
    select(player, team, position, pos_group, mpg, ppg) %>%
    print(n = 50)
  stop("Set `main_player` to one of the players listed above.")
}
player_row <- player_row[1, ]

# Conference position-group average for per-game stats (unweighted mean)
pos_avg <- wbb_conf_season_simple%>%
  filter(pos_group == player_row$pos_group[1]) %>%
  summarise(across(c(mpg, ppg, rpg, apg, spg, bpg, tovpg),
                   ~round(mean(., na.rm = TRUE), 2))) %>%
  mutate(player = paste0(conf_match$short_name, " ",
                         player_row$pos_group[1], " Avg"))

# Player row + conference position avg side by side
bind_rows(
  player_row %>% select(player, mpg, ppg, rpg, apg, spg, bpg, tovpg),
  pos_avg    %>% select(player, mpg, ppg, rpg, apg, spg, bpg, tovpg)
) %>%
  kable(format = "simple", digits = 2) %>%
  print()

###############################################
# (Optional) Decimal place and number type
###############################################

wbb_conf_season_simple<- wbb_conf_season_simple%>%
  mutate(across(where(is.numeric), ~round(., 2)))

###############################################
# 7. (Optional) Write CSV - comment out if not needed
###############################################

conf_slug <- gsub("[^a-z0-9]+", "_", tolower(conf_match$short_name))
conf_slug <- gsub("^_|_$", "", conf_slug)

# Where everything this script writes is saved -- the chart and the CSV.
# Change this one line to send them somewhere else, e.g. "~/Documents/scouting".
save_dir <- "~/Desktop"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

output_dir <- save_dir   # charts
data_dir   <- save_dir   # CSVs

write_csv(
  wbb_conf_season_simple,
  file = file.path(data_dir, paste0("wbb_", conf_slug, "_", season, ".csv"))
)

###############################################
# 8. Pizza Plot - Dark Theme
###############################################

# --- 8.1 Config: metrics (8 slices) ---
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

# --- 8.2 Theme colors ---
BACKGROUND_COLOR <- "#1a1a1a"
LINE_COLOR       <- "#0033A0"  # Utah State blue outline; change per school
LETTER_COLOR     <- "white"
AVG_BAR_COLOR    <- "white"    # conference-average ghost-bar overlay
FILL_COLOR       <- "#0C2340"  # bar fill

# Tidy metric labels (shared by bars + ring)
pretty_metric <- function(x) {
  dplyr::recode(x,
    ts_pctile_pos        = "True Shooting", ast_pctile_pos       = "Assists",
    dreb_pctile_pos      = "Def. Rebound",  oreb_pctile_pos      = "Off. Rebound",
    usage_pctile_pos     = "Usage",         tov_pctile_pos       = "Turnovers",
    fg3_pctile_pos       = "3PT",           ft_pctile_pos        = "Free Throw",
    ts_pos_avg_pctile    = "True Shooting", ast_pos_avg_pctile   = "Assists",
    dreb_pos_avg_pctile  = "Def. Rebound",  oreb_pos_avg_pctile  = "Off. Rebound",
    usage_pos_avg_pctile = "Usage",         tov_pos_avg_pctile   = "Turnovers",
    fg3_pos_avg_pctile   = "3PT",           ft_pos_avg_pctile    = "Free Throw"
  )
}

# --- 8.3 Prepare player plot data ---
plot_data <- player_row %>%
  select(player, all_of(key_metrics_pctile)) %>%
  mutate(across(all_of(key_metrics_pctile), ~replace_na(., 0.0))) %>%
  mutate(tov_pctile_pos = 1 - tov_pctile_pos) %>%   # lower TOV% is better
  pivot_longer(cols = -player, names_to = "Metric", values_to = "Percentile") %>%
  mutate(Metric = pretty_metric(Metric))

# --- 8.3b Prepare conference position-average ring data ---
avg_ring_data <- player_row %>%
  select(player, all_of(key_metrics_avg_pctile)) %>%
  mutate(tov_pos_avg_pctile = 1 - tov_pos_avg_pctile) %>%
  pivot_longer(cols = -player, names_to = "Metric", values_to = "AvgPctile") %>%
  mutate(Metric = pretty_metric(Metric))

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

  # Player bar
  geom_bar(width = 0.85, stat = "identity",
           color = LINE_COLOR, linewidth = 1) +

  # Conference-average ghost-bar overlay
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
  geom_text(
    aes(y = 1.20, label = Metric),
    vjust = -0.5,
    color = LETTER_COLOR, size = 4, fontface = "bold"
  ) +

  # Percentile badge
  geom_label(
    aes(y = 1.20, label = sprintf("%.0f%%", Percentile * 100)),
    vjust = 1.5,
    color = LETTER_COLOR, size = 3.5, fontface = "bold",
    label.r = unit(0.3, "lines"),
    label.padding = unit(0.2, "lines"),
    label.size = 0
  ) +

  # Fixed gridlines at 25 / 50 / 75
  geom_hline(yintercept = c(0.25, 0.5, 0.75),
             color = LETTER_COLOR, linetype = "dashed", linewidth = 0.25) +

  coord_polar(clip = "off") +
  scale_y_continuous(limits = c(-0.3, 1.35),
                     breaks = c(0.25, 0.5, 0.75),
                     expand = c(0, 0)) +
  scale_fill_gradient(low = FILL_COLOR, high = FILL_COLOR) +
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

# --- 8.5 Load headshot (local override, else ESPN CDN, else skip) ---
player_circle <- NULL
if (nzchar(headshot_local)) {
  player_circle <- tryCatch(
    magick::image_read(headshot_local),
    error = function(e) {
      message("Local headshot not found at ", headshot_local, " - skipping image.")
      NULL
    }
  )
} else {
  player_circle <- tryCatch(
    magick::image_read(player_row$headshot_url[1]),
    error = function(e) {
      message("No ESPN headshot for ", main_player, " - skipping image.")
      NULL
    }
  )
}

# --- 8.6 Composite: pizza + headshot + labels ---
pos_label <- paste0(player_row$pos_group[1], "s")  # e.g. "Guards"
season_label <- paste0(season - 1, "-", substr(season, 3, 4))

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
    paste0(main_player, " | ", school, " | ", season_label, " Season"),
    x = 0.5, y = 0.96, hjust = 0.5, vjust = 1,
    fontface = "bold", color = LETTER_COLOR, size = 14
  ) +
  cowplot::draw_label(
    paste0("Percentile Profile vs. ", conf_match$short_name, " ",
           pos_label, " | Per 40 Min"),
    x = 0.5, y = 0.932, hjust = 0.5, vjust = 1,
    color = LETTER_COLOR, size = 11
  ) +
  # Legend: player swatch
  cowplot::draw_label(
    "●",
    x = 0.30, y = 0.075, hjust = 0.5, vjust = 1,
    color = LINE_COLOR, size = 14
  ) +
  cowplot::draw_label(
    main_player,
    x = 0.385, y = 0.07, hjust = 0.5, vjust = 1,
    color = LETTER_COLOR, size = 9, fontface = "italic"
  ) +
  # Legend: conference average swatch
  cowplot::draw_label(
    "●",
    x = 0.52, y = 0.075, hjust = 0.5, vjust = 1,
    color = AVG_BAR_COLOR, size = 14
  ) +
  cowplot::draw_label(
    paste0(conf_match$short_name, " ", pos_label, " Avg (shaded)"),
    x = 0.66, y = 0.07, hjust = 0.5, vjust = 1,
    color = LETTER_COLOR, size = 9, fontface = "italic"
  )

# --- 8.7 Save ---
ggsave(
  filename = file.path(output_dir, paste0(
    "pizza_plot_", gsub(" ", "_", main_player), "_", conf_slug, "_", season, ".png"
  )),
  plot   = final_plot,
  width  = 8,
  height = 8,
  dpi    = 300,
  bg     = BACKGROUND_COLOR
)

message("Saved pizza plot for ", main_player, " vs ",
        conf_match$short_name, " ", pos_label, ".")
