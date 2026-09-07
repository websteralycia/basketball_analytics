# nba_player_metrics.R --------------------------------------------------------
#
# Player-season aggregation and advanced metrics, built from a player box score.
#
# Ported from the WNBA scripts:
#   final_project - build_wnba_player_dataset.R
#   01_wnba_pizza_plot.R
# Those two duplicated the entire player-season computation between them. Here
# it lives once and both scripts source it, so a formula fix lands in both.
#
# All formulas match the class definitions:
#   TS%      PTS / (2 * (FGA + 0.44*FTA))
#   Usage    ((FGA + 0.44*FTA + TOV) * (TmMP/5)) / (MP * (TmFGA + 0.44*TmFTA + TmTOV))
#   OREB%    OREB / oreb_chances, chances = (MP/(TmMP/5)) * (TmOREB + OppDREB)
#   TOV%     TOV / (FGA + 0.44*FTA + TOV)          [possessions USED]
#
# Note TmMP/5 is game length in minutes and adapts to the league automatically:
# 240/5 = 48 for the NBA where the WNBA's 200/5 = 40. No constant to change.
#
# NO SALARY. The WNBA original scraped Her Hoop Stats and Spotrac for cap data;
# that is all removed, along with the rvest/janitor dependencies it needed.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})


#' Weighted percentile: where each value sits in the weighted distribution
#'
#' Weighting by sample size (shot attempts, rebound chances) stops a player with
#' four attempts from outranking a full-season starter on a fluke percentage.
weighted_percentile <- function(x, w) {
  if (length(x) == 0) return(numeric(0))

  w[is.na(w)] <- 0
  x_na <- is.na(x)
  if (all(x_na) || sum(w[!x_na]) <= 0) return(rep(NA_real_, length(x)))

  x2 <- x[!x_na]; w2 <- w[!x_na]
  ord <- order(x2)
  p_sorted <- cumsum(w2[ord]) / sum(w2)

  p2 <- numeric(length(x2)); p2[ord] <- p_sorted
  out <- rep(NA_real_, length(x)); out[!x_na] <- p2
  out
}


#' Weighted mean, ignoring NAs and non-positive weights
weighted_mean_safe <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!sum(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}


#' Collapse ESPN's position labels to G / F / C
#'
#' ESPN tags most NBA players with the broad G/F/C but sprinkles in a handful of
#' PG/SG/SF/PF. Measured on 2024-25: G 252, F 206, C 80 -- but SG 10, PG 8, PF 7,
#' SF 7. Percentiles inside a seven-player group are meaningless (a player can
#' only land on 1/7ths), so leaving them split produces confident-looking
#' nonsense on the pizza plot. Collapsing gives G 270, F 219, C 80.
#'
#' Set collapse = FALSE to keep ESPN's raw labels.
nba_position_group <- function(pos, collapse = TRUE) {
  if (!collapse) return(pos)
  dplyr::case_when(
    grepl("G", pos) ~ "G",
    grepl("F", pos) ~ "F",
    pos == "C"      ~ "C",
    TRUE            ~ NA_character_
  )
}


#' Drop DNPs and zero-minute rows
nba_clean_player_box <- function(player_box) {
  player_box %>%
    filter(
      !(!is.na(did_not_play) & did_not_play),
      !is.na(minutes),
      minutes > 0
    )
}


#' Team-game and team-season totals, with opponent columns attached
#'
#' Needed as the denominators for usage, rebound percentages and assist rate.
nba_team_totals_from_player_box <- function(clean_box) {
  team_game <- clean_box %>%
    group_by(season, game_id, team_id) %>%
    summarise(
      team_minutes = sum(minutes, na.rm = TRUE),
      team_fgm     = sum(field_goals_made, na.rm = TRUE),
      team_fga     = sum(field_goals_attempted, na.rm = TRUE),
      team_fg3m    = sum(three_point_field_goals_made, na.rm = TRUE),
      team_fg3a    = sum(three_point_field_goals_attempted, na.rm = TRUE),
      team_ftm     = sum(free_throws_made, na.rm = TRUE),
      team_fta     = sum(free_throws_attempted, na.rm = TRUE),
      team_oreb    = sum(offensive_rebounds, na.rm = TRUE),
      team_dreb    = sum(defensive_rebounds, na.rm = TRUE),
      team_reb     = sum(rebounds, na.rm = TRUE),
      team_tov     = sum(turnovers, na.rm = TRUE),
      .groups = "drop"
    )

  team_game_w_opp <- team_game %>%
    left_join(team_game, by = c("season", "game_id"),
              suffix = c("", "_opp"), relationship = "many-to-many") %>%
    filter(team_id != team_id_opp)

  team_season <- team_game_w_opp %>%
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

  list(team_game = team_game, team_game_w_opp = team_game_w_opp,
       team_season = team_season)
}


#' Opponent shooting/turnover context per player (a rough on-court defence proxy)
nba_defensive_context <- function(clean_box, team_game_w_opp) {
  clean_box %>%
    left_join(team_game_w_opp, by = c("game_id", "team_id")) %>%
    group_by(athlete_id) %>%
    summarise(
      opp_efg = sum(team_fgm_opp + 0.5 * team_fg3m_opp, na.rm = TRUE) /
                sum(team_fga_opp, na.rm = TRUE),
      opp_tov_pct = sum(team_tov_opp, na.rm = TRUE) /
                    sum(team_fga_opp + 0.44 * team_fta_opp + team_tov_opp, na.rm = TRUE),
      .groups = "drop"
    )
}


#' Build the full player-season table with advanced metrics and percentiles
#'
#' @param player_box Raw player box scores (hoopR or a CSV of the same shape).
#' @param collapse_positions Collapse ESPN positions to G/F/C. See
#'   nba_position_group() for why this defaults to TRUE.
#' @param include_pos_avgs Also compute weighted position averages and where
#'   those averages sit in the distribution -- the pizza plot's reference ring.
nba_player_season <- function(player_box,
                              collapse_positions = TRUE,
                              include_pos_avgs = TRUE) {

  clean <- nba_clean_player_box(player_box)
  tt    <- nba_team_totals_from_player_box(clean)
  dfn   <- nba_defensive_context(clean, tt$team_game_w_opp)

  out <- clean %>%
    arrange(game_date) %>%                 # so last() picks the current team
    group_by(season, athlete_id) %>%
    summarise(
      athlete_display_name = last(athlete_display_name),
      position_raw         = last(athlete_position_abbreviation),
      team_id              = last(team_id),
      team_short_display_name = last(team_short_display_name),
      games_played  = n_distinct(game_id),
      games_started = sum(starter, na.rm = TRUE),
      minutes_total = sum(minutes, na.rm = TRUE),
      pts_total  = sum(points, na.rm = TRUE),
      fgm_total  = sum(field_goals_made, na.rm = TRUE),
      fga_total  = sum(field_goals_attempted, na.rm = TRUE),
      fg3m_total = sum(three_point_field_goals_made, na.rm = TRUE),
      fg3a_total = sum(three_point_field_goals_attempted, na.rm = TRUE),
      ftm_total  = sum(free_throws_made, na.rm = TRUE),
      fta_total  = sum(free_throws_attempted, na.rm = TRUE),
      oreb_total = sum(offensive_rebounds, na.rm = TRUE),
      dreb_total = sum(defensive_rebounds, na.rm = TRUE),
      reb_total  = sum(rebounds, na.rm = TRUE),
      ast_total  = sum(assists, na.rm = TRUE),
      stl_total  = sum(steals, na.rm = TRUE),
      blk_total  = sum(blocks, na.rm = TRUE),
      tov_total  = sum(turnovers, na.rm = TRUE),
      pf_total   = sum(fouls, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(position = nba_position_group(position_raw, collapse_positions)) %>%
    left_join(tt$team_season, by = c("season", "team_id")) %>%
    left_join(dfn, by = "athlete_id") %>%
    mutate(
      # Per game
      mpg = minutes_total / games_played,
      ppg = pts_total / games_played,
      rpg = reb_total / games_played,
      apg = ast_total / games_played,
      spg = stl_total / games_played,
      bpg = blk_total / games_played,
      tovpg = tov_total / games_played,
      oreb_pg = oreb_total / games_played,
      dreb_pg = dreb_total / games_played,

      # Shooting
      fg_pct  = if_else(fga_total  > 0, fgm_total  / fga_total,  NA_real_),
      fg3_pct = if_else(fg3a_total > 0, fg3m_total / fg3a_total, NA_real_),
      ft_pct  = if_else(fta_total  > 0, ftm_total  / fta_total,  NA_real_),
      efg_pct = if_else(fga_total  > 0, (fgm_total + 0.5 * fg3m_total) / fga_total, NA_real_),
      ts_pct  = if_else((fga_total + 0.44 * fta_total) > 0,
                        pts_total / (2 * (fga_total + 0.44 * fta_total)), NA_real_),

      # Per 36
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

      # Assist rate
      ast_denom = (minutes_total / (team_minutes_total / 5)) * team_fgm_total - fgm_total,
      ast_pct = if_else(minutes_total > 0 & team_minutes_total > 0 & ast_denom > 0,
                        ast_total / ast_denom, NA_real_),

      # Turnover rate, on possessions used
      tov_denom = fga_total + 0.44 * fta_total + tov_total,
      tov_pct   = if_else(tov_denom > 0, tov_total / tov_denom, NA_real_),

      # Rebound rates, on estimated chances
      oreb_chances = (minutes_total / (team_minutes_total / 5)) *
        (team_oreb_total + opp_dreb_total),
      oreb_pct = if_else(minutes_total > 0 & oreb_chances > 0,
                         oreb_total / oreb_chances, NA_real_),
      dreb_chances = (minutes_total / (team_minutes_total / 5)) *
        (team_dreb_total + opp_oreb_total),
      dreb_pct = if_else(minutes_total > 0 & dreb_chances > 0,
                         dreb_total / dreb_chances, NA_real_),

      # Shot profile
      threepar = if_else(fga_total > 0, fg3a_total / fga_total, NA_real_),
      fta_rate = if_else(fga_total > 0, fta_total  / fga_total, NA_real_),

      # Sample-size weights for the percentiles below
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

      athlete_id = as.character(athlete_id),
      headshot_url = paste0(
        "https://a.espncdn.com/i/headshots/nba/players/full/", athlete_id, ".png")
    ) %>%
    group_by(position) %>%
    mutate(
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
      def_efg_pctile_pos  = weighted_percentile(-opp_efg,    minutes_total),
      def_tov_pctile_pos  = weighted_percentile(opp_tov_pct, minutes_total)
    ) %>%
    ungroup()

  if (include_pos_avgs) out <- nba_add_position_averages(out)
  out
}


#' Weighted position averages, and where each average sits in its distribution
#'
#' The pizza plot draws the position average as a reference ring, so it needs
#' the average expressed as a percentile, not as a raw rate.
nba_add_position_averages <- function(df) {
  specs <- list(
    usage    = c("usage",    "usg_weight"),
    ts       = c("ts_pct",   "ts_weight"),
    efg      = c("efg_pct",  "efg_weight"),
    ast      = c("ast_pct",  "ast_weight"),
    stl      = c("spg",      "stl_weight"),
    tov      = c("tov_pct",  "tov_weight"),
    oreb     = c("oreb_pct", "oreb_weight"),
    dreb     = c("dreb_pct", "dreb_weight"),
    fg3      = c("fg3_pct",  "fg3_weight"),
    ft       = c("ft_pct",   "ft_weight"),
    threepar = c("threepar", "threepar_weight"),
    fta_rate = c("fta_rate", "fta_rate_weight")
  )

  df %>%
    group_by(position) %>%
    group_modify(function(g, key) {
      for (nm in names(specs)) {
        val <- g[[specs[[nm]][1]]]
        wt  <- g[[specs[[nm]][2]]]
        avg <- weighted_mean_safe(val, wt)
        g[[paste0(nm, "_pos_avg")]] <- avg
        # Append the average as a synthetic observation to read its percentile.
        g[[paste0(nm, "_pos_avg_pctile")]] <-
          weighted_percentile(c(val, avg), c(wt, mean(wt, na.rm = TRUE)))[length(val) + 1]
      }
      g
    }) %>%
    ungroup()
}
