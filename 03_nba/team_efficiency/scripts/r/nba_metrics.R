# nba_metrics.R ---------------------------------------------------------------
#
# Pure metric functions for NBA team analysis: possessions, efficiency ratings,
# and Dean Oliver's Four Factors.
#
# DESIGN CONTRACT
#   Every function here takes a data frame (or vectors) and returns a data frame
#   (or vector). Nothing in this file makes a network call, reads or writes a
#   file, or prints. That keeps the metrics testable, and it means the same code
#   works whether the data came from hoopR, a supplied CSV, or a fixture.
#
#   Fetching lives in nba_data.R. Presentation lives in the analysis script.
#
# CONVENTIONS AND WHY
#   Possessions   Oliver's estimate, averaged across the two teams:
#
#                   FGA + 0.44*FTA + TOV
#                       - 1.07 * (OREB / (OREB + OppDREB)) * (FGA - FGM)
#
#                 VALIDATED AGAINST NBA.COM, all 30 teams, 2025-26. Mean error
#                 +0.06 ORtg and +0.05 DRtg (sd 0.24 / 0.20); worst single team
#                 0.64. Possession count within ~4 of their tracked figure.
#
#                 The simpler FGA + 0.44*FTA + TOV - OREB is still here as
#                 nba_possessions(), but do not reach for it expecting NBA.com:
#                 measured against all 30 teams it overstates possessions by
#                 ~147 a season (~1.8 a game) and so reads 2.0 points LOW on
#                 both ORtg and DRtg. Net rating survives, since both ends
#                 shift together, but neither component matches the site.
#
#                 The reason is the rebound term. Subtracting raw OREB
#                 undercounts extended possessions, because team offensive
#                 rebounds and missed-free-throw rebounds are never credited
#                 to a player and so never reach the box score. Oliver's term
#                 estimates the true count instead of trusting the tally.
#
#                 Averaged rather than computed per side: two teams in a game
#                 face nearly the same number of possessions, so averaging the
#                 two estimates cancels noise. Measured, it cuts the spread of
#                 the error against NBA.com from sd 0.33 to 0.24.
#
#   Turnovers     ESPN box scores carry THREE turnover columns:
#                   turnovers        player-attributed turnovers only
#                   team_turnovers   team-level (shot clock, 5-second, etc.)
#                   total_turnovers  the sum of the two
#                 Validated against NBA.com's own season totals (2024-25):
#                   total_turnovers  -> 22/30 teams exact, league diff 66
#                   turnovers        ->  0/30 teams exact, league diff 1932
#                 Always use total_turnovers. Using `turnovers` understates
#                 turnovers ~5.5% and inflates offensive rating accordingly.
#
#   ORB%          OREB / (OREB + OPP_DREB)
#                 The share of available offensive rebounds captured. Note this
#                 is NOT OREB / (OREB + OPP_OREB) -- that compares two offensive
#                 rebound counts and measures nothing meaningful.
#
#   TOV%          TOV / Possessions
#   eFG%          (FGM + 0.5 * FG3M) / FGA
#   FT Rate       FTA / FGA   (some sources use FTM/FGA; stated in the output)
#
#   Aggregation   Season figures are computed by summing counting stats first,
#                 then taking the ratio -- never by averaging per-game ratios,
#                 which silently weights low-possession games too heavily.
#
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})


# ESPN season_type codes (verified against 2024-25 NBA game dates) -------------
#   2 = regular season   2468 team-games, Oct-Apr
#   3 = postseason        168 team-games, Apr-Jun
#   5 = play-in            12 team-games, Apr only (6 games)
# Filtering on `season_type == 2` alone silently drops play-in games, which is
# usually what you want -- but be deliberate about it.
NBA_SEASON_TYPE <- c(regular = 2L, postseason = 3L, play_in = 5L)


# THE COLUMN CONTRACT ---------------------------------------------------------
#
# Everything downstream expects these names. Any source -- hoopR, a CSV handed
# to you in a take-home, a fixture -- becomes usable by renaming into them via
# nba_standardize(). This is the seam that makes the pipeline source-agnostic.
NBA_STAT_COLS <- c("pts", "fga", "fgm", "fg3m", "fta", "oreb", "dreb", "tov")
NBA_ID_COLS   <- c("game_id", "team_id")


#' Rename an arbitrary table into the canonical column contract
#'
#' Lower-cases every incoming name first, so a CSV with PTS / FGA / OREB needs
#' no mapping at all. Supply `mapping` for anything that does not line up, in
#' `canonical = "source"` form:
#'
#'   nba_standardize(df, c(tov = "turnovers", fg3m = "three_pm"))
#'
#' Fails loudly and specifically rather than producing a frame that computes
#' wrong numbers -- a silent NA here would propagate into every metric.
#'
#' @param df Any data frame with one row per team per game.
#' @param mapping Named character vector, canonical name = source name.
#' @param require_ids Require game_id/team_id (needed to pair opponents).
nba_standardize <- function(df, mapping = NULL, require_ids = TRUE) {
  names(df) <- tolower(names(df))

  if (!is.null(mapping)) {
    mapping <- mapping[mapping %in% names(df)]
    if (length(mapping)) df <- dplyr::rename(df, !!!mapping)
  }

  needed <- if (require_ids) c(NBA_ID_COLS, NBA_STAT_COLS) else NBA_STAT_COLS
  missing <- setdiff(needed, names(df))

  if (length(missing)) {
    stop(
      "nba_standardize(): missing required column(s): ",
      paste(missing, collapse = ", "), "\n",
      "  Columns present: ", paste(head(names(df), 40), collapse = ", "), "\n",
      "  Supply a mapping, e.g. nba_standardize(df, c(",
      paste0(missing[1], ' = "YOUR_COLUMN"'), "))",
      call. = FALSE
    )
  }
  df
}


#' Attach each team-game to its opponent's line from the same game
#'
#' Pure counterpart to the hoopR-specific loader: works on any standardized
#' frame with one row per team per game. Opponent columns get an `opp_` prefix.
nba_pair_opponents <- function(df) {
  df <- nba_standardize(df)

  bad <- df %>% dplyr::count(game_id) %>% dplyr::filter(n != 2)
  if (nrow(bad)) {
    warning(nrow(bad), " game_id(s) do not have exactly 2 rows; ",
            "those games will be dropped. First few: ",
            paste(utils::head(bad$game_id, 3), collapse = ", "))
  }

  opp <- df %>%
    dplyr::select(dplyr::all_of(c("game_id", "team_id", NBA_STAT_COLS))) %>%
    dplyr::rename_with(\(x) paste0("opp_", x), -game_id)

  df %>%
    dplyr::inner_join(opp, by = "game_id", relationship = "many-to-many") %>%
    dplyr::filter(team_id != opp_team_id)
}


#' Possessions, simple box-score estimate
#'
#' Kept for comparison and for callers that have no rebound split. This is NOT
#' what NBA.com reports -- see the note at the top of this file. Prefer
#' `nba_possessions_oliver()`.
#'
#' @param fga,fta,tov,oreb Numeric vectors of equal length.
#' @param ft_coef Free-throw coefficient, default 0.44.
#' @return Numeric vector of estimated possessions.
nba_possessions <- function(fga, fta, tov, oreb, ft_coef = 0.44) {
  fga + ft_coef * fta + tov - oreb
}


#' Possessions, Oliver's estimate with the team-rebound adjustment
#'
#' Replaces the raw OREB subtraction with an estimate of offensive rebounds
#' that includes the ones the box score never attributes to a player. This is
#' the one that reproduces NBA.com.
#'
#' @param fga,fgm,fta,tov,oreb Team counting stats.
#' @param opp_dreb Opponent defensive rebounds, for the rebound rate.
#' @param ft_coef Free-throw coefficient, default 0.44.
#' @return Numeric vector of estimated possessions.
nba_possessions_oliver <- function(fga, fgm, fta, tov, oreb, opp_dreb,
                                   ft_coef = 0.44) {
  reb_chances <- oreb + opp_dreb
  orb_rate    <- ifelse(reb_chances > 0, oreb / reb_chances, 0)
  fga + ft_coef * fta + tov - 1.07 * orb_rate * (fga - fgm)
}


#' Effective field goal percentage
nba_efg <- function(fgm, fg3m, fga) {
  ifelse(fga > 0, (fgm + 0.5 * fg3m) / fga, NA_real_)
}


#' Offensive rebound percentage
#'
#' @param oreb Offensive rebounds by the team.
#' @param opp_dreb Defensive rebounds by the opponent.
nba_orb_pct <- function(oreb, opp_dreb) {
  denom <- oreb + opp_dreb
  ifelse(denom > 0, oreb / denom, NA_real_)
}


#' True shooting percentage
#'
#' Note the 2 in the denominator: FGA + 0.44*FTA counts scoring possessions,
#' but points arrive two at a time, so without it TS% reads ~1.10 rather than
#' ~0.55.
nba_ts_pct <- function(pts, fga, fta) {
  denom <- 2 * (fga + 0.44 * fta)
  ifelse(denom > 0, pts / denom, NA_real_)
}


#' Pace: possessions per 48 minutes
#'
#' Pace = 48 * (Tm Poss + Opp Poss) / (2 * (Tm MP / 5))
#'
#' `Tm MP` is the sum across all five floor slots -- 240 in a regulation game --
#' so `Tm MP / 5` is simply the length of the game in minutes. We take that from
#' the period count rather than summing player minutes, because ESPN rounds
#' each player's minutes to whole numbers and team totals land on 237-239 about
#' 60% of the time. Period count is exact.
#'
#' @param poss,opp_poss Possession estimates for the two sides.
#' @param game_minutes Length of the game: 48 in regulation, +5 per overtime.
nba_pace <- function(poss, opp_poss, game_minutes = 48) {
  ifelse(game_minutes > 0,
         48 * (poss + opp_poss) / (2 * game_minutes),
         NA_real_)
}


#' Game length in minutes from ESPN's period count
#'
#' 4 periods = 48 minutes; each overtime adds 5.
nba_game_minutes <- function(periods) {
  ifelse(is.na(periods) | periods < 4, NA_real_, 48 + 5 * (periods - 4))
}


#' Attach possessions, ratings, and Four Factors to a paired team/opponent frame
#'
#' Expects one row per team-game with both sides present, using the column
#' names produced by `nba_team_game_pairs()`.
#'
#' @return The input frame with metric columns appended.
nba_add_metrics <- function(df, ft_coef = 0.44) {
  df %>%
    mutate(
      # Both sides get the SAME possession count: the average of the two
      # estimates. A game hands the two teams within a possession or two of
      # each other, so the average is the better estimate of a quantity they
      # effectively share, and it is what matches NBA.com.
      poss_tm_  = nba_possessions_oliver(fga, fgm, fta, tov, oreb, opp_dreb,
                                         ft_coef = ft_coef),
      poss_opp_ = nba_possessions_oliver(opp_fga, opp_fgm, opp_fta, opp_tov,
                                         opp_oreb, dreb, ft_coef = ft_coef),
      poss     = (poss_tm_ + poss_opp_) / 2,
      opp_poss = poss,

      ortg = ifelse(poss > 0, 100 * pts / poss, NA_real_),
      drtg = ifelse(opp_poss > 0, 100 * opp_pts / opp_poss, NA_real_),
      net_rtg = ortg - drtg,

      # Four Factors -- team
      efg      = nba_efg(fgm, fg3m, fga),
      tov_pct  = ifelse(poss > 0, tov / poss, NA_real_),
      orb_pct  = nba_orb_pct(oreb, opp_dreb),
      ft_rate  = ifelse(fga > 0, fta / fga, NA_real_),
      ts_pct   = nba_ts_pct(pts, fga, fta),

      # Four Factors -- opponent (i.e. the defensive side of the ledger)
      opp_efg     = nba_efg(opp_fgm, opp_fg3m, opp_fga),
      opp_tov_pct = ifelse(opp_poss > 0, opp_tov / opp_poss, NA_real_),
      opp_orb_pct = nba_orb_pct(opp_oreb, dreb),
      opp_ft_rate = ifelse(opp_fga > 0, opp_fta / opp_fga, NA_real_),
      opp_ts_pct  = nba_ts_pct(opp_pts, opp_fga, opp_fta)
    ) %>%
    # Pace requires game length, which only exists once the schedule has been
    # joined in (nba_team_game_pairs does this). Skip it otherwise rather than
    # silently assuming every game ran 48 minutes.
    {
      if ("game_minutes" %in% names(.)) {
        mutate(., pace = nba_pace(poss, opp_poss, game_minutes))
      } else .
    }
}


#' Aggregate per-game rows to totals, then recompute rate metrics
#'
#' Sums counting stats across the grouping columns and derives ratios from the
#' summed totals. This is deliberately NOT `mean()` of the per-game rates.
#'
#' @param df Per-game frame from `nba_team_game_pairs()`.
#' @param ... Grouping columns (e.g. team_id, team_display_name).
nba_aggregate <- function(df, ..., ft_coef = 0.44) {
  count_cols <- c(
    "pts", "fga", "fgm", "fg3m", "fta", "oreb", "dreb", "tov",
    "opp_pts", "opp_fga", "opp_fgm", "opp_fg3m", "opp_fta",
    "opp_oreb", "opp_dreb", "opp_tov"
  )

  if ("game_minutes" %in% names(df)) count_cols <- c(count_cols, "game_minutes")

  df %>%
    group_by(...) %>%
    summarise(
      games = n(),
      across(all_of(count_cols), \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    nba_add_metrics(ft_coef = ft_coef)
}
