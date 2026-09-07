## ============================================================
## profile_display.R — how a profile row is SAID, in any surface
## ------------------------------------------------------------
## These primitives used to live in consumers/gameprep_cards/, which
## was fine while the card was the only surface. It is not: the
## dashboard and the Q&A agent render the same rows and must not
## re-derive the rules, because the two that matter here are both
## non-obvious and both got the card wrong once already.
##
##   1. PPP IS DEGENERATE ON SOME DIMENSIONS. A turnover scores zero by
##      definition, so every team's ball_security PPP is ~0.00 and
##      printing it says nothing while looking like a measurement.
##      dimension_headers.csv carries `ppp_meaningful` for exactly this.
##
##   2. SOME DIMENSIONS RANK INVERTED. ball_security ranks on
##      -off_freq (see FREQ_RANKED_DIMENSIONS in team_play_profile.R),
##      so a high percentile means "keeps the ball". The row is LABELLED
##      with the turnovers, so printing the raw rank tells a reader the
##      opposite of the truth: NU's worst-in-conference 13% live-ball
##      turnover rate renders as "18th of 18", which reads as fewest.
##
## Both were fixed on the card's bullet path in September and NOT on its
## profile table, which is how the same two bugs shipped twice. Fixing
## them here is what stops them shipping a third time on the dashboard.
##
## Depends on: team_play_profile.R (FREQ_RANKED_DIMENSIONS)
## ============================================================

# How each unit reads in prose. Keyed off the `unit` column that
# build_team_play_profile() stamps on every row. The three are NOT
# interchangeable and must never share an axis or column header.
UNIT_PHRASE <- c(
  possession = "points per possession",
  attempt    = "points per attempt",
  made_fg    = "points per made FG"
)

UNIT_DENOMINATOR <- c(
  possession = "of possessions",
  attempt    = "of attempts",
  made_fg    = "of made FGs"
)

#' Number with the right ordinal suffix
#'
#' Percentiles land on 73, 82 and 91 often enough at conference
#' population sizes that "73th" would be on most cards.
ordinal <- function(n) {
  suffix <- if (n %% 100 %in% 11:13) "th" else
    switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(n, suffix)
}

#' Recover 1-based rank from a stored percentile
#'
#' Percentiles are stored as `100 * (rank_asc - 1) / (n - 1)`, which puts
#' the WORST team at 0. Coaches read "1st" as best, so this returns the
#' descending rank: 1 = best in the population, n = worst.
#'
#' Exact for untied values. Ties average in the stored percentile and so
#' produce a fractional rank here; rounding is the right call because no
#' surface can show "4.5th of 12".
rank_from_percentile <- function(pctl, n) {
  ifelse(is.na(pctl), NA_integer_, as.integer(round(n - pctl * (n - 1) / 100)))
}

#' Flip a stored defensive percentile for display
#'
#' `def_ppp_pctl` is stored PRE-FLIPPED so that high = attackable. Any
#' display that is not the attack side must flip it back, and exactly
#' once — flipping twice is silent and looks plausible.
display_def_percentile <- function(def_ppp_percentile) {
  ifelse(is.na(def_ppp_percentile), NA_integer_,
         as.integer(100L - def_ppp_percentile))
}

#' Does PPP carry signal on this dimension?
#'
#' Reads `ppp_meaningful` from the dimension headers rather than naming
#' dimensions inline, so adding a rate-only dimension is a config edit.
#' Absent config means TRUE: the flag is an exception, not a requirement.
ppp_meaningful_for <- function(dimension, source, headers) {
  if (is.null(headers) || !"ppp_meaningful" %in% names(headers)) return(TRUE)
  f <- headers$ppp_meaningful[headers$dimension == dimension &
                              headers$source == source]
  if (length(f) == 0 || is.na(f[1])) TRUE else isTRUE(as.logical(f[1]))
}

#' Does this row's standing read in the opposite direction to its rank?
#'
#' True only where a HIGHER frequency is worse — the turnover categories,
#' not "kept the ball", which sits in the same dimension and runs the
#' other way. Passing the dimension alone was the bug: it inverted the
#' wording for a row whose rank was not inverted.
is_freq_ranked <- function(dimension, category = NULL) {
  if (!dimension %in% FREQ_RANKED_DIMENSIONS) return(FALSE)
  if (is.null(category)) return(TRUE)
  freq_is_bad(category)
}

#' The volume-and-rate line under a row's label
#'
#' "25% of attempts · 0.83 points per attempt", or just "13% of
#' possessions" where the rate is degenerate. Callers must not append
#' their own PPP clause; that is the bug this exists to prevent.
profile_row_meta <- function(freq, ppp, unit, dimension, source, headers,
                             sep = " · ") {
  vol <- sprintf("%.0f%% %s", freq * 100, UNIT_DENOMINATOR[[unit]])
  if (!ppp_meaningful_for(dimension, source, headers)) return(vol)
  paste0(vol, sep, sprintf("%.2f %s", ppp, UNIT_PHRASE[[unit]]))
}

#' Standing for a row whose rank is inverted relative to its label
#'
#' Expressed as a rank of the NAMED quantity: pctl 0 (worst at keeping
#' the ball) is the MOST turnovers, pctl 100 the fewest. The extremes get
#' words because "18th-most" is a sentence nobody finishes reading.
#'
#' `style = "compact"` drops the population name for a narrow column.
format_inverted_standing <- function(pctl, n, population,
                                     style = c("long", "compact")) {
  style <- match.arg(style)
  if (is.na(pctl)) return("unranked")
  rk_good <- rank_from_percentile(pctl, n)   # 1 = best at keeping the ball
  tail_txt <- if (style == "long") paste0(" in the ", population) else ""

  # Say it in whichever direction is the notable one. A team near the top
  # of the population has the FEWEST; one near the bottom has the MOST.
  # Reporting UCLA as "17th-most" is technically true and useless.
  if (rk_good <= n / 2) {
    if (rk_good == 1L) return(paste0(if (style == "long") "the fewest" else "fewest", tail_txt))
    paste0(if (style == "long") "the " else "", ordinal(rk_good), "-fewest", tail_txt)
  } else {
    rk_most <- n + 1L - rk_good
    if (rk_most == 1L) return(paste0(if (style == "long") "the most" else "most", tail_txt))
    paste0(if (style == "long") "the " else "", ordinal(rk_most), "-most", tail_txt)
  }
}

#' A row's standing, correct for whichever way its dimension ranks
#'
#' The one call every surface should make. Non-inverted dimensions read
#' "13th of 18"; inverted ones read in the direction their label names.
profile_row_standing <- function(pctl, n, population, dimension,
                                 category = NULL,
                                 style = c("long", "compact")) {
  style <- match.arg(style)
  if (is_freq_ranked(dimension, category)) {
    return(format_inverted_standing(pctl, n, population, style = style))
  }
  if (is.na(pctl)) return("unranked")
  paste0(ordinal(rank_from_percentile(pctl, n)), " of ", n)
}
