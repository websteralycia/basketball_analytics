## ============================================================
## team_play_profile.R — per-team-season category profiles
## ------------------------------------------------------------
## Table 1 of SPEC_play_profile.md. One row per team x dimension x
## category, carrying BOTH sides (what the team does on offence, and
## what it allows on defence), so any matchup card is a join of two
## profiles rather than a recomputation.
##
## Season-wide by necessity: a percentile needs a population, so the
## whole comparison set has to be processed, not just the two teams on
## the card.
##
## Depends on: league_config, conferences, possessions, shot_zones
## ============================================================

# Below this many possessions/attempts a percentile is NA, never 50.
# Matches the reference card's "50+ possessions for percentile grades".
MIN_POSS_FOR_PERCENTILE <- 50

# Dimensions where PPP is DEGENERATE and the percentile must rank on
# frequency instead.
#
# `ball_security` is the case: a turnover scores zero, so off_ppp should be
# 0 for every team and the ranking should be a flat tie. It is not quite --
# a few possessions record a point before the turnover lands, giving MD
# 0.0043 and OSU 0.0079 where most teams sit at exactly 0.000. Ranking on
# that put eleven teams in a tie at the 29th percentile and let three
# teams outrank them on pure noise, which is how UCLA -- 2nd BEST in the
# Big Ten at 8.5% live-ball turnovers -- came out as a "struggle" on a card.
#
# For these, rank on off_freq INVERTED: turning it over more often is worse,
# so a low frequency must read as a high percentile, matching the "high =
# good for this team" contract every other percentile keeps.
FREQ_RANKED_DIMENSIONS <- c("ball_security")

# ...but the DIRECTION is a property of the category, not the dimension.
# Turning it over more often is worse; KEEPING it more often is better,
# and `no_turnover` lives in the same dimension. Inverting the whole
# dimension ranked NU -- who keep the ball 78% of the time, the worst end
# of the Big Ten -- as 4th of 18. The card never showed it because
# no_turnover is a residual bucket excluded from bullets; the dashboard
# surfaces every row, which is how it came out.
#
# So: within a freq-ranked dimension, these are the categories where MORE
# IS WORSE. Anything else there is ranked on frequency ascending, the
# normal "high = good for this team" direction.
FREQ_BAD_CATEGORIES <- c("live_ball_to", "dead_ball_to")

#' Does a higher frequency make this row worse?
#'
#' Only meaningful inside a FREQ_RANKED_DIMENSIONS dimension.
freq_is_bad <- function(category) category %in% FREQ_BAD_CATEGORIES

#' Per-game category rows for every dimension
#'
#' Returns long rows: off_team, def_team, dimension, category, n, points.
#' `unit` distinguishes possession-denominated dimensions from
#' attempt-denominated ones — they are NOT interchangeable and a card
#' must label them differently.
game_category_rows <- function(pbp_game, league) {
  out <- list()

  # --- context: possession-denominated ---------------------------------
  poss <- classify_possessions(pbp_game, league)
  if (nrow(poss) > 0) {
    out[[1]] <- poss |>
      dplyr::group_by(off_team, def_team, category = context) |>
      dplyr::summarise(n = dplyr::n(), points = sum(points), .groups = "drop") |>
      dplyr::mutate(dimension = "context", unit = "possession")
  }

  # --- ball_security: how possessions are lost -------------------------
  # A turnover is LIVE-ball when the opponent\'s next possession starts on a
  # steal, and dead-ball otherwise -- verified exhaustive on 401851531,
  # where 29 turnovers split 19 steal / 10 turnover with nothing left over.
  #
  # The distinction is the whole point for scouting: a live-ball turnover
  # hands over a running start, a dead-ball one does not.
  #
  # PPP IS DEGENERATE HERE -- a turnover scores zero by definition, so the
  # ppp column on these rows is 0 and must not be printed. dimension_headers
  # carries ppp_meaningful = FALSE and the sentence template drops the
  # clause. `no_turnover` keeps the denominator at ALL possessions so freq
  # reads as a share of possessions rather than a share of turnovers.
  if (nrow(poss) > 0) {
    nxt <- dplyr::lead(poss$start_reason)
    cat_bs <- ifelse(
      poss$end_reason != "turnover", "no_turnover",
      ifelse(!is.na(nxt) & nxt == "steal", "live_ball_to", "dead_ball_to"))
    out[[length(out) + 1L]] <- poss |>
      dplyr::mutate(category = cat_bs) |>
      dplyr::group_by(off_team, def_team, category) |>
      dplyr::summarise(n = dplyr::n(), points = sum(points), .groups = "drop") |>
      dplyr::mutate(dimension = "ball_security", unit = "possession")
  }

  # --- shot_zone and creation: attempt-denominated ---------------------
  p <- chr_ids(pbp_game)
  ab <- stats::setNames(c(as.character(p$home_team_abbrev[1]), as.character(p$away_team_abbrev[1])),
                        c(as.character(p$home_team_id[1]), as.character(p$away_team_id[1])))
  blob <- paste(p$type_text, p$text)
  is_shot <- grepl("shot|jumper|layup|dunk", blob, ignore.case = TRUE) &
             !grepl("rebound|free throw|block", blob, ignore.case = TRUE)
  sh <- p[is_shot & !is.na(p$team_id) & p$team_id %in% names(ab), ]

  if (nrow(sh) > 0) {
    three <- is_three_pt(sh)
    made  <- is_made(sh)
    shots <- tibble::tibble(
      off_team = unname(ab[sh$team_id]),
      def_team = unname(ab[ifelse(sh$team_id == names(ab)[1], names(ab)[2], names(ab)[1])]),
      zone     = classify_shot_zone(sh$coordinate_x, sh$coordinate_y, three),
      assisted = grepl("assist", sh$text, ignore.case = TRUE),
      points   = ifelse(made, ifelse(three, 3L, 2L), 0L),
      made     = made
    )

    z <- shots[!is.na(shots$zone), ]
    if (nrow(z) > 0) {
      out[[length(out) + 1L]] <- z |>
        dplyr::group_by(off_team, def_team, category = zone) |>
        dplyr::summarise(n = dplyr::n(), points = sum(points), .groups = "drop") |>
        dplyr::mutate(dimension = "shot_zone", unit = "attempt")
    }

    # creation is defined over MADE field goals only: ESPN records an
    # assist only on a make, so this is not a shot-creation rate.
    mk <- shots[shots$made, ]
    if (nrow(mk) > 0) {
      out[[length(out) + 1L]] <- mk |>
        dplyr::mutate(category = ifelse(assisted, "assisted", "unassisted")) |>
        dplyr::group_by(off_team, def_team, category) |>
        dplyr::summarise(n = dplyr::n(), points = sum(points), .groups = "drop") |>
        dplyr::mutate(dimension = "creation", unit = "made_fg")
    }
  }

  dplyr::bind_rows(out)
}

#' Build the team play profile for a league-season and population
#'
#' @param league,season  As elsewhere.
#' @param population `"national"`, `"conference"` (needs `team`), or an
#'   explicit conference key — ESPN name or custom, see conferences.R.
#'   Only games involving population teams are processed, which is why a
#'   conference build costs ~5% of a national one.
#' @param team Required when `population = "conference"`.
#' @param pbp Optional pre-loaded season play-by-play, to avoid a repeat
#'   bulk load when building several profiles.
#' @param min_n Percentile floor; below this the percentile is NA.
#'
#' @return tibble: league, season, population, team, dimension, category,
#'   unit, source, off_n, off_freq, off_ppp, off_ppp_pctl, def_n,
#'   def_ppp, def_ppp_pctl
build_team_play_profile <- function(league, season,
                                    population = "national",
                                    team   = NULL,
                                    pbp    = NULL,
                                    min_n  = MIN_POSS_FOR_PERCENTILE,
                                    quiet  = FALSE) {

  pop_teams <- population_teams(population, league, season, team = team)
  if (!quiet) message(sprintf("Population '%s': %d teams", population, length(pop_teams)))

  if (is.null(pbp)) {
    cfg <- league_cfg(league)
    if (is.null(cfg$bulk_pbp_fn)) {
      stop("No bulk play-by-play loader configured for league '", league, "'.", call. = FALSE)
    }
    if (!quiet) message("Loading season play-by-play ...")
    pbp <- cfg$bulk_pbp_fn(season)
  }
  pbp <- chr_ids(pbp)

  # Keep only games involving a population team. This is the whole
  # performance story: a 12-team conference touches a few hundred games
  # out of six thousand.
  keep <- pbp$home_team_abbrev %in% pop_teams | pbp$away_team_abbrev %in% pop_teams
  pbp  <- pbp[!is.na(keep) & keep, ]
  gids <- unique(pbp$game_id)
  if (length(gids) == 0) stop("No games found for this population.", call. = FALSE)
  if (!quiet) message(sprintf("Processing %s games ...", format(length(gids), big.mark = ",")))

  split_pbp <- split(pbp, pbp$game_id)
  rows <- vector("list", length(split_pbp))
  failed <- 0L
  for (i in seq_along(split_pbp)) {
    rows[[i]] <- tryCatch(game_category_rows(split_pbp[[i]], league),
                          error = function(e) { failed <<- failed + 1L; NULL })
    if (!quiet && i %% 250 == 0) message(sprintf("  %d / %d", i, length(split_pbp)))
  }
  long <- dplyr::bind_rows(rows)
  if (failed > 0 && !quiet) message(sprintf("  %d game(s) failed to classify", failed))
  if (nrow(long) == 0) stop("No category rows produced.", call. = FALSE)

  # --- offence: what the team did --------------------------------------
  off <- long |>
    dplyr::filter(off_team %in% pop_teams) |>
    dplyr::group_by(team = off_team, dimension, category, unit) |>
    dplyr::summarise(off_n = sum(n), off_pts = sum(points), .groups = "drop") |>
    dplyr::group_by(team, dimension) |>
    dplyr::mutate(off_freq = off_n / sum(off_n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(off_ppp = off_pts / off_n)

  # --- defence: what the team allowed ----------------------------------
  def <- long |>
    dplyr::filter(def_team %in% pop_teams) |>
    dplyr::group_by(team = def_team, dimension, category, unit) |>
    dplyr::summarise(def_n = sum(n), def_pts = sum(points), .groups = "drop") |>
    dplyr::mutate(def_ppp = def_pts / def_n)

  prof <- dplyr::full_join(off, def, by = c("team", "dimension", "category", "unit"))

  # --- percentiles within the population -------------------------------
  # Offence: higher points per unit is better, so a high percentile means
  # a strength. Defence: percentile rises with points ALLOWED, so a high
  # number on the card's right-hand side always means "attack here".
  # Storing it defence-favourable and flipping at render time is how the
  # two sides of a card end up meaning opposite things by accident.
  # The profile stores def_n but not a defensive frequency. Within a
  # dimension x category group, def_n IS the count, so its share of the
  # group's total plays the same role.
  def_freq_safe <- function(dn, on) {
    tot <- sum(dn, na.rm = TRUE)
    if (!is.finite(tot) || tot <= 0) rep(NA_real_, length(dn)) else dn / tot
  }

  pctl <- function(x, n, min_n) {
    ok <- !is.na(x) & !is.na(n) & n >= min_n
    out <- rep(NA_real_, length(x))
    if (sum(ok) >= 2) out[ok] <- 100 * (rank(x[ok], ties.method = "average") - 1) / (sum(ok) - 1)
    round(out)
  }

  prof <- prof |>
    dplyr::group_by(dimension, category) |>
    dplyr::mutate(
      # See FREQ_RANKED_DIMENSIONS: for those, PPP carries no signal and
      # frequency does, inverted so that "gives it away less" ranks high.
      off_ppp_pctl = if (dplyr::first(dimension) %in% FREQ_RANKED_DIMENSIONS) {
        # Sign is per-CATEGORY: more turnovers is worse, more kept
        # possessions is better. See FREQ_BAD_CATEGORIES.
        pctl(if (freq_is_bad(dplyr::first(category))) -off_freq else off_freq,
             off_n, min_n)
      } else {
        pctl(off_ppp, off_n, min_n)
      },
      def_ppp_pctl = if (dplyr::first(dimension) %in% FREQ_RANKED_DIMENSIONS) {
        # Defence keeps its attackable orientation: a defence that forces
        # turnovers OFTEN is hard to attack, so a high forced-turnover
        # frequency must read as a LOW attackable percentile.
        pctl(if (freq_is_bad(dplyr::first(category)))
               -def_freq_safe(def_n, off_n) else def_freq_safe(def_n, off_n),
             def_n, min_n)
      } else {
        pctl(def_ppp, def_n, min_n)
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(league = league, season = as.integer(season),
                  population = population, source = "espn_derived") |>
    dplyr::select(league, season, population, team, dimension, category, unit, source,
                  off_n, off_freq, off_ppp, off_ppp_pctl,
                  def_n, def_ppp, def_ppp_pctl) |>
    dplyr::arrange(team, dimension, category)

  prof
}

#' Join two profiles into the render-ready card view
#'
#' Table 2 of the spec: our offence against their defence, one row per
#' category.
attack_index_card <- function(profile, team, opponent, dimension = NULL) {
  us   <- profile[profile$team == team, ]
  them <- profile[profile$team == opponent, ]
  if (nrow(us) == 0)   stop("No profile rows for team '", team, "'.", call. = FALSE)
  if (nrow(them) == 0) stop("No profile rows for opponent '", opponent, "'.", call. = FALSE)
  if (!is.null(dimension)) {
    us   <- us[us$dimension %in% dimension, ]
    them <- them[them$dimension %in% dimension, ]
  }
  dplyr::inner_join(
    us[, c("dimension", "category", "unit", "source", "off_n", "off_freq",
           "off_ppp", "off_ppp_pctl")],
    them[, c("dimension", "category", "def_ppp", "def_ppp_pctl")],
    by = c("dimension", "category")
  ) |>
    dplyr::transmute(
      dimension, category, unit, source,
      poss = off_n, freq = off_freq,
      ppp = off_ppp, ppp_percentile = off_ppp_pctl,
      def_ppp, def_ppp_percentile = def_ppp_pctl
    ) |>
    dplyr::arrange(dimension, dplyr::desc(freq))
}
