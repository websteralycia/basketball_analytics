## ============================================================
## team_season_summary.R — season context for the card header
## ------------------------------------------------------------
## The pregame card's header tiles (record, pace, recent form) had no
## producer: render_pregame_card() took them as `stats = list(...)`
## parameters and the committed sample cards carried hand-typed values.
## They were wrong, not merely fake — the USU card showed the opponent
## at 71.2 pace where SDSU actually played 66.7, and the SDSU card
## showed 68.4 where SJSU played 73.6.
##
## This file is that producer. It computes NOTHING about play types or
## efficiency; it answers "how good are they, how fast do they play, and
## how are they going lately".
##
## PACE CONVENTION — deliberate, and it must be stated on the card.
##
##   Pace here = the play profile's OWN possession count per 40 minutes
##   (per 48 for the NBA). It does NOT use the 0.44 box-score estimator.
##
##   Why: the card already prints PPP figures derived from
##   classify_possessions(), so taking pace from anywhere else would put
##   two different possession definitions on one card, and a reader
##   dividing points by pace would not reproduce the PPP beside it.
##
##   Consequence: this reads about 2.7% below a 0.44 box estimate — that
##   gap was measured over 10 team-games while fixing the and-1 bugs
##   (mean signed -1.25% after the fix, mean absolute 2.66%). Anyone
##   checking against a site that publishes the estimator will see a
##   couple of possessions of difference. That is a convention gap, not
##   an error, which is exactly why the card should name its estimator.
##
## MINUTES are taken from the schedule's period count, not assumed to be
## regulation: 40 + 5 per overtime for wbb/wnba, 48 + 5 for the NBA,
## 40 + 5 for mbb (two 20-minute halves). Overtime is rare enough that
## ignoring it looks harmless and is not — USU's pace moves 71.8 -> 71.5.
##
## Depends on: league_config.R, dplyr
## ============================================================

#' Season context for one or more teams
#'
#' @param league One of "wnba", "wbb", "mbb", "nba".
#' @param season Season end year.
#' @param teams Team abbreviations to summarise. NULL returns every team
#'   found in the schedule. Abbreviations are the same key the play
#'   profile uses (`home_team_abbrev` in the play-by-play), so results
#'   join to a profile directly.
#' @param profile Optional play profile. Supplies the possession counts
#'   pace is built from; without it `pace` comes back NA rather than
#'   silently switching to a different estimator.
#' @param schedule Optional pre-loaded schedule, to avoid refetching.
#' @param last_n Length of the recent-form window.
#'
#' @return One row per team: games, wins, losses, record, win_pct,
#'   points for/against per game, margin, possessions, minutes, pace,
#'   and the last-N record.
team_season_summary <- function(league, season,
                                teams    = NULL,
                                profile  = NULL,
                                schedule = NULL,
                                team_box = NULL,
                                as_of    = NULL,
                                last_n   = 5L) {

  cfg <- league_cfg(league)

  if (!is.null(as_of)) as_of <- as.Date(as_of)

  if (is.null(schedule)) {
    if (is.null(cfg$sched_fn)) {
      stop("No schedule loader configured for league '", league, "'.", call. = FALSE)
    }
    schedule <- cfg$sched_fn(season)
  }

  need <- c("game_id", "game_date", "home_abbreviation", "away_abbreviation",
            "home_score", "away_score", "status_type_completed")
  missing <- setdiff(need, names(schedule))
  if (length(missing)) {
    stop("Schedule is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  sched <- schedule[!is.na(schedule$status_type_completed) &
                      schedule$status_type_completed, ]

  # "As of" cutoff -- everything derived from the schedule and the team box
  # respects it. Pace does NOT (see the pace section below), which is why
  # the window each figure covers is returned as a column.
  if (!is.null(as_of)) {
    sched <- sched[!is.na(sched$game_date) & as.Date(sched$game_date) <= as_of, ]
    if (nrow(sched) == 0) {
      stop("No completed games on or before ", format(as_of), ".", call. = FALSE)
    }
  }
  if (nrow(sched) == 0) {
    stop("No completed games in the ", season, " ", league, " schedule.", call. = FALSE)
  }

  # Period count gives exact game length. Absent, assume regulation and
  # say so rather than quietly under-counting overtime minutes.
  if ("status_period" %in% names(sched)) {
    periods <- suppressWarnings(as.integer(sched$status_period))
    periods[is.na(periods) | periods < cfg$reg_periods] <- cfg$reg_periods
  } else {
    warning("Schedule has no status_period; assuming every game was regulation.",
            call. = FALSE)
    periods <- rep(cfg$reg_periods, nrow(sched))
  }
  reg_minutes  <- cfg$reg_periods * cfg$period_seconds / 60
  ot_minutes   <- (periods - cfg$reg_periods) * cfg$ot_seconds / 60
  game_minutes <- reg_minutes + ot_minutes

  # One row per team per game, so a team's own and opponent's scores are
  # on the same row whichever side it played.
  long <- dplyr::bind_rows(
    tibble::tibble(
      team = sched$home_abbreviation, opponent = sched$away_abbreviation,
      game_date = as.Date(sched$game_date),
      pts = suppressWarnings(as.numeric(sched$home_score)),
      opp_pts = suppressWarnings(as.numeric(sched$away_score)),
      minutes = game_minutes
    ),
    tibble::tibble(
      team = sched$away_abbreviation, opponent = sched$home_abbreviation,
      game_date = as.Date(sched$game_date),
      pts = suppressWarnings(as.numeric(sched$away_score)),
      opp_pts = suppressWarnings(as.numeric(sched$home_score)),
      minutes = game_minutes
    )
  )
  long <- long[!is.na(long$team) & !is.na(long$pts) & !is.na(long$opp_pts), ]

  # Derive the result from the scores rather than trusting home_winner:
  # it is NA on some rows even where both scores are present.
  long$won <- long$pts > long$opp_pts

  if (!is.null(teams)) long <- long[long$team %in% teams, ]
  if (nrow(long) == 0) {
    stop("No completed games found for the requested team(s).", call. = FALSE)
  }

  base <- long |>
    dplyr::group_by(team) |>
    dplyr::summarise(
      games       = dplyr::n(),
      wins        = sum(won),
      losses      = sum(!won),
      minutes     = sum(minutes),
      pts_per_game     = mean(pts),
      opp_pts_per_game = mean(opp_pts),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      record  = paste0(wins, "-", losses),
      win_pct = wins / games,
      margin  = pts_per_game - opp_pts_per_game
    )

  # Recent form: the last N COMPLETED games by date.
  form <- long |>
    dplyr::group_by(team) |>
    dplyr::arrange(dplyr::desc(game_date), .by_group = TRUE) |>
    dplyr::slice_head(n = last_n) |>
    dplyr::summarise(
      last_n_games = dplyr::n(),
      last_n_wins  = sum(won),
      .groups = "drop"
    ) |>
    dplyr::mutate(last_n_record = paste0(last_n_wins, "-",
                                         last_n_games - last_n_wins))

  out <- dplyr::left_join(base, form, by = "team")

  # --- four factors (from the team box) --------------------------
  # Conventional box definitions, so each number matches what a coach
  # would find on a public site:
  #
  #   Poss = FGA + 0.44*FTA + TOV - OREB      (the 0.44 estimator)
  #   eFG% = (FGM + 0.5*FG3M) / FGA
  #   TOV% = TOV / Poss
  #   ORB% = OREB / (OREB + Opp DREB)
  #   FTr  = FTA / FGA
  #
  # NOTE the deliberate inconsistency with `pace`: the four factors need a
  # possession DENOMINATOR and the conventional one is the 0.44 estimator,
  # while pace comes from the play profile's counted possessions. The two
  # differ by ~2.7%. Matching the convention wins here, because a coach
  # checking TOV% against another source should find the same number --
  # but the card must not imply pace and TOV% share a denominator.
  out$efg <- NA_real_; out$tov_pct <- NA_real_
  out$orb_pct <- NA_real_; out$ft_rate <- NA_real_
  out$def_efg <- NA_real_; out$opp_tov_pct <- NA_real_

  if (!is.null(team_box)) {
    need_box <- c("game_id", "team_abbreviation", "game_date", "team_score",
                  "field_goals_made", "field_goals_attempted",
                  "three_point_field_goals_made", "free_throws_attempted",
                  "offensive_rebounds", "defensive_rebounds", "total_turnovers")
    miss_box <- setdiff(need_box, names(team_box))
    if (length(miss_box)) {
      stop("`team_box` is missing required column(s): ",
           paste(miss_box, collapse = ", "), call. = FALSE)
    }

    tb <- team_box
    if (!is.null(as_of)) {
      tb <- tb[!is.na(tb$game_date) & as.Date(tb$game_date) <= as_of, ]
    }

    bx <- tibble::tibble(
      game_id = as.character(tb$game_id),
      team    = tb$team_abbreviation,
      fga  = suppressWarnings(as.numeric(tb$field_goals_attempted)),
      fgm  = suppressWarnings(as.numeric(tb$field_goals_made)),
      fg3m = suppressWarnings(as.numeric(tb$three_point_field_goals_made)),
      fta  = suppressWarnings(as.numeric(tb$free_throws_attempted)),
      oreb = suppressWarnings(as.numeric(tb$offensive_rebounds)),
      dreb = suppressWarnings(as.numeric(tb$defensive_rebounds)),
      tov  = suppressWarnings(as.numeric(tb$total_turnovers))
    )
    bx <- bx[!is.na(bx$team) & !is.na(bx$fga), ]

    # Self-join to attach the opponent's line -- ORB% needs their DREB and
    # the defensive factors need their whole shooting line.
    opp_bx <- bx |>
      dplyr::rename_with(\(x) paste0("opp_", x), -game_id)
    paired <- bx |>
      dplyr::inner_join(opp_bx, by = "game_id", relationship = "many-to-many") |>
      dplyr::filter(team != opp_team)

    ff <- paired |>
      dplyr::group_by(team) |>
      dplyr::summarise(dplyr::across(c(fga, fgm, fg3m, fta, oreb, dreb, tov,
                                       opp_fga, opp_fgm, opp_fg3m, opp_fta,
                                       opp_oreb, opp_dreb, opp_tov),
                                     \(x) sum(x, na.rm = TRUE)),
                       .groups = "drop") |>
      dplyr::transmute(
        team,
        box_poss     = fga + 0.44 * fta + tov - oreb,
        opp_box_poss = opp_fga + 0.44 * opp_fta + opp_tov - opp_oreb,
        efg     = dplyr::if_else(fga > 0, (fgm + 0.5 * fg3m) / fga, NA_real_),
        tov_pct = dplyr::if_else(box_poss > 0, tov / box_poss, NA_real_),
        orb_pct = dplyr::if_else((oreb + opp_dreb) > 0,
                                 oreb / (oreb + opp_dreb), NA_real_),
        ft_rate = dplyr::if_else(fga > 0, fta / fga, NA_real_),
        # what they ALLOW -- the defensive side of the same two factors
        def_efg = dplyr::if_else(opp_fga > 0,
                                 (opp_fgm + 0.5 * opp_fg3m) / opp_fga, NA_real_),
        opp_tov_pct = dplyr::if_else(opp_box_poss > 0,
                                     opp_tov / opp_box_poss, NA_real_)
      )

    out <- out[, setdiff(names(out), c("efg", "tov_pct", "orb_pct", "ft_rate",
                                       "def_efg", "opp_tov_pct"))]
    out <- dplyr::left_join(out, ff, by = "team")
  }

  # --- pace ------------------------------------------------------
  # Possessions come from the profile so the card carries ONE possession
  # definition. See the header note.
  out$possessions <- NA_real_
  out$pace        <- NA_real_

  if (!is.null(profile)) {
    if (!all(c("team", "dimension", "off_n") %in% names(profile))) {
      stop("`profile` must have team, dimension and off_n columns.", call. = FALSE)
    }
    # off_freq sums to 1 within team x dimension, so off_n over any ONE
    # dimension is that team's total possessions. `context` is the
    # possession-denominated dimension; shot_zone counts attempts and
    # creation counts made FGs, so neither would give possessions.
    poss <- profile[profile$dimension == "context", ] |>
      dplyr::group_by(team) |>
      dplyr::summarise(possessions = sum(off_n, na.rm = TRUE), .groups = "drop")

    out$possessions <- NULL
    out <- dplyr::left_join(out, poss, by = "team")
    out$pace <- out$possessions / out$minutes * reg_minutes

    # The profile is a FIXED artefact covering whatever window it was built
    # over. An as_of cutoff shortens `minutes` but cannot shorten the
    # profile's possession count, so pace would be inflated. Refuse rather
    # than print a wrong number on a card.
    if (!is.null(as_of)) {
      warning("as_of is set, but `profile` covers its own (full) window: ",
              "pace would divide full-window possessions by truncated ",
              "minutes. Returning NA for pace.", call. = FALSE)
      out$pace <- NA_real_
      out$possessions <- NA_real_
    }
  }

  out$as_of <- if (is.null(as_of)) NA else as_of
  dplyr::arrange(out, dplyr::desc(win_pct), team)
}


#' Header-tile values for one team, ready for render_pregame_card(stats=)
#'
#' Convenience wrapper: pulls one row and shapes it into the list the
#' renderer expects, with pace already rounded for display.
#'
#' @return A list with record, pace, last_5 and margin. Any value the
#'   summary could not compute comes back NULL, which the renderer
#'   already turns into an em dash — so a missing source shows as absent
#'   rather than as a plausible-looking number.
team_header_stats <- function(summary, team, last_n_label = "last_5") {
  row <- summary[summary$team == team, ]
  if (nrow(row) == 0) {
    stop("No season summary row for team '", team, "'.", call. = FALSE)
  }
  row <- row[1, ]
  drop_na <- function(x) if (length(x) != 1 || is.na(x)) NULL else x

  num <- function(col) {
    if (!col %in% names(row)) return(NULL)
    v <- row[[col]]
    if (length(v) != 1 || is.na(v)) NULL else as.numeric(v)
  }

  out <- list(
    record = drop_na(row$record),
    pace   = drop_na(if (is.na(row$pace)) NA else sprintf("%.1f", row$pace)),
    margin = drop_na(if (is.na(row$margin)) NA else sprintf("%+.1f", row$margin)),
    # Four factors are passed as PROPORTIONS, not preformatted strings --
    # the renderer decides the precision, and a NULL here becomes an em
    # dash rather than a stale or invented number.
    efg         = num("efg"),
    tov_pct     = num("tov_pct"),
    orb_pct     = num("orb_pct"),
    ft_rate     = num("ft_rate"),
    def_efg     = num("def_efg"),
    opp_tov_pct = num("opp_tov_pct")
  )
  out[[last_n_label]] <- drop_na(row$last_n_record)
  out
}
