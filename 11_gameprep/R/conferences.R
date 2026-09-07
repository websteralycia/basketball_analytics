## ============================================================
## conferences.R — comparison populations for percentile grading
## ------------------------------------------------------------
## A percentile is only meaningful relative to a population, and the
## population is a CHOICE, not a property of the team. Any team can be
## graded against any conference: a school changing leagues wants to see
## itself against both the conference it just left and the one it is
## joining, and neither is "wrong".
##
## Two kinds of conference:
##   * ESPN-native  — membership read from the schedule feed, which
##     carries home_conference_id / away_conference_id per game.
##   * CUSTOM       — a hand-listed set of schools. Needed whenever a
##     conference does not yet exist in the data: realignment, or a
##     league whose current membership post-dates the season you have.
##
## A custom conference is just a named set of schools. It can be applied
## to ANY season's data, which is the point — grading a team against a
## conference it has not played in yet means measuring it against those
## schools' most recent completed season.
## ============================================================

# --- Custom conference definitions -----------------------------------
# Keyed by full ESPN display name ("Location Nickname"), which is the
# most reliable match. conference_teams() warns loudly about any school
# that fails to resolve rather than silently shrinking the population.
CUSTOM_CONFERENCES <- list(

  pac12_2027 = list(
    label   = "Pac-12 (2026-27)",
    league  = "wbb",
    note    = paste("Rebuilt Pac-12, effective the 2026-27 season. Members are",
                    "drawn from several current conferences (Mountain West, WCC,",
                    "Sun Belt), so this cannot be read from the schedule feed",
                    "until those games are actually played."),
    schools = c(
      "Boise State Broncos",
      "Colorado State Rams",
      "Fresno State Bulldogs",
      "Gonzaga Bulldogs",
      "Oregon State Beavers",
      "San Diego State Aztecs",
      "Texas State Bobcats",
      "Utah State Aggies",
      "Washington State Cougars"
    )
  )
)

# in-session cache: building the map is a season-wide schedule pull
.conf_cache <- new.env(parent = emptyenv())

#' Team -> conference map for a league-season, from the schedule feed
#'
#' @return tibble: team_id, team (abbrev), name (display), conf_id, conf
team_conference_map <- function(league, season) {
  key <- paste(league, season, sep = "_")
  if (!is.null(.conf_cache[[key]])) return(.conf_cache[[key]])

  cfg <- league_cfg(league)
  if (is.null(cfg$sched_fn)) {
    stop("No schedule function configured for league '", league, "'.", call. = FALSE)
  }
  sch <- cfg$sched_fn(season)

  need <- c("home_id", "away_id", "home_conference_id", "away_conference_id")
  if (!all(need %in% names(sch))) {
    stop("Schedule for ", league, " ", season, " carries no conference ids; ",
         "conference populations are unavailable for this league.", call. = FALSE)
  }

  sides <- dplyr::bind_rows(
    dplyr::transmute(sch,
      team_id = as.character(home_id), team = as.character(home_abbreviation),
      name = as.character(home_display_name), conf_id = as.character(home_conference_id)),
    dplyr::transmute(sch,
      team_id = as.character(away_id), team = as.character(away_abbreviation),
      name = as.character(away_display_name), conf_id = as.character(away_conference_id))
  )
  sides <- sides[!is.na(sides$team_id) & !is.na(sides$conf_id), ]

  # A team can appear with more than one conference id across a season
  # (neutral-site and tournament games are tagged oddly); take the mode.
  tm <- sides |>
    dplyr::count(team_id, team, name, conf_id, sort = TRUE) |>
    dplyr::group_by(team_id) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-n)

  # conference id -> readable name
  cnames <- if (all(c("groups_id", "groups_short_name") %in% names(sch))) {
    sch |>
      dplyr::filter(!is.na(groups_id)) |>
      dplyr::transmute(conf_id = as.character(groups_id),
                       conf = as.character(groups_short_name)) |>
      dplyr::distinct(conf_id, .keep_all = TRUE)
  } else {
    tibble::tibble(conf_id = character(0), conf = character(0))
  }

  out <- dplyr::left_join(tm, cnames, by = "conf_id")
  .conf_cache[[key]] <- out
  out
}

#' Every conference available as a comparison population
#'
#' ESPN-native conferences for the league-season, plus any custom ones
#' defined for that league.
list_conferences <- function(league, season) {
  m <- team_conference_map(league, season)
  espn <- m |>
    dplyr::filter(!is.na(conf)) |>
    dplyr::count(key = conf, name = "teams") |>
    dplyr::mutate(label = key, source = "espn")

  custom <- CUSTOM_CONFERENCES[vapply(CUSTOM_CONFERENCES,
                                      function(x) identical(x$league, league), logical(1))]
  cust <- if (length(custom) == 0) NULL else tibble::tibble(
    key    = names(custom),
    teams  = vapply(custom, function(x) length(x$schools), integer(1)),
    label  = vapply(custom, function(x) x$label, character(1)),
    source = "custom"
  )
  dplyr::bind_rows(espn, cust) |> dplyr::arrange(source, key)
}

#' Resolve a conference to its member teams
#'
#' @param conference Either a custom key (e.g. "pac12_2027") or an
#'   ESPN conference name / fragment (e.g. "Mountain West", "MWC").
#' @return tibble: team_id, team, name
conference_teams <- function(conference, league, season) {
  # Validate the cheap things BEFORE pulling a season of schedule data,
  # so a wrong-league custom conference reports that rather than failing
  # deeper with an unrelated message about the schedule feed.
  def <- if (conference %in% names(CUSTOM_CONFERENCES)) CUSTOM_CONFERENCES[[conference]] else NULL
  if (!is.null(def) && !identical(def$league, league)) {
    stop("Custom conference '", conference, "' is defined for league '",
         def$league, "', not '", league, "'.", call. = FALSE)
  }

  m <- team_conference_map(league, season)

  # --- custom conference: match hand-listed schools by display name ---
  if (!is.null(def)) {
    hit <- m[m$name %in% def$schools, c("team_id", "team", "name")]
    missing <- setdiff(def$schools, hit$name)
    if (length(missing) > 0) {
      warning(sprintf("Conference '%s': %d of %d schools did not resolve in %s %s: %s",
                      conference, length(missing), length(def$schools), league, season,
                      paste(missing, collapse = "; ")), call. = FALSE)
    }
    if (nrow(hit) == 0) {
      stop("Conference '", conference, "' resolved to no teams.", call. = FALSE)
    }
    return(hit[order(hit$name), ])
  }

  # --- ESPN conference: match on name, case-insensitive substring ------
  hit <- m[!is.na(m$conf) & grepl(conference, m$conf, ignore.case = TRUE),
           c("team_id", "team", "name")]
  if (nrow(hit) == 0) {
    avail <- sort(unique(stats::na.omit(m$conf)))
    stop("No conference matched '", conference, "' in ", league, " ", season,
         ".\nAvailable: ", paste(avail, collapse = ", "),
         "\nCustom: ", paste(names(CUSTOM_CONFERENCES), collapse = ", "), call. = FALSE)
  }
  hit[order(hit$name), ]
}

#' Which ESPN conference a team played in that season
#'
#' Historic membership — what the data says, not where the team is
#' heading. Use conference_teams() to grade against a different one.
team_conference <- function(team, league, season) {
  m <- team_conference_map(league, season)
  hit <- m[!is.na(m$team) & toupper(m$team) == toupper(team), ]
  if (nrow(hit) == 0) {
    hit <- m[!is.na(m$name) & grepl(team, m$name, ignore.case = TRUE), ]
  }
  if (nrow(hit) == 0) return(NA_character_)
  hit$conf[1]
}

#' Resolve a percentile population to team ids
#'
#' @param population One of:
#'   * `"national"` — every team in the league-season
#'   * `"conference"` — the graded team's own conference that season
#'     (requires `team`)
#'   * any other string — an explicit conference, ESPN or custom
#' @return character vector of team abbreviations
#' Every team in a league-season, taken from the schedule
#'
#' For leagues with no conference structure. Teams appearing in fewer than
#' `min_games` games are dropped: ESPN gives All-Star sides their own team
#' entries (WNBA 2026 lists COOP and SPO, one game each) and they are not
#' franchises. The real teams play 40+, so the threshold does not need to be
#' precise -- it only has to sit in the gap.
league_teams_from_schedule <- function(league, season, min_games = 5L) {
  cfg <- league_cfg(league)
  sch <- cfg$sched_fn(season)
  ab  <- c(as.character(sch$home_abbreviation), as.character(sch$away_abbreviation))
  ab  <- ab[!is.na(ab) & nzchar(ab)]
  if (!length(ab)) {
    stop("Schedule for ", league, " ", season, " carries no team abbreviations.",
         call. = FALSE)
  }
  tb <- table(ab)
  sort(names(tb)[tb >= min_games])
}

population_teams <- function(population, league, season, team = NULL) {
  if (identical(population, "national")) {
    # "national" means every team in the league. That does not require
    # conference structure, and the pro leagues have none -- routing it
    # through the conference map made `national` fail for wnba and nba with
    # an error about conferences, which is not what was being asked for.
    tm <- tryCatch(sort(unique(team_conference_map(league, season)$team)),
                   error = function(e) NULL)
    if (length(tm)) return(tm)
    return(league_teams_from_schedule(league, season))
  }
  conf <- if (identical(population, "conference")) {
    if (is.null(team)) {
      stop("population = 'conference' needs `team` to know whose conference to use.",
           call. = FALSE)
    }
    tc <- team_conference(team, league, season)
    if (is.na(tc)) stop("Could not determine a conference for team '", team, "'.", call. = FALSE)
    tc
  } else {
    population
  }
  sort(conference_teams(conf, league, season)$team)
}
