## ============================================================
## build_card.R — (league, population, us, them) -> a rendered card
## ------------------------------------------------------------
## The entry point that did not exist. Every sample card in output/ was
## previously produced by a hand-written Rscript call that resolved the
## profile path, loaded a season summary, shaped the header stats and
## called the renderer — which is why two of the three cards went stale
## against the renderer without anyone noticing.
##
## This is also the seam the other two surfaces sit on. A dashboard
## rendering a card, and a Q&A agent asked for one, should both call
## build_pregame_card() rather than reassembling these four steps.
##
## Depends on: paths.R, team_play_profile.R, team_season_summary.R,
##             league_config.R, keys_to_the_game.R, render_pregame_card.R
## ============================================================

## load_cached_profile() MOVED to R/query_layer.R on 2026-09-03 — the
## dashboard and the Q&A agent need it too, and consumers are sourced
## after the library, so a copy left here would silently shadow it.

#' Build one pregame card end to end
#'
#' @param league One of "wnba", "wbb", "mbb", "nba".
#' @param season Season end year.
#' @param population Percentile population, e.g. "Big Ten". A PARAMETER,
#'   never derived from the team — the same team scouts differently
#'   inside its conference and nationally.
#' @param team Us. The team the card is written for.
#' @param opponent Them. The team being scouted.
#' @param file Optional output path. Relative paths resolve under
#'   consumers/gameprep_cards/output/.
#' @param team_box Optional preloaded team box. Supplied once and reused
#'   across a slate of cards, this is the difference between one download
#'   and one per matchup.
#' @param as_of Optional date cutoff, so a card can be rebuilt as it would
#'   have read before a given game.
#'
#' @return The card HTML, invisibly. Written to `file` when given.
build_pregame_card <- function(league, season, population, team, opponent,
                               file       = NULL,
                               date_label = NULL,
                               venue      = NULL,
                               profile    = NULL,
                               team_box   = NULL,
                               schedule   = NULL,
                               as_of      = NULL,
                               ...) {

  cfg <- league_cfg(league)
  if (is.null(profile)) profile <- load_cached_profile(population, season, league)

  for (t in c(team, opponent)) {
    if (!t %in% profile$team) {
      stop("Team '", t, "' is not in the ", population, " ", season,
           " profile. Available: ",
           paste(sort(unique(profile$team)), collapse = ", "), call. = FALSE)
    }
  }

  # The four factors need a team box; without one the tiles come back
  # NULL and render as em dashes rather than as invented numbers.
  if (is.null(team_box) && !is.null(cfg$team_box_fn)) {
    team_box <- tryCatch(cfg$team_box_fn(season), error = function(e) NULL)
  }

  summary <- team_season_summary(league, season,
                                 teams    = unique(c(team, opponent)),
                                 profile  = profile,
                                 schedule = schedule,
                                 team_box = team_box,
                                 as_of    = as_of)

  # The card scouts the OPPONENT, so the header tiles are theirs.
  stats <- team_header_stats(summary, opponent)

  if (!is.null(file) && !grepl("^(/|~)", file)) {
    file <- file.path(card_output_dir(), file)
  }

  render_pregame_card(profile, team, opponent,
                      date_label = date_label, venue = venue,
                      stats = stats, file = file, ...)
}

#' Rebuild a slate of cards from one profile and one box download
#'
#' @param matchups A list of c(team, opponent) pairs, or a data frame with
#'   `team` and `opponent` columns.
#' @param files Output filenames, parallel to `matchups`.
build_pregame_cards <- function(league, season, population, matchups, files,
                                team_box = NULL, ...) {
  if (is.data.frame(matchups)) {
    matchups <- lapply(seq_len(nrow(matchups)),
                       function(i) c(matchups$team[i], matchups$opponent[i]))
  }
  stopifnot(length(matchups) == length(files))

  profile <- load_cached_profile(population, season, league)
  cfg     <- league_cfg(league)
  if (is.null(team_box) && !is.null(cfg$team_box_fn)) {
    team_box <- tryCatch(cfg$team_box_fn(season), error = function(e) NULL)
  }

  invisible(Map(function(m, f) {
    build_pregame_card(league, season, population, m[1], m[2],
                       file = f, profile = profile, team_box = team_box, ...)
  }, matchups, files))
}
