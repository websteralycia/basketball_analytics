## ============================================================
## cache_tier1.R — build the per-game corpus the surfaces read
## ------------------------------------------------------------
## The dashboard and the Q&A agent cannot fetch from ESPN on every
## interaction: a single game is two API calls, and a conference season
## is a few hundred games. Both surfaces need the per-game tables
## already on disk.
##
## get_lineup_stints_many() already did this for stints. This adds the
## same for possessions — which is the more consequential of the two,
## since classify_possessions() is what the entire play profile is built
## on — and a population-level driver that fills both in one pass.
##
## WHAT IS AND IS NOT CACHED. Two of the four per-game tables have
## writers: `lineups` and `possessions`. `box` and `shots` do not, and
## are derived in memory where they are needed. If a surface comes to
## need them on disk, they get writers here rather than a second caching
## mechanism somewhere else.
##
## Failures are collected, never fatal. A season pull that dies on game
## 180 of 257 because one feed is malformed is worse than useless — it
## has spent the fetch budget and returns nothing.
##
## Depends on: paths.R, league_config.R, possessions.R,
##             get_lineup_stints_many.R, conferences.R
## ============================================================

#' Which games already have cached possessions
list_cached_possession_games <- function(league) {
  dir <- tidy_games_dir(league)
  if (!dir.exists(dir)) return(character(0))
  sub("_possessions\\.csv$", "",
      list.files(dir, pattern = "_possessions\\.csv$"))
}

#' Read one game's possessions, from cache when available
#'
#' @param refresh Ignore any cached copy and re-fetch. Use after a
#'   possession-logic fix — the and-1 bugs in September changed 2% of
#'   possessions, and a stale cache would have hidden that.
get_possessions_one <- function(game_id, league, refresh = FALSE) {
  path <- possession_cache_path(game_id, league)
  if (!refresh && file.exists(path)) {
    out <- readr::read_csv(path, col_types = possession_col_types())
    # Strip readr's provenance so a cached read is indistinguishable from
    # a fresh one; consumers must not branch on how a game arrived.
    attr(out, "spec")     <- NULL
    attr(out, "problems") <- NULL
    class(out) <- setdiff(class(out), "spec_tbl_df")
    attr(out, "source")   <- "cache"
    # classify_possessions() attaches uncredited_points as a data-quality
    # signal and a CSV cannot carry it. Set it to NA rather than leaving
    # it absent: absent reads as zero to a consumer that checks, which is
    # the difference between "this game was clean" and "we do not know".
    attr(out, "uncredited_points") <- NA_integer_
    attr(out, "uncredited_text")   <- NA_character_
    return(out)
  }
  out <- get_possessions(game_id, league = league, write = TRUE)
  attr(out, "source") <- "fetch"
  out
}

#' Possessions for many games, cached
#'
#' Mirrors get_lineup_stints_many(): supply `game_ids`, or let them
#' resolve from the schedule via `season` plus optional team/from/to.
#'
#' @return A tibble stacked across games, carrying a "failed" attribute
#'   listing any game_ids that errored.
get_possessions_many <- function(game_ids = NULL,
                                 league  = c("wnba", "wbb", "mbb", "nba"),
                                 season  = NULL,
                                 team    = NULL,
                                 from    = NULL,
                                 to      = NULL,
                                 refresh = FALSE,
                                 quiet   = FALSE) {

  league <- match.arg(league)

  if (is.null(game_ids)) {
    if (is.null(season)) {
      stop("Supply either `game_ids` or `season` (with optional team/from/to).",
           call. = FALSE)
    }
    game_ids <- resolve_game_ids(league, season, team = team, from = from, to = to)
  }
  game_ids <- unique(as.character(game_ids))

  if (length(game_ids) == 0) {
    warning("No games matched.", call. = FALSE)
    return(tibble::tibble())
  }

  if (!quiet) {
    cached_now <- if (refresh) 0L else sum(game_ids %in% list_cached_possession_games(league))
    message(sprintf("%d game(s): %d cached, %d to fetch%s",
                    length(game_ids), cached_now, length(game_ids) - cached_now,
                    if (refresh) " (refresh = TRUE)" else ""))
  }

  results <- vector("list", length(game_ids))
  failed  <- character(0)

  for (i in seq_along(game_ids)) {
    gid <- game_ids[i]
    res <- tryCatch(
      suppressWarnings(get_possessions_one(gid, league = league, refresh = refresh)),
      error = function(e) {
        failed <<- c(failed, gid)
        if (!quiet) message(sprintf("  [%d/%d] %s FAILED: %s",
                                    i, length(game_ids), gid, conditionMessage(e)))
        NULL
      })
    results[[i]] <- res
  }

  out <- dplyr::bind_rows(results[!vapply(results, is.null, logical(1))])
  attr(out, "failed") <- failed
  if (!quiet && length(failed)) {
    message(sprintf("%d game(s) failed: %s", length(failed),
                    paste(failed, collapse = ", ")))
  }
  out
}

#' Fill the Tier 1 cache for every game a population played
#'
#' The driver the dashboard build needs. Resolves the population's teams
#' the same way the play profile does — population is a PARAMETER, never
#' derived from a team — then caches each team's games once. Games
#' between two population members resolve twice and are fetched once,
#' because the per-game cache is checked before every fetch.
#'
#' @param population "national", "conference", an ESPN conference name,
#'   or a CUSTOM_CONFERENCES key.
#' @param what Which tables to cache. Both by default.
#' @param team Needed only when population = "conference".
#'
#' @return Invisibly, a data frame of game_id x table with the outcome,
#'   so a caller can see what failed without re-reading the directory.
cache_population_tier1 <- function(league, season, population,
                                   what    = c("possessions", "lineups"),
                                   team    = NULL,
                                   refresh = FALSE,
                                   quiet   = FALSE) {

  what  <- match.arg(what, several.ok = TRUE)
  teams <- population_teams(population, league, season, team = team)
  if (!quiet) {
    message(sprintf("Population '%s' %s: %d team(s)", population, season, length(teams)))
  }

  # Resolve once per team, then de-duplicate: an intra-conference game
  # appears on both teams' schedules and must not be fetched twice.
  ids <- unique(unlist(lapply(teams, function(t) {
    tryCatch(resolve_game_ids(league, season, team = t),
             error = function(e) character(0))
  })))

  if (length(ids) == 0) {
    warning("No games resolved for population '", population, "'.", call. = FALSE)
    return(invisible(data.frame()))
  }
  if (!quiet) message(sprintf("%d unique game(s) to cache", length(ids)))

  out <- list()
  if ("possessions" %in% what) {
    r <- get_possessions_many(game_ids = ids, league = league,
                              refresh = refresh, quiet = quiet)
    out$possessions <- data.frame(game_id = ids, table = "possessions",
                                  ok = !ids %in% attr(r, "failed"),
                                  stringsAsFactors = FALSE)
  }
  if ("lineups" %in% what) {
    r <- get_lineup_stints_many(game_ids = ids, league = league,
                                refresh = refresh, quiet = quiet)
    out$lineups <- data.frame(game_id = ids, table = "lineups",
                              ok = !ids %in% attr(r, "failed"),
                              stringsAsFactors = FALSE)
  }

  res <- do.call(rbind, out)
  if (!quiet) {
    for (tb in unique(res$table)) {
      n <- sum(res$table == tb); k <- sum(res$table == tb & res$ok)
      message(sprintf("%s: %d/%d cached", tb, k, n))
    }
  }
  invisible(res)
}

#' What the Tier 1 cache currently holds
#'
#' Cheap directory read, so a surface can tell a coach "this population
#' is 82% cached" instead of discovering the gaps one fetch at a time.
tier1_cache_status <- function(league, game_ids = NULL) {
  poss <- list_cached_possession_games(league)
  lin  <- list_cached_games(league)
  if (is.null(game_ids)) {
    return(data.frame(league = league,
                      possessions = length(poss), lineups = length(lin),
                      stringsAsFactors = FALSE))
  }
  game_ids <- unique(as.character(game_ids))
  data.frame(
    league      = league,
    games       = length(game_ids),
    possessions = sum(game_ids %in% poss),
    lineups     = sum(game_ids %in% lin),
    stringsAsFactors = FALSE)
}
