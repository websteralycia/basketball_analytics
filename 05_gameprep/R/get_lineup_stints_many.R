## ============================================================
## get_lineup_stints_many.R — batch stints with on-disk caching
## ------------------------------------------------------------
## get_lineup_stints() is one game, two API calls. The report generator
## and the scouting agent want a season, a team, or a date range, and
## they want repeat calls to be cheap.
##
## This wraps the single-game fetch: any game already written to
## data/tidy/<league>/games/ is read from disk instead of re-fetched.
## Pass refresh = TRUE to force a re-pull (e.g. after a stat correction,
## or after fixing a reconstruction bug).
##
## Depends on: league_config.R, paths.R, get_lineup_stints.R
## ============================================================

#' Which games are already cached for a league
#'
#' @return character vector of game_ids
list_cached_games <- function(league) {
  dir <- tidy_games_dir(league)
  if (!dir.exists(dir)) return(character(0))
  f <- list.files(dir, pattern = "_lineups\\.csv$")
  sub("_lineups\\.csv$", "", f)
}

#' Read one game's stints, from cache when available
#'
#' @param refresh Ignore any cached copy and re-fetch.
get_lineup_stints_one <- function(game_id, league, refresh = FALSE) {
  path <- stint_cache_path(game_id, league)
  if (!refresh && file.exists(path)) {
    out <- readr::read_csv(path, col_types = stint_col_types())
    # readr tags its output with spec/problems attributes AND a
    # "spec_tbl_df" class. Strip all three so a cached read is genuinely
    # indistinguishable from a fresh fetch — consumers must not behave
    # differently depending on whether a game happened to be cached.
    attr(out, "spec")     <- NULL
    attr(out, "problems") <- NULL
    class(out) <- setdiff(class(out), "spec_tbl_df")
    attr(out, "source")   <- "cache"
    return(out)
  }
  out <- get_lineup_stints(game_id, league = league, write = TRUE)
  attr(out, "source") <- "fetch"
  out
}

#' Resolve a set of game_ids from the schedule
#'
#' @param league  One of supported_leagues().
#' @param season  Season end-year (2026 = the 2025-26 season). Required
#'   unless game_ids are supplied directly.
#' @param team    Optional team abbreviation, e.g. "CON". Matches either side.
#' @param from,to Optional date bounds (Date or "YYYY-MM-DD"), inclusive.
#' @param completed_only Keep only games ESPN marks as final. Stints can't
#'   be reconstructed from an unplayed game, so this defaults to TRUE.
resolve_game_ids <- function(league, season, team = NULL,
                             from = NULL, to = NULL,
                             completed_only = TRUE) {
  cfg <- league_cfg(league)
  if (is.null(cfg$sched_fn)) {
    stop("No schedule function configured for league '", league, "'.", call. = FALSE)
  }
  sch <- cfg$sched_fn(season)

  id_col <- if ("game_id" %in% names(sch)) "game_id" else "id"
  sch$.gid  <- as.character(sch[[id_col]])
  sch$.date <- as.Date(sch$date)

  if (completed_only && "status_type_completed" %in% names(sch)) {
    sch <- sch[!is.na(sch$status_type_completed) & sch$status_type_completed, ]
  }
  if (!is.null(team)) {
    team <- toupper(team)
    hit <- toupper(as.character(sch$home_abbreviation)) %in% team |
           toupper(as.character(sch$away_abbreviation)) %in% team
    sch <- sch[!is.na(hit) & hit, ]
  }
  if (!is.null(from)) sch <- sch[!is.na(sch$.date) & sch$.date >= as.Date(from), ]
  if (!is.null(to))   sch <- sch[!is.na(sch$.date) & sch$.date <= as.Date(to), ]

  sch <- sch[order(sch$.date), ]
  unique(sch$.gid[!is.na(sch$.gid)])
}

#' Lineup stints for many games, cached
#'
#' Supply `game_ids` directly, or let them be resolved from the schedule
#' via `season` plus optional `team` / `from` / `to`.
#'
#' Failures are collected rather than fatal: one unreconstructable game
#' does not lose the rest of the batch. The returned tibble carries a
#' "failed" attribute listing any game_ids that errored.
#'
#' @return A tibble with the same schema as get_lineup_stints(), stacked
#'   across games.
get_lineup_stints_many <- function(game_ids = NULL,
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
    cached_now <- sum(game_ids %in% list_cached_games(league))
    message(sprintf("%d game(s): %d cached, %d to fetch%s",
                    length(game_ids), if (refresh) 0L else cached_now,
                    length(game_ids) - if (refresh) 0L else cached_now,
                    if (refresh) " (refresh = TRUE)" else ""))
  }

  results <- vector("list", length(game_ids))
  sources <- character(length(game_ids))
  failed  <- character(0)

  for (i in seq_along(game_ids)) {
    gid <- game_ids[i]
    res <- tryCatch(
      suppressWarnings(get_lineup_stints_one(gid, league = league, refresh = refresh)),
      error = function(e) {
        failed <<- c(failed, gid)
        if (!quiet) message(sprintf("  [%d/%d] %s FAILED: %s",
                                    i, length(game_ids), gid, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(res)) {
      results[[i]] <- res
      sources[i]   <- attr(res, "source") %||% "?"
      if (!quiet && identical(sources[i], "fetch")) {
        message(sprintf("  [%d/%d] %s fetched (%d rows)", i, length(game_ids), gid, nrow(res)))
      }
    }
  }

  out <- dplyr::bind_rows(results)
  # bind_rows inherits attributes from its first element, which would leave
  # a per-game "source" tag on the whole batch reflecting only whichever
  # game came first. Drop it; the batch reports counts instead.
  attr(out, "source") <- NULL

  if (!quiet) {
    message(sprintf("Done: %d game(s), %s rows | %d from cache, %d fetched, %d failed",
                    dplyr::n_distinct(out$game_id),
                    format(nrow(out), big.mark = ","),
                    sum(sources == "cache"), sum(sources == "fetch"), length(failed)))
  }
  if (length(failed) > 0) {
    warning(sprintf("%d game(s) failed: %s", length(failed),
                    paste(utils::head(failed, 10), collapse = ", ")), call. = FALSE)
  }

  attr(out, "failed") <- failed
  out
}
