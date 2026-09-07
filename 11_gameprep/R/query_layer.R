## ============================================================
## query_layer.R — the typed surface all three consumers call
## ------------------------------------------------------------
## The dashboard renders these. The Q&A agent CALLS these as tools. The
## card builder uses them too. Nothing above this layer touches a CSV or
## re-derives a display rule.
##
## WHY NOT RAG. The obvious instinct for a scouting chatbot is to embed
## the profile rows and retrieve by similarity. That answers the wrong
## shape of question. "What do they run late in games" is a filter and an
## aggregate; "who else in the conference turns it over this much" is a
## sort. Embeddings answer both approximately, lose the ranking semantics
## that took the most work to get right, and cannot say "13th of 18" at
## all. Function calls answer them exactly. Retrieval still earns its
## place over the coach-authored prose in tactical_phrases.csv, where
## similarity is the actual question — but not over the numbers.
##
## EVERY ROW IS DISPLAY-READY. Each returns the raw numbers AND the
## formatted strings, both produced by R/profile_display.R. A React
## front end renders `label`, `meta` and `standing`; it never computes
## them. That is deliberate: JavaScript recomputing which dimensions rank
## inverted would fork the exact guardrail this project consolidated into
## one place, in a second language, where it is harder to test.
##
## Depends on: profile_display.R, team_play_profile.R, conferences.R,
##             paths.R, cache_tier1.R
## ============================================================

#' Read a cached play profile
#'
#' Reads only. Building one is a minutes-long job over a season of
#' play-by-play, so a query must never silently trigger it.
load_cached_profile <- function(population, season, league) {
  path <- profile_cache_path(population, season, league)
  if (!file.exists(path)) {
    stop("No cached play profile at ", path,
         "\nBuild it first with build_team_play_profile(population = \"",
         population, "\", season = ", season, ", league = \"", league, "\").",
         call. = FALSE)
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

#' Attach display-ready columns to profile or card rows
#'
#' The single place raw rows become renderable. `label`, `meta` and
#' `standing` are what a UI prints; the raw columns stay for charts.
#'
#' @param rows Rows carrying dimension, category, source, unit, freq,
#'   ppp and a percentile column.
#' @param pctl_col Which percentile drives `standing`.
as_display_rows <- function(rows, pop_n, population,
                            pctl_col = "ppp_percentile",
                            display  = load_category_display(),
                            headers  = load_dimension_headers(),
                            style    = "long") {
  if (nrow(rows) == 0) {
    return(tibble::add_column(rows, label = character(0), meta = character(0),
                              standing = character(0), rank = integer(0),
                              bulletable = logical(0)))
  }
  lab <- vapply(seq_len(nrow(rows)), function(i) {
    v <- display$display_label[display$source    == rows$source[i] &
                               display$dimension == rows$dimension[i] &
                               display$category  == rows$category[i]]
    if (length(v) == 0) rows$category[i] else v[1]
  }, character(1))
  blt <- vapply(seq_len(nrow(rows)), function(i) {
    v <- display$bulletable[display$source    == rows$source[i] &
                            display$dimension == rows$dimension[i] &
                            display$category  == rows$category[i]]
    if (length(v) == 0) TRUE else isTRUE(as.logical(v[1]))
  }, logical(1))

  pctl <- rows[[pctl_col]]
  rows$label      <- lab
  rows$bulletable <- blt
  rows$meta <- vapply(seq_len(nrow(rows)), function(i) {
    profile_row_meta(rows$freq[i], rows$ppp[i], rows$unit[i],
                     rows$dimension[i], rows$source[i], headers)
  }, character(1))
  rows$standing <- vapply(seq_len(nrow(rows)), function(i) {
    profile_row_standing(pctl[i], pop_n, population, rows$dimension[i],
                         rows$category[i], style = style)
  }, character(1))
  rows$rank <- rank_from_percentile(pctl, pop_n)
  rows
}

#' Populations available for a league and season
#'
#' The agent needs this to answer "which conferences can you compare
#' within" without guessing at a name the profile builder would reject.
q_populations <- function(league, season) {
  list_conferences(league, season)
}

#' Which teams make up a population, and how big it is
#'
#' Population size is not cosmetic: it decides whether a standing reads
#' as a rank or a percentile, and a 12-team population gives a percentile
#' only 12 distinct values.
q_population <- function(league, season, population, team = NULL) {
  teams <- population_teams(population, league, season, team = team)
  list(population = population, league = league, season = season,
       n = length(teams), teams = teams,
       standing_mode = if (length(teams) <= RANK_DISPLAY_MAX_POPULATION)
         "rank" else "percentile")
}

#' One team's full play profile, display-ready
#'
#' @param dimension Optional filter, e.g. "shot_zone".
#' @param bulletable_only Drop residual buckets and the creation pair.
q_team_profile <- function(league, season, population, team,
                           dimension       = NULL,
                           bulletable_only = FALSE,
                           profile         = NULL) {
  if (is.null(profile)) profile <- load_cached_profile(population, season, league)
  pop_n <- length(unique(profile$team))
  if (!team %in% profile$team) {
    stop("Team '", team, "' is not in the ", population, " ", season,
         " profile. Available: ", paste(sort(unique(profile$team)), collapse = ", "),
         call. = FALSE)
  }
  rows <- profile[profile$team == team, ]
  if (!is.null(dimension)) rows <- rows[rows$dimension %in% dimension, ]

  rows <- tibble::tibble(
    dimension = rows$dimension, category = rows$category,
    unit = rows$unit, source = rows$source,
    poss = rows$off_n, freq = rows$off_freq,
    ppp = rows$off_ppp, ppp_percentile = rows$off_ppp_pctl,
    def_ppp = rows$def_ppp, def_ppp_percentile = rows$def_ppp_pctl)

  out <- as_display_rows(rows, pop_n, population)
  if (bulletable_only) out <- out[out$bulletable, ]
  out[order(out$dimension, -out$freq), ]
}

#' A matchup: our team against theirs, display-ready
#'
#' Wraps attack_index_card(), which joins our offence to their defence.
#' `def_ppp_percentile` stays stored-oriented (high = attackable); use
#' `def_display_percentile` for any non-attack reading — it is already
#' flipped, exactly once.
q_matchup <- function(league, season, population, team, opponent,
                      profile = NULL) {
  if (is.null(profile)) profile <- load_cached_profile(population, season, league)
  pop_n <- length(unique(profile$team))
  card  <- attack_index_card(profile, team, opponent)
  out   <- as_display_rows(card, pop_n, population)
  out$def_display_percentile <- display_def_percentile(out$def_ppp_percentile)
  out
}

#' Who leads or trails a population in one category
#'
#' The "who else in the conference does this" question, which a card
#' scoped to one matchup cannot answer and an embedding search answers
#' only approximately.
#'
#' @param best TRUE ranks best-first by the dimension's own convention —
#'   which on a freq-ranked dimension means fewest turnovers, not highest
#'   PPP.
q_leaders <- function(league, season, population, dimension, category,
                      n = 5L, best = TRUE, profile = NULL) {
  if (is.null(profile)) profile <- load_cached_profile(population, season, league)
  pop_n <- length(unique(profile$team))
  rows  <- profile[profile$dimension == dimension & profile$category == category, ]
  if (nrow(rows) == 0) {
    stop("No rows for dimension '", dimension, "' category '", category,
         "'. Try q_dimensions() for what exists.", call. = FALSE)
  }
  rows <- rows[order(rows$off_ppp_pctl, decreasing = best), ]
  rows <- utils::head(rows, n)

  d <- as_display_rows(tibble::tibble(
    dimension = rows$dimension, category = rows$category,
    unit = rows$unit, source = rows$source,
    poss = rows$off_n, freq = rows$off_freq,
    ppp = rows$off_ppp, ppp_percentile = rows$off_ppp_pctl,
    def_ppp = rows$def_ppp, def_ppp_percentile = rows$def_ppp_pctl),
    pop_n, population)
  d$team <- rows$team
  d[, c("team", "label", "meta", "standing", "rank", "freq", "ppp",
        "ppp_percentile", "dimension", "category", "unit")]
}

#' What dimensions and categories exist, with their reading rules
#'
#' The agent needs this to avoid inventing a category name, and to know
#' which rows carry a meaningful rate before it quotes one.
q_dimensions <- function() {
  hdr  <- load_dimension_headers()
  disp <- load_category_display()
  dplyr::left_join(
    disp[, c("source", "dimension", "category", "display_label",
             "bulletable", "exclusion_reason")],
    hdr[, c("source", "dimension", "ppp_definition", "unit_noun",
            "ppp_meaningful")],
    by = c("source", "dimension"))
}

#' Season context for one or more teams: record, pace, four factors
#'
#' Hits the schedule feed unless `schedule`/`team_box` are supplied, so a
#' caller answering several questions should fetch once and pass them in.
q_team_summary <- function(league, season, teams, population = NULL,
                           profile = NULL, schedule = NULL, team_box = NULL,
                           as_of = NULL) {
  if (is.null(profile) && !is.null(population)) {
    profile <- tryCatch(load_cached_profile(population, season, league),
                        error = function(e) NULL)
  }
  team_season_summary(league, season, teams = teams, profile = profile,
                      schedule = schedule, team_box = team_box, as_of = as_of)
}

#' Lineup stints for one team, from the Tier 1 cache
#'
#' Reads cached games only by default. A stint carries team_pts and
#' opp_pts, so a differential is available; a NET RATING is not, because
#' possessions per stint are deliberately not in the stint table.
q_stints <- function(league, season, team, game_ids = NULL,
                     min_seconds = 0, cached_only = TRUE) {
  if (is.null(game_ids)) {
    game_ids <- resolve_game_ids(league, season, team = team)
  }
  if (cached_only) {
    have <- list_cached_games(league)
    game_ids <- game_ids[game_ids %in% have]
    if (length(game_ids) == 0) {
      stop("No cached stint files for ", team, " ", season,
           ". Run cache_population_tier1() first.", call. = FALSE)
    }
  }
  out <- get_lineup_stints_many(game_ids = game_ids, league = league, quiet = TRUE)
  out <- out[out$team == team, ]
  out <- out[out$stint_seconds >= min_seconds, ]
  out$plus_minus <- out$team_pts - out$opp_pts
  out
}

## --- the tool manifest ----------------------------------------
## What the agent is allowed to call, and how to describe it. Kept
## beside the functions so a signature change and its description cannot
## drift apart. A tool NOT in this list is not reachable by the agent --
## that is the guardrail, not a suggestion.
QUERY_TOOLS <- list(
  list(name = "q_populations",
       description = paste("List the conferences/populations available for a league",
                           "and season. Call this before guessing a population name."),
       params = c("league", "season"),
       required = c("league", "season"),
       schema = list(
         league = list(type = "string", enum = c("wbb", "wnba", "mbb", "nba"),
                       description = "League code."),
         season = list(type = "integer",
                       description = "Season END year: 2026 is the 2025-26 season."))),

  list(name = "q_population",
       description = paste("Teams in a population, its size, and whether standings",
                           "should be read as ranks or percentiles. Population size",
                           "matters: a 12-team population gives a percentile only 12",
                           "distinct values."),
       params = c("league", "season", "population"),
       required = c("league", "season", "population"),
       schema = list(
         league = list(type = "string", enum = c("wbb", "wnba", "mbb", "nba")),
         season = list(type = "integer"),
         population = list(type = "string",
                           description = paste("Conference name such as 'Big Ten', or",
                                               "'national'. Never derived from the team.")))),

  list(name = "q_team_profile",
       description = paste("One team's full play profile: for every category, its",
                           "frequency, rate and standing in the population, already",
                           "formatted for display. Use the `meta` and `standing`",
                           "strings verbatim when quoting numbers -- they encode",
                           "rules you must not recompute."),
       params = c("league", "season", "population", "team", "dimension",
                  "bulletable_only"),
       required = c("league", "season", "population", "team"),
       schema = list(
         league = list(type = "string", enum = c("wbb", "wnba", "mbb", "nba")),
         season = list(type = "integer"),
         population = list(type = "string"),
         team = list(type = "string", description = "Team abbreviation, e.g. 'MICH'."),
         dimension = list(type = "string",
                          enum = c("context", "ball_security", "shot_zone", "creation"),
                          description = "Optional filter to one dimension."),
         bulletable_only = list(type = "boolean",
                                description = paste("Drop residual buckets and the",
                                                    "creation pair. Default false.")))),

  list(name = "q_matchup",
       description = paste("Our offence against their defence for one matchup.",
                           "`def_ppp_percentile` is attackable-oriented (high = weak",
                           "defence, go at it); `def_display_percentile` is the",
                           "ordinary reading. Never flip either yourself."),
       params = c("league", "season", "population", "team", "opponent"),
       required = c("league", "season", "population", "team", "opponent"),
       schema = list(
         league = list(type = "string", enum = c("wbb", "wnba", "mbb", "nba")),
         season = list(type = "integer"),
         population = list(type = "string"),
         team = list(type = "string", description = "Us."),
         opponent = list(type = "string", description = "Them, the team being scouted."))),

  list(name = "q_leaders",
       description = paste("Who leads or trails a population in one category. This is",
                           "how to answer 'who else in the conference does this' --",
                           "a single team's profile cannot."),
       params = c("league", "season", "population", "dimension", "category", "n", "best"),
       required = c("league", "season", "population", "dimension", "category"),
       schema = list(
         league = list(type = "string", enum = c("wbb", "wnba", "mbb", "nba")),
         season = list(type = "integer"),
         population = list(type = "string"),
         dimension = list(type = "string",
                          enum = c("context", "ball_security", "shot_zone", "creation")),
         category = list(type = "string",
                         description = "Exact category id. Call q_dimensions for valid ids."),
         n = list(type = "integer", description = "How many teams. Default 5."),
         best = list(type = "boolean",
                     description = paste("TRUE ranks best-first by the dimension's own",
                                         "convention. Default true.")))),

  list(name = "q_dimensions",
       description = paste("Every dimension and category with its display label,",
                           "bullet eligibility, exclusion reason, and whether its rate",
                           "is meaningful. Call this before naming a category or",
                           "quoting a rate."),
       params = character(0), required = character(0), schema = list()),

  list(name = "q_team_summary",
       description = paste("Season context: record, pace, points for and against, and",
                           "the four factors. Slower than the other tools -- it reads",
                           "the schedule feed."),
       params = c("league", "season", "teams"),
       required = c("league", "season", "teams"),
       schema = list(
         league = list(type = "string", enum = c("wbb", "wnba", "mbb", "nba")),
         season = list(type = "integer"),
         teams = list(type = "array", items = list(type = "string"),
                      description = "One or more team abbreviations."))),

  list(name = "q_stints",
       description = paste("Lineup stints for a team: five player ids, duration and",
                           "point differential. NET RATING IS NOT AVAILABLE -- there",
                           "are no possessions per stint. Do not compute or imply one."),
       params = c("league", "season", "team", "min_seconds"),
       required = c("league", "season", "team"),
       schema = list(
         league = list(type = "string", enum = c("wbb", "wnba", "mbb", "nba")),
         season = list(type = "integer"),
         team = list(type = "string"),
         min_seconds = list(type = "integer",
                            description = "Drop stints shorter than this. Default 0.")))
)

#' The tool array in Anthropic Messages API shape
#'
#' Built from QUERY_TOOLS so a signature, its description and its schema
#' cannot drift apart. `strict` is set so arguments validate exactly --
#' a hallucinated parameter name should fail loudly, not be dropped.
query_tool_defs <- function(tools = QUERY_TOOLS) {
  lapply(tools, function(t) {
    props <- t$schema
    # additionalProperties = FALSE is required for strict tools.
    list(
      name        = t$name,
      description = t$description,
      strict      = TRUE,
      input_schema = list(
        type                 = "object",
        properties           = if (length(props)) props else
                                 stats::setNames(list(), character(0)),
        required             = as.list(t$required),
        additionalProperties = FALSE
      )
    )
  })
}

#' Names of the callable query tools
query_tool_names <- function() vapply(QUERY_TOOLS, `[[`, character(1), "name")

#' Cached play profiles on disk for a league
#'
#' A directory read, so a UI can populate its population picker at
#' startup without touching the network.
#'
#' @return data frame: population (as stored in the profile), season, path
list_cached_profiles <- function(league) {
  dir <- file.path(gameprep_root(), "data", "tidy", tolower(league), "profiles")
  if (!dir.exists(dir)) {
    return(data.frame(population = character(0), season = integer(0),
                      path = character(0), stringsAsFactors = FALSE))
  }
  files <- list.files(dir, pattern = "_play_profile\\.csv$", full.names = TRUE)
  if (!length(files)) {
    return(data.frame(population = character(0), season = integer(0),
                      path = character(0), stringsAsFactors = FALSE))
  }
  # Read the population NAME from the file rather than un-slugging the
  # filename: "big_ten" -> "Big Ten" is not reversible in general.
  rows <- lapply(files, function(f) {
    h <- tryCatch(readr::read_csv(f, n_max = 1, show_col_types = FALSE,
                                  progress = FALSE),
                  error = function(e) NULL)
    if (is.null(h) || !nrow(h)) return(NULL)
    data.frame(population = as.character(h$population[1]),
               season = as.integer(h$season[1]), path = f,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(out)) out <- data.frame(population = character(0),
                                      season = integer(0), path = character(0),
                                      stringsAsFactors = FALSE)
  out[order(out$population, out$season), ]
}
