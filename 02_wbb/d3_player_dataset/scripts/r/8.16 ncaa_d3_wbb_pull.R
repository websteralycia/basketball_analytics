###############################################
# NCAA Division III Women's Basketball pull
# Source: stats.ncaa.org (via headful Chrome + chromote)
#
# Replaces wehoop::load_wbb_player_box() for D3, which
# ESPN does not cover.
#
# WHY A BROWSER IS REQUIRED
#   stats.ncaa.org team and contest pages sit behind an
#   Akamai Bot Manager JS challenge (bm-verify). Plain
#   httr/rvest gets a 403 or an empty stub page. A real
#   browser executes the challenge and is then cookied
#   for the rest of the session.
#
# WHY *HEADFUL* CHROME
#   Headless Chrome crashes on this machine (macOS 13 /
#   Intel: "Assertion failed: (NULL == _txn) ...
#   NSCGSTransaction dealloc"), so chromote's default
#   headless launch never opens its debugging port.
#   We launch Chrome normally with --remote-debugging-port
#   and attach with chromote::ChromeRemote instead.
#   A Chrome window will open; leave it alone while the
#   pull runs.
#
# Output columns are named to match wehoop's
# load_wbb_player_box() so downstream code is unchanged.
###############################################

suppressPackageStartupMessages({
  library(chromote)
  library(rvest)
  library(httr)
  library(stringr)
  library(dplyr)
  library(readr)
  library(purrr)
})

###############################################
# Config
###############################################

# NCAA table headers come through as either "3FG" or "X3FG"
# depending on how the table is coerced; this lets us try both.
`%||%` <- function(a, b) if (is.null(a)) b else a

NCAA_PORT      <- 9340L
NCAA_PROFILE   <- file.path(tempdir(), "ncaa_chrome_profile")
CHROME_BIN     <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

# Be polite to stats.ncaa.org
REQUEST_DELAY  <- 1.0    # seconds between page loads
CHECKPOINT_N   <- 25     # write partial results every N games

# Where everything this pipeline reads and writes lives -- the four scripts
# hand files to each other through this one folder, so they all agree.
# Change this one line to point somewhere else.
save_dir <- "~/Desktop"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

data_dir   <- save_dir
output_dir <- save_dir
cache_dir  <- file.path(path.expand(data_dir), "ncaa_d3_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

###############################################
# Browser plumbing
###############################################

ncaa_chrome_up <- function(port = NCAA_PORT) {
  ok <- suppressWarnings(try(
    httr::GET(sprintf("http://127.0.0.1:%d/json/version", port),
              httr::timeout(3)),
    silent = TRUE
  ))
  !inherits(ok, "try-error") && httr::status_code(ok) == 200
}

ncaa_chrome_start <- function(port = NCAA_PORT, profile = NCAA_PROFILE) {
  if (ncaa_chrome_up(port)) {
    message("Chrome already listening on port ", port)
    return(invisible(TRUE))
  }
  dir.create(profile, recursive = TRUE, showWarnings = FALSE)
  message("Launching Chrome on port ", port, " ...")
  system2(CHROME_BIN,
          args = c(paste0("--remote-debugging-port=", port),
                   paste0("--user-data-dir=", shQuote(profile)),
                   "--no-first-run", "--no-default-browser-check",
                   "--window-size=1200,900", "about:blank"),
          wait = FALSE, stdout = FALSE, stderr = FALSE)
  for (i in 1:30) {
    Sys.sleep(1)
    if (ncaa_chrome_up(port)) {
      message("Chrome ready.")
      return(invisible(TRUE))
    }
  }
  stop("Chrome debugging port ", port, " never opened.")
}

ncaa_session <- function(port = NCAA_PORT) {
  ncaa_chrome_start(port)
  b <- Chromote$new(browser = ChromeRemote$new(host = "127.0.0.1", port = port))
  ChromoteSession$new(parent = b)
}

# Navigate and wait until the bot-challenge has cleared and
# real content is present. Polls rather than fixed-sleeping,
# so warmed-up requests return quickly.
# require_text: keep polling until this string appears in the
# rendered HTML. Needed because some pages (rosters) render a
# nav table immediately and populate the real table via JS a
# moment later -- waiting on table count alone returns too early.
ncaa_fetch <- function(session, url, min_tables = 1, require_text = NULL,
                       timeout = 30) {
  session$Page$navigate(url)
  deadline <- Sys.time() + timeout
  html <- ""
  repeat {
    Sys.sleep(0.4)
    html <- tryCatch(
      session$Runtime$evaluate("document.documentElement.outerHTML")$result$value,
      error = function(e) ""
    )
    challenged <- grepl("bm-verify", html, fixed = TRUE)
    m <- gregexpr("<table", html, fixed = TRUE)[[1]]
    n_tab <- if (nzchar(html)) sum(m > 0) else 0
    has_text <- is.null(require_text) ||
      grepl(require_text, html, fixed = TRUE)
    if (!challenged && n_tab >= min_tables && has_text) break
    if (Sys.time() > deadline) {
      stop("timed out waiting for ", url,
           " (challenged=", challenged, ", tables=", n_tab,
           ", marker=", has_text, ")")
    }
  }
  Sys.sleep(REQUEST_DELAY)
  html
}

###############################################
# 1. D3 team list  (this endpoint is NOT challenged,
#    so plain httr works and no browser is needed)
###############################################

NCAA_HEADERS <- c(
  "User-Agent" = paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                       "AppleWebKit/537.36 (KHTML, like Gecko)",
                       "Chrome/124.0.0.0 Safari/537.36"),
  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
  "Accept-Language" = "en-US,en;q=0.9",
  "sec-ch-ua" = '"Chromium";v="124", "Google Chrome";v="124", "Not-A.Brand";v="99"',
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = '"macOS"',
  "Sec-Fetch-Dest" = "document", "Sec-Fetch-Mode" = "navigate",
  "Sec-Fetch-Site" = "none", "Sec-Fetch-User" = "?1",
  "Upgrade-Insecure-Requests" = "1"
)

get_d3_teams <- function(academic_year = 2026, division = 3, sport_code = "WBB") {
  url <- sprintf(
    "https://stats.ncaa.org/team/inst_team_list?academic_year=%d&conf_id=-1&division=%d&sport_code=%s",
    academic_year, division, sport_code)
  r <- httr::GET(url, httr::add_headers(.headers = NCAA_HEADERS), httr::timeout(45))
  stopifnot(httr::status_code(r) == 200)
  pg <- read_html(httr::content(r, "text", encoding = "UTF-8"))
  a  <- html_elements(pg, "a")
  df <- tibble(
    href = html_attr(a, "href"),
    team_display_name = trimws(html_text(a))
  ) %>%
    filter(!is.na(href), str_detect(href, "^/teams/\\d+")) %>%
    mutate(team_id = str_extract(href, "(?<=/teams/)\\d+")) %>%
    select(team_id, team_display_name) %>%
    distinct()
  df
}

###############################################
# 2. Team schedule -> contest (game) ids
###############################################

get_team_game_ids <- function(session, team_id) {
  html <- ncaa_fetch(session, paste0("https://stats.ncaa.org/teams/", team_id))
  unique(unlist(str_extract_all(html, "(?<=contests/)\\d+(?=/)")))
}

###############################################
# 3. Team roster -> position for every player
#    Needed because the box score shows "*" instead of a
#    position for starters.
###############################################

POS_LONG <- c(G = "Guard", F = "Forward", C = "Center")

expand_pos <- function(x) {
  vapply(x, function(p) {
    if (is.na(p) || !nzchar(p)) return(NA_character_)
    parts <- str_split(p, "/")[[1]]
    out <- POS_LONG[parts]
    if (any(is.na(out))) return(p)
    paste(out, collapse = "/")
  }, character(1), USE.NAMES = FALSE)
}

get_team_roster <- function(session, team_id) {
  html <- ncaa_fetch(session,
                     paste0("https://stats.ncaa.org/teams/", team_id, "/roster"),
                     min_tables = 2, require_text = "Position")
  pg   <- read_html(html)
  tabs <- html_elements(pg, "table")
  # The roster page renders TWO tables with identical headers:
  # a header-only shell and the populated one. Match on columns,
  # then take whichever actually has rows.
  cand <- vapply(tabs, function(t) {
    d <- as.data.frame(html_table(t))
    if (all(c("Name", "Position") %in% names(d))) nrow(d) else -1L
  }, integer(1))
  if (all(cand < 1)) {
    warning("no populated roster table for team ", team_id)
    return(tibble())
  }
  tn <- tabs[[which.max(cand)]]
  d  <- as.data.frame(html_table(tn), stringsAsFactors = FALSE)
  rows <- html_elements(tn, "tbody tr")
  pid <- vapply(rows, function(r) {
    h <- html_attr(html_elements(r, "a"), "href")
    p <- str_extract(h[grepl("/players/", h)], "(?<=/players/)\\d+")
    if (length(p) == 0) NA_character_ else p[1]
  }, character(1))
  n <- min(nrow(d), length(pid))
  tibble(
    team_id = as.character(team_id),
    athlete_id = pid[seq_len(n)],
    roster_name = trimws(d[["Name"]][seq_len(n)]),
    athlete_position_abbreviation = na_if(trimws(d[["Position"]][seq_len(n)]), ""),
    athlete_jersey_roster = as.character(d[["#"]][seq_len(n)]),
    # season games played / games started. GS is the only
    # starter information NCAA exposes for feeds that report a
    # position (rather than "*") in the box score P column.
    roster_gp = num(d[["GP"]][seq_len(n)]),
    roster_gs = num(d[["GS"]][seq_len(n)])
  ) %>%
    filter(!is.na(athlete_id)) %>%
    mutate(athlete_position_name = expand_pos(athlete_position_abbreviation))
}

###############################################
# 4. Box score for one contest
#    Mirrors bigballR::scrape_box's table layout
#    (table 1 = background, 4 = away, 5 = home) but also
#    keeps PF, player ids and team ids, which
#    bigballR::get_box_scores() drops.
###############################################

parse_minutes <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA_character_
  mm <- suppressWarnings(as.numeric(str_extract(x, "^\\d+(?=:)")))
  ss <- suppressWarnings(as.numeric(str_extract(x, "(?<=:)\\d+$")))
  plain <- suppressWarnings(as.numeric(x))
  ifelse(!is.na(mm), mm + ss / 60, plain)
}

num <- function(x) {
  x <- gsub("[^0-9.-]", "", trimws(as.character(x)))
  x[x == ""] <- "0"
  suppressWarnings(as.numeric(x))
}

short_name <- function(full) {
  parts <- str_split(trimws(full), "\\s+")
  vapply(parts, function(p) {
    if (length(p) < 2) return(paste(p, collapse = " "))
    paste0(substr(p[1], 1, 1), ". ", paste(p[-1], collapse = " "))
  }, character(1))
}

# Each team must account for ~200 player-minutes (5 on the floor
# x 40 min), more with overtime. A short total means the box
# table was still rendering when we grabbed the HTML -- a real
# failure mode we hit once in 362 team-games, and one that is
# silent unless checked, because a truncated table parses fine.
MIN_TEAM_MINUTES <- 190

scrape_box_d3 <- function(session, game_id, season = 2026, attempts = 3) {
  for (k in seq_len(attempts)) {
    out <- try(scrape_box_d3_once(session, game_id, season = season), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    if (k < attempts) {
      message("  retry ", k, "/", attempts - 1, " for ", game_id, ": ",
              trimws(conditionMessage(attr(out, "condition"))))
      Sys.sleep(2 * k)
    }
  }
  stop(trimws(conditionMessage(attr(out, "condition"))))
}

scrape_box_d3_once <- function(session, game_id, season = 2026) {
  html <- ncaa_fetch(session,
                     paste0("https://stats.ncaa.org/contests/", game_id, "/individual_stats"),
                     min_tables = 5)
  pg   <- read_html(html)
  tabs <- html_elements(pg, "table")
  if (length(tabs) < 5) stop("only ", length(tabs), " tables on contest ", game_id)

  bg <- as.data.frame(html_table(tabs[[1]]), stringsAsFactors = FALSE)
  away_name <- trimws(bg[3, 1]); home_name <- trimws(bg[4, 1])

  # doc-order team links are (away, home) -- verified against
  # the linescore + background table on multiple contests
  tids <- unique(unlist(str_extract_all(html, "(?<=/teams/)\\d+")))
  away_id <- if (length(tids) >= 1) tids[1] else NA_character_
  home_id <- if (length(tids) >= 2) tids[2] else NA_character_

  one_side <- function(tbl_idx, team_name, team_id) {
    tn   <- tabs[[tbl_idx]]
    d    <- as.data.frame(html_table(tn), stringsAsFactors = FALSE)
    rows <- html_elements(tn, "tbody tr")
    pid  <- vapply(rows, function(r) {
      h <- html_attr(html_elements(r, "a"), "href")
      p <- str_extract(h[grepl("/players/", h)], "(?<=/players/)\\d+")
      if (length(p) == 0) NA_character_ else p[1]
    }, character(1))
    n <- min(nrow(d), length(pid))
    d <- d[seq_len(n), , drop = FALSE]; pid <- pid[seq_len(n)]

    # cut the TEAM / totals rows off the bottom
    stop_at <- which(trimws(d[["Name"]]) == "TEAM")
    keep <- if (length(stop_at) == 1) seq_len(stop_at - 1) else seq_len(max(nrow(d) - 2, 0))
    d <- d[keep, , drop = FALSE]; pid <- pid[keep]

    tibble(
      game_id   = as.character(game_id),
      season    = season,
      athlete_id = pid,
      athlete_display_name = trimws(d[["Name"]]),
      athlete_jersey = as.character(d[["#"]]),
      # "*" marks a starter; the real position comes from the roster join
      starter   = trimws(d[["P"]]) == "*",
      pos_box   = na_if(na_if(trimws(d[["P"]]), "*"), ""),
      minutes   = parse_minutes(d[["MP"]]),
      field_goals_made = num(d[["FGM"]]),
      field_goals_attempted = num(d[["FGA"]]),
      three_point_field_goals_made = num(d[["X3FG"]] %||% d[["3FG"]]),
      three_point_field_goals_attempted = num(d[["X3FGA"]] %||% d[["3FGA"]]),
      free_throws_made = num(d[["FT"]]),
      free_throws_attempted = num(d[["FTA"]]),
      points = num(d[["PTS"]]),
      offensive_rebounds = num(d[["ORebs"]]),
      defensive_rebounds = num(d[["DRebs"]]),
      rebounds = num(d[["TotReb"]]),
      assists = num(d[["AST"]]),
      turnovers = num(d[["TO"]]),
      steals = num(d[["STL"]]),
      blocks = num(d[["BLK"]]),
      fouls = num(d[["PF"]]),
      team_id = as.character(team_id),
      team_display_name = team_name
    ) %>%
      filter(!is.na(athlete_display_name), athlete_display_name != "")
  }

  res <- bind_rows(
    one_side(4, away_name, away_id),
    one_side(5, home_name, home_id)
  ) %>%
    mutate(
      did_not_play = is.na(minutes) | minutes == 0,
      athlete_short_name = short_name(athlete_display_name),
      team_short_display_name = team_display_name,
      team_abbreviation = team_display_name
    )

  # Integrity check -- catches partially-rendered box tables
  mins <- res %>%
    group_by(team_id) %>%
    summarise(m = sum(minutes, na.rm = TRUE), .groups = "drop")
  if (nrow(mins) < 2 || any(mins$m < MIN_TEAM_MINUTES)) {
    stop("contest ", game_id, ": incomplete box (team minutes ",
         paste(round(mins$m), collapse = "/"), ")")
  }

  res
}

###############################################
# 5. Driver: scrape many games with checkpointing
###############################################

scrape_games <- function(game_ids, session = NULL, label = "run",
                         season = 2026, checkpoint_every = CHECKPOINT_N) {
  own <- is.null(session)
  if (own) session <- ncaa_session()
  on.exit(if (own) try(session$close(), silent = TRUE), add = TRUE)

  game_ids <- unique(as.character(game_ids[!is.na(game_ids)]))
  ck_file  <- file.path(cache_dir, paste0("box_", label, ".rds"))
  fail_file <- file.path(cache_dir, paste0("failed_", label, ".csv"))

  done <- list(); failed <- tibble(game_id = character(), error = character())
  if (file.exists(ck_file)) {
    prev <- readRDS(ck_file)
    done <- list(prev)
    already <- unique(prev$game_id)
    game_ids <- setdiff(game_ids, already)
    message("Resuming: ", length(already), " games already cached, ",
            length(game_ids), " to go.")
  }

  for (i in seq_along(game_ids)) {
    g <- game_ids[i]
    res <- tryCatch(scrape_box_d3(session, g, season = season),
                    error = function(e) e)
    if (inherits(res, "error")) {
      message(sprintf("[%d/%d] FAILED %s :: %s", i, length(game_ids), g, conditionMessage(res)))
      failed <- bind_rows(failed, tibble(game_id = g, error = conditionMessage(res)))
    } else {
      message(sprintf("[%d/%d] %s (%d rows)", i, length(game_ids), g, nrow(res)))
      done <- c(done, list(res))
    }
    if (i %% checkpoint_every == 0 || i == length(game_ids)) {
      saveRDS(bind_rows(done), ck_file)
      if (nrow(failed) > 0) write_csv(failed, fail_file)
      message("  ... checkpoint written (", nrow(bind_rows(done)), " rows)")
    }
  }

  out <- bind_rows(done)
  saveRDS(out, ck_file)
  if (nrow(failed) > 0) {
    write_csv(failed, fail_file)
    message("\n", nrow(failed), " game(s) failed -- see ", fail_file)
  }
  attr(out, "failed") <- failed
  out
}

scrape_rosters <- function(team_ids, session = NULL, label = "run") {
  own <- is.null(session)
  if (own) session <- ncaa_session()
  on.exit(if (own) try(session$close(), silent = TRUE), add = TRUE)

  ck_file <- file.path(cache_dir, paste0("roster_", label, ".rds"))
  out <- list()
  for (i in seq_along(team_ids)) {
    t <- team_ids[i]
    r <- tryCatch(get_team_roster(session, t), error = function(e) {
      message("  roster FAILED ", t, " :: ", conditionMessage(e)); tibble()
    })
    message(sprintf("[roster %d/%d] team %s (%d players)", i, length(team_ids), t, nrow(r)))
    out <- c(out, list(r))
  }
  res <- bind_rows(out)
  saveRDS(res, ck_file)
  res
}

###############################################
# 6. Assemble a wehoop-shaped player box data frame
###############################################

build_player_box <- function(box, rosters) {
  box %>%
    left_join(
      rosters %>%
        select(athlete_id, athlete_position_abbreviation, athlete_position_name,
               roster_gp, roster_gs) %>%
        distinct(athlete_id, .keep_all = TRUE),
      by = "athlete_id"
    ) %>%
    mutate(
      # roster position is authoritative; fall back to the box "P"
      # column for bench players when the roster join misses
      athlete_position_abbreviation = coalesce(athlete_position_abbreviation, pos_box),
      athlete_position_name = coalesce(athlete_position_name,
                                       expand_pos(pos_box)),
      # NCAA's box score "P" column holds EITHER a position OR "*"
      # for starters -- never both, and which one depends on the
      # scoring feed. So a per-game starter flag only exists for
      # some games. games_started_season comes from the roster's
      # GS column and is always available.
      games_started_season = roster_gs
    ) %>%
    select(
      season, game_id,
      athlete_id, athlete_display_name, athlete_short_name, athlete_jersey,
      athlete_position_name, athlete_position_abbreviation,
      team_id, team_display_name, team_short_display_name, team_abbreviation,
      minutes, starter, games_started_season, did_not_play,
      field_goals_made, field_goals_attempted,
      three_point_field_goals_made, three_point_field_goals_attempted,
      free_throws_made, free_throws_attempted,
      offensive_rebounds, defensive_rebounds, rebounds,
      assists, steals, blocks, turnovers, fouls, points
    )
}
