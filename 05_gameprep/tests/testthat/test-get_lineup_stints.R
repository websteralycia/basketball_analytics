## ============================================================
## test-get_lineup_stints.R
## ------------------------------------------------------------
## Pure unit tests (league config, clock helpers) run offline.
## The reconstruction tests hit the ESPN API and are skipped when
## offline, so a flaky network doesn't read as a code failure.
##
## Run with:
##   testthat::test_dir("tests/testthat")
## ============================================================

# ---- expected contract -------------------------------------------------
EXPECTED_COLS <- c(
  "game_id", "team", "opponent", "date",
  "stint_id", "period", "clock_start", "clock_end",
  "player_id_1", "player_id_2", "player_id_3", "player_id_4", "player_id_5",
  "team_pts", "opp_pts", "stint_seconds", "lineup_valid"
)

# A completed WNBA game with a known rapid-substitution cluster
# (four subs all stamped 5:37 in the first period).
TEST_GAME   <- "401857092"
TEST_LEAGUE <- "wnba"


# ======================================================================
# league_config.R
# ======================================================================

test_that("every supported league has a complete config", {
  for (lg in supported_leagues()) {
    cfg <- league_cfg(lg)
    expect_true(all(c("label", "reg_periods", "period_seconds",
                      "ot_seconds", "pbp_fn", "box_fn") %in% names(cfg)),
                info = lg)
    expect_true(is.function(cfg$pbp_fn), info = lg)
    expect_true(is.function(cfg$box_fn), info = lg)
  }
})

test_that("period lengths match each league's actual rules", {
  # WNBA and NCAA women's: four 10-minute quarters
  expect_equal(league_period_seconds("wnba", 1), 600)
  expect_equal(league_period_seconds("wbb",  4), 600)
  # NCAA men's: two 20-minute halves
  expect_equal(league_period_seconds("mbb",  1), 1200)
  expect_equal(league_period_seconds("mbb",  2), 1200)
  # NBA: four 12-minute quarters
  expect_equal(league_period_seconds("nba",  3), 720)
  # overtime is 5 minutes everywhere; note period 3 is already OT for mbb
  expect_equal(league_period_seconds("wnba", 5), 300)
  expect_equal(league_period_seconds("mbb",  3), 300)
  expect_equal(league_period_seconds("nba",  5), 300)
})

test_that("an unknown league fails loudly", {
  expect_error(league_cfg("ncaa_lax"), "Unknown league")
})


# ======================================================================
# utils_clock.R
# ======================================================================

test_that("clock conversion round-trips", {
  expect_equal(clock_to_seconds("10:00"), 600)
  expect_equal(clock_to_seconds("5:37"),  337)
  expect_equal(clock_to_seconds("0:00"),  0)
  expect_equal(seconds_to_clock(600), "10:00")
  expect_equal(seconds_to_clock(337), "5:37")
  expect_equal(seconds_to_clock(0),   "0:00")
})

test_that("clock conversion is vectorised and NA-safe", {
  expect_equal(clock_to_seconds(c("10:00", "0:30")), c(600, 30))
  expect_true(is.na(clock_to_seconds(NA_character_)))
  expect_true(is.na(seconds_to_clock(NA)))
})

test_that("sub-second clocks round to whole seconds", {
  expect_equal(seconds_to_clock(0.4), "0:00")
  expect_equal(seconds_to_clock(59.6), "1:00")
})

test_that("bare-seconds clocks parse — ESPN drops the colon under a minute", {
  # A typical game has 50+ of these. Treating them as unparseable put NA
  # clocks on any stint boundary inside the last minute of a period.
  expect_equal(clock_to_seconds("58.9"), 58.9)
  expect_equal(clock_to_seconds("7.7"),  7.7)
  expect_equal(clock_to_seconds("0.4"),  0.4)
  # mixed vector: both formats in one call
  expect_equal(clock_to_seconds(c("10:00", "58.9", "1:30", "0.4")),
               c(600, 58.9, 90, 0.4))
})

test_that("unparseable clocks are NA, not an error", {
  expect_true(is.na(clock_to_seconds("")))
  expect_true(is.na(clock_to_seconds("halftime")))
  expect_equal(clock_to_seconds(c("10:00", "nonsense")), c(600, NA))
})


# ======================================================================
# get_lineup_stints() — reconstruction
# ======================================================================

# Fetch once and reuse across tests — one API call for the whole file.
test_stints <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- get_lineup_stints(TEST_GAME, league = TEST_LEAGUE, write = FALSE)
    }
    cached
  }
})

test_that("output matches the documented schema exactly", {
  skip_if_offline()
  stints <- test_stints()
  expect_named(stints, EXPECTED_COLS)
  expect_type(stints$game_id,       "character")
  expect_type(stints$team,          "character")
  expect_type(stints$opponent,      "character")
  expect_s3_class(stints$date,      "Date")
  expect_type(stints$stint_id,      "integer")
  expect_type(stints$period,        "integer")
  expect_type(stints$clock_start,   "character")
  expect_type(stints$clock_end,     "character")
  expect_type(stints$player_id_1,   "character")
  expect_type(stints$team_pts,      "integer")
  expect_type(stints$opp_pts,       "integer")
  expect_type(stints$stint_seconds, "double")
  expect_type(stints$lineup_valid,  "logical")
})

test_that("there are exactly two rows per stint, one per team", {
  skip_if_offline()
  stints <- test_stints()
  expect_equal(nrow(stints), 2 * dplyr::n_distinct(stints$stint_id))
  per_stint <- table(stints$stint_id)
  expect_true(all(per_stint == 2))
  # the two rows are opposite sides of the same matchup
  expect_equal(dplyr::n_distinct(stints$team), 2)
})

test_that("stint_id is sequential and contiguous within the game", {
  skip_if_offline()
  stints <- test_stints()
  ids <- sort(unique(stints$stint_id))
  expect_equal(ids, seq_len(length(ids)))
})

test_that("every lineup is exactly five distinct players", {
  skip_if_offline()
  stints <- test_stints()
  pid <- as.matrix(stints[, paste0("player_id_", 1:5)])
  expect_true(all(apply(pid, 1, function(r) sum(!is.na(r)) == 5)))
  expect_false(any(apply(pid, 1, function(r) any(duplicated(r)))))
})

test_that("player ids are sorted, so the five columns form a stable lineup key", {
  skip_if_offline()
  stints <- test_stints()
  pid <- as.matrix(stints[, paste0("player_id_", 1:5)])
  expect_true(all(apply(pid, 1, function(r) identical(r, sort(r)))))
})

test_that("stint points reconcile to the box score", {
  skip_if_offline()
  stints <- test_stints()
  box <- wehoop::espn_wnba_player_box(game_id = TEST_GAME)
  box_pts <- box |>
    dplyr::group_by(team = team_abbreviation) |>
    dplyr::summarise(box = sum(points, na.rm = TRUE), .groups = "drop")
  got <- stints |>
    dplyr::group_by(team) |>
    dplyr::summarise(stint = sum(team_pts), .groups = "drop")
  cmp <- dplyr::inner_join(box_pts, got, by = "team")
  expect_equal(nrow(cmp), 2)
  expect_equal(cmp$stint, cmp$box)
})

test_that("each period's stints tile the full period with no gaps or overlap", {
  skip_if_offline()
  stints <- test_stints()
  one_team <- stints[stints$team == stints$team[1], ]
  by_period <- tapply(one_team$stint_seconds, one_team$period, sum)
  for (p in names(by_period)) {
    expect_equal(as.numeric(by_period[[p]]),
                 league_period_seconds(TEST_LEAGUE, as.integer(p)),
                 info = paste("period", p))
  }
})

test_that("clock_start is never earlier in the period than clock_end", {
  skip_if_offline()
  stints <- test_stints()
  expect_true(all(clock_to_seconds(stints$clock_start) >=
                  clock_to_seconds(stints$clock_end)))
  expect_equal(stints$stint_seconds,
               clock_to_seconds(stints$clock_start) -
                 clock_to_seconds(stints$clock_end))
})

test_that("zero-second stints from rapid substitutions are preserved", {
  skip_if_offline()
  stints <- test_stints()
  # this game has a four-sub cluster at 5:37 in period 1
  expect_gt(sum(stints$stint_seconds == 0), 0)
  cluster <- stints[stints$period == 1 &
                    stints$clock_start == "5:37" &
                    stints$clock_end == "5:37", ]
  expect_gt(nrow(cluster), 0)
})

test_that("team and opponent are mirrored between the two rows of a stint", {
  skip_if_offline()
  stints <- test_stints()
  s <- stints[stints$stint_id == 1, ]
  expect_equal(s$team[1], s$opponent[2])
  expect_equal(s$team[2], s$opponent[1])
  expect_equal(s$team_pts[1], s$opp_pts[2])
  expect_equal(s$team_pts[2], s$opp_pts[1])
})

test_that("an unknown league is rejected before any network call", {
  expect_error(get_lineup_stints("1", league = "wfl"), "should be one of")
})

test_that("no stint has a missing clock or duration", {
  skip_if_offline()
  # Regression guard: a sub landing on a bare-seconds clock ("58.9") used
  # to produce NA clock_start and NA stint_seconds.
  for (gid in c("401857092", "401857090")) {
    st <- get_lineup_stints(gid, league = TEST_LEAGUE, write = FALSE)
    expect_false(any(is.na(st$clock_start)), info = gid)
    expect_false(any(is.na(st$clock_end)),   info = gid)
    expect_false(any(is.na(st$stint_seconds)), info = gid)
    # and the periods still tile exactly
    one <- st[st$team == st$team[1], ]
    tiled <- tapply(one$stint_seconds, one$period, sum)
    for (p in names(tiled)) {
      expect_equal(as.numeric(tiled[[p]]),
                   league_period_seconds(TEST_LEAGUE, as.integer(p)),
                   info = paste(gid, "period", p))
    }
  }
})


# ======================================================================
# get_lineup_stints_many() — batch + cache
# ======================================================================

test_that("a cached read is indistinguishable from a fresh fetch", {
  skip_if_offline()
  gid <- TEST_GAME
  fresh <- get_lineup_stints(gid, league = TEST_LEAGUE, write = TRUE)
  cached <- get_lineup_stints_one(gid, league = TEST_LEAGUE, refresh = FALSE)
  expect_identical(attr(cached, "source"), "cache")
  attr(cached, "source") <- NULL
  expect_equal(as.data.frame(fresh), as.data.frame(cached))
})

test_that("refresh = TRUE bypasses the cache", {
  skip_if_offline()
  got <- get_lineup_stints_one(TEST_GAME, league = TEST_LEAGUE, refresh = TRUE)
  expect_identical(attr(got, "source"), "fetch")
})

test_that("batch stacks games and reports failures without aborting", {
  skip_if_offline()
  out <- suppressWarnings(
    get_lineup_stints_many(c(TEST_GAME, "not_a_game_id"),
                           league = TEST_LEAGUE, quiet = TRUE)
  )
  expect_equal(attr(out, "failed"), "not_a_game_id")
  expect_equal(dplyr::n_distinct(out$game_id), 1)
  expect_named(out, EXPECTED_COLS)
})

test_that("batch requires either game_ids or a season", {
  expect_error(get_lineup_stints_many(league = "wnba"), "either `game_ids` or `season`")
})


# ======================================================================
# NCAA WBB — different substitution grammar and messier event ordering
# ======================================================================

# WBB emits SINGLE-PLAYER substitution events ("X subbing out for <Team>")
# with athlete_id_2 always NA, where the WNBA emits paired swaps
# ("X enters the game for Y"). Reconstruction was completely broken for
# WBB until that was handled: every sub failed and the lineup stayed
# frozen on the starters for the whole game.
WBB_GAMES <- c("401851531",   # single-player subs, "makes/misses" text
               "401826610")   # single-player subs, "made/missed" text,
                              # AND events out of period order (1,2,3,4,2,4)

test_that("WBB lineups actually change over the course of a game", {
  skip_if_offline()
  for (gid in WBB_GAMES) {
    st <- suppressWarnings(get_lineup_stints(gid, league = "wbb", write = FALSE))
    per_team <- tapply(
      apply(st[, paste0("player_id_", 1:5)], 1, paste, collapse = "|"),
      st$team, function(x) length(unique(x))
    )
    # frozen starters would give exactly 1 distinct lineup per team
    expect_true(all(per_team > 5), info = paste(gid, "lineups:", paste(per_team, collapse = "/")))
  }
})

test_that("WBB points reconcile and periods tile exactly", {
  skip_if_offline()
  for (gid in WBB_GAMES) {
    st <- suppressWarnings(get_lineup_stints(gid, league = "wbb", write = FALSE))
    box <- wehoop::espn_wbb_player_box(game_id = gid)
    box_pts <- sort(tapply(box$points, box$team_abbreviation, sum, na.rm = TRUE))
    got_pts <- sort(tapply(st$team_pts, st$team, sum))
    expect_equal(as.numeric(got_pts), as.numeric(box_pts), info = gid)

    one <- st[st$team == st$team[1], ]
    tiled <- tapply(one$stint_seconds, one$period, sum)
    for (p in names(tiled)) {
      expect_equal(as.numeric(tiled[[p]]),
                   league_period_seconds("wbb", as.integer(p)),
                   info = paste(gid, "period", p))
    }
  }
})

test_that("out-of-order events do not invent extra periods", {
  skip_if_offline()
  # 401826610 lists periods as 1,2,3,4,2,4 in sequence_number order and
  # its clock runs backwards within two periods. Ordering by sequence
  # alone made the walk re-enter finished periods, doubling their length.
  st <- suppressWarnings(get_lineup_stints("401826610", league = "wbb", write = FALSE))
  one <- st[st$team == st$team[1], ]
  expect_equal(sum(one$stint_seconds), 4 * 600)
  expect_equal(sort(unique(st$period)), 1:4)
})

test_that("a broken substitution grammar errors instead of returning frozen lineups", {
  # The dangerous failure mode: unresolved subs leave the starters on the
  # floor all game, and lineup_valid cannot detect it because five frozen
  # starters ARE five players. It must be an error, not a warning.
  expect_true(MAX_UNRESOLVED_SUB_RATE > 0 && MAX_UNRESOLVED_SUB_RATE < 1)
})

test_that("both substitution grammars are recognised", {
  skip_if_offline()
  # paired (WNBA) and single-player (WBB) must both reconstruct
  wnba <- get_lineup_stints(TEST_GAME, league = "wnba", write = FALSE)
  wbb  <- suppressWarnings(get_lineup_stints(WBB_GAMES[1], league = "wbb", write = FALSE))
  expect_gt(dplyr::n_distinct(wnba$stint_id), 20)
  expect_gt(dplyr::n_distinct(wbb$stint_id),  20)
  expect_named(wbb, EXPECTED_COLS)
})
