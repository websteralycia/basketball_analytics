## ============================================================
## test-cache_tier1.R
## ------------------------------------------------------------
## The Tier 1 cache is what stops the dashboard and the Q&A agent
## fetching from ESPN on every interaction. Its failure mode is quiet:
## a cache that reads back subtly different from a fresh fetch makes
## every downstream number depend on whether a game happened to be
## cached, which is the least debuggable bug this project could ship.
##
## Writing this file caught exactly that — possession_col_types() forced
## clock_start/clock_end to integer when they are character ("9:49"), so
## every cached clock parsed to NA and readr reported it as a warning the
## caller never saw. Hence the round-trip test below, which is the point
## of the file.
## ============================================================

REF_GAME <- "401851531"   # WBB, the documented reference game

test_that("a cached read is indistinguishable from a fresh fetch", {
  skip_if_offline()
  fresh  <- get_possessions(REF_GAME, "wbb", write = TRUE)
  cached <- get_possessions_one(REF_GAME, "wbb")

  expect_equal(attr(cached, "source"), "cache")
  expect_equal(nrow(fresh), nrow(cached))
  expect_equal(names(fresh), names(cached))

  # column by column, values AND types
  for (n in names(fresh)) {
    expect_equal(cached[[n]], fresh[[n]], info = n)
    expect_equal(class(cached[[n]])[1], class(fresh[[n]])[1], info = n)
  }
})

test_that("clocks survive the round trip as text, not as NA", {
  # The bug this file was written for. "9:49" is not an integer, and
  # under a minute ESPN sends a bare "58.9" — neither parses as one.
  skip_if_offline()
  cached <- get_possessions_one(REF_GAME, "wbb")
  expect_type(cached$clock_start, "character")
  expect_false(any(is.na(cached$clock_start)))
  expect_true(any(grepl(":", cached$clock_start)))
})

test_that("a cached read reports uncredited points as unknown, not zero", {
  # The attribute is a data-quality signal that a CSV cannot carry.
  # Absent would read as "clean game" to anything that checks it.
  skip_if_offline()
  cached <- get_possessions_one(REF_GAME, "wbb")
  expect_true("uncredited_points" %in% names(attributes(cached)))
  expect_true(is.na(attr(cached, "uncredited_points")))
})

test_that("the cache listing sees what was written", {
  skip_if_offline()
  get_possessions_one(REF_GAME, "wbb")
  expect_true(REF_GAME %in% list_cached_possession_games("wbb"))
})

test_that("tier1_cache_status counts against a supplied game list", {
  skip_if_offline()
  get_possessions_one(REF_GAME, "wbb")
  st <- tier1_cache_status("wbb", c(REF_GAME, "999999999"))
  expect_equal(st$games, 2L)
  expect_equal(st$possessions, 1L)
  # a game nobody has fetched must not be counted as present
  expect_lte(st$lineups, 1L)
})

test_that("tier1_cache_status works with no game list", {
  st <- tier1_cache_status("wbb")
  expect_true(st$possessions >= 0)
  expect_true(st$lineups >= 0)
})

test_that("a batch collects failures instead of dying on one bad game", {
  # A season pull that aborts on game 180 of 257 has spent the fetch
  # budget and returns nothing. This is the property that matters most.
  skip_if_offline()
  out <- get_possessions_many(game_ids = c(REF_GAME, "999999999"),
                              league = "wbb", quiet = TRUE)
  expect_true(nrow(out) > 0)
  expect_equal(attr(out, "failed"), "999999999")
})

test_that("an empty game list warns rather than erroring", {
  expect_warning(out <- get_possessions_many(game_ids = character(0),
                                             league = "wbb", quiet = TRUE),
                 "No games matched")
  expect_equal(nrow(out), 0L)
})

test_that("get_possessions_many needs either ids or a season", {
  expect_error(get_possessions_many(league = "wbb", quiet = TRUE),
               "game_ids|season")
})

test_that("possession and stint caches use distinct filenames", {
  # Both live in the same per-game directory; a collision would have one
  # silently read as the other.
  expect_false(identical(possession_cache_path("1", "wbb"),
                         stint_cache_path("1", "wbb")))
  expect_match(possession_cache_path("1", "wbb"), "_possessions\\.csv$")
  expect_match(stint_cache_path("1", "wbb"), "_lineups\\.csv$")
})
