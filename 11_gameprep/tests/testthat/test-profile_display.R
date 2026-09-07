## ============================================================
## test-profile_display.R
## ------------------------------------------------------------
## These two rules shipped wrong TWICE — fixed on the card's bullet path
## in September and not on its profile table, so the same card printed
## the correct wording in one section and the opposite wording in
## another. They now live in R/profile_display.R precisely so the
## dashboard and Q&A agent cannot repeat that, and these tests pin the
## behaviour at the layer all three surfaces share.
## ============================================================

headers <- local({
  root <- Sys.getenv("GAMEPREP_ROOT", unset = "")
  if (!nzchar(root)) {
    for (d in c(".", "..", "../..", "../../..")) {
      if (file.exists(file.path(d, "source_all.R"))) { root <- normalizePath(d); break }
    }
  }
  if (!nzchar(root)) skip("Cannot locate the project root")
  readr::read_csv(file.path(root, "consumers", "gameprep_cards", "config",
                            "dimension_headers.csv"),
                  show_col_types = FALSE, progress = FALSE)
})


## --- trap 1: degenerate PPP -----------------------------------

test_that("ball_security is the one dimension whose PPP is suppressed", {
  # A turnover scores zero by definition, so every team's ball_security
  # PPP is ~0.00 and printing it looks like a measurement while saying
  # nothing. The card printed "0.00 points per possession" for months.
  expect_false(ppp_meaningful_for("ball_security", "espn_derived", headers))
  expect_true(ppp_meaningful_for("context",   "espn_derived", headers))
  expect_true(ppp_meaningful_for("shot_zone", "espn_derived", headers))
  # creation IS meaningful -- it is excluded from bullets for the
  # inversion, which is a different mechanism entirely.
  expect_true(ppp_meaningful_for("creation",  "espn_derived", headers))
})

test_that("a dimension with no config row keeps its PPP", {
  # The flag is an exception, not a requirement. A new dimension must not
  # silently lose its rate because nobody added a header row.
  expect_true(ppp_meaningful_for("not_a_dimension", "espn_derived", headers))
  expect_true(ppp_meaningful_for("context", "espn_derived", NULL))
})

test_that("profile_row_meta drops the rate clause only where degenerate", {
  expect_equal(
    profile_row_meta(0.25, 0.83, "attempt", "shot_zone", "espn_derived", headers),
    "25% of attempts · 0.83 points per attempt")
  expect_equal(
    profile_row_meta(0.13, 0.00, "possession", "ball_security", "espn_derived", headers),
    "13% of possessions")
  # and never emits a bare "0.00" anywhere
  expect_false(grepl("0.00", profile_row_meta(0.13, 0.00, "possession",
                                              "ball_security", "espn_derived", headers),
                     fixed = TRUE))
})


## --- trap 2: inverted ranking ---------------------------------

test_that("ball_security ranks inverted and nothing else does", {
  expect_true(is_freq_ranked("ball_security"))
  expect_false(is_freq_ranked("shot_zone"))
  expect_false(is_freq_ranked("context"))
})

test_that("an inverted row states the rank of the quantity it NAMES", {
  # NU: worst in the Big Ten at keeping the ball -> percentile 0. The row
  # is labelled "Live-ball turnovers", so it has the MOST of them. The
  # old code printed "18th of 18", which a reader takes as the fewest.
  expect_equal(profile_row_standing(0, 18L, "Big Ten", "ball_security"),
               "the most in the Big Ten")
  # ILL: best at keeping the ball -> the fewest turnovers.
  expect_equal(profile_row_standing(100, 18L, "Big Ten", "ball_security"),
               "the fewest in the Big Ten")
})

test_that("the notable end is the one reported", {
  # "17th-most" is technically true and useless; a team near the top of
  # the population should be described from the top.
  s <- profile_row_standing(88, 18L, "Big Ten", "ball_security")
  expect_match(s, "fewest")
  expect_false(grepl("most", s))
})

test_that("a non-inverted row reads as a plain rank", {
  expect_equal(profile_row_standing(100, 18L, "Big Ten", "shot_zone"), "1st of 18")
  expect_equal(profile_row_standing(0,   18L, "Big Ten", "shot_zone"), "18th of 18")
})

test_that("compact style fits a narrow column and keeps the direction", {
  # The card's rank column is 74px, 58px on a narrow screen, so the long
  # form does not fit. Dropping the population must not drop the word
  # that carries the meaning.
  expect_equal(profile_row_standing(0,   18L, "Big Ten", "ball_security",
                                    style = "compact"), "most")
  expect_equal(profile_row_standing(100, 18L, "Big Ten", "ball_security",
                                    style = "compact"), "fewest")
  expect_match(profile_row_standing(88, 18L, "Big Ten", "ball_security",
                                    style = "compact"), "^\\d+\\w{2}-fewest$")
})

test_that("an unranked row says so rather than inventing a position", {
  expect_equal(profile_row_standing(NA, 18L, "Big Ten", "shot_zone"), "unranked")
  expect_equal(profile_row_standing(NA, 18L, "Big Ten", "ball_security"), "unranked")
})


## --- the defensive flip ---------------------------------------

test_that("display_def_percentile flips once and survives NA", {
  expect_equal(display_def_percentile(100), 0L)
  expect_equal(display_def_percentile(0), 100L)
  expect_equal(display_def_percentile(32), 68L)
  expect_true(is.na(display_def_percentile(NA)))
  # flipping twice returns the stored value -- the silent failure this
  # helper exists to make visible
  expect_equal(display_def_percentile(display_def_percentile(32)), 32L)
})


## --- rank recovery --------------------------------------------

test_that("rank_from_percentile puts the best at 1 and the worst at n", {
  expect_equal(rank_from_percentile(100, 18L), 1L)
  expect_equal(rank_from_percentile(0,   18L), 18L)
  expect_true(is.na(rank_from_percentile(NA, 18L)))
})


## --- direction is per CATEGORY, not per dimension -------------

test_that("only the turnover categories invert, not the whole dimension", {
  # The bug the query layer surfaced. `no_turnover` shares a dimension
  # with the turnover categories but runs the other way: keeping the ball
  # MORE is better. Inverting the dimension ranked NU -- who keep it 78%
  # of the time, the worst end of the Big Ten -- as 4th of 18.
  expect_true(freq_is_bad("live_ball_to"))
  expect_true(freq_is_bad("dead_ball_to"))
  expect_false(freq_is_bad("no_turnover"))

  expect_true(is_freq_ranked("ball_security", "live_ball_to"))
  expect_false(is_freq_ranked("ball_security", "no_turnover"))
  expect_false(is_freq_ranked("shot_zone", "at_rim"))
})

test_that("a non-inverting category in an inverting dimension reads plainly", {
  # "the 4th-fewest" is nonsense applied to "Kept the ball".
  expect_equal(profile_row_standing(0, 18L, "Big Ten", "ball_security", "no_turnover"),
               "18th of 18")
  expect_equal(profile_row_standing(0, 18L, "Big Ten", "ball_security", "live_ball_to"),
               "the most in the Big Ten")
})

test_that("omitting the category keeps the old dimension-wide behaviour", {
  # Back-compat: callers that have not been threaded through yet still
  # get the inverted wording rather than a silent direction flip.
  expect_equal(profile_row_standing(0, 18L, "Big Ten", "ball_security"),
               "the most in the Big Ten")
})
