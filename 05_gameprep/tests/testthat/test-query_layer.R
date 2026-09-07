## ============================================================
## test-query_layer.R
## ------------------------------------------------------------
## The typed surface the dashboard renders and the Q&A agent calls. Two
## things matter here and they are different:
##
##   1. the CONTRACT — every row comes back display-ready, so a React
##      front end renders `label`/`meta`/`standing` and never recomputes
##      them. If these columns can go missing, the guardrails fork into
##      JavaScript, which is the whole reason this layer exists.
##   2. the DIRECTION rule that writing this layer uncovered. See
##      test-profile_display.R for the unit-level version.
## ============================================================

BIG_TEN <- list(league = "wbb", season = 2026, population = "Big Ten")

has_profile <- file.exists(profile_cache_path("Big Ten", 2026, "wbb"))

test_that("every profile row comes back display-ready", {
  skip_if_not(has_profile, "cached Big Ten profile not present")
  p <- q_team_profile("wbb", 2026, "Big Ten", "NU")
  for (col in c("label", "meta", "standing", "rank", "bulletable")) {
    expect_true(col %in% names(p), info = col)
  }
  # nothing blank -- a UI printing "" is worse than one printing a number
  expect_true(all(nzchar(p$label)))
  expect_true(all(nzchar(p$meta)))
  expect_true(all(nzchar(p$standing)))
  # and the raw numbers survive for charting
  for (col in c("freq", "ppp", "ppp_percentile", "poss")) {
    expect_true(col %in% names(p), info = col)
  }
})

test_that("the display layer is the only thing that formats", {
  skip_if_not(has_profile, "cached Big Ten profile not present")
  p <- q_team_profile("wbb", 2026, "Big Ten", "NU")
  bs <- p[p$dimension == "ball_security", ]
  # degenerate rate suppressed, exactly as on the card
  expect_false(any(grepl("points per possession", bs$meta)))
  # and present where it is real
  sz <- p[p$dimension == "shot_zone", ]
  expect_true(all(grepl("points per attempt", sz$meta)))
})

test_that("q_team_profile can restrict to the bulletable pool", {
  skip_if_not(has_profile, "cached Big Ten profile not present")
  all_rows <- q_team_profile("wbb", 2026, "Big Ten", "NU")
  pool     <- q_team_profile("wbb", 2026, "Big Ten", "NU", bulletable_only = TRUE)
  expect_lt(nrow(pool), nrow(all_rows))
  expect_true(all(pool$bulletable))
  # the residual buckets and the creation pair are what came out
  expect_false(any(pool$category %in% c("halfcourt", "no_turnover",
                                        "assisted", "unassisted")))
})

test_that("an unknown team errors with the roster, not a blank frame", {
  skip_if_not(has_profile, "cached Big Ten profile not present")
  expect_error(q_team_profile("wbb", 2026, "Big Ten", "DUKE"), "not in the Big Ten")
  expect_error(q_team_profile("wbb", 2026, "Big Ten", "DUKE"), "MICH")
})

test_that("q_matchup exposes both readings of the defensive percentile", {
  skip_if_not(has_profile, "cached Big Ten profile not present")
  m <- q_matchup("wbb", 2026, "Big Ten", "MICH", "NU")
  expect_true(all(c("def_ppp_percentile", "def_display_percentile") %in% names(m)))
  # flipped exactly once, never twice
  ok <- !is.na(m$def_ppp_percentile)
  expect_equal(m$def_display_percentile[ok], 100L - m$def_ppp_percentile[ok])
})

test_that("q_leaders answers 'who else in the conference'", {
  skip_if_not(has_profile, "cached Big Ten profile not present")
  top <- q_leaders("wbb", 2026, "Big Ten", "shot_zone", "at_rim", n = 5L)
  expect_equal(nrow(top), 5L)
  expect_true("team" %in% names(top))
  # best-first means descending percentile
  expect_true(all(diff(top$ppp_percentile) <= 0))
  bottom <- q_leaders("wbb", 2026, "Big Ten", "shot_zone", "at_rim",
                      n = 5L, best = FALSE)
  expect_true(all(diff(bottom$ppp_percentile) >= 0))
  expect_false(identical(top$team, bottom$team))
})

test_that("q_leaders names the fix rather than returning nothing", {
  skip_if_not(has_profile, "cached Big Ten profile not present")
  expect_error(q_leaders("wbb", 2026, "Big Ten", "shot_zone", "not_a_zone"),
               "q_dimensions")
})

test_that("q_population reports the size AND how it should be displayed", {
  skip_if_offline()
  p <- q_population("wbb", 2026, "Big Ten")
  expect_equal(p$n, 18L)
  expect_equal(p$standing_mode, "rank")   # 18 teams -> ranks, not percentiles
  expect_true("MICH" %in% p$teams)
})

test_that("q_dimensions carries the rules the agent must not invent", {
  d <- q_dimensions()
  expect_true(all(c("display_label", "bulletable", "exclusion_reason",
                    "ppp_definition", "ppp_meaningful") %in% names(d)))
  # the two guardrails the agent could otherwise talk past
  bs <- d[d$dimension == "ball_security" & d$source == "espn_derived", ]
  expect_true(all(!as.logical(bs$ppp_meaningful)))
  cr <- d[d$dimension == "creation" & d$source == "espn_derived", ]
  expect_true(all(!as.logical(cr$bulletable)))
  expect_true(all(nzchar(cr$exclusion_reason)))
})

test_that("the tool manifest matches the functions that exist", {
  # A tool the agent can name but not call, or a function reachable but
  # undescribed, are both live bugs the moment the agent ships.
  for (nm in query_tool_names()) {
    expect_true(exists(nm, mode = "function"), info = nm)
  }
  for (t in QUERY_TOOLS) {
    expect_true(nzchar(t$description), info = t$name)
    if (length(t$params)) {
      formals_have <- names(formals(get(t$name)))
      expect_true(all(t$params %in% formals_have),
                  info = paste(t$name, ":", paste(setdiff(t$params, formals_have),
                                                  collapse = ", ")))
    }
  }
})
