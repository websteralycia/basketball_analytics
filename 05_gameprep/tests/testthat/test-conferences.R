## ============================================================
## test-conferences.R
## ------------------------------------------------------------
## Comparison populations for percentile grading. The population is a
## CHOICE, independent of where the team actually plays — a school
## changing conferences needs both.
## ============================================================

test_that("custom conference definitions are well formed", {
  for (k in names(CUSTOM_CONFERENCES)) {
    d <- CUSTOM_CONFERENCES[[k]]
    expect_true(all(c("label", "league", "schools") %in% names(d)), info = k)
    expect_true(d$league %in% supported_leagues(), info = k)
    expect_gt(length(d$schools), 1)
    expect_false(any(duplicated(d$schools)), info = k)
  }
})

test_that("the rebuilt Pac-12 holds the nine expected schools", {
  d <- CUSTOM_CONFERENCES$pac12_2027
  expect_equal(length(d$schools), 9)
  expect_true("Utah State Aggies" %in% d$schools)
  expect_true("Gonzaga Bulldogs" %in% d$schools)   # from the WCC, not the MWC
  expect_true("Texas State Bobcats" %in% d$schools) # from the Sun Belt
})

test_that("every custom school resolves against real data", {
  skip_if_offline()
  # A school that fails to match silently shrinks the comparison
  # population, which quietly biases every percentile computed from it.
  for (k in names(CUSTOM_CONFERENCES)) {
    d <- CUSTOM_CONFERENCES[[k]]
    got <- expect_no_warning(conference_teams(k, d$league, 2026))
    expect_equal(nrow(got), length(d$schools), info = k)
  }
})

test_that("a team's historic conference is read from the data", {
  skip_if_offline()
  expect_equal(team_conference("USU", "wbb", 2026), "Mountain West")
  expect_equal(team_conference("UCLA", "wbb", 2026), "Big Ten")
  expect_equal(team_conference("GONZ", "wbb", 2026), "WCC")
})

test_that("the same team can be graded against different populations", {
  skip_if_offline()
  # This is the whole point: Utah State left the Mountain West for the
  # Pac-12, and both comparisons are legitimate.
  mwc  <- population_teams("Mountain West", "wbb", 2026)
  p12  <- population_teams("pac12_2027",    "wbb", 2026)
  expect_true("USU" %in% mwc)
  expect_true("USU" %in% p12)
  expect_false(setequal(mwc, p12))
  # the new conference draws from several current ones
  expect_true("GONZ" %in% p12)
  expect_false("GONZ" %in% mwc)
})

test_that("population works for any school, not just Utah State", {
  skip_if_offline()
  for (tm in c("UCLA", "SDSU", "GONZ", "MRSH")) {
    peers <- population_teams("conference", "wbb", 2026, team = tm)
    expect_gt(length(peers), 5)
    expect_true(tm %in% peers, info = tm)
  }
})

test_that("national population is the whole league", {
  skip_if_offline()
  nat <- population_teams("national", "wbb", 2026)
  expect_gt(length(nat), 300)
  expect_true(all(population_teams("Mountain West", "wbb", 2026) %in% nat))
})

test_that("population = 'conference' requires a team", {
  expect_error(population_teams("conference", "wbb", 2026), "needs `team`")
})

test_that("an unknown conference lists what is available", {
  skip_if_offline()
  expect_error(conference_teams("Atlantic Coast Hockey", "wbb", 2026),
               "No conference matched")
})

test_that("a custom conference cannot be used with the wrong league", {
  skip_if_offline()
  expect_error(conference_teams("pac12_2027", "wnba", 2026), "defined for league")
})

test_that("list_conferences covers both sources", {
  skip_if_offline()
  lc <- list_conferences("wbb", 2026)
  expect_true(all(c("key", "teams", "label", "source") %in% names(lc)))
  expect_true("custom" %in% lc$source)
  expect_true("espn" %in% lc$source)
  expect_true("pac12_2027" %in% lc$key)
})
