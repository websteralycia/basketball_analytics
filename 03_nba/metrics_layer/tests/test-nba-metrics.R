# Tests for nba_metrics.R / nba_data.R
#
# Run:  Rscript 03_nba/scripts/r/tests/test-nba-metrics.R
#
# Unit tests use hand-computed fixtures. Integration tests hit hoopR's cached
# ESPN release and check invariants that must hold for any correct pipeline.

suppressPackageStartupMessages({
  library(testthat)
  library(dplyr)
})

# Locate the source files whether we're run from the repo root, from the tests
# directory, or via testthat (which relocates the working directory).
find_src <- function(fname) {
  candidates <- c(
    fname,
    file.path("..", fname),
    file.path("..", "scripts", "r", fname),
    file.path("scripts", "r", fname),
    file.path("03_nba", "metrics_layer", "scripts", "r", fname),
    file.path("03_nba", "scripts", "r", fname),
    file.path("..", "..", "..", "..", "03_nba", "scripts", "r", fname)
  )
  hit <- Filter(file.exists, candidates)
  if (!length(hit)) stop("Cannot locate ", fname, " from ", getwd())
  hit[[1]]
}

source(find_src("nba_metrics.R"))
source(find_src("nba_data.R"))


# -- unit: the formulas -------------------------------------------------------

test_that("possessions follows the NBA.com convention", {
  # 85 + 0.44*20 + 14 - 10 = 97.8
  expect_equal(nba_possessions(85, 20, 14, 10), 97.8)
  expect_equal(nba_possessions(0, 0, 0, 0), 0)
  # the FT coefficient is configurable
  expect_equal(nba_possessions(85, 20, 14, 10, ft_coef = 0.475), 98.5)
})

test_that("eFG credits threes at 1.5x and guards divide-by-zero", {
  # (40 + 0.5*12) / 88 = 0.5227...
  expect_equal(nba_efg(40, 12, 88), 46 / 88)
  # a team with no threes: eFG collapses to FG%
  expect_equal(nba_efg(40, 0, 80), 0.5)
  expect_true(is.na(nba_efg(0, 0, 0)))
})

test_that("ORB% uses opponent DEFENSIVE rebounds, not offensive", {
  # 10 offensive boards against 30 opponent defensive boards = 25%
  expect_equal(nba_orb_pct(10, 30), 0.25)
  expect_true(is.na(nba_orb_pct(0, 0)))
  # guard against the common wrong version: OREB/(OREB+OPP_OREB).
  # With OREB=10, OPP_OREB=10, OPP_DREB=30 the wrong formula gives 0.5.
  expect_false(isTRUE(all.equal(nba_orb_pct(10, 30), 10 / (10 + 10))))
})


# -- unit: aggregation semantics ----------------------------------------------

test_that("aggregation sums totals before taking ratios", {
  # Two games of wildly different pace. Averaging the per-game ratios gives a
  # different (wrong) answer than pooling the totals.
  df <- tibble::tibble(
    team_id = c(1, 1),
    pts = c(120, 80), fga = c(90, 60), fgm = c(45, 30), fg3m = c(15, 5),
    fta = c(20, 10), oreb = c(10, 5), dreb = c(35, 30), tov = c(12, 18),
    opp_pts = c(110, 90), opp_fga = c(88, 62), opp_fgm = c(42, 34),
    opp_fg3m = c(12, 8), opp_fta = c(18, 22), opp_oreb = c(9, 11),
    opp_dreb = c(33, 31), opp_tov = c(14, 13)
  ) %>% nba_add_metrics()

  agg <- nba_aggregate(df, team_id)

  expect_equal(agg$games, 2)
  expect_equal(agg$pts, 200)
  expect_equal(agg$fga, 150)

  # pooled eFG from totals: (75 + 0.5*20)/150
  expect_equal(agg$efg, (75 + 0.5 * 20) / 150)
  # and that is NOT the mean of the two per-game eFGs
  expect_false(isTRUE(all.equal(agg$efg, mean(df$efg))))
})


test_that("TS% includes the factor of 2", {
  # 25 pts on 20 FGA and 5 FTA: 25 / (2 * (20 + 0.44*5)) = 25/44.4
  expect_equal(nba_ts_pct(25, 20, 5), 25 / (2 * 22.2))
  # dropping the 2 would roughly double it -- guard against that regression
  expect_lt(nba_ts_pct(25, 20, 5), 0.75)
  expect_true(is.na(nba_ts_pct(0, 0, 0)))
})

test_that("pace converts possessions to a per-48 rate", {
  # 100 poss each in a regulation game is, by definition, a pace of 100
  expect_equal(nba_pace(100, 100, 48), 100)
  # the same possession count over an overtime game is a SLOWER pace
  expect_lt(nba_pace(100, 100, 53), 100)
  expect_equal(nba_pace(105, 95, 48), 100)
})

test_that("game length follows the period count", {
  expect_equal(nba_game_minutes(4), 48)
  expect_equal(nba_game_minutes(5), 53)   # one overtime
  expect_equal(nba_game_minutes(6), 58)   # double overtime
  expect_true(is.na(nba_game_minutes(NA)))
  expect_true(is.na(nba_game_minutes(0)))  # postponed / unplayed
})

# -- unit: the source-agnostic seam -------------------------------------------

make_box <- function() {
  tibble::tibble(
    game_id = c("g1", "g1"), team_id = c(1, 2),
    pts  = c(110, 100), fga = c(88, 85), fgm = c(42, 38), fg3m = c(14, 10),
    fta  = c(20, 24),   oreb = c(10, 12), dreb = c(35, 33), tov = c(13, 15)
  )
}

test_that("standardize passes through already-canonical columns", {
  expect_equal(nba_standardize(make_box()), make_box())
  # and lower-cases incoming names, so an ALL-CAPS CSV needs no mapping
  shouty <- make_box(); names(shouty) <- toupper(names(shouty))
  expect_equal(nba_standardize(shouty), make_box())
})

test_that("standardize renames via a canonical = source mapping", {
  odd <- make_box() %>% dplyr::rename(Giveaways = tov, Points = pts)
  out <- nba_standardize(odd, c(tov = "giveaways", pts = "points"))
  expect_true(all(c("tov", "pts") %in% names(out)))
  expect_equal(out$tov, c(13, 15))
})

test_that("standardize fails loudly, naming only what is actually missing", {
  broken <- make_box() %>% dplyr::select(-tov)
  expect_error(nba_standardize(broken), "missing required column\\(s\\): tov")
  # it must not silently succeed and produce NA-poisoned metrics
  expect_error(nba_standardize(broken), "tov")
})

test_that("pairing attaches the opponent and warns on malformed games", {
  paired <- nba_pair_opponents(make_box())
  expect_equal(nrow(paired), 2)
  expect_equal(paired$opp_pts, c(100, 110))
  expect_equal(paired$opp_oreb, c(12, 10))
  # a game with only one side present is dropped, with a warning
  lone <- make_box()[1, ]
  expect_warning(p2 <- nba_pair_opponents(lone), "do not have exactly 2 rows")
  expect_equal(nrow(p2), 0)
})

test_that("a CSV round-trip reproduces the API numbers exactly", {
  # the whole point of the seam: same rows, same answers, different source
  direct <- make_box() %>% nba_pair_opponents() %>% nba_add_metrics()

  f <- tempfile(fileext = ".csv")
  readr::write_csv(make_box(), f)
  from_csv <- readr::read_csv(f, show_col_types = FALSE) %>%
    nba_pair_opponents() %>% nba_add_metrics()
  unlink(f)

  expect_equal(direct$ortg, from_csv$ortg)
  expect_equal(direct$efg,  from_csv$efg)
  expect_equal(direct$net_rtg, from_csv$net_rtg)
})

# -- integration: real data invariants ----------------------------------------

test_that("hoopR team box pairs cleanly into team/opponent rows", {
  skip_on_cran()
  tb <- nba_team_box(seasons = 2025, season_type = "regular")
  pairs <- nba_team_game_pairs(tb)

  # every team-game gets exactly one opponent
  expect_equal(nrow(pairs), nrow(tb))
  expect_true(all(pairs$team_id != pairs$opp_team_id))
  # each game_id appears exactly twice, once per side
  expect_true(all(table(pairs$game_id) == 2))
})

test_that("a team's ORtg equals its opponent's DRtg in the same game", {
  skip_on_cran()
  # This is the strongest structural check available: if the opponent join or
  # the possession pairing is wrong, this identity breaks immediately.
  g <- nba_team_games(seasons = 2025, season_type = "regular")

  chk <- g %>%
    select(game_id, team_id, ortg) %>%
    inner_join(
      g %>% select(game_id, opp_team_id = team_id, drtg_of_other = drtg),
      by = "game_id", relationship = "many-to-many"
    ) %>%
    filter(team_id != opp_team_id)

  expect_equal(chk$ortg, chk$drtg_of_other, tolerance = 1e-8)
})

test_that("league-wide offensive and defensive ratings are identical", {
  skip_on_cran()
  # Across the whole league every point scored is also a point allowed, so the
  # pooled ORtg and DRtg must agree exactly.
  lg <- nba_team_games(seasons = 2025, season_type = "regular") %>%
    nba_aggregate()

  expect_equal(lg$ortg, lg$drtg, tolerance = 1e-8)
  # and land in a plausible modern-NBA band
  expect_gt(lg$ortg, 105)
  expect_lt(lg$ortg, 125)
})

test_that("four factors land in plausible ranges", {
  skip_on_cran()
  t <- nba_team_games(seasons = 2025, season_type = "regular") %>%
    nba_aggregate(team_id, team_display_name)

  expect_equal(nrow(t), 30)
  expect_true(all(t$efg     > 0.45 & t$efg     < 0.62))
  expect_true(all(t$tov_pct > 0.08 & t$tov_pct < 0.20))
  expect_true(all(t$orb_pct > 0.15 & t$orb_pct < 0.40))
  expect_true(all(t$ft_rate > 0.15 & t$ft_rate < 0.35))
})

test_that("play-in, All-Star and Cup-final games are excluded", {
  skip_on_cran()
  reg <- nba_team_box(seasons = 2025, season_type = "regular")
  expect_true(all(reg$season_type == 2))

  # 30 teams x 82 games, once the non-standard games are removed
  expect_equal(nrow(reg), 2460)
  expect_equal(dplyr::n_distinct(reg$team_id), 30)
  expect_true(all(table(reg$team_id) == 82))

  # unfiltered, ESPN tags four All-Star clubs and the Cup final as regular season
  raw <- nba_team_box(seasons = 2025, season_type = "regular",
                      standard_only = FALSE)
  expect_equal(nrow(raw), 2468)
  expect_equal(dplyr::n_distinct(raw$team_id), 34)

  pi <- nba_team_box(seasons = 2025, season_type = "play_in")
  expect_equal(nrow(pi), 12)  # 6 play-in games
})

test_that("pace and TS% match NBA.com within their known convention gap", {
  skip_on_cran()
  t <- nba_team_games(seasons = 2025, season_type = "regular") %>%
    nba_aggregate(team_id, team_display_name)

  # TS% and eFG% are convention-free: they should land essentially on NBA.com's
  expect_equal(mean(t$ts_pct), 0.5762, tolerance = 1e-3)
  expect_equal(mean(t$efg),    0.5430, tolerance = 1e-3)

  # Pace used to run ~1.8 HIGH here, and this test asserted it -- the simple
  # possession formula overstated possessions and the drift was written off as
  # a convention gap. It was not: measured against all 30 teams, that formula
  # reads 2.0 points low on both ORtg and DRtg versus NBA.com. Oliver's
  # estimate with the team-rebound adjustment closed it, so pace should now
  # sit ON NBA.com's figure rather than above it.
  expect_gt(mean(t$pace), 98.5)
  expect_lt(mean(t$pace), 100.5)

  # every game length must be a legal one
  g <- nba_team_games(seasons = 2025, season_type = "regular")
  expect_true(all(g$game_minutes %in% c(48, 53, 58, 63)))
})
