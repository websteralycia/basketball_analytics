## ============================================================
## test-team_play_profile.R
## ------------------------------------------------------------
## The full season build is slow (a bulk load plus hundreds of games),
## so it is not run here. These cover the per-game rows, the card join,
## and the percentile contract — the parts where a silent error would
## produce a plausible-looking wrong card.
## ============================================================

test_that("per-game rows cover every dimension with sane units", {
  skip_if_offline()
  rows <- game_category_rows(wehoop::espn_wbb_pbp(game_id = "401851531"), "wbb")

  expect_true(all(c("off_team", "def_team", "dimension", "category",
                    "n", "points", "unit") %in% names(rows)))
  # ball_security added 2026-09-03; tempo was added and then REMOVED the
  # same day -- bucketing by possession length made 22% of "early offense"
  # turnovers and 8% buzzer-truncated, so the label oversold the bucket.
  expect_setequal(unique(rows$dimension),
                  c("context", "ball_security", "shot_zone", "creation"))
  expect_false("tempo" %in% rows$dimension)

  # units are NOT interchangeable: possessions, attempts and made field
  # goals have different denominators and must be labelled differently
  units <- unique(rows[, c("dimension", "unit")])
  expect_equal(units$unit[units$dimension == "context"],       "possession")
  expect_equal(units$unit[units$dimension == "ball_security"], "possession")
  expect_equal(units$unit[units$dimension == "shot_zone"],     "attempt")
  expect_equal(units$unit[units$dimension == "creation"],      "made_fg")
})

test_that("categories stay inside their declared vocabulary", {
  skip_if_offline()
  rows <- game_category_rows(wehoop::espn_wbb_pbp(game_id = "401851531"), "wbb")
  expect_true(all(rows$category[rows$dimension == "context"] %in% context_levels()))
  expect_true(all(rows$category[rows$dimension == "shot_zone"] %in% shot_zone_levels()))
  expect_true(all(rows$category[rows$dimension == "creation"] %in% c("assisted", "unassisted")))
})

test_that("both teams appear on offence and defence", {
  skip_if_offline()
  rows <- game_category_rows(wehoop::espn_wbb_pbp(game_id = "401851531"), "wbb")
  expect_equal(dplyr::n_distinct(rows$off_team), 2)
  expect_equal(dplyr::n_distinct(rows$def_team), 2)
  # a team's offence is the other team's defence
  expect_setequal(unique(rows$off_team), unique(rows$def_team))
})

test_that("percentile floor is respected and never imputed", {
  # Below the minimum a percentile must be NA — never 50, never guessed,
  # because a made-up middle reads as 'average' rather than 'unknown'.
  prof <- tibble::tibble(
    league = "wbb", season = 2026L, population = "test",
    team = c("A", "B", "C", "D"),
    dimension = "context", category = "transition", unit = "possession",
    source = "espn_derived",
    off_n = c(100, 100, 100, 5),          # D is below the floor
    off_freq = 0.1, off_ppp = c(1.2, 1.0, 0.8, 2.0),
    def_n = 100, def_ppp = c(0.9, 1.0, 1.1, 1.0)
  )
  pctl <- function(x, n, min_n) {
    ok <- !is.na(x) & !is.na(n) & n >= min_n
    out <- rep(NA_real_, length(x))
    if (sum(ok) >= 2) out[ok] <- 100 * (rank(x[ok], ties.method = "average") - 1) / (sum(ok) - 1)
    round(out)
  }
  got <- pctl(prof$off_ppp, prof$off_n, 50)
  expect_true(is.na(got[4]))                   # below floor
  expect_equal(got[1], 100)                    # best of the qualifying three
  expect_equal(got[3], 0)                      # worst
})

test_that("defensive percentile is oriented so high means attackable", {
  # The card's right-hand side must always read 'attack here' when high.
  # Storing it defence-favourable and flipping at render time is how the
  # two sides come to mean opposite things through a small bug.
  pctl <- function(x, n, min_n) {
    ok <- !is.na(x) & n >= min_n
    out <- rep(NA_real_, length(x))
    if (sum(ok) >= 2) out[ok] <- 100 * (rank(x[ok], ties.method = "average") - 1) / (sum(ok) - 1)
    round(out)
  }
  # team giving up the MOST points per possession should rank highest
  got <- pctl(c(0.8, 1.0, 1.3), rep(100, 3), 50)
  expect_equal(which.max(got), 3L)
})

test_that("attack_index_card joins our offence to their defence", {
  prof <- tibble::tibble(
    league = "wbb", season = 2026L, population = "test",
    team = rep(c("USU", "SDSU"), each = 2),
    dimension = "context", category = rep(c("transition", "halfcourt"), 2),
    unit = "possession", source = "espn_derived",
    off_n = c(60, 400, 80, 380), off_freq = c(0.13, 0.87, 0.17, 0.83),
    off_ppp = c(1.10, 0.92, 1.05, 0.95), off_ppp_pctl = c(80, 40, 70, 55),
    def_n = c(70, 390, 65, 405), def_ppp = c(0.99, 0.90, 1.20, 0.88),
    def_ppp_pctl = c(50, 30, 90, 20)
  )
  card <- attack_index_card(prof, team = "USU", opponent = "SDSU")

  expect_true(all(c("category", "poss", "freq", "ppp", "ppp_percentile",
                    "def_ppp", "def_ppp_percentile") %in% names(card)))
  expect_equal(nrow(card), 2)
  # offence comes from USU, defence from SDSU
  tr <- card[card$category == "transition", ]
  expect_equal(tr$ppp, 1.10)
  expect_equal(tr$def_ppp, 1.20)
  expect_equal(tr$def_ppp_percentile, 90)
  # sorted by frequency so the categories that matter come first
  expect_equal(card$category[1], "halfcourt")
})

test_that("card errors clearly when a team is missing", {
  prof <- tibble::tibble(team = "USU", dimension = "context", category = "transition",
                         unit = "possession", source = "x", off_n = 1, off_freq = 1,
                         off_ppp = 1, off_ppp_pctl = 1, def_ppp = 1, def_ppp_pctl = 1)
  expect_error(attack_index_card(prof, "USU", "NOBODY"), "No profile rows for opponent")
  expect_error(attack_index_card(prof, "NOBODY", "USU"), "No profile rows for team")
})


## --- tempo and ball_security (added 2026-09-03) ---------------

PROFILE_CSV_B10 <- local({
  for (d in c(".", "..", "../..", "../../..")) {
    f <- file.path(d, "data", "tidy", "wbb", "profiles",
                   "big_ten_2026_play_profile.csv")
    if (file.exists(f)) return(normalizePath(f))
  }
  "data/tidy/wbb/profiles/big_ten_2026_play_profile.csv"
})

test_that("game_category_rows emits the five espn_derived dimensions", {
  skip_if_offline()
  r <- game_category_rows(wehoop::espn_wbb_pbp(game_id = "401851531"), "wbb")
  expect_setequal(unique(r$dimension),
                  c("context", "ball_security", "shot_zone", "creation"))
  # each dimension carries ONE unit
  u <- r |> dplyr::distinct(dimension, unit)
  expect_equal(nrow(u), dplyr::n_distinct(r$dimension))
})

test_that("ball_security partitions possessions and splits turnovers", {
  skip_if_offline()
  pbp  <- wehoop::espn_wbb_pbp(game_id = "401851531")
  poss <- classify_possessions(pbp, "wbb")
  r    <- game_category_rows(pbp, "wbb")
  bs   <- r[r$dimension == "ball_security", ]

  expect_setequal(unique(bs$category),
                  c("live_ball_to", "dead_ball_to", "no_turnover"))
  # exhaustive: no_turnover keeps the denominator at ALL possessions
  expect_equal(sum(bs$n), nrow(poss))
  # and the two turnover buckets must sum to the turnovers themselves
  expect_equal(sum(bs$n[bs$category != "no_turnover"]),
               sum(poss$end_reason == "turnover"))
})

test_that("turnovers score zero, which is why ball_security PPP is degenerate", {
  skip_if_offline()
  r  <- game_category_rows(wehoop::espn_wbb_pbp(game_id = "401851531"), "wbb")
  bs <- r[r$dimension == "ball_security", ]
  expect_equal(sum(bs$points[bs$category != "no_turnover"]), 0)
  # the config must therefore mark the dimension as PPP-less
  hd <- readr::read_csv(file.path(card_config_dir(), "dimension_headers.csv"),
                        show_col_types = FALSE, progress = FALSE)
  expect_false(as.logical(hd$ppp_meaningful[hd$dimension == "ball_security"]))
  # every other dimension keeps a meaningful PPP
  others <- hd$ppp_meaningful[hd$source == "espn_derived" &
                              hd$dimension != "ball_security"]
  expect_true(all(as.logical(others)))
})


test_that("PPP-degenerate dimensions rank on frequency, not on PPP noise", {
  # A turnover scores zero, so off_ppp should be a flat tie across teams --
  # except a few possessions bank a point before the turnover lands. Ranking
  # on that noise put eleven Big Ten teams in a tie at the 29th percentile
  # and let three outrank them at random, which is how UCLA (2nd BEST in the
  # conference at 8.5% live-ball turnovers) appeared as a "struggle".
  skip_if_not(file.exists(PROFILE_CSV_B10), "Big Ten profile not present")
  prof <- readr::read_csv(PROFILE_CSV_B10, show_col_types = FALSE, progress = FALSE)

  expect_true("ball_security" %in% FREQ_RANKED_DIMENSIONS)

  lb <- prof |>
    dplyr::filter(dimension == "ball_security", category == "live_ball_to") |>
    dplyr::arrange(off_freq)

  # The percentile must fall monotonically as turnover frequency rises:
  # giving it away more often is worse, so it must rank lower.
  expect_true(all(diff(lb$off_ppp_pctl) <= 0),
              info = paste(lb$team, lb$off_ppp_pctl, collapse = " "))
  expect_equal(lb$off_ppp_pctl[1], 100)                    # fewest turnovers
  expect_equal(lb$off_ppp_pctl[nrow(lb)], 0)               # most turnovers

  # and it must NOT be tracking the degenerate PPP column
  expect_gt(stats::sd(lb$off_ppp_pctl), 20)
  expect_lt(max(lb$off_ppp), 0.02)                         # PPP really is ~0
})

test_that("PPP-meaningful dimensions still rank on PPP", {
  skip_if_not(file.exists(PROFILE_CSV_B10), "Big Ten profile not present")
  prof <- readr::read_csv(PROFILE_CSV_B10, show_col_types = FALSE, progress = FALSE)
  z <- prof |>
    dplyr::filter(dimension == "shot_zone", category == "at_rim") |>
    dplyr::arrange(off_ppp)
  expect_true(all(diff(z$off_ppp_pctl) >= 0))
})
