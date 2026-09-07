## ============================================================
## test-team_season_summary.R
## ------------------------------------------------------------
## The header tiles this feeds were previously hand-typed, and the typed
## values were wrong — SJSU appeared as 5-15 on a card where they went
## 4-28. So the properties worth pinning are the ones that would let a
## wrong number look plausible again:
##
##   * wins + losses must equal games (a dropped game shifts a record)
##   * pace must be NA without a profile, never silently computed from a
##     different possession definition
##   * minutes must include overtime (USU's pace moves 71.8 -> 71.5)
##   * team_header_stats() must return NULL, not NA, for anything it
##     cannot compute, so the renderer prints an em dash instead of
##     "NA" or a stale value
##
## Most tests use a synthetic schedule so they run offline and the
## arithmetic is checkable by hand.
## ============================================================

# A minimal schedule: 4 teams, a round robin, one overtime game.
fake_schedule <- function() {
  tibble::tibble(
    game_id = as.character(1:6),
    game_date = as.Date("2026-01-01") + c(0, 2, 4, 6, 8, 10),
    home_abbreviation = c("AAA", "BBB", "CCC", "AAA", "BBB", "AAA"),
    away_abbreviation = c("BBB", "CCC", "AAA", "CCC", "AAA", "BBB"),
    home_score = c(70, 60, 55, 80, 75, 62),
    away_score = c(65, 58, 70, 70, 71, 68),
    status_type_completed = TRUE,
    status_period = c(4, 4, 4, 4, 5, 4)   # game 5 went to one overtime
  )
}

# Matching profile: only `context` rows matter for possessions.
fake_profile <- function(poss = c(AAA = 300, BBB = 300, CCC = 200)) {
  dplyr::bind_rows(lapply(names(poss), function(tm) {
    tibble::tibble(
      team = tm, dimension = "context",
      category = c("transition", "second_chance", "halfcourt"),
      off_n = c(poss[[tm]] * 0.2, poss[[tm]] * 0.1, poss[[tm]] * 0.7),
      off_freq = c(0.2, 0.1, 0.7)
    )
  }))
}


## --- record ---------------------------------------------------

test_that("wins and losses reconcile with games played", {
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule())
  expect_true(all(s$wins + s$losses == s$games))
  expect_equal(s$record, paste0(s$wins, "-", s$losses))
  # 6 games, 2 teams per game = 12 team-games over 3 teams
  expect_equal(sum(s$games), 12L)
  # every game produces exactly one win and one loss
  expect_equal(sum(s$wins), sum(s$losses))
  expect_equal(sum(s$wins), 6L)
})

test_that("the result comes from the scores, not the home_winner flag", {
  # home_winner is NA on some real rows even where both scores exist, so
  # the summary must not depend on it. Here it is absent entirely.
  sched <- fake_schedule()
  expect_false("home_winner" %in% names(sched))
  s <- team_season_summary("wbb", 2026, schedule = sched)
  # AAA: L(55-70 away@CCC? no) -- check one directly instead
  aaa <- s[s$team == "AAA", ]
  # AAA plays games 1, 3, 4, 5, 6 -> W W W L L
  expect_equal(aaa$games, 5L)
  expect_equal(aaa$record, "3-2")
})

test_that("incomplete games are excluded", {
  sched <- fake_schedule()
  sched$status_type_completed[6] <- FALSE
  s <- team_season_summary("wbb", 2026, schedule = sched)
  expect_equal(sum(s$games), 10L)   # 5 completed games
})

test_that("a schedule with no completed games is an error, not an empty frame", {
  sched <- fake_schedule()
  sched$status_type_completed <- FALSE
  expect_error(team_season_summary("wbb", 2026, schedule = sched),
               "No completed games")
})

test_that("a missing required column fails loudly", {
  sched <- fake_schedule()
  sched$home_score <- NULL
  expect_error(team_season_summary("wbb", 2026, schedule = sched),
               "missing required column")
})


## --- recent form ----------------------------------------------

test_that("last-N uses the most recent completed games", {
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule(), last_n = 2)
  expect_true(all(s$last_n_games <= 2))
  expect_equal(s$last_n_record,
               paste0(s$last_n_wins, "-", s$last_n_games - s$last_n_wins))
  # AAA's last two by date are games 5 (away, 71-75 L) and 6 (home, 62-68 L)
  expect_equal(s$last_n_record[s$team == "AAA"], "0-2")
})

test_that("last-N shrinks when a team has played fewer than N", {
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule(), last_n = 99)
  expect_equal(s$last_n_games, s$games)
})


## --- minutes and pace -----------------------------------------

test_that("minutes include overtime", {
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule())
  # wbb regulation is 40 minutes; game 5 (BBB vs AAA) went one OT = 45
  aaa <- s[s$team == "AAA", ]
  expect_equal(aaa$games, 5L)
  # four regulation games plus game 5, which went one overtime
  expect_equal(aaa$minutes, 4 * 40 + 45)
  ccc <- s[s$team == "CCC", ]
  expect_equal(ccc$minutes, ccc$games * 40)   # no OT games for CCC
})

test_that("pace is NA without a profile rather than silently estimated", {
  # The card must never mix two possession definitions. No profile means
  # no pace, which the renderer shows as an em dash.
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule())
  expect_true(all(is.na(s$pace)))
  expect_true(all(is.na(s$possessions)))
})

test_that("pace uses the profile's own possession counts", {
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule(),
                           profile = fake_profile())
  aaa <- s[s$team == "AAA", ]
  expect_equal(aaa$possessions, 300)
  # 300 possessions over 205 minutes (4 x 40 + one OT game at 45), per 40
  expect_equal(aaa$pace, 300 / 205 * 40)
})

test_that("pace is possessions per regulation game, in any league", {
  # reg_minutes appears in BOTH the numerator (as the normaliser) and the
  # denominator (via each game's length), so they cancel: with no overtime
  # pace IS possessions per game, whatever the league's period length.
  # That is the property that makes the number comparable across leagues.
  sched <- fake_schedule()
  wbb <- team_season_summary("wbb", 2026, schedule = sched, profile = fake_profile())
  nba <- team_season_summary("nba", 2026, schedule = sched, profile = fake_profile())

  # CCC plays 3 games, none of them the overtime one
  ccc_w <- wbb[wbb$team == "CCC", ]
  expect_equal(ccc_w$pace, ccc_w$possessions / ccc_w$games)
  expect_equal(nba$pace[nba$team == "CCC"], ccc_w$pace, tolerance = 1e-9)

  # AAA played one overtime game, so its pace sits BELOW possessions per
  # game -- the extra five minutes bought extra possessions.
  aaa <- wbb[wbb$team == "AAA", ]
  expect_lt(aaa$pace, aaa$possessions / aaa$games)
})

test_that("a profile without the needed columns is rejected", {
  bad <- fake_profile()
  bad$off_n <- NULL
  expect_error(team_season_summary("wbb", 2026, schedule = fake_schedule(),
                                   profile = bad),
               "must have team, dimension and off_n")
})

test_that("possessions come from `context`, not the other dimensions", {
  # shot_zone counts ATTEMPTS and creation counts MADE FGs, so summing
  # either would not be a possession count.
  prof <- dplyr::bind_rows(
    fake_profile(),
    tibble::tibble(team = "AAA", dimension = "shot_zone",
                   category = "at_rim", off_n = 9999, off_freq = 1)
  )
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule(), profile = prof)
  expect_equal(s$possessions[s$team == "AAA"], 300)
})


## --- four factors ---------------------------------------------

# Team box matching fake_schedule(). Hand-pickable numbers so the four
# factors can be verified by arithmetic rather than by re-running the code.
fake_team_box <- function() {
  tibble::tibble(
    game_id = rep(as.character(1:6), each = 2),
    game_date = rep(as.Date("2026-01-01") + c(0, 2, 4, 6, 8, 10), each = 2),
    team_abbreviation = c("AAA","BBB", "BBB","CCC", "CCC","AAA",
                          "AAA","CCC", "BBB","AAA", "AAA","BBB"),
    team_score  = c(70,65, 60,58, 55,70, 80,70, 75,71, 62,68),
    field_goals_made      = rep(25, 12),
    field_goals_attempted = rep(60, 12),
    three_point_field_goals_made = rep(6, 12),
    free_throws_attempted = rep(20, 12),
    offensive_rebounds    = rep(10, 12),
    defensive_rebounds    = rep(30, 12),
    total_turnovers       = rep(14, 12)
  )
}

test_that("four factors are NA without a team box", {
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule())
  expect_true(all(is.na(s$efg)))
  expect_true(all(is.na(s$tov_pct)))
  expect_true(all(is.na(s$orb_pct)))
  expect_true(all(is.na(s$ft_rate)))
})

test_that("four factors use the conventional box definitions", {
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule(),
                           team_box = fake_team_box())
  a <- s[s$team == "AAA", ]
  # every row is identical, so per-team totals scale with games played (5)
  fga <- 60 * 5; fgm <- 25 * 5; fg3m <- 6 * 5
  fta <- 20 * 5; oreb <- 10 * 5; tov <- 14 * 5
  poss <- fga + 0.44 * fta + tov - oreb

  expect_equal(a$efg,     (fgm + 0.5 * fg3m) / fga)
  expect_equal(a$tov_pct, tov / poss)
  expect_equal(a$ft_rate, fta / fga)
  # ORB% needs the OPPONENT's defensive rebounds, not its own
  expect_equal(a$orb_pct, oreb / (oreb + 30 * 5))
})

test_that("TOV% uses the 0.44 estimator, deliberately differing from pace", {
  # The card labels both because they do NOT share a denominator: pace
  # comes from counted possessions, TOV% from the box estimator.
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule(),
                           team_box = fake_team_box(), profile = fake_profile())
  a <- s[s$team == "AAA", ]
  box_poss <- (60 + 0.44 * 20 + 14 - 10) * 5     # 336
  expect_equal(a$tov_pct, (14 * 5) / box_poss)
  # counted possessions from the profile are a different number entirely
  expect_equal(a$possessions, 300)
  expect_false(isTRUE(all.equal(a$possessions, box_poss)))
})

test_that("a team box missing columns fails loudly", {
  tb <- fake_team_box(); tb$total_turnovers <- NULL
  expect_error(team_season_summary("wbb", 2026, schedule = fake_schedule(),
                                   team_box = tb),
               "missing required column")
})


## --- as-of cutoff ---------------------------------------------

test_that("as_of truncates the schedule", {
  # Games run Jan 1 to Jan 11 every two days; cutting at Jan 5 keeps 3.
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule(),
                           as_of = "2026-01-05")
  expect_equal(sum(s$games), 6L)          # 3 games x 2 teams
  expect_equal(unique(s$as_of), as.Date("2026-01-05"))
})

test_that("as_of truncates the team box too, not just the schedule", {
  full <- team_season_summary("wbb", 2026, schedule = fake_schedule(),
                              team_box = fake_team_box())
  cut  <- team_season_summary("wbb", 2026, schedule = fake_schedule(),
                              team_box = fake_team_box(), as_of = "2026-01-05")
  # identical per-game rows, so the RATES match but the volume must not
  expect_equal(full$efg[full$team == "AAA"], cut$efg[cut$team == "AAA"])
  expect_lt(sum(cut$games), sum(full$games))
})

test_that("as_of before the first game is an error", {
  expect_error(team_season_summary("wbb", 2026, schedule = fake_schedule(),
                                   as_of = "2025-12-01"),
               "No completed games on or before")
})

test_that("as_of with a profile refuses to report pace", {
  # The profile is a fixed artefact covering its own full window. Dividing
  # full-window possessions by truncated minutes would inflate pace, so it
  # returns NA and warns rather than printing a wrong number on a card.
  expect_warning(
    s <- team_season_summary("wbb", 2026, schedule = fake_schedule(),
                             profile = fake_profile(), as_of = "2026-01-05"),
    "pace")
  expect_true(all(is.na(s$pace)))
  expect_true(all(is.na(s$possessions)))
})


## --- header stats ---------------------------------------------

test_that("team_header_stats() drops what it cannot compute", {
  # NULL (not NA) so the renderer's %||% prints an em dash. An "NA" on a
  # scouting card reads as a data error; a dash reads as "not available".
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule())
  h <- team_header_stats(s, "AAA")
  expect_type(h$record, "character")
  expect_null(h$pace)                       # no profile -> no pace
  expect_equal(h$last_5, "3-2")   # default last_n = 5, AAA played 5
  expect_false(any(vapply(h, function(x) isTRUE(is.na(x)), logical(1))))
})

test_that("team_header_stats() passes four factors as proportions", {
  # NOT preformatted strings: the renderer sets precision. Passing "51.5%"
  # here would hard-code the card's formatting into the data layer.
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule(),
                           team_box = fake_team_box())
  h <- team_header_stats(s, "AAA")
  expect_type(h$efg, "double")
  expect_true(h$efg > 0 && h$efg < 1)
  expect_type(h$tov_pct, "double")
  expect_type(h$orb_pct, "double")
  expect_type(h$ft_rate, "double")
})

test_that("team_header_stats() omits four factors when there is no box", {
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule())
  h <- team_header_stats(s, "AAA")
  expect_null(h$efg)
  expect_null(h$tov_pct)
})

test_that("team_header_stats() formats pace and margin for display", {
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule(),
                           profile = fake_profile())
  h <- team_header_stats(s, "AAA")
  expect_match(h$pace, "^[0-9]+\\.[0-9]$")
  expect_match(h$margin, "^[+-][0-9]+\\.[0-9]$")
})

test_that("team_header_stats() errors on an unknown team", {
  s <- team_season_summary("wbb", 2026, schedule = fake_schedule())
  expect_error(team_header_stats(s, "ZZZ"), "No season summary row")
})


## --- against the real schedule --------------------------------

test_that("the Mountain West summary reconciles with the cached profile", {
  skip_if_offline()
  prof_csv <- file.path("..", "..", "data", "tidy", "wbb", "profiles",
                        "mountain_west_2026_play_profile.csv")
  skip_if_not(file.exists(prof_csv), "cached profile not present")
  prof <- readr::read_csv(prof_csv, show_col_types = FALSE, progress = FALSE)

  s <- team_season_summary("wbb", 2026, teams = sort(unique(prof$team)),
                           profile = prof)

  expect_equal(nrow(s), 12L)
  expect_true(all(s$wins + s$losses == s$games))
  # every team's possessions must match the profile's context total
  ctx <- prof |>
    dplyr::filter(dimension == "context") |>
    dplyr::group_by(team) |>
    dplyr::summarise(p = sum(off_n), .groups = "drop")
  chk <- dplyr::inner_join(s, ctx, by = "team")
  expect_equal(chk$possessions, chk$p)
  # WBB pace lives in a believable band
  expect_true(all(s$pace > 55 & s$pace < 85))
  # and the margin identity holds
  expect_equal(s$margin, s$pts_per_game - s$opp_pts_per_game)
})
