## ============================================================
## test-possessions.R
## ------------------------------------------------------------
## Reconciliation tests hit the ESPN API and skip when offline.
## ============================================================

POSS_COLS <- c("game_id", "period", "poss_id", "off_team", "def_team",
               "clock_start", "clock_end", "seconds", "points",
               "context", "start_reason", "end_reason", "had_oreb",
               "start_idx", "end_idx")

# Games chosen for the edge cases each exposed while building this:
#   401851531  period opens with subs + a foul before the first ball event
#   401817374  free throws of one trip separated by four substitutions
#   401827253  free-throw text carries no "N of M" trip counter
POSS_GAMES <- c("401851531", "401817374", "401827253")

test_that("constants are sane", {
  expect_true(TRANSITION_SECONDS > 0 && TRANSITION_SECONDS < 24)
  expect_setequal(context_levels(), c("transition", "second_chance", "halfcourt"))
})

test_that("output matches the documented schema", {
  skip_if_offline()
  ps <- classify_possessions(wehoop::espn_wbb_pbp(game_id = POSS_GAMES[1]), "wbb")
  expect_named(ps, POSS_COLS)
  expect_type(ps$points, "integer")
  expect_type(ps$seconds, "double")
  expect_type(ps$had_oreb, "logical")
})

test_that("start_idx/end_idx address the ordered play-by-play", {
  skip_if_offline()
  for (gid in POSS_GAMES) {
    pbp <- wehoop::espn_wbb_pbp(game_id = gid)
    n   <- nrow(pbp)
    ps  <- classify_possessions(pbp, "wbb")

    # In range, and every possession spans at least one event.
    expect_true(all(ps$start_idx >= 1L & ps$start_idx <= n))
    expect_true(all(ps$end_idx   >= 1L & ps$end_idx   <= n))
    expect_true(all(ps$end_idx >= ps$start_idx))

    # Monotonic: possessions walk forward through the feed and never
    # revisit. This is what makes the indices safe to slice with.
    expect_false(is.unsorted(ps$start_idx))
    expect_false(is.unsorted(ps$end_idx))

    # The index must agree with the clock the walker recorded, or the
    # join it exists to enable would attach the wrong game state.
    ord <- order_pbp(pbp)
    expect_equal(seconds_to_clock(round(clock_to_seconds(
      ord$clock_display_value[ps$start_idx]))), ps$clock_start)
  }
})

test_that("context is exhaustive and mutually exclusive", {
  skip_if_offline()
  for (gid in POSS_GAMES) {
    ps <- classify_possessions(wehoop::espn_wbb_pbp(game_id = gid), "wbb")
    expect_true(all(ps$context %in% context_levels()), info = gid)
    expect_false(any(is.na(ps$context)), info = gid)
    # one row per possession means freq within the dimension sums to 1
    expect_equal(sum(prop.table(table(ps$context))), 1)
  }
})

test_that("possessions alternate sensibly and are numbered contiguously", {
  skip_if_offline()
  ps <- classify_possessions(wehoop::espn_wbb_pbp(game_id = POSS_GAMES[1]), "wbb")
  expect_equal(ps$poss_id, seq_len(nrow(ps)))
  expect_equal(dplyr::n_distinct(ps$off_team), 2)
  # neither team should own a wildly lopsided share of possessions
  share <- prop.table(table(ps$off_team))
  expect_true(all(share > 0.35 & share < 0.65))
})

test_that("second chance requires an offensive rebound", {
  skip_if_offline()
  ps <- classify_possessions(wehoop::espn_wbb_pbp(game_id = POSS_GAMES[1]), "wbb")
  expect_true(all(ps$had_oreb[ps$context == "second_chance"]))
  expect_false(any(ps$had_oreb[ps$context != "second_chance"]))
})

test_that("possession counts land near the 0.44 box estimate", {
  skip_if_offline()
  for (gid in POSS_GAMES) {
    ps  <- classify_possessions(wehoop::espn_wbb_pbp(game_id = gid), "wbb")
    box <- wehoop::espn_wbb_player_box(game_id = gid)
    est <- tapply(seq_len(nrow(box)), box$team_abbreviation, function(ix) {
      sum(box$field_goals_attempted[ix], na.rm = TRUE) +
        0.44 * sum(box$free_throws_attempted[ix], na.rm = TRUE) +
        sum(box$turnovers[ix], na.rm = TRUE) -
        sum(box$offensive_rebounds[ix], na.rm = TRUE)
    })
    got <- table(ps$off_team)
    for (tm in names(est)) {
      expect_lt(abs(as.numeric(got[[tm]]) - est[[tm]]) / est[[tm]], 0.15,
                label = paste(gid, tm))
    }
  }
})

test_that("points are very close to the box score", {
  skip_if_offline()
  # Not exact: a minority of team-games lose 2-3 points to rare
  # attribution edge cases, surfaced via the uncredited_points
  # attribute rather than silently dropped. Guard the magnitude so a
  # regression that loses real points is caught.
  for (gid in POSS_GAMES) {
    ps  <- classify_possessions(wehoop::espn_wbb_pbp(game_id = gid), "wbb")
    box <- wehoop::espn_wbb_player_box(game_id = gid)
    box_pts <- tapply(box$points, box$team_abbreviation, sum, na.rm = TRUE)
    got_pts <- tapply(ps$points, ps$off_team, sum)
    for (tm in names(box_pts)) {
      expect_lt(abs(as.numeric(got_pts[[tm]]) - box_pts[[tm]]), 5,
                label = paste(gid, tm))
    }
    expect_lt(attr(ps, "uncredited_points"), 8, label = gid)
  }
})

test_that("free throws split by substitutions stay in one trip", {
  skip_if_offline()
  # 401817374 has four substitutions between two attempts of one trip.
  # A naive adjacent-row lookahead ended the trip early and orphaned the
  # remaining attempts.
  ps <- classify_possessions(wehoop::espn_wbb_pbp(game_id = "401817374"), "wbb")
  expect_lt(attr(ps, "uncredited_points"), 8)
})

test_that("a period opening with substitutions does not hand over possession", {
  skip_if_offline()
  # 401851531 period 2 opens: subs, foul, then free throws by the OTHER
  # team. Opening a possession on the first team-tagged event gave the
  # ball to whoever substituted first and orphaned the free throws.
  ps <- classify_possessions(wehoop::espn_wbb_pbp(game_id = "401851531"), "wbb")
  expect_lt(attr(ps, "uncredited_points"), 5)
})

test_that("both leagues reconstruct", {
  skip_if_offline()
  wnba <- classify_possessions(wehoop::espn_wnba_pbp(game_id = "401857092"), "wnba")
  expect_named(wnba, POSS_COLS)
  expect_gt(nrow(wnba), 100)
})

test_that("an and-1 free throw does not split the opponent's possession", {
  skip_if_offline()
  # WBB 401851531, 6:45 in Q1: USU makes a layup (closing their own
  # possession and handing GCU the ball), is fouled, and makes the bonus
  # free throw eight substitutions later. That free throw must not end
  # GCU's possession — GCU has not touched the ball.
  ps <- classify_possessions(wehoop::espn_wbb_pbp(game_id = "401851531"), "wbb")

  # No possession may be closed by a free throw belonging to the defence.
  # The symptom that leaves behind is a 0-point, 0-second possession
  # immediately followed by another possession for the SAME offence.
  ft_closed <- which(ps$end_reason == "made_ft")
  phantom <- ft_closed[ft_closed < nrow(ps)]
  phantom <- phantom[ps$points[phantom] == 0 & ps$seconds[phantom] <= 1 &
                     ps$off_team[phantom] == ps$off_team[phantom + 1L]]
  expect_length(phantom, 0)
})

test_that("possessions almost always alternate within a period", {
  skip_if_offline()
  # Back-to-back possessions for one team are legitimate across a period
  # boundary (a team can open the next period too). WITHIN a period they
  # mean the walker has the wrong team on offence.
  #
  # The and-1 fixes took these three games from 7 such cases to 1. The
  # survivor is in 401817374 at 2:47 — an offensive rebound followed by a
  # made layup — and is NOT yet diagnosed. The bound is deliberately tight
  # so it fails the moment anything makes this worse, and should be
  # tightened to 0 when that case is understood.
  KNOWN_UNDIAGNOSED <- 1L
  total <- 0L
  for (gid in POSS_GAMES) {
    ps <- classify_possessions(wehoop::espn_wbb_pbp(game_id = gid), "wbb")
    same <- which(ps$off_team[-1] == ps$off_team[-nrow(ps)])
    total <- total + length(same[ps$period[same] == ps$period[same + 1L]])
  }
  expect_lte(total, KNOWN_UNDIAGNOSED)
})
