## ============================================================
## test-shot_zones.R
## ------------------------------------------------------------
## Geometry tests run offline. The validation test re-checks the
## coordinate-system claim against live data and is skipped offline.
## ============================================================

test_that("distance is measured to the nearer basket", {
  # dead under each hoop
  expect_equal(shot_distance( 41.75, 0), 0)
  expect_equal(shot_distance(-41.75, 0), 0)
  # 10 ft straight out from the right-hand hoop
  expect_equal(shot_distance(31.75, 0), 10)
  # symmetric: the same shot at the other end
  expect_equal(shot_distance(-31.75, 0), 10)
  # centre court is 41.75 ft from both
  expect_equal(shot_distance(0, 0), 41.75)
})

test_that("the paint is bounded by the lane width and the free-throw line", {
  expect_true(in_paint(38, 0))     # in the lane, near the hoop
  expect_true(in_paint(28, 6))     # on the free-throw line, lane edge
  expect_false(in_paint(38, 7))    # just outside the lane width
  expect_false(in_paint(27, 0))    # beyond the free-throw line
  expect_false(in_paint(0, 0))     # centre court
})

test_that("shot zones classify correctly", {
  # at the rim
  expect_equal(classify_shot_zone(39.75, 0, FALSE), "at_rim")
  # in the lane but outside 4 ft
  expect_equal(classify_shot_zone(31.75, 2, FALSE), "paint")
  # a two outside the lane
  expect_equal(classify_shot_zone(30, 15, FALSE), "mid_range")
  # threes split by corner
  expect_equal(classify_shot_zone(41.75, 22, TRUE), "corner_3")
  expect_equal(classify_shot_zone(20, 0, TRUE), "above_break_3")
})

test_that("a corner three must be near the baseline, not merely wide", {
  # Regression guard. The rule was once |y| >= 19 with no constraint on
  # x, which tagged wing threes as corners and put 41.7% of all 3PA in
  # the corner bucket against a real rate of 20-25%.
  expect_equal(classify_shot_zone(41.75, 22, TRUE), "corner_3")   # true corner
  expect_equal(classify_shot_zone(34,    21, TRUE), "corner_3")   # on both cuts
  expect_equal(classify_shot_zone(25,    22, TRUE), "above_break_3")  # wide but high
  expect_equal(classify_shot_zone(41.75, 20, TRUE), "above_break_3")  # deep but narrow
  # symmetric at the far end of the floor
  expect_equal(classify_shot_zone(-41.75, -22, TRUE), "corner_3")
  expect_equal(classify_shot_zone(-25,    -22, TRUE), "above_break_3")
})

test_that("both ends of the floor classify the same", {
  expect_equal(classify_shot_zone( 39.75, 0, FALSE),
               classify_shot_zone(-39.75, 0, FALSE))
  expect_equal(classify_shot_zone( 30, 15, FALSE),
               classify_shot_zone(-30, 15, FALSE))
})

test_that("missing coordinates give NA, not a guess", {
  expect_true(is.na(classify_shot_zone(NA, 0, FALSE)))
  expect_true(is.na(classify_shot_zone(30, NA, FALSE)))
})

test_that("classification is vectorised", {
  z <- classify_shot_zone(c(39.75, 30, 41.75), c(0, 15, 22), c(FALSE, FALSE, TRUE))
  expect_equal(z, c("at_rim", "mid_range", "corner_3"))
})

test_that("every zone level is reachable", {
  z <- classify_shot_zone(
    x        = c(39.75, 31.75, 30,    41.75, 20),
    y        = c(0,     2,     15,    22,    0),
    is_three = c(FALSE, FALSE, FALSE, TRUE,  TRUE)
  )
  expect_setequal(z, shot_zone_levels())
})

test_that("computed distance matches ESPN's stated distance on real shots", {
  skip_if_offline()
  # The coordinate-system claim this whole file rests on. If ESPN ever
  # changes its origin or units, this fails loudly instead of silently
  # producing a wrong zone map.
  suppressMessages(library(wehoop))
  pbp <- wehoop::espn_wbb_pbp(game_id = "401851531")
  sh <- pbp[grepl("[0-9]+-foot", pbp$text) &
            !is.na(pbp$coordinate_x) & !is.na(pbp$coordinate_y), ]
  skip_if(nrow(sh) < 20, "too few shots with a stated distance")

  stated   <- as.numeric(sub(".*?([0-9]+)-foot.*", "\\1", sh$text))
  computed <- shot_distance(sh$coordinate_x, sh$coordinate_y)

  expect_gt(cor(computed, stated, use = "complete.obs"), 0.95)
  expect_lt(mean(abs(computed - stated), na.rm = TRUE), 2)
})
