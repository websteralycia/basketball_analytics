## ============================================================
## shot_zones.R — court geometry and shot-zone classification
## ------------------------------------------------------------
## ESPN coordinate system, VALIDATED 2026-07-30 against 228 WBB shots
## whose play text states a distance (corr +0.998, mean error 0.3 ft):
##
##   origin       = centre court
##   coordinate_x = along the length of the floor
##   coordinate_y = across the width
##   baskets      = (+41.75, 0) and (-41.75, 0)
##
## 41.75 = 47 ft (half of a 94 ft court) less 5.25 ft from baseline to
## the centre of the hoop.
##
## Two competing hypotheses were tested and rejected: coordinates being
## already basket-relative (corr -0.363) and axes swapped (-0.885).
## Either would have produced a plausible-looking but wrong zone map.
## ============================================================

# Court constants, in feet.
BASKET_X       <- 41.75   # |x| of each hoop centre
LANE_HALF_W    <- 6       # NCAA lane is 12 ft wide
FT_LINE_X      <- 28      # free-throw line: 47 - 19 ft from baseline
AT_RIM_FT      <- 4       # "at the rim" radius

# A corner three has to be BOTH wide and near the baseline. NCAA women's
# uses a uniform 22'1.75" arc, so unlike the NBA there is no flat corner
# segment to read the definition off of -- it is a stated convention.
#
# Width alone is not enough: |y| >= 19 on a ~23 ft arc admits everything
# within ~34 degrees of the baseline, a 68-degree wedge, which measured
# 41.7% of all 3PA across a 400-game 2026 sample. Real corner rates run
# 20-25%. Requiring |x| >= 34 (within 13 ft of the baseline) as well
# brings it to 21.8%.
CORNER_Y       <- 21      # |y| at least this wide, AND
CORNER_X       <- 34      # |x| at least this close to the baseline

#' Distance from each shot to the nearer basket, in feet
#'
#' Vectorised. Teams shoot at both hoops over the course of a game and
#' ESPN does not normalise to one half, so distance is the minimum over
#' the two baskets.
shot_distance <- function(x, y) {
  x <- as.numeric(x); y <- as.numeric(y)
  pmin(sqrt((x - BASKET_X)^2 + y^2),
       sqrt((x + BASKET_X)^2 + y^2))
}

#' Is this shot inside the painted area?
#'
#' The lane runs from the baseline to the free-throw line, so "in the
#' paint" means within the lane's width AND beyond the free-throw line
#' toward whichever basket the shot is nearer.
in_paint <- function(x, y) {
  x <- as.numeric(x); y <- as.numeric(y)
  !is.na(x) & !is.na(y) & abs(y) <= LANE_HALF_W & abs(x) >= FT_LINE_X
}

#' Classify a shot into a scoring zone
#'
#' @param x,y      ESPN coordinates.
#' @param is_three Logical, whether the attempt was a 3-pointer. Taken
#'   from score_value / play text rather than inferred from geometry —
#'   the arc is close enough to the coordinate quantisation that
#'   geometric inference misclassifies shots near the line.
#'
#' @return character vector of
#'   at_rim / paint / mid_range / corner_3 / above_break_3, NA where
#'   coordinates are missing.
classify_shot_zone <- function(x, y, is_three) {
  x <- as.numeric(x); y <- as.numeric(y)
  is_three <- as.logical(is_three)

  d   <- shot_distance(x, y)
  out <- rep(NA_character_, length(x))
  ok  <- !is.na(x) & !is.na(y)

  three <- ok & !is.na(is_three) & is_three
  two   <- ok & !three

  # Threes split by corner vs above the break. Corner requires width AND
  # proximity to the baseline -- see CORNER_Y / CORNER_X above.
  corner <- three & abs(y) >= CORNER_Y & abs(x) >= CORNER_X
  out[corner]          <- "corner_3"
  out[three & !corner] <- "above_break_3"

  # Twos: rim, then the rest of the paint, then everything else.
  out[two & d <= AT_RIM_FT]                      <- "at_rim"
  out[two & d >  AT_RIM_FT & in_paint(x, y)]     <- "paint"
  out[two & d >  AT_RIM_FT & !in_paint(x, y)]    <- "mid_range"

  out
}

#' The zone levels, in the order a card should display them
shot_zone_levels <- function() {
  c("at_rim", "paint", "mid_range", "corner_3", "above_break_3")
}
