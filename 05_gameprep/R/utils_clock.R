## ============================================================
## utils_clock.R — game-clock conversion helpers
## ------------------------------------------------------------
## ESPN reports the clock as a countdown string ("10:00" -> "0:00")
## within each period. The tidy layer stores clocks as strings (to
## stay human-readable in the CSVs) but does all arithmetic in
## seconds remaining.
## ============================================================

#' Clock string -> seconds remaining in the period
#'
#' Vectorised. Handles BOTH formats ESPN emits:
#'   "10:00", "7:32", "0:04"  — minutes:seconds
#'   "58.9",  "42.9", "7.7"   — bare seconds, no colon, under one minute
#'
#' That second form is not an edge case: a typical game has 50+ of them,
#' and treating them as unparseable puts NA clocks on any stint boundary
#' inside the final minute of a period.
#'
#' Anything genuinely unparseable comes back NA rather than erroring, so
#' one malformed play cannot kill a whole game.
clock_to_seconds <- function(clock_str) {
  x   <- trimws(as.character(clock_str))
  out <- rep(NA_real_, length(x))

  has_colon <- !is.na(x) & grepl(":", x, fixed = TRUE)
  if (any(has_colon)) {
    parts <- stringr::str_split_fixed(x[has_colon], ":", 2)
    mins  <- suppressWarnings(as.numeric(parts[, 1]))
    secs  <- suppressWarnings(as.numeric(parts[, 2]))
    out[has_colon] <- mins * 60 + secs
  }

  bare <- !is.na(x) & !has_colon & nzchar(x)
  if (any(bare)) out[bare] <- suppressWarnings(as.numeric(x[bare]))

  out
}

#' Seconds remaining -> clock string
#'
#' Inverse of clock_to_seconds(), formatted "M:SS" to match ESPN's own
#' display (no leading zero on minutes). Sub-second values round to the
#' nearest second, so a stint boundary never prints as "0:00.4".
seconds_to_clock <- function(secs) {
  secs <- round(as.numeric(secs))
  out <- ifelse(
    is.na(secs), NA_character_,
    sprintf("%d:%02d", secs %/% 60, secs %% 60)
  )
  out
}
