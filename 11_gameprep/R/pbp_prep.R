## ============================================================
## pbp_prep.R — shared play-by-play normalisation
## ------------------------------------------------------------
## Anything that walks the play-by-play (lineup stints, possessions)
## MUST see events in the same order. Keeping that logic here rather
## than inline in each walker is the difference between one definition
## of "chronological" and two that quietly disagree.
## ============================================================

#' Put play-by-play events in true chronological order
#'
#' Sort key: period ASC, clock DESCENDING, sequence_number as tie-breaker.
#'
#' Neither available key works alone on real ESPN data:
#'
#' * **Clock alone** scrambles ties, and ties are everywhere — a made
#'   basket and the substitutions that follow it share a clock value.
#' * **`sequence_number` alone is not monotonic.** WBB game 401826610
#'   lists its periods as 1,2,3,4,2,4 in sequence order, and its clock
#'   runs backwards within periods 2 and 3. Walking that order makes a
#'   reconstruction re-enter a finished period and invent a full extra
#'   period of elapsed time.
#'
#' Clock-descending fixes the ordering; sequence_number still breaks
#' ties, which is the property that mattered originally. Events with no
#' parseable clock sort last within their period.
order_pbp <- function(pbp) {
  secs <- clock_to_seconds(pbp$clock_display_value)
  secs[is.na(secs)] <- -Inf
  seq_no <- if ("sequence_number" %in% names(pbp)) {
    suppressWarnings(as.numeric(pbp$sequence_number))
  } else {
    seq_len(nrow(pbp))
  }
  pbp[order(suppressWarnings(as.integer(pbp$period_number)), -secs, seq_no), ]
}

#' Coerce id columns to character
#'
#' Ids arrive as integer or double depending on whether arrow is
#' installed, and float ids silently break joins.
chr_ids <- function(df, cols = c("team_id", "athlete_id", "game_id",
                                 "athlete_id_1", "athlete_id_2", "athlete_id_3")) {
  for (cl in intersect(cols, names(df))) df[[cl]] <- as.character(df[[cl]])
  df
}

#' Was this event a made shot?
#'
#' ALWAYS use `scoring_play`, never the play text. ESPN emits two text
#' conventions that vary by game — `makes layup` in some, `made Layup.`
#' in others — and 3 of 8 sampled WBB games used the second, where
#' matching on "makes" finds nothing. `scoring_play` matched the box
#' score exactly in both styles.
is_made <- function(pbp) {
  if (!"scoring_play" %in% names(pbp)) return(rep(FALSE, nrow(pbp)))
  x <- pbp$scoring_play
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.numeric(x)) return(!is.na(x) & x == 1)
  !is.na(x) & tolower(as.character(x)) %in% c("true", "t", "yes")
}

#' Was this event a three-point attempt?
#'
#' Prefers `score_value` on makes; falls back to the text, which carries
#' "three point" in both of ESPN's conventions (case differs, so the
#' match is case-insensitive).
is_three_pt <- function(pbp) {
  txt <- if ("text" %in% names(pbp)) as.character(pbp$text) else rep(NA_character_, nrow(pbp))
  out <- !is.na(txt) & grepl("three point|3-point", txt, ignore.case = TRUE)
  if ("score_value" %in% names(pbp)) {
    sv <- suppressWarnings(as.numeric(pbp$score_value))
    out <- out | (!is.na(sv) & sv == 3)
  }
  out
}
