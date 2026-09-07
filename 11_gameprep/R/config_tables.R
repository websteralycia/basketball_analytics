## ============================================================
## config_tables.R — the coach-editable tables, read by every surface
## ------------------------------------------------------------
## These three CSVs are the project's editorial guardrails: which
## categories may be spoken about, how each is labelled, what a phrase
## says, and whether a dimension's rate means anything. They live under
## consumers/gameprep_cards/config/ because a coach authors them, not a
## build — but they are READ by the card, the dashboard and the Q&A
## agent alike.
##
## They were loaded from the card consumer until 2026-09-03, which made
## the library depend on a consumer the moment the query layer needed a
## display label. Moved down here; the card still calls the same names.
##
## Depends on: paths.R (card_config_dir)
## ============================================================

#' Read the category labels and bullet-eligibility flags
#'
#' `bulletable = FALSE` keeps a category on the attack-index table but out
#' of the bullet pool. Two kinds of row are excluded, both for reasons
#' recorded in the config's `exclusion_reason` column:
#'
#'   - residual buckets (`halfcourt`, Synergy's `miscellaneous`), which are
#'     large by construction and describe no tendency;
#'   - the `creation` pair, which is a share of MADE field goals rather
#'     than a rate. That number RISES as an offense gets worse: SJSU 2026
#'     is 0th percentile at the rim, 0th in the paint and 0th in halfcourt,
#'     yet 91st percentile "assisted". In the pool it would lead SJSU's
#'     card with a strength they do not have.
load_category_display <- function(path = file.path(card_config_dir(), "category_display.csv")) {
  if (!file.exists(path)) {
    stop("Category display config not found at ", path, call. = FALSE)
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

#' Read the coach-editable phrase table
#'
#' Keyed on side + source + dimension + category. Source is part of the
#' key because the same category name means different things across
#' sources (espn_derived `transition` is a possession context; Synergy
#' `transition` is a play type). `weakness` rows exist as of 2026-09-03
#' and are instructions to OUR team — see the header.
load_tactical_phrases <- function(path = file.path(card_config_dir(), "tactical_phrases.csv")) {
  if (!file.exists(path)) {
    stop("Tactical phrase config not found at ", path, call. = FALSE)
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

#' Read the per-dimension group headers
#'
#' Each dimension carries its own PPP definition because the three do not
#' share a denominator and must never share an axis or a column header.
load_dimension_headers <- function(path = file.path(card_config_dir(), "dimension_headers.csv")) {
  if (!file.exists(path)) {
    stop("Dimension header config not found at ", path, call. = FALSE)
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}
