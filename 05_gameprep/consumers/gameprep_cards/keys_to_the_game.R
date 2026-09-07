## ============================================================
## keys_to_the_game.R — selection + text for the pregame card
## ------------------------------------------------------------
## Consumes attack_index_card() output. Computes nothing about the
## game: every number here is already in Table 2 of
## SPEC_play_profile.md, and this file only decides which rows are
## worth saying out loud and how to say them.
##
## THREE SIDES. The card as currently scoped uses the first two —
## it is a scouting report on the opponent. The third is built and
## tested but off by default; "what we should do about it" is a
## later card.
##
##   "strength"  what the opponent does well and often.
##               Card is attack_index_card(profile, them, us).
##               Ranks on the opponent's OWN offensive percentile.
##               Our defense is not consulted. Carries a phrase.
##
##   "weakness"  what the opponent struggles at.
##               Same card, low end of the same percentile.
##               CARRIES A PHRASE as of 2026-09-03. It deliberately did
##               not: "the finding is the number, and the response
##               belongs to a later card". The coaching-first redesign
##               requires every analytic to map to an implication, so
##               weakness phrases now exist and are instructions to OUR
##               team. Set them empty in the config to revert.
##
##   "attack"    where we should go at them. OFF BY DEFAULT.
##               Card is attack_index_card(profile, us, them).
##               Ranks on the opponent's DEFENSIVE percentile.
##
## Strength and attack were conflated in the first draft (attack-side
## selection paired with defensive phrasing), which produced bullets
## telling a team to defend a shot it was about to take. The `side`
## column in config/tactical_phrases.csv is what keeps them apart.
##
## The tactic itself NEVER originates here — it comes from
## config/tactical_phrases.csv. A row with no phrase produces no
## bullet rather than an invented one.
##
## Depends on: paths.R (card_config_dir), team_play_profile.R, dplyr, readr
## ============================================================

# A row is extreme enough to mention outside these bounds. Every ranking
# percentile is oriented high = the team is good at it, so strengths sit
# above the upper bound and weaknesses below the lower.
KEY_STRENGTH_PERCENTILE_MIN <- 70
KEY_WEAKNESS_PERCENTILE_MAX <- 30

# Below this many possessions/attempts the row is too thin to coach off.
#
# INERT ON espn_derived DATA, deliberately kept. build_team_play_profile()
# already NAs any percentile computed on fewer than MIN_POSS_FOR_PERCENTILE
# rows, so nothing that reaches here can fail this test — measured at 0
# rows cut across all 264 Mountain West side-cards. It exists for
# finer-grained sources like Synergy, where 11 play types split the same
# possessions and thin categories are real.
KEY_MIN_POSS <- 50

# Never more than this many bullets per side, however many rows qualify.
KEY_MAX_BULLETS <- 3


## --- 1. display direction ------------------------------------

## Display primitives (ordinal, rank_from_percentile,
## format_inverted_standing, display_def_percentile, UNIT_PHRASE,
## UNIT_DENOMINATOR) MOVED to R/profile_display.R on 2026-09-03, so the
## dashboard and the Q&A agent render rows by the same rules rather than
## re-deriving them. Do not redefine them here: this file is sourced
## after the library, so a copy left behind would silently shadow it.


## --- 2. config ------------------------------------------------

## The three config loaders (load_category_display,
## load_tactical_phrases, load_dimension_headers) MOVED to
## R/config_tables.R on 2026-09-03 — the dashboard and the Q&A agent
## read them too, and a library cannot depend on a consumer. Do not
## redefine them here: this file is sourced after the library.


## --- 3. selection ---------------------------------------------

#' The percentile column each side ranks on
side_rank_column <- function(side) {
  switch(side,
    strength = "ppp_percentile",       # opponent's own offence
    weakness = "ppp_percentile",       # same column, low end
    attack   = "def_ppp_percentile",   # opponent's defensive weakness
    stop("Unknown side '", side,
         "' — expected \"strength\", \"weakness\" or \"attack\".", call. = FALSE)
  )
}

#' Restrict a card to rows eligible to become bullets
bullet_pool <- function(card, display = load_category_display()) {
  keep <- display[display$bulletable, c("source", "dimension", "category")]
  dplyr::semi_join(card, keep, by = c("source", "dimension", "category"))
}

#' Pick the rows that become bullets on one side of the card
#'
#' Two mechanisms doing two different jobs, and both are needed:
#'
#'   the GATE (percentile bound) answers "is this worth mentioning at
#'   all". It is what lets a card come back with one bullet or none —
#'   measured at 42% of Mountain West side-cards under 2 bullets, 21% at
#'   zero — and it stops the frequency term below from dragging in a
#'   high-volume row the team is unremarkable at. It cannot rank: a
#'   12-team population gives the percentile only 12 distinct values, so
#'   ties are everywhere.
#'
#'   the SCORE (frequency x percentile) answers "of those, which first".
#'   Volume is what makes a coaching point worth a bullet. It cannot
#'   qualify: left alone it always returns three rows, however
#'   unremarkable, which is the padding we explicitly do not want.
#'
#' Frequency is only comparable WITHIN a dimension — it sums to 1 there,
#' so a 3-category dimension has mechanically larger values than a
#' 5-category one. bullet_pool() removes the residual buckets that make
#' this bite; the remaining spread is small enough to rank across.
select_keys <- function(card,
                        side        = c("strength", "weakness", "attack"),
                        percentile  = NULL,
                        min_poss    = KEY_MIN_POSS,
                        max_bullets = KEY_MAX_BULLETS,
                        display     = load_category_display()) {
  side     <- match.arg(side)
  rank_col <- side_rank_column(side)

  pool <- bullet_pool(card, display)
  pool <- pool[!is.na(pool[[rank_col]]) & pool$poss >= min_poss, ]

  # RELATIVE frequency, within dimension. Raw `freq` is not comparable
  # across dimensions and using it directly buried the context rows.
  #
  # Measured over all 132 Mountain West matchups with raw freq: 83% of
  # strength bullets and 82% of weakness bullets were shot_zone. That is
  # arithmetic, not importance. After bullet_pool() drops `halfcourt`,
  # the two surviving context categories hold only 0.206 of the frequency
  # mass while the five shot_zone categories hold 1.000 -- so a context
  # row tops out near 0.10 * 100 = 10 where a shot_zone row reaches
  # 0.28 * 92 = 26. A context row essentially could not win a slot.
  #
  # On SDSU's card that hid the sharpest point available: second chance,
  # 1st of 12 in the conference, lost to three shot_zone rows.
  #
  # Dividing by the dimension's own maximum makes the term read "how
  # often, FOR THIS KIND OF THING" -- 1.0 is the most frequent category
  # in its dimension whatever the dimension's absolute scale.
  #
  # Normalised BEFORE the percentile gate, deliberately: normalising
  # after would hand freq_rel = 1 to any dimension with a single
  # qualifying row and over-promote it for being alone.
  pool <- pool |>
    dplyr::group_by(dimension) |>
    dplyr::mutate(
      freq_rel = {
        mx <- suppressWarnings(max(freq, na.rm = TRUE))
        if (!is.finite(mx) || mx <= 0) 0 else freq / mx
      }
    ) |>
    dplyr::ungroup()

  if (side == "weakness") {
    bound <- percentile %||% KEY_WEAKNESS_PERCENTILE_MAX
    pool  <- pool[pool[[rank_col]] < bound, ]
    # Furthest below the bound, weighted by how often they go there.
    pool$key_score <- pool$freq_rel * (100 - pool[[rank_col]])
  } else {
    bound <- percentile %||% KEY_STRENGTH_PERCENTILE_MIN
    pool  <- pool[pool[[rank_col]] > bound, ]
    pool$key_score <- pool$freq_rel * pool[[rank_col]]
  }

  pool |>
    dplyr::arrange(dplyr::desc(key_score)) |>
    utils::head(max_bullets)
}

`%||%` <- function(a, b) if (is.null(a)) b else a


## --- 4. text --------------------------------------------------



## --- standing: rank or percentile -----------------------------

# Above this population size a percentile is meaningful and a rank is
# not ("214th of 363" tells a coach nothing). At or below it, the
# reverse. Conferences run 8-20 teams; national WBB is 363.
RANK_DISPLAY_MAX_POPULATION <- 50

#' Population size behind a profile's percentiles
population_size <- function(profile) length(unique(profile$team))


#' How a row's standing reads on the card
#'
#' `mode = "auto"` picks rank for a conference-sized population and
#' percentile for a national one — the display follows the population,
#' which is the only way "0th percentile" stops meaning "12th of 12".
#'
#' Both readings are always available; this only decides which one prints.
#' `aspect` places the offensively/defensively qualifier where it reads.
#' A rank already carries its population ("2nd of 12 in the Mountain West"),
#' so the adverb only earns its place when the subject switches sides.
format_standing <- function(pctl, n, population,
                            mode   = c("auto", "rank", "percentile"),
                            aspect = c("offense", "defense")) {
  mode   <- match.arg(mode)
  aspect <- match.arg(aspect)
  if (mode == "auto") {
    mode <- if (n <= RANK_DISPLAY_MAX_POPULATION) "rank" else "percentile"
  }
  if (is.na(pctl)) return("unranked")

  if (mode == "rank") {
    r <- paste0(ordinal(rank_from_percentile(pctl, n)), " of ", n)
    if (aspect == "defense") r <- paste0(r, " defensively")
    paste0(r, " in the ", population)
  } else {
    paste0(ordinal(pctl), " percentile ",
           if (aspect == "defense") "defensively" else "offensively")
  }
}

#' Assemble one bullet from a selected row
#'
#' Deterministic template. Everything in the output sentence is either a
#' number off the row or a string from config — nothing is generated, so
#' the sentence cannot claim a tactic the phrase table does not contain.
#' If a narrow LLM pass takes over the wording later, this is the single
#' function it replaces, and its inputs are the contract: the phrase plus
#' the row's own numbers, nothing else.
#'
#' The three sides cite different numbers. Strength and weakness quote the
#' opponent's own frequency and offensive percentile and never touch our
#' defense. Attack quotes our frequency and the opponent's defensive
#' percentile, flipped for display.
#'
#' Weakness bullets carry no tactical clause by design — the finding is
#' the number, and the response belongs to a later card.
#'
#' No gendered language: the phrase table uses role nouns ("the
#' ballhandler", "the shooter", "the cutter") and this template adds only
#' team names and numbers.
compose_key_sentence <- function(row, opponent,
                                 side       = c("strength", "weakness", "attack"),
                                 population = "the conference",
                                 pop_n      = 12L,
                                 standing   = "auto",
                                 phrases    = load_tactical_phrases(),
                                 display    = load_category_display(),
                                 headers    = load_dimension_headers()) {
  side <- match.arg(side)

  # Some dimensions have a degenerate PPP. `ball_security` is the first: a
  # turnover scores zero by definition, so every row would read "at 0.00
  # points per possession", which is arithmetic, not a finding. Those rows
  # state the RATE only. The flag lives in dimension_headers.csv so adding
  # such a dimension later needs no change here.
  ppp_ok <- TRUE
  if ("ppp_meaningful" %in% names(headers)) {
    f <- headers$ppp_meaningful[headers$source == row$source &
                                headers$dimension == row$dimension]
    if (length(f)) ppp_ok <- isTRUE(as.logical(f[1]))
  }

  lab <- display[
    display$source    == row$source &
    display$dimension == row$dimension &
    display$category  == row$category, "display_label", drop = TRUE]
  if (length(lab) == 0) return(NA_character_)

  unit_of  <- UNIT_DENOMINATOR[[row$unit]]
  unit_ppp <- UNIT_PHRASE[[row$unit]]
  stand    <- function(p, a = "offense") format_standing(p, pop_n, population, standing, a)

  # Weakness now carries a phrase too; a MISSING one still yields no
  # bullet rather than an invented tactic, same contract as the other sides.
  if (side == "weakness") {
    wp <- phrases[
      phrases$side      == "weakness" &
      phrases$source    == row$source &
      phrases$dimension == row$dimension &
      phrases$category  == row$category, "phrase", drop = TRUE]
    tail_txt <- if (length(wp) && nzchar(wp[1])) paste0(" — ", wp[1], ".") else "."
    return(if (ppp_ok) paste0(
      lab[1], " is ", round(row$freq * 100), "% ", unit_of,
      " for ", opponent, " at only ", sprintf("%.2f", row$ppp), " ", unit_ppp,
      ", ", stand(row$ppp_percentile), tail_txt
    ) else paste0(
      lab[1], " account for ", round(row$freq * 100), "% ", unit_of,
      " for ", opponent, ", ",
      profile_row_standing(row$ppp_percentile, pop_n, population,
                           row$dimension, row$category), tail_txt
    ))
  }

  phrase <- phrases[
    phrases$side      == side &
    phrases$source    == row$source &
    phrases$dimension == row$dimension &
    phrases$category  == row$category, "phrase", drop = TRUE]
  if (length(phrase) == 0) return(NA_character_)

  if (side == "attack") {
    paste0(
      lab[1], " is ", round(row$freq * 100), "% ", unit_of, " for us at ",
      sprintf("%.2f", row$ppp), " ", unit_ppp,
      "; ", opponent, " allows ", sprintf("%.2f", row$def_ppp),
      " there, ", stand(display_def_percentile(row$def_ppp_percentile), "defense"),
      " — ", phrase[1], "."
    )
  } else if (ppp_ok) {
    paste0(
      lab[1], " is ", round(row$freq * 100), "% ", unit_of,
      " for ", opponent, " at ", sprintf("%.2f", row$ppp), " ", unit_ppp,
      ", ", stand(row$ppp_percentile), " — ", phrase[1], "."
    )
  } else {
    paste0(
      lab[1], " account for ", round(row$freq * 100), "% ", unit_of,
      " for ", opponent, ", ",
      profile_row_standing(row$ppp_percentile, pop_n, population,
                          row$dimension, row$category),
      " — ", phrase[1], "."
    )
  }
}

#' Bullets for one side, in display order
keys_for_side <- function(card, opponent, side,
                          population = "the conference",
                          pop_n      = 12L,
                          standing   = "auto",
                          phrases    = load_tactical_phrases(),
                          display    = load_category_display(),
                          headers    = load_dimension_headers(), ...) {
  sel <- select_keys(card, side = side, display = display, ...)
  if (nrow(sel) == 0) return(character(0))
  out <- vapply(seq_len(nrow(sel)),
                function(i) compose_key_sentence(sel[i, ], opponent, side,
                                                 population, pop_n, standing,
                                                 phrases, display, headers),
                character(1))
  out[!is.na(out)]
}


## --- 5. the whole card ----------------------------------------

# Section headings, kept here rather than in the renderer so the sides
# stay labelled consistently whether they land on one card or several.
SIDE_HEADING <- c(
  strength = "What they do well",
  weakness = "Where they struggle",
  attack   = "Where to attack"
)

#' Build every requested side for one matchup
#'
#' Returns a named list, one element per side, each with the selected rows
#' and their bullets. The renderer decides whether that list becomes
#' sections of one card or several separate cards — this function is
#' indifferent, which is the point: the layout question stays open without
#' forking the data path.
#'
#' Defaults to the opponent scouting report. Pass
#' `sides = c("strength", "weakness", "attack")` to include our own
#' offense once that card exists.
build_keys <- function(profile, team, opponent,
                       sides    = c("strength", "weakness"),
                       standing = c("auto", "rank", "percentile"),
                       phrases  = load_tactical_phrases(),
                       display  = load_category_display(), ...) {
  standing <- match.arg(standing)
  opp_card <- attack_index_card(profile, opponent, team)
  our_card <- if ("attack" %in% sides) attack_index_card(profile, team, opponent)

  pop_n <- population_size(profile)
  pop   <- profile$population[1]

  out <- stats::setNames(lapply(sides, function(s) {
    card <- if (s == "attack") our_card else opp_card
    list(
      side    = s,
      heading = unname(SIDE_HEADING[s]),
      rows    = select_keys(card, side = s, display = display, ...),
      bullets = keys_for_side(card, opponent, s, pop, pop_n, standing,
                              phrases, display, ...)
    )
  }), sides)

  # The population must be printed on the card — SPEC_play_profile.md is
  # explicit, and "12th of 12" is meaningless without knowing of what.
  attr(out, "population")   <- pop
  attr(out, "population_n") <- pop_n
  attr(out, "standing")     <- if (standing == "auto") {
    if (pop_n <= RANK_DISPLAY_MAX_POPULATION) "rank" else "percentile"
  } else standing
  out
}
