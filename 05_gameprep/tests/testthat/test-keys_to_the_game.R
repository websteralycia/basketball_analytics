## ============================================================
## test-keys_to_the_game.R
## ------------------------------------------------------------
## First tests for the card layer. source_all.R loads R/ only, so the
## consumer is sourced here.
##
## Priority is the places where a silent error produces a card that
## LOOKS right and says something false. Three such traps are already
## documented in the project's own comments, and each gets a test:
##
##   1. the def_ppp_pctl flip. Stored attackable-oriented (high = worse
##      defence); every other percentile reads high = good. Flipping in
##      the wrong direction once produced bullets telling a team to
##      defend a shot it was about to take.
##   2. the `creation` inversion. It is a share of MADE field goals, so
##      it RISES as an offence gets worse -- SJSU 2026 is 0th percentile
##      at the rim and 91st "assisted". It must stay out of the pool.
##   3. weakness bullets carry a tactical clause AS OF 2026-09-03, and it
##      must come from config. This reversed -- the response used to be
##      deferred to a later card -- but the failure mode did not: an
##      invented clause, or one addressed to the wrong team.
## ============================================================

card_dir <- local({
  root <- Sys.getenv("GAMEPREP_ROOT", unset = "")
  if (!nzchar(root)) {
    for (d in c(".", "..", "../..", "../../..")) {
      if (file.exists(file.path(d, "source_all.R"))) { root <- normalizePath(d); break }
    }
  }
  if (!nzchar(root)) skip("Cannot locate the project root")
  file.path(root, "consumers", "gameprep_cards")
})

source(file.path(card_dir, "keys_to_the_game.R"))

PROFILE_CSV <- file.path(dirname(dirname(card_dir)),
                         "data", "tidy", "wbb", "profiles",
                         "mountain_west_2026_play_profile.csv")

# A card is one row per dimension x category for a matchup. This builder
# keeps the tests readable and independent of any live fetch.
fake_card <- function(...) {
  rows <- list(...)
  dplyr::bind_rows(lapply(rows, function(r) {
    tibble::tibble(
      dimension = r$dim, category = r$cat,
      unit = r$unit %||% "possession", source = "espn_derived",
      poss = r$poss %||% 200, freq = r$freq,
      ppp = r$ppp %||% 1.00, ppp_percentile = r$pctl,
      def_ppp = r$def_ppp %||% 1.00,
      def_ppp_percentile = r$def_pctl %||% 50L
    )
  }))
}


## --- pure helpers ---------------------------------------------

test_that("ordinal() handles the teens, which percentiles hit often", {
  expect_equal(ordinal(1), "1st")
  expect_equal(ordinal(2), "2nd")
  expect_equal(ordinal(3), "3rd")
  expect_equal(ordinal(4), "4th")
  # the %% 100 %in% 11:13 branch -- "73th" would otherwise appear on cards
  expect_equal(ordinal(11), "11th")
  expect_equal(ordinal(12), "12th")
  expect_equal(ordinal(13), "13th")
  expect_equal(ordinal(21), "21st")
  expect_equal(ordinal(73), "73rd")
  expect_equal(ordinal(111), "111th")
  expect_equal(ordinal(0), "0th")
})

test_that("rank_from_percentile() inverts the stored percentile", {
  n <- 12L
  # stored as 100 * (rank_asc - 1) / (n - 1): worst team is 0, best is 100
  expect_equal(rank_from_percentile(100, n), 1L)   # best
  expect_equal(rank_from_percentile(0,   n), n)    # worst
  # round-trips for every untied rank in a 12-team population
  for (rk in 1:n) {
    pctl <- 100 * (n - rk) / (n - 1)
    expect_equal(rank_from_percentile(pctl, n), as.integer(rk))
  }
  expect_true(is.na(rank_from_percentile(NA, n)))
})

test_that("display_def_percentile() flips defence to normal semantics", {
  # TRAP 1. Stored 100 = worst in the population at defending it, which
  # must render as 0. Getting this backwards inverts every attack bullet.
  expect_equal(display_def_percentile(100), 0L)
  expect_equal(display_def_percentile(0), 100L)
  expect_equal(display_def_percentile(30), 70L)
  expect_true(is.na(display_def_percentile(NA)))
  # involution: flipping twice returns the original
  x <- c(0, 17, 50, 83, 100)
  expect_equal(display_def_percentile(display_def_percentile(x)), as.integer(x))
})

test_that("side_rank_column() maps each side and rejects unknown ones", {
  expect_equal(side_rank_column("strength"), "ppp_percentile")
  expect_equal(side_rank_column("weakness"), "ppp_percentile")
  expect_equal(side_rank_column("attack"),   "def_ppp_percentile")
  expect_error(side_rank_column("defence"), "Unknown side")
})


## --- standing display -----------------------------------------

test_that("format_standing() picks rank for a conference, percentile nationally", {
  # auto follows the POPULATION -- the only way "0th percentile" stops
  # reading as "12th of 12"
  expect_match(format_standing(100, 12, "the Mountain West"), "^1st of 12")
  expect_match(format_standing(100, 363, "Division I"), "percentile")
  # and the boundary is honoured
  expect_match(format_standing(50, RANK_DISPLAY_MAX_POPULATION, "P"), " of ")
  expect_match(format_standing(50, RANK_DISPLAY_MAX_POPULATION + 1L, "P"), "percentile")
})

test_that("format_standing() marks the defensive aspect and handles NA", {
  expect_match(format_standing(80, 12, "the Mountain West", aspect = "defense"),
               "defensively")
  expect_false(grepl("defensively",
                     format_standing(80, 12, "the Mountain West", aspect = "offense")))
  expect_equal(format_standing(NA, 12, "the Mountain West"), "unranked")
  # an explicit mode overrides the population-based choice
  expect_match(format_standing(100, 12, "P", mode = "percentile"), "percentile")
  expect_match(format_standing(100, 363, "P", mode = "rank"), " of 363")
})


## --- the bullet pool ------------------------------------------

test_that("bullet_pool() drops residual buckets and the creation pair", {
  # TRAP 2. halfcourt is ~75-80% of possessions by construction, and
  # `creation` rises as an offence gets worse. Both must leave the pool.
  disp <- load_category_display()
  expect_true(any(!disp$bulletable))
  excluded <- disp[!disp$bulletable, ]
  expect_true("halfcourt" %in% excluded$category)
  expect_true(all(disp$dimension[disp$dimension == "creation"] %in% "creation"))
  expect_true(all(!disp$bulletable[disp$dimension == "creation"]),
              info = "every creation category must be unbulletable")

  # and every excluded row must record WHY
  expect_true(all(nzchar(excluded$exclusion_reason)))

  card <- fake_card(
    list(dim = "context", cat = "transition",  freq = 0.20, pctl = 90),
    list(dim = "context", cat = "halfcourt",   freq = 0.78, pctl = 95),
    list(dim = "creation", cat = "assisted",   freq = 0.60, pctl = 91,
         unit = "made_fg")
  )
  pool <- bullet_pool(card, disp)
  expect_equal(pool$category, "transition")
})


## --- selection: the gate and the score ------------------------

test_that("select_keys() gates strengths above the bound", {
  card <- fake_card(
    list(dim = "context",   cat = "transition",     freq = 0.20, pctl = 90),
    list(dim = "shot_zone", cat = "at_rim",            freq = 0.35, pctl = 71),
    list(dim = "context",   cat = "second_chance",  freq = 0.15, pctl = 70),  # not > 70
    list(dim = "shot_zone", cat = "mid_range",      freq = 0.30, pctl = 40)
  )
  sel <- select_keys(card, side = "strength")
  expect_setequal(sel$category, c("transition", "at_rim"))
  expect_true(all(sel$ppp_percentile > KEY_STRENGTH_PERCENTILE_MIN))
})

test_that("select_keys() gates weaknesses below the bound", {
  card <- fake_card(
    list(dim = "context",   cat = "transition", freq = 0.20, pctl = 5),
    list(dim = "shot_zone", cat = "at_rim",        freq = 0.35, pctl = 29),
    list(dim = "shot_zone", cat = "mid_range",  freq = 0.30, pctl = 30),  # not < 30
    list(dim = "shot_zone", cat = "corner_3",      freq = 0.25, pctl = 80)
  )
  sel <- select_keys(card, side = "weakness")
  expect_setequal(sel$category, c("transition", "at_rim"))
  expect_true(all(sel$ppp_percentile < KEY_WEAKNESS_PERCENTILE_MAX))
})

test_that("a side can legitimately come back empty", {
  # The gate is what allows a one-bullet or zero-bullet card. Measured at
  # 21% of Mountain West side-cards at zero, so this is normal output.
  card <- fake_card(
    list(dim = "context",   cat = "transition", freq = 0.20, pctl = 50),
    list(dim = "shot_zone", cat = "at_rim",        freq = 0.35, pctl = 55)
  )
  expect_equal(nrow(select_keys(card, side = "strength")), 0L)
  expect_equal(nrow(select_keys(card, side = "weakness")), 0L)
  expect_equal(keys_for_side(card, "SJSU", "strength"), character(0))
})

test_that("within a dimension, frequency still decides the order", {
  # Both rows are shot_zone, so they share a normaliser and the raw
  # frequency gap survives: 0.45/0.45 * 75 = 75 beats 0.12/0.45 * 99 = 26.
  card <- fake_card(
    list(dim = "shot_zone", cat = "at_rim",   freq = 0.45, pctl = 75),
    list(dim = "shot_zone", cat = "corner_3", freq = 0.12, pctl = 99)
  )
  sel <- select_keys(card, side = "strength")
  expect_equal(sel$category[1], "at_rim")
})

test_that("across dimensions, frequency is relative to the dimension", {
  # THE FIX. Raw freq is not comparable across dimensions: after
  # bullet_pool() drops halfcourt, context holds ~0.206 of the frequency
  # mass against shot_zone's 1.000, so a context row could never win a
  # slot. Measured over all 132 Mountain West matchups, raw freq gave
  # shot_zone 83% of strength bullets; normalised it gives 61%.
  #
  # Here each row is the most frequent in its own dimension, so both
  # normalise to 1.0 and the percentile alone decides -- which is the
  # point. Under the old scoring at_rim won on raw frequency despite the
  # far weaker percentile.
  card <- fake_card(
    list(dim = "shot_zone", cat = "at_rim",     freq = 0.45, pctl = 75),
    list(dim = "context",   cat = "transition", freq = 0.10, pctl = 99)
  )
  sel <- select_keys(card, side = "strength")
  expect_equal(sel$category[1], "transition")
  expect_equal(sel$freq_rel, c(1, 1))
})

test_that("freq_rel is the share of the dimension's own maximum", {
  card <- fake_card(
    list(dim = "shot_zone", cat = "at_rim",        freq = 0.40, pctl = 90),
    list(dim = "shot_zone", cat = "paint",         freq = 0.20, pctl = 85),
    list(dim = "context",   cat = "transition",    freq = 0.12, pctl = 80),
    list(dim = "context",   cat = "second_chance", freq = 0.06, pctl = 75)
  )
  sel <- select_keys(card, side = "strength", max_bullets = 99)
  rel <- setNames(sel$freq_rel, sel$category)
  expect_equal(rel[["at_rim"]], 1)
  expect_equal(rel[["paint"]], 0.5)
  expect_equal(rel[["transition"]], 1)
  expect_equal(rel[["second_chance"]], 0.5)
})

test_that("freq_rel is computed before the percentile gate", {
  # Normalising AFTER the gate would give a lone survivor freq_rel = 1
  # and over-promote it for being alone. paint is the dimension maximum
  # but fails the gate, so at_rim must still be scored against 0.40.
  card <- fake_card(
    list(dim = "shot_zone", cat = "paint",      freq = 0.40, pctl = 20),  # gated out
    list(dim = "shot_zone", cat = "at_rim",     freq = 0.20, pctl = 90),
    list(dim = "context",   cat = "transition", freq = 0.10, pctl = 85)
  )
  sel <- select_keys(card, side = "strength", max_bullets = 99)
  expect_equal(sel$freq_rel[sel$category == "at_rim"], 0.5)
  # so the context row, relative-frequency 1.0, leads despite the lower percentile
  expect_equal(sel$category[1], "transition")
})

test_that("select_keys() caps the bullet count and respects min_poss", {
  card <- fake_card(
    list(dim = "shot_zone", cat = "at_rim",           freq = 0.30, pctl = 99),
    list(dim = "shot_zone", cat = "corner_3",         freq = 0.25, pctl = 95),
    list(dim = "shot_zone", cat = "mid_range",     freq = 0.20, pctl = 90),
    list(dim = "context",   cat = "transition",    freq = 0.15, pctl = 85),
    list(dim = "context",   cat = "second_chance", freq = 0.10, pctl = 80)
  )
  expect_equal(nrow(select_keys(card, side = "strength")), KEY_MAX_BULLETS)
  expect_equal(nrow(select_keys(card, side = "strength", max_bullets = 1)), 1L)

  thin <- fake_card(
    list(dim = "context", cat = "transition", freq = 0.20, pctl = 90, poss = 10)
  )
  expect_equal(nrow(select_keys(thin, side = "strength")), 0L)
  expect_equal(nrow(select_keys(thin, side = "strength", min_poss = 5)), 1L)
})

test_that("select_keys() skips rows with a missing ranking percentile", {
  card <- fake_card(
    list(dim = "context",   cat = "transition", freq = 0.20, pctl = NA),
    list(dim = "shot_zone", cat = "at_rim",        freq = 0.35, pctl = 90)
  )
  expect_equal(select_keys(card, side = "strength")$category, "at_rim")
})

test_that("attack side ranks on the DEFENSIVE percentile", {
  # Stored attackable-oriented, so a HIGH def_ppp_percentile is the
  # weakness worth attacking.
  card <- fake_card(
    list(dim = "shot_zone", cat = "at_rim",   freq = 0.40, pctl = 20, def_pctl = 95),
    list(dim = "shot_zone", cat = "corner_3", freq = 0.30, pctl = 95, def_pctl = 10)
  )
  expect_equal(select_keys(card, side = "attack")$category, "at_rim")
})


## --- sentence composition -------------------------------------

test_that("weakness bullets carry a tactical clause addressed to US", {
  # This REVERSED on 2026-09-03. The rule was "the finding is the number;
  # the response is a later card" -- the coaching-first redesign requires
  # every analytic to map to an implication, so weakness now carries a
  # phrase too. It is still config-sourced, never generated here.
  row <- fake_card(
    list(dim = "shot_zone", cat = "at_rim", freq = 0.32, pctl = 8, ppp = 0.82)
  )
  s <- compose_key_sentence(row[1, ], "SJSU", "weakness",
                            population = "the Mountain West", pop_n = 12L)
  expect_true(nzchar(s))
  expect_match(s, " — ", info = "weakness now carries a phrase clause")
  expect_match(s, "32%")
  expect_match(s, "0\\.82")
  expect_match(s, "of 12")

  # the clause must be the config string, verbatim
  phr  <- load_tactical_phrases()
  want <- phr$phrase[phr$side == "weakness" & phr$dimension == "shot_zone" &
                     phr$category == "at_rim"][1]
  expect_match(s, want, fixed = TRUE)
})

test_that("strength bullets do carry a phrase from config", {
  row <- fake_card(
    list(dim = "context", cat = "transition", freq = 0.22, pctl = 92, ppp = 1.18)
  )
  s <- compose_key_sentence(row[1, ], "SJSU", "strength",
                            population = "the Mountain West", pop_n = 12L)
  expect_match(s, " — ")
  # the clause must be a config string, never generated here
  phr <- load_tactical_phrases()
  want <- phr$phrase[phr$side == "strength" & phr$dimension == "context" &
                     phr$category == "transition"][1]
  expect_match(s, want, fixed = TRUE)
})

test_that("a row with no phrase produces no bullet rather than an invented one", {
  row <- fake_card(
    list(dim = "context", cat = "not_a_real_category", freq = 0.2, pctl = 90)
  )
  # no display label either -> NA, and keys_for_side drops NAs
  expect_true(is.na(compose_key_sentence(row[1, ], "SJSU", "strength")))
})

test_that("a PPP-degenerate dimension states the rate, not points per possession", {
  # ball_security: a turnover scores zero by definition, so "at 0.00 points
  # per possession" would be arithmetic dressed as a finding. The flag lives
  # in dimension_headers.csv and the template drops the clause.
  row <- fake_card(list(dim = "ball_security", cat = "live_ball_to",
                        freq = 0.11, pctl = 8, ppp = 0))
  # population is passed WITHOUT the article; the formatter adds it
  w <- compose_key_sentence(row[1, ], "SJSU", "weakness",
                            population = "Big Ten", pop_n = 18L)
  expect_false(grepl("points per", w))
  expect_false(grepl("0\\.00", w))
  expect_match(w, "11%")
  expect_match(w, "in the Big Ten")
  expect_false(grepl("the the", w))          # article added exactly once

  st <- compose_key_sentence(row[1, ], "SJSU", "strength",
                             population = "Big Ten", pop_n = 18L)
  expect_false(grepl("points per", st))
  expect_match(st, " — ")            # a strength still carries its phrase
})

test_that("a PPP-meaningful dimension still quotes points per possession", {
  row <- fake_card(list(dim = "context", cat = "transition", freq = 0.30,
                        pctl = 90, ppp = 1.18))
  s <- compose_key_sentence(row[1, ], "SJSU", "strength",
                            population = "Big Ten", pop_n = 18L)
  expect_match(s, "points per possession")
  expect_match(s, "1\\.18")
})

test_that("the sentence uses the unit's own denominator", {
  # The ppp unit differs per dimension and must be labelled -- values near
  # 2.5 are correct for creation, not a bug.
  expect_setequal(names(UNIT_PHRASE), names(UNIT_DENOMINATOR))
  rim <- fake_card(list(dim = "shot_zone", cat = "at_rim", freq = 0.3, pctl = 90,
                        unit = "attempt"))
  s <- compose_key_sentence(rim[1, ], "SJSU", "strength")
  expect_match(s, "of attempts")
  expect_match(s, "points per attempt")
})


## --- config integrity -----------------------------------------

test_that("every unit the profile emits has prose", {
  skip_if_not(file.exists(PROFILE_CSV), "cached profile not present")
  prof <- readr::read_csv(PROFILE_CSV, show_col_types = FALSE, progress = FALSE)
  expect_true(all(unique(prof$unit) %in% names(UNIT_PHRASE)))
  expect_true(all(unique(prof$unit) %in% names(UNIT_DENOMINATOR)))
})

test_that("every bulletable category has a display label and a strength phrase", {
  disp <- load_category_display()
  phr  <- load_tactical_phrases()
  pool <- disp[disp$bulletable & disp$source == "espn_derived", ]

  expect_true(all(nzchar(pool$display_label)))

  have <- paste(phr$side, phr$source, phr$dimension, phr$category)
  want <- paste("strength", pool$source, pool$dimension, pool$category)
  expect_true(all(want %in% have),
              info = paste("missing strength phrases:",
                           paste(setdiff(want, have), collapse = "; ")))
})

test_that("every bulletable category has a weakness phrase too", {
  # Weakness phrases were added 2026-09-03. A category with no phrase row
  # yields NO bullet rather than an invented one, so a gap here silently
  # shrinks the card instead of failing loudly -- hence this check.
  phr  <- load_tactical_phrases()
  disp <- load_category_display()
  pool <- disp[disp$bulletable & disp$source == "espn_derived", ]

  have <- paste(phr$side, phr$source, phr$dimension, phr$category)
  want <- paste("weakness", pool$source, pool$dimension, pool$category)
  expect_true(all(want %in% have),
              info = paste("missing weakness phrases:",
                           paste(setdiff(want, have), collapse = "; ")))
})

test_that("dimension headers cover every dimension and state their unit", {
  hdr <- load_dimension_headers()
  disp <- load_category_display()
  expect_true(all(unique(disp$dimension[disp$source == "espn_derived"]) %in%
                  hdr$dimension[hdr$source == "espn_derived"]))
  expect_true(all(nzchar(hdr$ppp_definition)))
  expect_true(all(nzchar(hdr$unit_noun)))
})


## --- end to end on the cached profile -------------------------

test_that("build_keys() produces a scouting card off the real profile", {
  skip_if_not(file.exists(PROFILE_CSV), "cached profile not present")
  prof <- readr::read_csv(PROFILE_CSV, show_col_types = FALSE, progress = FALSE)

  keys <- build_keys(prof, team = "USU", opponent = "SJSU")

  expect_named(keys, c("strength", "weakness"))
  expect_equal(attr(keys, "population"), "Mountain West")
  expect_equal(attr(keys, "population_n"), 12L)
  expect_equal(attr(keys, "standing"), "rank")   # 12 teams -> ranks

  for (s in names(keys)) {
    expect_equal(keys[[s]]$side, s)
    expect_true(nzchar(keys[[s]]$heading))
    expect_lte(length(keys[[s]]$bullets), KEY_MAX_BULLETS)
    expect_true(all(nzchar(keys[[s]]$bullets)))
    # no bullet may name a non-bulletable category
    expect_false(any(grepl("Halfcourt|Assisted|Unassisted", keys[[s]]$bullets)))
  }
  # weakness carries a tactical clause on real data too, as of 2026-09-03
  expect_true(all(grepl(" — ", keys$weakness$bullets)))
})

test_that("no matchup in the population comes back blank on both sides", {
  # The selection rule was verified across all 132 Mountain West matchups
  # when it was written. This pins that property.
  skip_if_not(file.exists(PROFILE_CSV), "cached profile not present")
  prof <- readr::read_csv(PROFILE_CSV, show_col_types = FALSE, progress = FALSE)
  teams <- sort(unique(prof$team))

  blank <- 0L
  for (a in teams) for (b in teams) {
    if (a == b) next
    k <- build_keys(prof, team = a, opponent = b)
    if (length(k$strength$bullets) == 0 && length(k$weakness$bullets) == 0) {
      blank <- blank + 1L
    }
  }
  expect_equal(blank, 0L)
})

test_that("build_keys() rejects a team that is not in the profile", {
  skip_if_not(file.exists(PROFILE_CSV), "cached profile not present")
  prof <- readr::read_csv(PROFILE_CSV, show_col_types = FALSE, progress = FALSE)
  expect_error(build_keys(prof, team = "USU", opponent = "DUKE"),
               "No profile rows")
})

test_that("no dimension monopolises the bullets across the population", {
  # Regression guard for the normalisation. With raw frequency, shot_zone
  # took 83% of strength bullets and 82% of weakness bullets across all
  # 132 Mountain West matchups; context could not compete. If a future
  # scoring change re-buries a dimension this fails.
  skip_if_not(file.exists(PROFILE_CSV), "cached profile not present")
  prof <- readr::read_csv(PROFILE_CSV, show_col_types = FALSE, progress = FALSE)
  teams <- sort(unique(prof$team))

  picked <- list()
  for (a in teams) for (b in teams) {
    if (a == b) next
    k <- build_keys(prof, team = a, opponent = b)
    for (sd in names(k)) {
      if (nrow(k[[sd]]$rows)) picked[[length(picked) + 1L]] <- k[[sd]]$rows["dimension"]
    }
  }
  mix <- table(dplyr::bind_rows(picked)$dimension)
  share <- mix / sum(mix)

  # Both bulletable dimensions must be represented, and neither may take
  # more than 75% of the slots.
  expect_setequal(names(share), c("context", "shot_zone"))
  expect_true(all(share > 0.20), info = paste(names(share), round(share, 3), collapse = " "))
  expect_true(all(share < 0.75), info = paste(names(share), round(share, 3), collapse = " "))
})


test_that("an inverted-rank row reads in the direction of its own label", {
  # ball_security ranks on "keeps the ball" but the row is LABELLED with the
  # turnovers. Printing the raw rank made NU's worst-in-conference 13% read
  # as "18th of 18", which a reader takes as 18th-MOST, i.e. the fewest.
  n <- 18L
  expect_equal(format_inverted_standing(0,   n, "Big Ten"), "the most in the Big Ten")
  expect_equal(format_inverted_standing(100, n, "Big Ten"), "the fewest in the Big Ten")
  # and it flips to whichever direction is the notable one
  expect_match(format_inverted_standing(6,  n, "Big Ten"), "2nd-most")
  expect_match(format_inverted_standing(94, n, "Big Ten"), "2nd-fewest")
  expect_equal(format_inverted_standing(NA, n, "Big Ten"), "unranked")
})

test_that("the inverted standing never reports a near-best team as 'most'", {
  # The bug this guards: UCLA, 2nd BEST in the Big Ten at protecting the
  # ball, was described as "17th-most", which is true and useless.
  n <- 18L
  for (p in c(76, 82, 88, 94, 100)) {
    expect_match(format_inverted_standing(p, n, "Big Ten"), "fewest",
                 info = paste("percentile", p))
  }
  for (p in c(0, 6, 12, 18)) {
    expect_match(format_inverted_standing(p, n, "Big Ten"), "most",
                 info = paste("percentile", p))
  }
})
