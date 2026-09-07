## ============================================================
## test-render_pregame_card.R
## ------------------------------------------------------------
## The renderer went untested on the argument that its failures are
## visible: broken HTML and missing sections show up the moment you open
## the card, where a wrong header number did not. That held until the
## coaching-first redesign added four functions that make EDITORIAL
## choices — which three keys lead, what the identity sentence says,
## which half of a sentence is the action. Those fail quietly: the card
## still renders, it just leads with the wrong thing.
##
## So this file tests the redesign's judgment, plus the structural
## properties a coach would notice: the population is named, the units
## are labelled, and no non-bulletable category reaches the card.
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
source(file.path(card_dir, "render_pregame_card.R"))

BIG_TEN_CSV <- file.path(dirname(dirname(card_dir)), "data", "tidy", "wbb",
                         "profiles", "big_ten_2026_play_profile.csv")


## --- escaping -------------------------------------------------

test_that("esc() neutralises the characters that break a row", {
  # "P&R ball handler" is a real Synergy label and a bare & is enough.
  expect_equal(esc("P&R ball handler"), "P&amp;R ball handler")
  expect_equal(esc("<script>"), "&lt;script&gt;")
  # ampersand first, or the escapes escape each other
  expect_equal(esc("a & <b>"), "a &amp; &lt;b&gt;")
})


## --- sentence hierarchy ---------------------------------------

test_that("split_key_sentence puts the coaching action first", {
  s <- paste("Live-ball turnovers account for 13% of possessions for NU,",
             "the most in the Big Ten — pressure the ball.")
  out <- split_key_sentence(s)
  expect_equal(out$action, "Pressure the ball")      # capitalised, no full stop
  expect_match(out$evidence, "13% of possessions")
  expect_false(grepl("pressure the ball", out$evidence))
})

test_that("a sentence with no clause degrades to action-only", {
  # Not every row carries a phrase, and a missing em dash must not
  # produce an empty headline.
  out <- split_key_sentence("They turn it over a lot.")
  expect_equal(out$action, "They turn it over a lot.")
  expect_equal(out$evidence, "")
})

test_that("only the LAST em dash splits the sentence", {
  # Phrases themselves contain em dashes; splitting on the first would
  # move half the evidence into the headline.
  out <- split_key_sentence("A — B — do the thing.")
  expect_equal(out$action, "Do the thing")
  expect_equal(out$evidence, "A — B")
})


## --- which three keys lead ------------------------------------

fake_side <- function(side, scores, sentences) {
  list(side = side, heading = side,
       rows = tibble::tibble(key_score = scores),
       bullets = sentences)
}

test_that("priority_keys ranks across BOTH sides on key_score", {
  # A card whose opponent is strong everywhere and weak nowhere must
  # still lead with the strengths, and vice versa -- the sides are not
  # interleaved or balanced, they compete on score.
  keys <- list(
    fake_side("strength", c(0.9, 0.2), c("s1 — a.", "s2 — b.")),
    fake_side("weakness", c(0.8, 0.7), c("w1 — c.", "w2 — d.")))
  out <- priority_keys(keys, max_keys = 3L)
  expect_equal(length(out), 3L)
  expect_equal(vapply(out, `[[`, character(1), "sentence"),
               c("s1 — a.", "w1 — c.", "w2 — d."))
  expect_equal(out[[1]]$side, "strength")
})

test_that("priority_keys survives an empty side", {
  keys <- list(fake_side("strength", numeric(0), character(0)),
               fake_side("weakness", c(0.5), "w1 — c."))
  expect_equal(length(priority_keys(keys)), 1L)
})

test_that("priority_keys returns empty rather than erroring on a blank card", {
  keys <- list(fake_side("strength", numeric(0), character(0)),
               fake_side("weakness", numeric(0), character(0)))
  expect_equal(length(priority_keys(keys)), 0L)
})

test_that("priority_keys never reads past the shorter of rows/bullets", {
  # A row with no config phrase yields no bullet, so the two vectors can
  # differ in length. Indexing on rows alone would emit an NA headline.
  keys <- list(fake_side("strength", c(0.9, 0.8, 0.7), c("s1 — a.")))
  out <- priority_keys(keys)
  expect_equal(length(out), 1L)
  expect_false(any(is.na(vapply(out, `[[`, character(1), "sentence"))))
})


## --- the identity sentence ------------------------------------

test_that("opponent_identity names the highest-volume shot zone", {
  display <- load_category_display()
  card <- tibble::tibble(
    dimension = "shot_zone", source = "espn_derived",
    category = c("at_rim", "paint", "mid_range"),
    freq = c(0.23, 0.25, 0.22))
  id <- opponent_identity(card, display)
  expect_match(id$copy, "paint \\(non-rim\\)")   # the 25%, not the first row
  expect_equal(length(id$tags), 3L)
  expect_match(id$tags[1], "^Paint \\(non-rim\\) 25%$")
})

test_that("opponent_identity degrades rather than erroring without zones", {
  display <- load_category_display()
  card <- tibble::tibble(dimension = character(0), source = character(0),
                         category = character(0), freq = numeric(0))
  id <- opponent_identity(card, display)
  expect_true(nzchar(id$copy))
  expect_equal(length(id$tags), 0L)
})


## --- the bar --------------------------------------------------

test_that("gp_row anchors the fill at the centre and picks the right side", {
  above <- gp_row("At rim", "23% of attempts", "1st of 18", "strong",  0.8)
  below <- gp_row("At rim", "23% of attempts", "18th of 18", "weak",  -0.8)
  expect_match(above, "left:50%")
  expect_match(below, "right:50%")
  # rounded only on the outer end
  expect_match(above, "border-radius:0 4px 4px 0")
  expect_match(below, "border-radius:4px 0 0 4px")
  # the standing is printed verbatim, not reformatted
  expect_match(above, "1st of 18", fixed = TRUE)
})

test_that("gp_row prints an inverted standing untouched", {
  # The renderer must not re-derive this: it gets the word and prints it.
  row <- gp_row("Live-ball turnovers", "13% of possessions", "most", "weak", -1)
  expect_match(row, ">most<")
  expect_false(grepl("18", row))
})

test_that("gp_row rejects an unknown colour meaning", {
  # A typo here would silently invert every colour on the card.
  expect_error(gp_row("x", "y", "1st of 18", "strong", 0.5, color_means = "quality!"),
               "quality")
})


## --- the whole card -------------------------------------------

test_that("the rendered card is structurally sound", {
  skip_if_not(file.exists(BIG_TEN_CSV), "cached Big Ten profile not present")
  prof <- readr::read_csv(BIG_TEN_CSV, show_col_types = FALSE, progress = FALSE)
  html <- render_pregame_card(prof, "MICH", "IOWA")

  # tags balance -- the fragment is assembled by string paste, so an
  # unclosed div is a real risk and silently wrecks the layout
  expect_equal(lengths(regmatches(html, gregexpr("<div", html))),
               lengths(regmatches(html, gregexpr("</div>", html))))

  # the population must be named: SPEC_play_profile.md requires it, and a
  # rank means nothing without it
  expect_match(html, "Opponent ranked within Big Ten")
  expect_match(html, "18 teams")

  # every dimension group states its own unit, because the three PPPs do
  # not share a denominator
  expect_match(html, "PPP = per possession")
  expect_match(html, "PPP = per attempt")

  # non-bulletable categories stay off the card entirely
  for (lab in c("Halfcourt", "Assisted", "Unassisted", "Kept the ball")) {
    expect_false(grepl(lab, html, fixed = TRUE), info = lab)
  }
})

test_that("the card never prints a degenerate rate", {
  skip_if_not(file.exists(BIG_TEN_CSV), "cached Big Ten profile not present")
  prof <- readr::read_csv(BIG_TEN_CSV, show_col_types = FALSE, progress = FALSE)
  # NU is worst in the conference at ball security, so its card is the
  # one that carries these rows.
  html <- render_pregame_card(prof, "MICH", "NU")
  expect_false(grepl("0.00 points per possession", html, fixed = TRUE))
  # and the inverted rows read in the direction they name
  expect_match(html, "the most in the Big Ten")
})

test_that("missing stats render as em dashes, not as invented numbers", {
  skip_if_not(file.exists(BIG_TEN_CSV), "cached Big Ten profile not present")
  prof <- readr::read_csv(BIG_TEN_CSV, show_col_types = FALSE, progress = FALSE)
  html <- render_pregame_card(prof, "MICH", "IOWA", stats = list())
  expect_match(html, "&mdash;|—")
  # no tile may show a bare "NA" or "NULL"
  expect_false(grepl(">NA<", html, fixed = TRUE))
  expect_false(grepl(">NULL<", html, fixed = TRUE))
})

test_that("a written card is a complete UTF-8 document", {
  skip_if_not(file.exists(BIG_TEN_CSV), "cached Big Ten profile not present")
  prof <- readr::read_csv(BIG_TEN_CSV, show_col_types = FALSE, progress = FALSE)
  tmp  <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  render_pregame_card(prof, "MICH", "IOWA", file = tmp)

  doc <- readLines(tmp, warn = FALSE)
  # Without a charset the browser falls back to the locale codepage and
  # "·" renders as "Â·". That shipped once.
  expect_true(any(grepl("<meta charset=\"utf-8\">", doc, fixed = TRUE)))
  expect_true(any(grepl("^<!doctype html>", doc)))
  expect_true(any(grepl("</html>", doc, fixed = TRUE)))
})
