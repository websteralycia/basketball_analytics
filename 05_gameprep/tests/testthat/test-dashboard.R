## ============================================================
## test-dashboard.R
## ------------------------------------------------------------
## The app's reactive logic, driven through shiny::testServer -- no
## browser, no port. What is worth pinning is that the dashboard shows
## the SAME numbers as the card, because the whole design rests on both
## reading one display layer. A dashboard that quietly diverged from the
## card would be the failure this session spent its effort preventing.
## ============================================================

skip_if_no_shiny <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
}

app_dir <- local({
  root <- Sys.getenv("GAMEPREP_ROOT", unset = "")
  if (!nzchar(root)) {
    for (d in c(".", "..", "../..", "../../..")) {
      if (file.exists(file.path(d, "source_all.R"))) { root <- normalizePath(d); break }
    }
  }
  file.path(root, "consumers", "dashboard")
})

test_that("the app starts with no network call and finds its populations", {
  skip_if_no_shiny()
  skip_if_not(dir.exists(app_dir))
  profs <- list_cached_profiles("wbb")
  expect_gt(nrow(profs), 0)
  expect_true("Big Ten" %in% profs$population)
  # the population NAME is read from the file, not un-slugged from it:
  # "big_ten" -> "Big Ten" is not reversible in general
  expect_false(any(grepl("_", profs$population)))
})

test_that("selecting a population loads its profile and its teams", {
  skip_if_no_shiny()
  skip_if_not(dir.exists(app_dir))
  shiny::testServer(shiny::shinyAppDir(app_dir), {
    session$setInputs(league = "wbb", pop = "Big Ten|2026")
    expect_equal(pop_parts()$population, "Big Ten")
    expect_equal(pop_parts()$season, 2026L)
    expect_equal(length(unique(profile()$team)), 18L)
  })
})

test_that("the dashboard card is the SAME card the builder produces", {
  # The load-bearing test. Both must go through build_pregame_card() and
  # therefore through profile_display.R; if the app ever renders its own
  # rows, this fails.
  skip_if_no_shiny()
  skip_if_not(dir.exists(app_dir))
  shiny::testServer(shiny::shinyAppDir(app_dir), {
    session$setInputs(league = "wbb", pop = "Big Ten|2026", team = "MICH", opp = "NU")
    html <- card_html()
    expect_true(nchar(html) > 5000)
    expect_match(html, "Opponent ranked within Big Ten")
    # the two bugs, checked on the surface a coach actually looks at
    expect_false(grepl("0.00 points per possession", html, fixed = TRUE))
    expect_match(html, "the most in the Big Ten")
  })
})

test_that("picking the same team twice is refused, not rendered", {
  skip_if_no_shiny()
  skip_if_not(dir.exists(app_dir))
  shiny::testServer(shiny::shinyAppDir(app_dir), {
    session$setInputs(league = "wbb", pop = "Big Ten|2026", team = "MICH", opp = "MICH")
    expect_error(card_html())   # validate() stops the reactive
  })
})

test_that("changing the matchup clears the conversation", {
  # Context is pinned to the first message, so carrying history across
  # matchups would have the agent answering about the previous opponent
  # while the card shows the new one.
  skip_if_no_shiny()
  skip_if_not(dir.exists(app_dir))
  shiny::testServer(shiny::shinyAppDir(app_dir), {
    session$setInputs(league = "wbb", pop = "Big Ten|2026", team = "MICH", opp = "NU")
    log(list(list(role = "you", text = "prior question")))
    history(list(list(role = "user", content = "prior")))
    session$setInputs(opp = "IOWA")
    expect_equal(length(log()), 0L)
    expect_null(history())
  })
})
