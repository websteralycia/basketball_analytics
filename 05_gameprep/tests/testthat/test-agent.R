## ============================================================
## test-agent.R
## ------------------------------------------------------------
## The loop's own logic, tested WITHOUT an API key. What is exercised
## here is everything that decides whether an answer can be wrong:
## the allowlist, the dispatch, how results are rendered for the model,
## and whether the tool schemas are well-formed.
##
## The network call itself is one httr2 request and is not mocked --
## testing that would test httr2. What is worth pinning is that a model
## naming a function we did not expose cannot reach it, and that a tool
## that errors comes back as a readable message rather than killing the
## conversation.
## ============================================================

## --- the allowlist --------------------------------------------

test_that("only tools on the manifest are reachable", {
  # The guardrail. A model naming any other function must not reach an
  # eval(), and must be told what it may call instead.
  r <- dispatch_tool("system", list(command = "rm -rf /"))
  expect_false(r$ok)
  expect_match(r$text, "No such tool")
  expect_match(r$text, "q_team_profile")   # names the real options

  r2 <- dispatch_tool("q_dimensions", list())
  expect_true(r2$ok)
})

test_that("a tool that errors returns a readable message, not a crash", {
  # A bad team abbreviation is a normal event mid-conversation. The model
  # should get the error text and correct itself.
  skip_if_not(file.exists(profile_cache_path("Big Ten", 2026, "wbb")))
  r <- dispatch_tool("q_team_profile", list(league = "wbb", season = 2026,
                                            population = "Big Ten", team = "NOPE"))
  expect_false(r$ok)
  expect_match(r$text, "Tool error")
  expect_match(r$text, "not in the Big Ten")
})

test_that("dispatch passes named arguments through", {
  skip_if_not(file.exists(profile_cache_path("Big Ten", 2026, "wbb")))
  r <- dispatch_tool("q_team_profile",
                     list(league = "wbb", season = 2026, population = "Big Ten",
                          team = "NU", dimension = "shot_zone"))
  expect_true(r$ok)
  expect_match(r$text, "at_rim")
  expect_false(grepl("live_ball_to", r$text))   # the filter was honoured
})

## --- what the model actually reads ----------------------------

test_that("data frames render as records, not as a printed table", {
  df <- data.frame(team = c("A", "B"), pctl = c(90L, 10L), stringsAsFactors = FALSE)
  txt <- tool_result_text(df)
  expect_match(txt, '"team"')
  expect_match(txt, '"pctl"')
  # valid JSON round-trips
  back <- jsonlite::fromJSON(sub("\\n\\[.*", "", txt))
  expect_equal(nrow(back), 2L)
})

test_that("a long result is truncated AND says so", {
  # Silently sending the first 60 rows would have the model reason about
  # a conference as though it had six teams.
  df <- data.frame(i = 1:200)
  txt <- tool_result_text(df, max_rows = 60L)
  expect_match(txt, "200 rows total")
  expect_match(txt, "first 60 shown")
})

test_that("NA renders as null rather than the string NA", {
  df <- data.frame(x = c(1, NA))
  expect_match(tool_result_text(df), "null")
  expect_false(grepl('"NA"', tool_result_text(df), fixed = TRUE))
})

## --- the tool definitions sent to the API ---------------------

test_that("every tool definition is a well-formed strict schema", {
  defs <- query_tool_defs()
  expect_equal(length(defs), length(QUERY_TOOLS))
  for (d in defs) {
    expect_true(nzchar(d$name))
    expect_true(nzchar(d$description))
    expect_true(isTRUE(d$strict))
    expect_equal(d$input_schema$type, "object")
    # strict tool use requires both of these
    expect_false(is.null(d$input_schema$additionalProperties))
    expect_false(isTRUE(d$input_schema$additionalProperties))
    expect_false(is.null(d$input_schema$required))
  }
})

test_that("every required parameter is described in the schema", {
  # A required key with no property entry is a 400 at request time, and
  # the only symptom is the whole conversation failing.
  for (t in QUERY_TOOLS) {
    expect_true(all(t$required %in% names(t$schema)),
                info = paste(t$name, ":",
                             paste(setdiff(t$required, names(t$schema)), collapse = ", ")))
  }
})

test_that("every schema parameter is a real formal of the function", {
  # The drift that matters: a schema advertising an argument the R
  # function does not take produces a tool call that always errors.
  for (t in QUERY_TOOLS) {
    f <- names(formals(get(t$name)))
    expect_true(all(names(t$schema) %in% f),
                info = paste(t$name, ":",
                             paste(setdiff(names(t$schema), f), collapse = ", ")))
  }
})

test_that("no schema serialises an empty properties object as an array", {
  # q_dimensions takes no arguments. R's empty list becomes [] in JSON,
  # which is not a valid `properties` value and is rejected.
  d <- query_tool_defs()[[which(vapply(QUERY_TOOLS, `[[`, character(1), "name") ==
                                "q_dimensions")]]
  j <- jsonlite::toJSON(d, auto_unbox = TRUE)
  expect_match(as.character(j), '"properties":\\{\\}')
})

## --- the system prompt carries the guardrails -----------------

test_that("the system prompt states the traps the data actually has", {
  # Each of these corresponds to a mistake made and fixed in this
  # project. A model reasoning from raw numbers would repeat them.
  p <- scout_system_prompt()
  expect_match(p, "denominator")          # no cross-dimension rate compare
  expect_match(p, "turnover scores zero") # degenerate rate
  # matched without the line break -- the prompt is hard-wrapped
  expect_match(p, "RISES")                # the creation inversion
  expect_match(p, "not a strength")
  expect_match(p, "Net rating")           # not available per lineup
  expect_match(p, "Population is a parameter")
})

## --- credentials ----------------------------------------------

test_that("a missing key fails with instructions, not a 401", {
  withr::with_envvar(c(ANTHROPIC_API_KEY = ""), {
    expect_error(anthropic_key(), "ANTHROPIC_API_KEY is not set")
    expect_error(anthropic_key(), "export ANTHROPIC_API_KEY")
  })
})
