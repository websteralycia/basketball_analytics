## ============================================================
## agent.R — the scouting Q&A loop over the query layer
## ------------------------------------------------------------
## A coach asks a question in words; Claude answers it by CALLING the
## functions in query_layer.R and reading real rows back. Nothing here
## computes basketball. If a number is in an answer, a tool returned it.
##
## WHY TOOL CALLS AND NOT RAG. "What do they run late in games" is a
## filter and an aggregate. "Who else in the conference turns it over
## this much" is a sort. Embedding profile rows answers both
## approximately, cannot say "13th of 18" at all, and throws away the
## ranking semantics that were the hardest part of this project to get
## right. Retrieval still belongs over the coach-authored prose in
## tactical_phrases.csv -- but not over the numbers.
##
## THERE IS NO OFFICIAL R SDK for the Anthropic API, so this speaks raw
## HTTP via httr2, which is the documented path for languages without an
## SDK. Request shapes follow the Messages API reference; do not "tidy"
## them against memory of an older API. Two that have changed and will
## look wrong if you remember the old form:
##   - thinking is {type: "adaptive"}. `budget_tokens` is REMOVED on
##     Opus 5 and returns a 400.
##   - effort lives in output_config, not at the top level.
##
## THE ALLOWLIST IS THE GUARDRAIL. dispatch_tool() will only call a
## function named in QUERY_TOOLS. A model naming anything else gets an
## error back as a tool_result, not an eval().
##
## Depends on: query_layer.R, httr2, jsonlite
## ============================================================

ANTHROPIC_MODEL       <- "claude-opus-5"
ANTHROPIC_API_URL     <- "https://api.anthropic.com/v1/messages"
ANTHROPIC_VERSION     <- "2023-06-01"
AGENT_MAX_TOKENS      <- 16000L   # non-streaming; keeps us under HTTP timeouts
AGENT_MAX_TURNS       <- 12L      # a question needing more than this is a bug

#' Resolve the API key, with an error that says what to do
anthropic_key <- function() {
  k <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  if (!nzchar(k)) {
    stop("ANTHROPIC_API_KEY is not set.\n",
         "  export ANTHROPIC_API_KEY=sk-ant-...\n",
         "and restart R, or pass key= explicitly.", call. = FALSE)
  }
  k
}

#' The system prompt: what the agent is, and what it must not do
#'
#' The prohibitions are not stylistic. Each corresponds to a mistake this
#' project has actually made and fixed, and a model reasoning from raw
#' numbers would make them again.
scout_system_prompt <- function() paste(
  "You are a basketball scouting assistant for a coaching staff.",
  "You answer questions using ONLY numbers returned by your tools.",
  "",
  "Rules, in order of importance:",
  "1. Never state a number you did not get from a tool call. If a tool",
  "   cannot answer the question, say so plainly and say what is missing.",
  "2. Quote the `standing` and `meta` strings a tool returns rather than",
  "   rephrasing them. They encode direction rules that are easy to get",
  "   backwards -- for turnover categories, a high rank means MORE",
  "   turnovers, and the string already says which end is meant.",
  "3. Never compare a rate across dimensions. Points per possession,",
  "   points per attempt and points per made field goal share no",
  "   denominator. Do not average them, rank them together, or put them",
  "   on one scale.",
  "4. Some rows carry no meaningful rate -- a turnover scores zero by",
  "   definition. q_dimensions tells you which. Report those as",
  "   frequencies only.",
  "5. Some categories are excluded from scouting points and carry a",
  "   reason. Assisted field-goal share is the trap: it RISES as an",
  "   offence gets worse, so a high figure is not a strength.",
  "6. Net rating per lineup does not exist in this data. Point",
  "   differential does. Do not imply otherwise.",
  "7. Population is a parameter. Always say which population a standing",
  "   is measured within -- '13th of 18 in the Big Ten', never '13th'.",
  "",
  "Be brief and concrete. A coach is reading this before a game.",
  sep = "\n")

#' Call one query tool by name, safely
#'
#' Only functions in QUERY_TOOLS are reachable. Errors come back as text
#' for the model to read and recover from, because a failed tool call is
#' a normal event in a loop -- a malformed team abbreviation should
#' produce a correction, not a crash.
dispatch_tool <- function(name, input, tools = QUERY_TOOLS) {
  allowed <- vapply(tools, `[[`, character(1), "name")
  if (!name %in% allowed) {
    return(list(ok = FALSE,
                text = paste0("No such tool: '", name, "'. Available: ",
                              paste(allowed, collapse = ", "))))
  }
  fn <- get(name, mode = "function")
  args <- if (is.null(input) || !length(input)) list() else as.list(input)

  out <- tryCatch(do.call(fn, args),
                  error = function(e) structure(conditionMessage(e), class = "tool_error"))
  if (inherits(out, "tool_error")) {
    return(list(ok = FALSE, text = paste0("Tool error: ", as.character(out))))
  }
  list(ok = TRUE, text = tool_result_text(out))
}

#' Render a tool's return value as compact text for the model
#'
#' Data frames go out as records rather than a printed table: the model
#' reads JSON more reliably than aligned columns, and a wide profile
#' frame printed as a table wraps and loses its header.
tool_result_text <- function(x, max_rows = 60L) {
  if (is.data.frame(x)) {
    n <- nrow(x)
    if (n > max_rows) x <- utils::head(x, max_rows)
    txt <- jsonlite::toJSON(x, dataframe = "rows", auto_unbox = TRUE,
                            digits = 4, na = "null")
    if (n > max_rows) {
      return(paste0(txt, "\n[", n, " rows total; first ", max_rows, " shown]"))
    }
    return(as.character(txt))
  }
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = 4, na = "null"))
}

#' One POST to the Messages API
#'
#' `fallbacks` + its beta header are on by default: a policy decline
#' otherwise just stops the turn, and the rescue happens inside the same
#' call. Drop them only deliberately.
anthropic_messages <- function(body, key = anthropic_key(),
                               fallbacks = TRUE, timeout = 300) {
  if (isTRUE(fallbacks)) body$fallbacks <- "default"

  req <- httr2::request(ANTHROPIC_API_URL) |>
    httr2::req_headers(
      "x-api-key"         = key,
      "anthropic-version" = ANTHROPIC_VERSION,
      "content-type"      = "application/json") |>
    httr2::req_body_raw(
      jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
      type = "application/json") |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(max_tries = 3)   # 429 and 5xx only, per httr2 defaults

  if (isTRUE(fallbacks)) {
    req <- httr2::req_headers(req, "anthropic-beta" = "server-side-fallback-2026-07-01")
  }

  resp <- httr2::req_perform(req)
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

#' Ask the scouting agent a question
#'
#' @param question What the coach typed.
#' @param context Optional named list pinned into the first message —
#'   league, season, population, team, opponent. Supplying it stops the
#'   model spending turns discovering the season it is in.
#' @param effort "low" through "max". Scouting answers are short and the
#'   reasoning is in the tool choice, so "medium" is usually right.
#' @param verbose Print each tool call as it happens.
#'
#' @return A list: `answer` (text), `messages` (the full transcript, so a
#'   UI can continue the conversation), `tool_calls` (what was called),
#'   and `usage`.
scout_ask <- function(question,
                      context = list(),
                      effort  = "medium",
                      max_turns = AGENT_MAX_TURNS,
                      verbose = FALSE,
                      history = NULL,
                      key     = anthropic_key(),
                      tools   = QUERY_TOOLS) {

  # Context is pinned to the FIRST message only. Repeating it every turn
  # would change the cached prefix on each request and forfeit the cache.
  first <- if (length(context) && is.null(history)) {
    paste0("Context for this question: ",
           tool_result_text(context), "\n\n", question)
  } else question

  messages <- c(history %||% list(), list(list(role = "user", content = first)))
  tool_calls <- list()
  usage <- list(input = 0L, output = 0L)

  for (turn in seq_len(max_turns)) {
    body <- list(
      model         = ANTHROPIC_MODEL,
      max_tokens    = AGENT_MAX_TOKENS,
      system        = scout_system_prompt(),
      thinking      = list(type = "adaptive"),
      output_config = list(effort = effort),
      tools         = query_tool_defs(tools),
      messages      = messages)

    resp <- anthropic_messages(body, key = key)

    if (!is.null(resp$usage)) {
      usage$input  <- usage$input  + (resp$usage$input_tokens  %||% 0L)
      usage$output <- usage$output + (resp$usage$output_tokens %||% 0L)
    }

    # A policy decline is HTTP 200. Check before reading content.
    if (identical(resp$stop_reason, "refusal")) {
      return(list(answer = paste0("The request was declined",
                                  if (!is.null(resp$stop_details$category))
                                    paste0(" (", resp$stop_details$category, ")") else "",
                                  "."),
                  refusal = TRUE, messages = messages,
                  tool_calls = tool_calls, usage = usage))
    }

    messages <- c(messages, list(list(role = "assistant", content = resp$content)))

    if (!identical(resp$stop_reason, "tool_use")) {
      txt <- vapply(Filter(function(b) identical(b$type, "text"), resp$content),
                    function(b) b$text %||% "", character(1))
      return(list(answer = paste(txt, collapse = "\n"), refusal = FALSE,
                  messages = messages, tool_calls = tool_calls,
                  usage = usage, turns = turn))
    }

    # EVERY tool_result goes back in ONE user message. Splitting them
    # across messages quietly teaches the model to stop calling tools in
    # parallel, which costs a turn per question thereafter.
    uses <- Filter(function(b) identical(b$type, "tool_use"), resp$content)
    results <- lapply(uses, function(u) {
      if (verbose) message("  -> ", u$name, "(", tool_result_text(u$input), ")")
      r <- dispatch_tool(u$name, u$input, tools = tools)
      tool_calls[[length(tool_calls) + 1L]] <<- list(name = u$name, input = u$input,
                                                     ok = r$ok)
      list(type = "tool_result", tool_use_id = u$id,
           content = r$text, is_error = !r$ok)
    })
    messages <- c(messages, list(list(role = "user", content = results)))
  }

  list(answer = paste0("Stopped after ", max_turns,
                       " turns without a final answer."),
       refusal = FALSE, messages = messages, tool_calls = tool_calls,
       usage = usage, turns = max_turns)
}
