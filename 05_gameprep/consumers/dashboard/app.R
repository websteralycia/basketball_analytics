## ============================================================
## app.R — the gameprep dashboard
## ------------------------------------------------------------
## Surface 1 and 2 of the platform, over the same layer the card uses:
## pick a matchup, read the scouting card, ask questions about it.
##
##   Rscript -e 'shiny::runApp("11_gameprep_project/consumers/dashboard")'
##
## WHY SHINY, FOR NOW. A real Q&A agent needs somewhere server-side to
## hold an API key. Shiny already is a server, so scout_ask() is called
## directly with no endpoint, no CORS and no auth layer, and
## render_pregame_card() drops in as HTML untouched. If this moves to
## React later, nothing in R/ changes -- only this file, which is the
## cheapest part of the stack to rewrite. That is the whole argument.
##
## NO NETWORK AT STARTUP. Populations and teams come from the cached
## profiles on disk; cards read the Tier 1 cache. The one network call
## is the season team box for the four-factor tiles, fetched once per
## league-season and held for the session -- without it the tiles show
## em dashes rather than invented numbers, so the app is usable while it
## loads and correct if it fails.
## ============================================================

suppressMessages({
  library(shiny)
  library(bslib)
})

GP_ROOT <- Sys.getenv("GAMEPREP_ROOT", unset = "")
if (!nzchar(GP_ROOT)) {
  GP_ROOT <- file.path(Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics"),
                       "11_gameprep_project")
}
source(file.path(GP_ROOT, "source_all.R"))
for (f in c("keys_to_the_game.R", "render_pregame_card.R", "build_card.R")) {
  source(file.path(GP_ROOT, "consumers", "gameprep_cards", f))
}

# Every league the layer supports, scanned for cached profiles. Only leagues
# that actually have one are offered -- a league in the dropdown with nothing
# behind it is a dead end, and the list grows on its own as profiles get built.
GP_LEAGUES <- c(wbb = "NCAA Women's", wnba = "WNBA",
                mbb = "NCAA Men's",   nba = "NBA")

PROFILES_BY_LEAGUE <- Filter(
  function(d) nrow(d) > 0,
  stats::setNames(lapply(names(GP_LEAGUES), list_cached_profiles), names(GP_LEAGUES)))

if (!length(PROFILES_BY_LEAGUE)) {
  stop("No cached profiles found for any league.\n",
       "  Build one with build_team_play_profile(league, season, population).",
       call. = FALSE)
}

LEAGUE_CHOICES <- stats::setNames(names(PROFILES_BY_LEAGUE),
                                  GP_LEAGUES[names(PROFILES_BY_LEAGUE)])

pop_choices_for <- function(league) {
  d <- PROFILES_BY_LEAGUE[[league]]
  stats::setNames(paste(d$population, d$season, sep = "|"),
                  paste0(d$population, " ", d$season))
}

ui <- page_fillable(
  # Dark, and pinned rather than following the OS. The card has its own
  # light/dark rules keyed off prefers-color-scheme; leaving the app on a
  # fixed light preset meant a browser in dark mode rendered a dark card on
  # a white page. The two grounds below are the card's OWN tokens --
  # --surface-2 behind --surface-1 -- so the card nests on the page instead
  # of looking pasted onto it.
  theme = bs_theme(version = 5, preset = "shiny",
                   base_font = font_google("Inter", local = FALSE),
                   bg = "#0d0d0d", fg = "#e8e8e4",
                   primary = "#5b8fd6"),
  padding = 0,
  # Pin the card to dark too, so it cannot drift from the page it sits on.
  tags$head(tags$script(HTML(
    "document.documentElement.setAttribute('data-theme','dark');"))),
  tags$style(HTML("
    .gp-bar{display:flex;gap:14px;align-items:end;padding:10px 16px;
      border-bottom:1px solid rgba(128,128,128,.25);flex-wrap:wrap;}
    .gp-bar .form-group{margin-bottom:0;}
    .gp-bar label{font-size:11px;text-transform:uppercase;letter-spacing:.05em;
      opacity:.65;margin-bottom:2px;}
    .gp-brand{font-weight:700;font-size:15px;margin-right:6px;white-space:nowrap;}
    /* The sidebar is a column: status pinned at the top, the ask box pinned
       at the bottom, and the log taking whatever is left. A fixed
       calc(100vh - 210px) height guessed at the chrome above and below it and
       was wrong at most window sizes -- it pushed the ask box off-screen on
       short windows and left a dead gap on tall ones. */
    .gp-side{display:flex;flex-direction:column;height:100%;gap:10px;}
    .gp-chat-log{flex:1 1 auto;min-height:0;overflow-y:auto;padding-right:6px;}
    .gp-ask{flex:0 0 auto;}
    .gp-ask .form-group{margin-bottom:6px;}
    .bslib-sidebar-layout>.sidebar>.sidebar-content{height:100%;overflow:hidden;}
    .gp-msg{margin-bottom:12px;font-size:13px;line-height:1.45;}
    .gp-msg-you{font-weight:650;}
    .gp-msg-a > :first-child{margin-top:0;}
    .gp-msg-a > :last-child{margin-bottom:0;}
    .gp-msg-a h1,.gp-msg-a h2,.gp-msg-a h3{font-size:13px;font-weight:700;
      letter-spacing:.02em;margin:14px 0 6px;}
    .gp-msg-a p{margin:0 0 8px;}
    .gp-msg-a ul,.gp-msg-a ol{margin:0 0 8px;padding-left:18px;}
    .gp-msg-a li{margin-bottom:4px;}
    .gp-msg-a code{font-size:12px;padding:1px 4px;border-radius:3px;
      background:rgba(128,128,128,.18);}
    .gp-msg-a strong{font-weight:650;}
    .gp-tools{font-size:11px;opacity:.6;margin-top:4px;font-family:ui-monospace,monospace;}
    .gp-note{font-size:12px;opacity:.7;padding:10px 16px;}

    /* The card is one column of rows, so it should not stretch to fill a
       1400px pane -- but it should not hug the left edge either, which is
       what left the dead gutter. Centre it and give it a little more room. */
    /* align-items:flex-start is load-bearing. A flex container stretches its
       items to the container height by default, which pinned the card box to
       the viewport while its content ran on past -- the background stopped
       halfway down the card and the rest sat on the page ground. flex-start
       lets the card size to its own content. */
    .gp-cardwrap{display:flex;justify-content:center;align-items:flex-start;
      padding:20px 24px 32px;overflow-y:auto;height:100%;}
    .gp-cardwrap .gp-card{max-width:780px;width:100%;}

    /* Match the card: same family, and the same nesting of surfaces. */
    body,.gp-card{font-family:Inter,system-ui,-apple-system,'Segoe UI',sans-serif;}
    .gp-bar{background:#0d0d0d;}
    .bslib-sidebar-layout>.sidebar{background:#111110;}
  ")),

  div(class = "gp-bar",
      span(class = "gp-brand", "Gameprep"),
      selectInput("league", "League", choices = LEAGUE_CHOICES, width = "175px"),
      selectInput("pop", "Population",
                  choices = pop_choices_for(names(PROFILES_BY_LEAGUE)[1]),
                  width = "190px"),
      selectInput("team", "My team", choices = NULL, width = "130px"),
      selectInput("opp", "Opponent", choices = NULL, width = "130px")),

  layout_sidebar(
    sidebar = sidebar(
      width = 340, position = "right", open = TRUE,
      title = "Ask about this matchup",
      div(class = "gp-side",
        uiOutput("chat_status"),
        div(class = "gp-chat-log", uiOutput("chat_log")),
        div(class = "gp-ask",
          textAreaInput("q", NULL, placeholder = "What should we take away on defense?",
                        rows = 2, width = "100%"),
          div(style = "display:flex;gap:8px;",
              actionButton("ask", "Ask", class = "btn-sm btn-primary"),
              actionButton("clear", "Clear", class = "btn-sm btn-outline-secondary"))))),
    div(class = "gp-cardwrap", uiOutput("card")))
)

server <- function(input, output, session) {

  # --- selections -------------------------------------------------
  # Changing league repoints the population list. Everything downstream keys
  # off input$league, so nothing else has to know which league is showing.
  observeEvent(input$league, {
    updateSelectInput(session, "pop", choices = pop_choices_for(input$league))
  }, ignoreInit = TRUE)

  pop_parts <- reactive({
    req(input$league, input$pop)
    # updateSelectInput has to round-trip to the browser, so for one tick
    # after a league change input$pop still holds the PREVIOUS league's
    # population. Loading that combination throws ("no cached profile at
    # nba/profiles/big_ten_...") and wedges the app. req() makes every
    # downstream reactive wait for the client to catch up instead.
    req(input$pop %in% pop_choices_for(input$league))
    p <- strsplit(input$pop, "|", fixed = TRUE)[[1]]
    list(population = p[1], season = as.integer(p[2]))
  })

  profile <- reactive({
    pp <- pop_parts()
    load_cached_profile(pp$population, pp$season, input$league)
  })

  observeEvent(profile(), {
    teams <- sort(unique(profile()$team))
    sel_team <- if ("MICH" %in% teams) "MICH" else teams[1]
    updateSelectInput(session, "team", choices = teams, selected = sel_team)
    updateSelectInput(session, "opp", choices = teams,
                      selected = setdiff(teams, sel_team)[1])
  })

  # One team-box fetch per league-season, held for the session. Without
  # it the four-factor tiles render as em dashes, which is the correct
  # failure: absent, not invented.
  # Cached per league-season, not once per session. A single latch was fine
  # when the league was fixed; with a league selector it would hand the new
  # league the previous one's team box, and the four-factor tiles would show
  # the wrong numbers rather than none.
  #
  # The result is wrapped in a list so that a FAILED fetch caches too --
  # storing a bare NULL would look like "not fetched yet" and retry the same
  # failing pull on every reactive invalidation.
  box_cache <- reactiveValues()
  team_box <- reactive({
    pp  <- pop_parts()
    key <- paste(input$league, pp$season, sep = "|")
    hit <- box_cache[[key]]
    if (!is.null(hit)) return(hit$value)
    tb <- tryCatch(league_cfg(input$league)$team_box_fn(pp$season),
                   error = function(e) NULL)
    box_cache[[key]] <- list(value = tb)
    tb
  })

  # --- the card ---------------------------------------------------
  card_html <- eventReactive(
    list(input$team, input$opp, input$pop, input$league), {
      req(input$team, input$opp)
      validate(need(input$team != input$opp,
                    "Pick two different teams."))
      pp <- pop_parts()
      withProgress(message = "Building card", value = 0.4, {
        tryCatch(
          build_pregame_card(input$league, pp$season, pp$population,
                             input$team, input$opp,
                             profile = profile(), team_box = team_box()),
          error = function(e) paste0("<p class='gp-note'>Could not build this card: ",
                                     conditionMessage(e), "</p>"))
      })
    }, ignoreNULL = FALSE)

  output$card <- renderUI({
    HTML(card_html())
  })

  # --- chat -------------------------------------------------------
  have_key <- nzchar(Sys.getenv("ANTHROPIC_API_KEY", unset = ""))
  history  <- reactiveVal(NULL)
  log      <- reactiveVal(list())

  output$chat_status <- renderUI({
    if (have_key) return(NULL)
    div(class = "gp-note",
        "Set ANTHROPIC_API_KEY and restart to enable questions. ",
        "The card works without it.")
  })

  observeEvent(input$clear, { history(NULL); log(list()) })

  # Clearing on matchup change is deliberate: the context is pinned to
  # the first message, so carrying a conversation across matchups would
  # have the agent answering about the previous opponent.
  observeEvent(list(input$team, input$opp, input$pop), {
    history(NULL); log(list())
  }, ignoreInit = TRUE)

  observeEvent(input$ask, {
    q <- trimws(input$q %||% "")
    req(nzchar(q))
    if (!have_key) {
      showNotification("ANTHROPIC_API_KEY is not set.", type = "error")
      return()
    }
    pp <- pop_parts()
    log(c(log(), list(list(role = "you", text = q))))
    updateTextAreaInput(session, "q", value = "")

    res <- withProgress(message = "Thinking", value = 0.3, {
      tryCatch(
        scout_ask(q,
                  context = list(league = input$league, season = pp$season,
                                 population = pp$population,
                                 my_team = input$team, opponent = input$opp),
                  history = history()),
        error = function(e) list(answer = paste0("Error: ", conditionMessage(e)),
                                 messages = history(), tool_calls = list()))
    })

    history(res$messages)
    tools_used <- if (length(res$tool_calls))
      paste(unique(vapply(res$tool_calls, `[[`, character(1), "name")), collapse = ", ")
    else ""
    log(c(log(), list(list(role = "a", text = res$answer, tools = tools_used))))
  })

  output$chat_log <- renderUI({
    entries <- log()
    if (!length(entries)) {
      return(div(class = "gp-note",
                 "Ask about tendencies, matchups, or who else in the ",
                 "population does something. Answers come from the same ",
                 "numbers as the card."))
    }
    tagList(lapply(entries, function(m) {
      if (identical(m$role, "you")) {
        div(class = "gp-msg gp-msg-you", m$text)
      } else {
        div(class = "gp-msg",
            # The model answers in markdown -- headings, bold, bullets. Shown
            # as pre-wrap text that arrives as literal ## and ** on screen.
            # commonmark renders it; the sanitize guard is because this is
            # model output going into HTML.
            div(class = "gp-msg-a",
                HTML(commonmark::markdown_html(m$text, smart = TRUE,
                                               extensions = TRUE))),
            if (nzchar(m$tools %||% "")) div(class = "gp-tools", "via ", m$tools))
      }
    }))
  })
}

shinyApp(ui, server)
