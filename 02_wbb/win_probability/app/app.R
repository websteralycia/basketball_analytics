## ============================================================
## app.R — NCAA women's basketball win probability calculator
## ------------------------------------------------------------
## A Shiny front end over 06_winprob_calc.R.
##
## Modelled on inpredictable's wpCalc — Quarter, Time Remaining, Score
## Difference, Possession — plus the thing theirs cannot do: name the two
## teams. Theirs is team-agnostic because a matchup would require a Vegas
## line, and NCAAW does not have one. Phase 3 built the substitute.
##
## Leave the matchup blank and set the site to Neutral and this reproduces
## inpredictable's calculator. Home or Away still applies the home-court
## term on its own — worth about 4 points here, too much to make a user
## name two teams to get at.
##
##   Rscript -e 'shiny::runApp("12_wbb_winprob_calc/app")'
## ============================================================

suppressMessages({
  library(shiny)
  library(dplyr)
})

R_HOME_DIR <- Sys.getenv("BBALL_HOME", unset = "~/Desktop/Basketball Analytics")
for (f in c("03_team_ratings", "04_training_set", "05_winprob_model", "06_winprob_calc"))
  source(file.path(R_HOME_DIR, "12_wbb_winprob_calc/scripts/r", paste0(f, ".R")))

MODEL <- load_winprob_fit()
FIT   <- MODEL$fit
BOARD <- readr::read_csv(file.path(WINPROB_ROOT, "data/tidy/wbb/rating_board.csv"),
                         show_col_types = FALSE)

TEAM_CHOICES <- c("— none (pick'em) —" = "", stats::setNames(BOARD$team_id, BOARD$display))

# Spread from the snapshot rather than the full table: same arithmetic as
# matchup_spread(), reading the 663-row board the app already has loaded.
#
# The teams are OPTIONAL, which is the one place this departs from
# matchup_spread(). With none named the rating difference is zero and the
# venue term is the entire spread — home court is a fact about the game,
# not about the matchup, so an even game at home is not a pick'em. At a
# typical pace that term is worth about +3.9 points.
board_spread <- function(t1, t2, venue, pace) {
  diff <- 0
  if (nzchar(t1) && nzchar(t2) && t1 != t2) {
    r1 <- BOARD$rating[BOARD$team_id == t1]
    r2 <- BOARD$rating[BOARD$team_id == t2]
    if (!length(r1) || !length(r2)) return(NA_real_)
    diff <- r1 - r2
  }
  edge <- switch(venue, home = BOARD$home_term[1], away = -BOARD$home_term[1], neutral = 0)
  round((diff + edge) * pace / 100, 1)
}

# Expected tempo for a matchup: the mean of the two teams' season pace.
# Crude — a real pace model would account for who controls tempo — but far
# better than a league constant, and it is the same quantity Phase 3 uses
# per game (the mean of the two teams' possession estimates).
board_pace <- function(t1, t2) {
  p1 <- BOARD$pace[BOARD$team_id == t1]
  p2 <- BOARD$pace[BOARD$team_id == t2]
  if (!length(p1) || !length(p2)) return(DEFAULT_PACE)
  round((p1 + p2) / 2, 1)
}

# Both teams chosen, and not the same team — the condition for the matchup
# controls to mean anything. Written once here and mirrored as a JS
# expression in the conditionalPanel below.
HAS_MATCHUP_JS <- "input.team1 != '' && input.team2 != '' && input.team1 != input.team2"

ui <- fluidPage(
  tags$head(
    # Theme is applied before the body paints, so a dark-mode user never sees
    # a white flash. Saved choice wins; with none saved we follow the OS.
    tags$script(HTML("
      (function () {
        var KEY = 'wpcalc-theme';
        var saved = null;
        try { saved = localStorage.getItem(KEY); } catch (e) {}
        if (saved === 'light' || saved === 'dark')
          document.documentElement.setAttribute('data-theme', saved);

        function effective() {
          return document.documentElement.getAttribute('data-theme') ||
                 (window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light');
        }
        function paintBtn() {
          var b = document.getElementById('themebtn');
          if (b) b.textContent = effective() === 'dark' ? 'Light' : 'Dark';
        }
        window.wpToggleTheme = function () {
          var next = effective() === 'dark' ? 'light' : 'dark';
          document.documentElement.setAttribute('data-theme', next);
          try { localStorage.setItem(KEY, next); } catch (e) {}
          paintBtn();
        };
        document.addEventListener('DOMContentLoaded', paintBtn);
      })();
    ")),
    tags$style(HTML("
    /* ------------------------------------------------------------------
       Palette. Every colour below is a token so light and dark stay in
       step -- change a value once, both themes follow. Light is the bare
       :root; dark is defined twice, once for the OS preference and once
       for an explicit toggle, so the button wins in both directions.
       ------------------------------------------------------------------ */
    :root {
      --bg: #f6f7f5;  --card: #ffffff;      --border: #dde3e0;
      --text: #131a18; --text-2: #3d4a46;   --muted: #6e7c78;
      --accent: #146b62; --track: #e6ebe9;
      --input-bg: #ffffff; --input-border: #ccd5d2;
      --btn-bg: #ffffff;
    }
    @media (prefers-color-scheme: dark) {
      :root:not([data-theme='light']) {
        --bg: #121614;  --card: #1b211f;     --border: #2c3633;
        --text: #e9eeec; --text-2: #b9c4c0;  --muted: #8b9a95;
        --accent: #4ecdc4; --track: #2c3633;
        --input-bg: #232b28; --input-border: #3a4642;
        --btn-bg: #232b28;
      }
    }
    :root[data-theme='dark'] {
      --bg: #121614;  --card: #1b211f;     --border: #2c3633;
      --text: #e9eeec; --text-2: #b9c4c0;  --muted: #8b9a95;
      --accent: #4ecdc4; --track: #2c3633;
      --input-bg: #232b28; --input-border: #3a4642;
      --btn-bg: #232b28;
    }

    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
           background: var(--bg); color: var(--text); }
    .wrap { max-width: 940px; margin: 0 auto; padding: 28px 18px 64px; }
    h2 { font-weight: 600; letter-spacing: -.01em; margin: 0 0 4px; }
    .sub { color: var(--muted); font-size: 14px; margin-bottom: 26px; }
    .card { background: var(--card); border: 1px solid var(--border); border-radius: 6px;
            padding: 18px 20px; margin-bottom: 18px; }
    .card h4 { font-size: 11px; letter-spacing: .14em; text-transform: uppercase;
               color: var(--muted); font-weight: 700; margin: 0 0 14px; }

    /* Header row: title on the left, theme button on the right. */
    .hdr { display: flex; justify-content: space-between; align-items: flex-start; gap: 16px; }
    .themebtn { background: var(--btn-bg); color: var(--text-2);
                border: 1px solid var(--border); border-radius: 5px;
                padding: 5px 13px; font-size: 12.5px; cursor: pointer;
                flex: none; margin-top: 3px; }
    .themebtn:hover { color: var(--text); border-color: var(--muted); }

    /* Label-left / control-right, the way inpredictable's form reads. The
       inputs are the point; the labels should sit quietly beside them
       rather than stacking above and doubling the height of every row. */
    .grid { display: grid; grid-template-columns: 148px 1fr; gap: 11px 14px;
            align-items: center; }
    .grid .lbl { font-size: 14px; color: var(--text-2); }
    .grid .form-group { margin-bottom: 0; }
    .grid .shiny-input-container { width: 100% !important; margin-bottom: 0; }
    .grid input[type=number], .grid select { height: 32px; padding: 2px 8px; font-size: 14px; }
    /* tight enough that all six periods sit on one row, as theirs do */
    .grid .radio-inline { margin-right: 9px; font-size: 13.5px; }
    .grid .radio-inline + .radio-inline { margin-left: 0; }
    .grid .shiny-options-group { margin-top: 0; }

    /* Bootstrap paints its own white on form controls, so the tokens have to
       be pushed onto them explicitly or the inputs stay light in dark mode. */
    .form-control, select.form-control, input[type=number].form-control,
    .grid input[type=number], .grid select {
      background-color: var(--input-bg); color: var(--text);
      border: 1px solid var(--input-border); box-shadow: none;
    }
    .form-control:focus { border-color: var(--accent); box-shadow: none; }
    label, .radio-inline, .radio label, .control-label { color: var(--text-2); }
    option { background: var(--input-bg); color: var(--text); }

    /* selectInput is selectize.js, whose menu is a div rather than native
       <option> elements -- so the rule above never reaches it and the list
       stays white on a dark page. These four cover the closed control, the
       open menu, its rows, and the highlighted row. */
    .selectize-input, .selectize-input.focus, .selectize-control.single .selectize-input {
      background: var(--input-bg) !important; color: var(--text) !important;
      border-color: var(--input-border) !important; box-shadow: none !important;
    }
    .selectize-input input, .selectize-input .item { color: var(--text) !important; }
    .selectize-dropdown, .selectize-dropdown-content {
      background: var(--card) !important; color: var(--text) !important;
      border-color: var(--input-border) !important;
    }
    .selectize-dropdown .option { color: var(--text) !important; }
    .selectize-dropdown .active, .selectize-dropdown .option:hover {
      background: var(--track) !important; color: var(--text) !important;
    }
    .selectize-control.single .selectize-input:after { border-top-color: var(--muted); }

    /* the clock: two narrow boxes with a colon, not two labelled fields */
    .clock { display: flex; align-items: center; gap: 7px; }
    .clock .shiny-input-container { width: 68px !important; }
    .clock .sep { font-weight: 600; color: var(--muted); }
    .narrow .shiny-input-container { width: 92px !important; }
    .wp { font-size: 62px; font-weight: 650; letter-spacing: -.02em; line-height: 1;
          font-variant-numeric: tabular-nums; color: var(--accent); }
    .wp-sub { color: var(--text-2); font-size: 15px; margin-top: 8px; }
    .meter { height: 12px; border-radius: 6px; background: var(--track); overflow: hidden;
             margin: 16px 0 6px; }
    .meter > div { height: 100%; background: var(--accent); }
    .meter-ends { display: flex; justify-content: space-between; font-size: 12px;
                  color: var(--muted); font-variant-numeric: tabular-nums; }
    .note { font-size: 12.5px; color: var(--muted); margin-top: 12px; line-height: 1.5; }
    .spreadline { font-size: 13.5px; color: var(--text-2); margin-top: 6px; }
  "))),
  div(class = "wrap",
    div(class = "hdr",
      div(
        h2("NCAA Women's Win Probability"),
        div(class = "sub",
            "Describe a moment in a game. Naming the teams is optional; the site always counts.")
      ),
      # Follows the OS by default; the button overrides and the choice sticks.
      tags$button(id = "themebtn", class = "themebtn",
                  onclick = "wpToggleTheme()", "Dark")
    ),

    fluidRow(
      column(7,
        div(class = "card",
          h4("Game state"),
          div(class = "grid",
            div(class = "lbl", "Quarter"),
            # One OT only. Every overtime is the same 5-minute game to the
            # model, so a second one would be a duplicate control, not a
            # new state — secs_left_from_clock() treats period 5 and 6
            # identically.
            div(radioButtons("period", NULL,
                             c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "OT" = 5),
                             selected = 4, inline = TRUE)),

            div(class = "lbl", "Time Remaining"),
            div(class = "clock",
                numericInput("mins", NULL, value = 2, min = 0, max = 10, step = 1),
                span(class = "sep", ":"),
                numericInput("secs", NULL, value = 0, min = 0, max = 59, step = 1)),

            div(class = "lbl", "Score Difference"),
            div(class = "narrow", numericInput("margin", NULL, value = 0, step = 1)),

            div(class = "lbl", "Possesion?"),
            div(radioButtons("poss", NULL, c("Y" = "Y", "N" = "N"),
                             inline = TRUE, selected = "Y")),

            # Lives here, not with the matchup, because it applies with or
            # without one: home court is worth points even between two
            # teams the model has no opinion about.
            div(class = "lbl", "Your team is"),
            div(radioButtons("venue", NULL,
                             c("Home" = "home", "Away" = "away", "Neutral" = "neutral"),
                             selected = "home", inline = TRUE))
          ),
          # NO FREE-THROW CONTROL, deliberately. wp_free_throw() in
          # 06_winprob_calc.R still derives free-throw states and is still
          # tested — it is only unexposed here.
          #
          # Two reasons. It was the one part of the pipeline resting on
          # assumption rather than measurement (the constants are measured,
          # the derivation itself is unvalidated). And on its own it answers
          # little: the question a staff actually has is "do we foul?",
          # which needs BOTH branches compared, plus a shooter-quality input
          # (fouling targets a bad shooter, not a 71% one) and explicit
          # elapsed time per branch. That is a separate feature.
          #
          # Keep the code: any win-probability CHART over a real game hits
          # free-throw moments constantly, and without this it would have
          # holes at every trip to the line.
        ),

        div(class = "card",
          h4("Matchup — optional"),
          div(class = "grid",
            div(class = "lbl", "Your team"),
            div(selectInput("team1", NULL, TEAM_CHOICES, selected = "")),

            div(class = "lbl", "Opponent"),
            div(selectInput("team2", NULL, TEAM_CHOICES, selected = ""))
          ),
          # NO SPREAD CONTROL. The model still takes a spread and the matchup
          # still produces one — see spread_val() in the server — it is only
          # unexposed. A number that is simultaneously an output of the
          # pickers and an input to the model reads as neither, and the
          # NCAAW scale (routinely +/-30, see NOTES_spread_substitute.md)
          # gives a user no intuition for what to type into it.
          #
          # The cost: no way to hand-set a line for a hypothetical matchup.
          # Restore this control, and drop spread_val() back to reading
          # input$spread, if that scenario ever needs to come back.
          # Hidden without a matchup, where it genuinely does nothing: pace
          # only ever converts a rating edge into points, so with no teams
          # chosen it is an inert control that looks live.
          conditionalPanel(HAS_MATCHUP_JS,
            div(class = "grid", style = "margin-top:11px",
              div(class = "lbl", "Expected possessions"),
              div(class = "narrow",
                  numericInput("pace", NULL, value = DEFAULT_PACE,
                               min = 50, max = 95, step = 0.5)))),
          div(class = "spreadline", textOutput("spread_note"))
        )
      ),

      column(5,
        div(class = "card",
          h4("Win probability"),
          div(class = "wp", textOutput("wp", inline = TRUE)),
          div(class = "wp-sub", textOutput("wp_sub")),
          div(class = "meter", div(style = "width:0%", id = "meterfill")),
          uiOutput("meter_ui"),
          div(class = "meter-ends", span("0%"), span("100%")),
          div(class = "note", textOutput("caveat"))
        ),
        div(class = "card",
          h4("About the matchup"),
          div(class = "note",
            "Naming both teams tilts the numbers toward the stronger one. Most ",
            "NCAAW games have no betting line, so that tilt comes from ",
            "opponent-adjusted efficiency ratings instead.",
            br(), br(),
            "Leave the matchup blank and the two teams are treated as equally ",
            "strong. Home court advantage still applies either way.",
            br(), br(),
            textOutput("model_note", inline = TRUE))
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Changing the MATCHUP re-estimates tempo. Watches the teams only, so the
  # user can still type over the pace afterwards without it snapping back.
  observeEvent(list(input$team1, input$team2), {
    if (nzchar(input$team1) && nzchar(input$team2) && input$team1 != input$team2) {
      updateNumericInput(session, "pace", value = board_pace(input$team1, input$team2))
    }
  })

  # The spread the model is fed. Always computed: with no matchup it is the
  # venue term alone, with one it is the rating gap plus the venue term.
  spread_val <- reactive({
    has_mu <- nzchar(input$team1) && nzchar(input$team2) &&
              input$team1 != input$team2
    # Without a matchup the pace control is hidden, so whatever sits in it
    # is stale rather than chosen — fall back to the league default so no
    # invisible input moves the answer.
    pace <- if (has_mu) input$pace else DEFAULT_PACE
    if (is.null(pace) || is.na(pace)) pace <- DEFAULT_PACE
    s <- board_spread(input$team1, input$team2, input$venue, pace)
    if (is.na(s)) 0 else s
  })

  secs_left <- reactive({
    m <- input$mins; s <- input$secs
    if (is.null(m) || is.na(m)) m <- 0
    if (is.null(s) || is.na(s)) s <- 0
    secs_left_from_clock(as.integer(input$period), m * 60 + s)
  })

  wp <- reactive({
    mg <- input$margin; if (is.null(mg) || is.na(mg)) mg <- 0
    # free_throws is left at its default of 0 — the app does not expose
    # free-throw states. See the note where the control used to be.
    win_probability(FIT,
                    period = as.integer(input$period),
                    clock = (if (is.na(input$mins)) 0 else input$mins) * 60 +
                            (if (is.na(input$secs)) 0 else input$secs),
                    margin = mg,
                    possession = input$poss == "Y",
                    spread = spread_val())
  })

  output$wp <- renderText(sprintf("%.1f%%", 100 * wp()))

  output$wp_sub <- renderText({
    t1 <- if (nzchar(input$team1)) BOARD$short[BOARD$team_id == input$team1] else "your team"
    sprintf("chance %s wins", t1)
  })

  output$meter_ui <- renderUI({
    tags$script(HTML(sprintf(
      "document.getElementById('meterfill').style.width='%s%%';", 100 * wp())))
  })

  output$spread_note <- renderText({
    if (!nzchar(input$team1) || !nzchar(input$team2) || input$team1 == input$team2)
      return("Optional — without it both teams are treated as equally strong. The site above still counts.")
    sprintf("Team strength applied, from the ratings as of %s.",
            format(BOARD$as_of[1]))
  })

  output$caveat <- renderText({
    s <- secs_left()
    if (s <= 0) return("Game over — this is arithmetic, not a model.")
    if (s <= 5) return("Under 5 seconds the model is only slightly better than 'whoever leads wins'.")
    if (as.integer(input$period) >= 5)
      return("Overtime is modelled as its own 5-minute game, not as a fourth quarter.")
    ""
  })

  output$model_note <- renderText({
    sprintf("Model fitted on %s possessions, %d-%d seasons.",
            format(MODEL$n, big.mark = ","), MODEL$seasons[1], MODEL$seasons[2])
  })
}

shinyApp(ui, server)
