# app.R
library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(htmltools)
library(glue)
library(DT)

###############################################
# 1) Load Data
###############################################
players_raw <- readr::read_csv("wnba_player_dataset_2025_lab.csv",
                               show_col_types = FALSE)

###############################################
# 2) Model Coefficients
###############################################
# Fitted league-wide on five seasons of WNBA team-game four factors:
#   lm(net_rating ~ efg_pct + tov_pct + oreb_pct + ft_rate)
# See scripts/r/01_wnba_four_factor_weights.R.
#
# WHAT THE OUTPUT IS, AND IS NOT. These coefficients were fitted on TEAM
# four factors but are applied below to usage-weighted averages of INDIVIDUAL
# player season rates. Those are not the same quantity -- team eFG runs about
# .520, the player average in this file is .4905 -- so the intercept sits
# roughly 9 points low: feed it league-average inputs and it returns -9.18,
# where a calibrated net-rating model would return 0.
#
# The offset is a constant, so the RANKING of lineups is unaffected, and
# ranking is what this tool is for. The number is therefore surfaced as a
# "Lineup fit score" rather than a net rating: read it as relative, not as
# points per 100 possessions. Making it absolute means refitting against
# observed lineup net ratings rather than relabelling the intercept.
intercept <- -72.88772
beta_efg  <- 144.50778
beta_tov  <- -93.57270
beta_oreb <- 47.19972
beta_ftr  <- 18.79830

###############################################
# 3) Position constraint helpers
###############################################
split_positions <- function(pos) unlist(strsplit(pos, "-"))

lineup_position_counts <- function(player_names, data) {
  data %>%
    filter(player %in% player_names) %>%
    rowwise() %>%
    mutate(pos_list = list(split_positions(position))) %>%
    tidyr::unnest(pos_list) %>%
    count(pos_list, name = "count")
}

is_valid_lineup <- function(player_names, data, constraints) {
  counts <- lineup_position_counts(player_names, data)

  for (pos in names(constraints)) {
    pos_count <- counts$count[counts$pos_list == pos]
    pos_count <- ifelse(length(pos_count) == 0, 0, pos_count)

    if (pos_count < constraints[[pos]]["min"] || pos_count > constraints[[pos]]["max"]) {
      return(FALSE)
    }
  }
  TRUE
}

###############################################
# 4) Prediction + optimization
###############################################
predict_lineup <- function(player_names, data) {
  selected_lineup <- data %>% filter(player %in% player_names)

  if (nrow(selected_lineup) != 5) {
    found <- selected_lineup$player
    missing <- setdiff(player_names, found)
    stop(paste("Missing players:", paste(missing, collapse = ", ")))
  }

  raw_sum <- sum(selected_lineup$usage, na.rm = TRUE)
  usage_for_stress <- if (raw_sum < 2.0) raw_sum * 100 else raw_sum
  usage_stress <- usage_for_stress / 100

  selected_lineup <- selected_lineup %>%
    mutate(
      adj_usage_share = usage / sum(usage),
      stressed_efg = efg_pct / (usage_stress^0.25),
      stressed_tov = tov_pct * (usage_stress^0.25)
    )

  lineup_efg  <- sum(selected_lineup$stressed_efg * selected_lineup$adj_usage_share, na.rm = TRUE)
  lineup_tov  <- sum(selected_lineup$stressed_tov * selected_lineup$adj_usage_share, na.rm = TRUE)
  lineup_oreb <- mean(selected_lineup$oreb_pct, na.rm = TRUE)
  lineup_ftr  <- mean(selected_lineup$fta_rate, na.rm = TRUE)

  projected_net_rating <- intercept +
    (beta_efg  * lineup_efg) +
    (beta_tov  * lineup_tov) +
    (beta_oreb * lineup_oreb) +
    (beta_ftr  * lineup_ftr)

  list(
    projection = round(projected_net_rating, 2),
    lineup_stats = data.frame(lineup_efg, lineup_tov, lineup_oreb, lineup_ftr),
    usage_total = usage_for_stress
  )
}

find_best_additions <- function(core_names, data, constraints, top_n = 6) {
  if (length(core_names) < 2 || length(core_names) > 4) {
    stop("Please enter between 2 and 4 core players.")
  }

  core_check <- data %>% filter(player %in% core_names)
  if (nrow(core_check) != length(core_names)) {
    missing <- setdiff(core_names, core_check$player)
    stop(paste("Missing players:", paste(missing, collapse = ", ")))
  }

  slots_to_fill <- 5 - length(core_names)
  candidates <- data %>% filter(!player %in% core_names)

  candidate_combos <- combn(candidates$player, slots_to_fill, simplify = FALSE)

  results <- vector("list", length(candidate_combos))
  for (i in seq_along(candidate_combos)) {
    lineup <- c(core_names, candidate_combos[[i]])
    if (!is_valid_lineup(lineup, data, constraints)) next

    pred <- tryCatch(predict_lineup(lineup, data), error = function(e) NULL)
    if (!is.null(pred)) {
      results[[i]] <- data.frame(
        added_players = paste(candidate_combos[[i]], collapse = ", "),
        projected_net = pred$projection
      )
    }
  }

  bind_rows(results) %>%
    arrange(desc(projected_net)) %>%
    head(top_n)
}

parse_added_players <- function(x) str_split(x, ",\\s*", simplify = FALSE)[[1]]

lineup_roster_df <- function(player_names, data) {
  data %>%
    filter(player %in% player_names) %>%
    transmute(
      player,
      position,
      team = if ("team" %in% names(data)) team else NA_character_,
      headshot_url = if ("headshot_url" %in% names(data)) headshot_url else NA_character_
    )
}

get_top_lineups <- function(core_names, data, constraints, top_n = 6) {
  opts <- find_best_additions(core_names, data, constraints, top_n)

  res <- lapply(seq_len(nrow(opts)), function(i) {
    added <- parse_added_players(opts$added_players[i])
    lineup <- c(core_names, added)

    pred <- predict_lineup(lineup, data)
    roster <- lineup_roster_df(lineup, data)

    list(
      rank = i,
      projection = pred$projection,
      usage_total = pred$usage_total,
      lineup_stats = pred$lineup_stats,
      roster = roster
    )
  })

  list(lineups = res, top_options = opts)
}

###############################################
# 5) Insight table helpers (role + why)
###############################################
clamp01 <- function(x) pmin(pmax(x, 0), 1)

build_insight_table <- function(players, top_options) {
  picked <- unique(unlist(strsplit(top_options$added_players, ",\\s*")))

  players %>%
    filter(player %in% picked) %>%
    mutate(
      usage_pctile_pos = clamp01(usage_pctile_pos),
      efg_pctile_pos   = clamp01(efg_pctile_pos),
      tov_pctile_pos   = clamp01(tov_pctile_pos),
      oreb_pctile_pos  = clamp01(oreb_pctile_pos),
      fta_rate_pctile_pos = clamp01(fta_rate_pctile_pos),

      role = case_when(
        efg_pctile_pos >= 0.75 & usage_pctile_pos <= 0.55 ~ "Spacer",
        tov_pctile_pos >= 0.75 & usage_pctile_pos <= 0.70 ~ "Connector",
        oreb_pctile_pos >= 0.70 | fta_rate_pctile_pos >= 0.70 ~ "Stabilizer",
        TRUE ~ "Balanced"
      ),

      r_efg  = efg_pctile_pos,
      r_tov  = tov_pctile_pos,
      r_oreb = oreb_pctile_pos,
      r_ftr  = fta_rate_pctile_pos,

      reason1 = case_when(
        r_efg  == pmax(r_efg, r_tov, r_oreb, r_ftr, na.rm = TRUE) ~ "High eFG",
        r_tov  == pmax(r_efg, r_tov, r_oreb, r_ftr, na.rm = TRUE) ~ "Low TOV",
        r_oreb == pmax(r_efg, r_tov, r_oreb, r_ftr, na.rm = TRUE) ~ "High OREB",
        r_ftr  == pmax(r_efg, r_tov, r_oreb, r_ftr, na.rm = TRUE) ~ "High FTr",
        TRUE ~ NA_character_
      ),

      r_efg2  = if_else(reason1 == "High eFG",  -Inf, r_efg),
      r_tov2  = if_else(reason1 == "Low TOV",   -Inf, r_tov),
      r_oreb2 = if_else(reason1 == "High OREB", -Inf, r_oreb),
      r_ftr2  = if_else(reason1 == "High FTr",  -Inf, r_ftr),

      reason2 = case_when(
        r_efg2  == pmax(r_efg2, r_tov2, r_oreb2, r_ftr2, na.rm = TRUE) ~ "High eFG",
        r_tov2  == pmax(r_efg2, r_tov2, r_oreb2, r_ftr2, na.rm = TRUE) ~ "Low TOV",
        r_oreb2 == pmax(r_efg2, r_tov2, r_oreb2, r_ftr2, na.rm = TRUE) ~ "High OREB",
        r_ftr2  == pmax(r_efg2, r_tov2, r_oreb2, r_ftr2, na.rm = TRUE) ~ "High FTr",
        TRUE ~ NA_character_
      ),

      why = if_else(!is.na(reason2) & reason2 != reason1,
                    paste0(reason1, " + ", reason2),
                    reason1)
    ) %>%
    select(
      player, position, role, why,
      games_played, usage, efg_pct, tov_pct, oreb_pct, fta_rate,
      usage_pctile_pos, efg_pctile_pos, tov_pctile_pos, oreb_pctile_pos, fta_rate_pctile_pos
    ) %>%
    arrange(desc(efg_pctile_pos))
}

###############################################
# 6) Card renderer for UI (returns tags, no file)
###############################################
DEFAULT_HEADSHOT <- "https://upload.wikimedia.org/wikipedia/commons/8/89/Portrait_Placeholder.png"

cards_ui <- function(lineups) {
  css <- "
  .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(360px, 1fr)); gap: 16px; }
  .card { border: 1px solid #e5e7eb; border-radius: 16px; padding: 16px; box-shadow: 0 1px 2px rgba(0,0,0,0.06); background: white; }
  .header { display:flex; align-items: baseline; justify-content: space-between; gap: 12px; margin-bottom: 10px; }
  .title { font-size: 16px; font-weight: 700; }
  .rating { font-size: 18px; font-weight: 800; }
  .sub { color: #6b7280; font-size: 12px; margin-top: 2px; }
  .row { display:flex; gap: 10px; align-items:center; padding: 8px 0; border-top: 1px solid #f3f4f6; }
  .row:first-of-type { border-top: none; }
  .img { width: 44px; height: 44px; border-radius: 999px; object-fit: cover; background: #f3f4f6; }
  .name { font-weight: 600; }
  .meta { color: #6b7280; font-size: 12px; }
  .stats { margin-top: 10px; padding-top: 10px; border-top: 1px solid #f3f4f6; display: flex; gap: 10px; flex-wrap: wrap; }
  .pill { font-size: 12px; padding: 6px 10px; border-radius: 999px; background: #f3f4f6; }
  "

  tagList(
    tags$style(HTML(css)),
    div(class = "grid",
        lapply(lineups, function(L) {
          s <- L$lineup_stats

          pills <- tagList(
            div(class="pill", glue("eFG: {round(s$lineup_efg, 3)}")),
            div(class="pill", glue("TOV: {round(s$lineup_tov, 3)}")),
            div(class="pill", glue("OREB: {round(s$lineup_oreb, 3)}")),
            div(class="pill", glue("FTr: {round(s$lineup_ftr, 3)}")),
            div(class="pill", glue("Usage: {round(L$usage_total, 1)}"))
          )

          roster_rows <- lapply(seq_len(nrow(L$roster)), function(i) {
            r <- L$roster[i, ]
            raw_url <- str_trim(as.character(r$headshot_url))
            img_src <- ifelse(!is.na(raw_url) && raw_url != "", raw_url, DEFAULT_HEADSHOT)

            div(class="row",
                tags$img(
                  src = img_src,
                  class = "img",
                  onerror = glue("this.onerror=null;this.src='{DEFAULT_HEADSHOT}';")
                ),
                div(
                  div(class="name", r$player),
                  div(class="meta", glue("{r$position}{if(!is.na(r$team) && r$team != '') glue(' • {r$team}') else ''}"))
                )
            )
          })

          div(class="card",
              div(class="header",
                  div(
                    div(class="title", glue("Lineup #{L$rank}")),
                    div(class="sub", "Lineup fit score")
                  ),
                  div(class="rating", glue("{L$projection}"))
              ),
              tagList(roster_rows),
              div(class="stats", pills)
          )
        })
    )
  )
}

###############################################
# 7) Constraint presets
###############################################
constraints_presets <- list(
  "Balanced" = list(G=c(min=1,max=3), F=c(min=1,max=3), C=c(min=1,max=2)),
  "Big"      = list(G=c(min=1,max=2), F=c(min=1,max=2), C=c(min=2,max=2)),
  "Small"    = list(G=c(min=2,max=4), F=c(min=2,max=3), C=c(min=0,max=1))
)

###############################################
# UI
###############################################
ui <- fluidPage(
  tags$div(style="margin: 18px 0 8px 0;",
           tags$h2("WNBA Lineup Optimizer"),
           tags$p(style="color:#6b7280;",
                  "Pick 2–4 core players, choose constraints, and generate top lineup fits + explanations.")
  ),

  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "core_players",
        "Core players (2–4):",
        choices = sort(unique(players_raw$player)),
        multiple = TRUE,
        options = list(placeholder = "Type a name…")
      ),
      selectInput("preset", "Constraint preset:", choices = names(constraints_presets), selected = "Balanced"),
      numericInput("min_games", "Minimum games played:", value = 24, min = 0, max = 40, step = 1),
      selectizeInput(
        "contract_types",
        "Include contract types:",
        choices = sort(unique(na.omit(players_raw$contract_type))),
        selected = sort(unique(na.omit(players_raw$contract_type))),  # default = all
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      numericInput("top_n", "How many lineups:", value = 6, min = 1, max = 12, step = 1),
      actionButton("run", "Run Optimization", class = "btn-primary"),
      tags$hr(),
      tags$small(style="color:#6b7280;",
                 "Tips: If you get no results, loosen constraints or reduce min games. First run can take a few minutes. 3-4 core players runs faster than 2, fewer combinations")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Lineup Cards", uiOutput("cards")),
        tabPanel("Why These Players", DTOutput("insight")),
        tabPanel("Method", tags$div(
          style="margin-top:12px;",
          tags$h4("What the model optimizes"),
          tags$ul(
            tags$li("Maximizes lineup eFG; penalizes lineup TOV; rewards OREB and FTr."),
            tags$li("Applies usage-stress diminishing returns (efficiency down, turnovers up as total usage rises)."),
            tags$li("Finds best additions to complete a 5-player lineup under position constraints.")
          ),
          tags$h4("Why players show up"),
          tags$p("Optimal additions tend to be efficient low-friction fits: spacers (high eFG), connectors (low TOV), and stabilizers (OREB/FTr)."),
          tags$h4("Reading the score"),
          tags$p(tags$b("The fit score ranks lineups; it is not points per 100 possessions."),
                 " The four-factor weights were fitted league-wide on team-game data, then applied here to usage-weighted averages of individual player rates. Those are different quantities, so the scale sits about 9 points low: league-average inputs return -9.18 rather than 0."),
          tags$p("The offset is constant, so the ordering of lineups is unaffected, which is what the tool is for. Compare lineups to each other, not to zero."),
          tags$h4("Where the weights came from"),
          tags$p("A regression of net rating on the four factors across five WNBA seasons. OREB and FT rate carry positive weight, which is what the league data says. The engine was built for the New York Liberty, who were weak on the offensive glass, so it surfaces lineups that shore that up. The weights are league-wide, not fitted to the Liberty.")
        ))
      )
    )
  )
)

###############################################
# Server
###############################################
server <- function(input, output, session) {

  results <- eventReactive(input$run, {
    core <- input$core_players

    validate(
      need(length(core) >= 2 && length(core) <= 4, "Select 2–4 core players.")
    )

    # Apply min games globally for this run
    players <- players_raw %>% filter(games_played >= input$min_games)

    # Filter candidate pool by contract type
    selected_ct <- input$contract_types
    if (!is.null(selected_ct) && length(selected_ct) > 0) {
      players_candidates <- players %>% filter(is.na(contract_type) == FALSE, contract_type %in% selected_ct)
    } else {
      players_candidates <- players
    }

    # Ensure core players are still present even if contract_type is NA or excluded
    players_for_opt <- bind_rows(
      players %>% filter(player %in% core),
      players_candidates
    ) %>% distinct(player, .keep_all = TRUE)


    # Ensure cores still exist after filter
    missing_core <- setdiff(core, players$player)
    validate(
      need(length(missing_core) == 0,
           paste0("These core players are below min games threshold: ", paste(missing_core, collapse = ", ")))
    )

    constraints <- constraints_presets[[input$preset]]

    out <- get_top_lineups(core, data = players_for_opt, constraints = constraints, top_n = input$top_n)
    validate(need(length(out$lineups) > 0, "No valid lineups found. Try loosening constraints or reducing min games."))

    insight <- build_insight_table(players_for_opt, out$top_options)

    list(lineups = out$lineups, insight = insight)
  })

  output$cards <- renderUI({
    req(results())
    cards_ui(results()$lineups)
  })

  output$insight <- renderDT({
    req(results())
    datatable(results()$insight, options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)