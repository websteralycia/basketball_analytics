###############################################
# NCAA WBB Team Four-Factors Leaderboard (Light Theme)
# Compare ONE team to EVERY team in its conference on
# the Four Factors -- e.g. Utah State vs. every Mountain
# West team, one ranked dot-chart panel per factor.
#
# Built for coach-facing viewing: raw values (not
# percentiles), every conference team visible by name,
# conference average marked as a reference line.
#
# Template: 7.2_wbb_team_pizza_plot.R (same data pipeline,
#           steps 1-6 are unchanged from that script)
# Data:     wehoop load_wbb_player_box (college stats)
#           + ESPN conference/standings API for membership
#
# HOW TO USE: edit the CONFIG block (section 0). Pick the
# school + conference + season.
###############################################

# 0. Install packages (run once if needed)
# install.packages(c("wehoop","dplyr","readr","ggplot2",
#                     "tidyr","tibble","httr","jsonlite",
#                     "tidytext","scales"))

library(wehoop)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tibble)
library(httr)
library(jsonlite)
library(tidytext)   # reorder_within / scale_y_reordered for per-facet ranking
library(scales)     # percent-formatted axis labels

###############################################
# CONFIG: edit these to change the plot
###############################################

school          <- "Michigan"               # team to feature (exact or partial)
conference_name <- "Big Ten"                # comparison pool (partial match OK)
season          <- 2026                        # wehoop uses END year (2026 = 2025-26)

# School accent color for the highlighted dots/segments.
ACCENT_COLOR    <- "#0033A0"

# Tip: source list_wbb_conferences.R or run get_wbb_conferences()
# (defined below) to print every conference name you can pass.

###############################################
# Conference helpers (ESPN API)
###############################################

# Catalog of conferences: group_id + name (~33 conferences)
get_wbb_conferences <- function() {
  u <- paste0(
    "http://site.api.espn.com/apis/site/v2/sports/basketball/",
    "womens-college-basketball/scoreboard/conferences?seasontype=2"
  )
  j  <- jsonlite::fromJSON(httr::content(httr::RETRY("GET", u),
                                         as = "text", encoding = "UTF-8"))
  cf <- j[["conferences"]]
  tibble(
    group_id   = as.integer(cf$groupId),
    name       = cf$name,
    short_name = cf$shortName
  ) %>%
    filter(!group_id %in% c(0, 50)) %>%   # drop "All"/divisional roll-ups
    arrange(name)
}

# Team IDs that belong to a conference, via the standings endpoint
get_conference_teams <- function(group_id, season) {
  u <- paste0(
    "https://site.web.api.espn.com/apis/v2/sports/basketball/",
    "womens-college-basketball/standings?season=", season,
    "&group=", group_id
  )
  j <- jsonlite::fromJSON(httr::content(httr::RETRY("GET", u),
                                        as = "text", encoding = "UTF-8"),
                          flatten = TRUE)
  e <- j$standings$entries
  tibble(
    team_id       = as.character(e$team.id),
    team_name     = e$team.displayName,
    team_location = e$team.location
  )
}

# Resolve the configured conference name -> group_id (partial, case-insensitive)
conf_catalog <- get_wbb_conferences()
conf_match   <- conf_catalog %>%
  filter(grepl(conference_name, name, ignore.case = TRUE))

if (nrow(conf_match) == 0) {
  message("No conference matched '", conference_name,
          "'. Available conferences:")
  print(conf_catalog, n = nrow(conf_catalog))
  stop("Set `conference_name` to one of the names listed above.")
}
if (nrow(conf_match) > 1) {
  message("'", conference_name, "' matched multiple conferences; using the first:")
  print(conf_match)
}
conf_match <- conf_match[1, ]

conf_teams    <- get_conference_teams(conf_match$group_id, season)
conf_team_ids <- conf_teams$team_id

message("Conference: ", conf_match$name,
        " (group ", conf_match$group_id, ") - ",
        nrow(conf_teams), " teams")

# Resolve the focal school -> team_id.
#
# Partial matching is the convenience here -- "Utah State" rather than the full
# "Utah State Aggies" -- but a substring can hit more than one team: "Michigan"
# is inside both "Michigan" and "Michigan State". ESPN returns Michigan State
# first, so silently taking the first row handed back the wrong team's chart
# with no warning.
#
# So: try an exact match on the location or display name first, and only fall
# back to substring matching. If the substring is still ambiguous, stop and
# list the candidates rather than guessing.
school_lc <- tolower(school)

focal_match <- conf_teams %>%
  filter(tolower(team_location) == school_lc | tolower(team_name) == school_lc)

if (nrow(focal_match) == 0) {
  # fixed = TRUE so a "." in a name like "St. John's" is a literal dot
  focal_match <- conf_teams %>%
    filter(grepl(school_lc, tolower(team_name),     fixed = TRUE) |
           grepl(school_lc, tolower(team_location), fixed = TRUE))
}

if (nrow(focal_match) == 0) {
  message("No team matched '", school, "' in ", conf_match$name,
          ". Teams in this conference:")
  print(conf_teams, n = nrow(conf_teams))
  stop("Set `school` to one of the teams listed above.")
}
if (nrow(focal_match) > 1) {
  message("'", school, "' matches ", nrow(focal_match), " teams in ",
          conf_match$name, ":")
  print(as.data.frame(focal_match[, c("team_id", "team_name", "team_location")]))
  stop("Set `school` to one of the names above so the match is unambiguous.")
}
focal_match   <- focal_match[1, ]
focal_team_id <- focal_match$team_id
message("Focal team: ", focal_match$team_name, " (id ", focal_team_id, ")")

###############################################
# 1. Pull ALL game-level player box data
###############################################

wbb_player_box <- load_wbb_player_box(seasons = season)

###############################################
# 2. Clean out DNPs / no-minutes rows
###############################################

wbb_player_box_clean <- wbb_player_box %>%
  filter(
    !isTRUE(did_not_play),
    !is.na(minutes),
    minutes > 0
  )

# Uncomment to restrict to regular season only:
# wbb_player_box_clean <- wbb_player_box_clean %>%
#   filter(season_type == 2)

###############################################
# 3. Team-game totals (adds points + 3PM so we can
#    build eFG% and the full four factors)
###############################################

team_game_totals <- wbb_player_box_clean %>%
  group_by(season, game_id, team_id) %>%
  summarise(
    team_pts   = sum(points, na.rm = TRUE),
    team_fgm   = sum(field_goals_made, na.rm = TRUE),
    team_fga   = sum(field_goals_attempted, na.rm = TRUE),
    team_fg3m  = sum(three_point_field_goals_made, na.rm = TRUE),
    team_fg3a  = sum(three_point_field_goals_attempted, na.rm = TRUE),
    team_ftm   = sum(free_throws_made, na.rm = TRUE),
    team_fta   = sum(free_throws_attempted, na.rm = TRUE),
    team_oreb  = sum(offensive_rebounds, na.rm = TRUE),
    team_dreb  = sum(defensive_rebounds, na.rm = TRUE),
    team_tov   = sum(turnovers, na.rm = TRUE),
    .groups = "drop"
  )

# Attach the opponent's totals for the same game (defensive four factors).
team_game_w_opp <- team_game_totals %>%
  left_join(
    team_game_totals,
    by = c("season", "game_id"),
    suffix = c("", "_opp"),
    relationship = "many-to-many"
  ) %>%
  filter(team_id != team_id_opp)

###############################################
# 4. Team-season totals (team + opponent), season-long
#    vs ALL opponents (not just conference games)
###############################################

team_season_totals <- team_game_w_opp %>%
  group_by(season, team_id) %>%
  summarise(
    team_fgm_total  = sum(team_fgm),   team_fga_total  = sum(team_fga),
    team_fg3m_total = sum(team_fg3m),  team_ftm_total  = sum(team_ftm),
    team_fta_total  = sum(team_fta),   team_oreb_total = sum(team_oreb),
    team_dreb_total = sum(team_dreb),  team_tov_total  = sum(team_tov),
    opp_fgm_total   = sum(team_fgm_opp),  opp_fga_total  = sum(team_fga_opp),
    opp_fg3m_total  = sum(team_fg3m_opp), opp_fta_total  = sum(team_fta_opp),
    opp_oreb_total  = sum(team_oreb_opp), opp_dreb_total = sum(team_dreb_opp),
    opp_tov_total   = sum(team_tov_opp),
    .groups = "drop"
  ) %>%
  mutate(team_id = as.character(team_id))

# Team display names + location for labels, from the box data.
team_lookup <- wbb_player_box_clean %>%
  mutate(team_id = as.character(team_id)) %>%
  group_by(team_id) %>%
  summarise(
    team_short = last(team_short_display_name),
    team_name  = last(team_display_name),
    .groups = "drop"
  )

###############################################
# 5. Four Factors (offense + defense) per team
###############################################

team_factors <- team_season_totals %>%
  left_join(team_lookup, by = "team_id") %>%
  mutate(
    # --- Offense ---
    efg     = if_else(team_fga_total > 0,
                      (team_fgm_total + 0.5 * team_fg3m_total) / team_fga_total,
                      NA_real_),
    tov_pct = if_else((team_fga_total + 0.44 * team_fta_total + team_tov_total) > 0,
                      team_tov_total /
                        (team_fga_total + 0.44 * team_fta_total + team_tov_total),
                      NA_real_),
    oreb_pct = if_else((team_oreb_total + opp_dreb_total) > 0,
                       team_oreb_total / (team_oreb_total + opp_dreb_total),
                       NA_real_),
    ft_rate  = if_else(team_fga_total > 0, team_fta_total / team_fga_total, NA_real_),

    # --- Defense (opponent's offense) ---
    opp_efg     = if_else(opp_fga_total > 0,
                          (opp_fgm_total + 0.5 * opp_fg3m_total) / opp_fga_total,
                          NA_real_),
    opp_tov_pct = if_else((opp_fga_total + 0.44 * opp_fta_total + opp_tov_total) > 0,
                          opp_tov_total /
                            (opp_fga_total + 0.44 * opp_fta_total + opp_tov_total),
                          NA_real_),
    dreb_pct    = if_else((team_dreb_total + opp_oreb_total) > 0,
                          team_dreb_total / (team_dreb_total + opp_oreb_total),
                          NA_real_),
    opp_ft_rate = if_else(opp_fga_total > 0, opp_fta_total / opp_fga_total, NA_real_)
  )

###############################################
# 6. Metric config: which panels, labels, and which
#    direction counts as "good" (used for ranking + the
#    arrow annotation on each panel, since raw values don't
#    self-explain direction the way a percentile does)
###############################################

metric_config <- tibble::tribble(
  ~metric,       ~label,          ~higher_better,
  # Offense
  "efg",         "eFG%",          TRUE,
  "tov_pct",     "TOV%",          FALSE,
  "oreb_pct",    "OREB%",         TRUE,
  "ft_rate",     "FT Rate",       TRUE,
  # Defense
  "opp_efg",     "Opp eFG%",      FALSE,
  "opp_tov_pct", "Opp TOV%",      TRUE,
  "dreb_pct",    "DREB%",         TRUE,
  "opp_ft_rate", "Opp FT Rate",   FALSE
) %>%
  mutate(facet_label = paste0(label,
                               if_else(higher_better, "  \u25B2 higher better",
                                                       "  \u25BC lower better")))

# Which panels to draw. Default = all 8 (offense + defense).
# For the OFFENSIVE four factors only, comment the line below and
# uncomment the next one:
selected_metrics <- metric_config$metric
# selected_metrics <- c("efg", "tov_pct", "oreb_pct", "ft_rate")

metric_config <- metric_config %>% filter(metric %in% selected_metrics)

###############################################
# 7. Restrict to conference; build long-format raw data
#    (NO percentile transform -- raw values only)
###############################################

team_factors_conf <- team_factors %>%
  filter(team_id %in% conf_team_ids)

if (!focal_team_id %in% team_factors_conf$team_id) {
  stop("Focal team '", focal_match$team_name,
       "' has no box-score data yet for season ", season, ".")
}

# Conference average = leave-one-out mean of the OTHER teams (raw units).
conf_avg <- team_factors_conf %>%
  filter(team_id != focal_team_id) %>%
  summarise(across(all_of(metric_config$metric), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "ConfAvg")

# One row per team x metric.
strip_data <- team_factors_conf %>%
  select(team_id, team_name, all_of(metric_config$metric)) %>%
  pivot_longer(all_of(metric_config$metric), names_to = "metric", values_to = "Value") %>%
  left_join(metric_config, by = "metric") %>%
  left_join(conf_avg, by = "metric") %>%
  mutate(
    is_focal = team_id == focal_team_id,
    Metric   = factor(facet_label, levels = metric_config$facet_label)
  ) %>%
  filter(!is.na(Value))

# Console readout: focal team vs conference average (raw values).
cat("\n", focal_match$team_name, " vs. rest of ", conf_match$short_name,
    " (", season - 1, "-", substr(season, 3, 4), ")\n", sep = "")
strip_data %>%
  filter(is_focal) %>%
  transmute(Metric = label, Team = round(Value, 3), ConfOthers = round(ConfAvg, 3)) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

###############################################
# 8. Ranked leaderboard plot -- one panel per factor,
#    every conference team as a dot, sorted best-to-worst
###############################################

# Order teams within each facet by value; direction-aware so
# "better" always lands at the top regardless of higher/lower-is-better.
strip_data <- strip_data %>%
  group_by(Metric) %>%
  mutate(order_key = if_else(higher_better, Value, -Value)) %>%
  ungroup() %>%
  mutate(team_ord = tidytext::reorder_within(team_name, order_key, Metric))

BG_COLOR   <- "white"
TEXT_COLOR <- "grey15"
DOT_GREY   <- "grey65"
AVG_COLOR  <- "grey40"

strip_plot <- ggplot(strip_data, aes(y = team_ord, x = Value)) +
  geom_vline(aes(xintercept = ConfAvg),
             color = AVG_COLOR, linetype = "dashed", linewidth = 0.4) +
  geom_segment(aes(x = 0, xend = Value, yend = team_ord,
                   color = is_focal, linewidth = is_focal)) +
  geom_point(aes(color = is_focal, size = is_focal)) +
  geom_text(aes(label = scales::percent(Value, accuracy = 0.1),
                fontface = if_else(strip_data$is_focal, "bold", "plain")),
            hjust = -0.25, size = 2.6, color = TEXT_COLOR) +
  scale_color_manual(values = c(`TRUE` = ACCENT_COLOR, `FALSE` = DOT_GREY), guide = "none") +
  scale_linewidth_manual(values = c(`TRUE` = 1, `FALSE` = 0.5), guide = "none") +
  scale_size_manual(values = c(`TRUE` = 3, `FALSE` = 1.6), guide = "none") +
  tidytext::scale_y_reordered() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                      expand = expansion(mult = c(0.05, 0.3))) +
  facet_wrap(~ Metric, scales = "free", ncol = 2) +
  labs(
    title    = paste0(focal_match$team_name, " Four Factors | ",
                       season - 1, "-", substr(season, 3, 4), " Season"),
    subtitle = paste0("Ranked vs. every ", conf_match$short_name,
                       " team  \u2022  dashed line = conference average (excl. ",
                       focal_match$team_name, ")"),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background     = element_rect(fill = BG_COLOR, color = NA),
    panel.background    = element_rect(fill = BG_COLOR, color = NA),
    panel.grid.minor    = element_blank(),
    panel.grid.major.y  = element_blank(),
    panel.spacing       = unit(1.2, "lines"),
    strip.text          = element_text(face = "bold", color = TEXT_COLOR, size = 10),
    axis.text           = element_text(color = TEXT_COLOR, size = 7.5),
    plot.title          = element_text(face = "bold", color = TEXT_COLOR, size = 15),
    plot.subtitle       = element_text(color = TEXT_COLOR, size = 9.5,
                                        margin = margin(b = 10))
  )

# Where the chart is saved.
# Change this one line to send it somewhere else, e.g. "~/Documents/scouting".
save_dir <- "~/Desktop"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

output_dir <- save_dir

conf_slug <- gsub("[^a-z0-9]+", "_", tolower(conf_match$short_name))
conf_slug <- gsub("^_|_$", "", conf_slug)
team_slug <- gsub("[^a-z0-9]+", "_", tolower(focal_match$team_name))
team_slug <- gsub("^_|_$", "", team_slug)

ggsave(
  filename = file.path(
    output_dir,
    paste0("team_four_factors_leaderboard_",
           team_slug, "_", conf_slug, "_", season, ".png")
  ),
  plot   = strip_plot,
  width  = 10, height = 14, dpi = 300, bg = BG_COLOR
)

message("Saved four-factors leaderboard for ", focal_match$team_name,
        " vs ", conf_match$short_name, ".")