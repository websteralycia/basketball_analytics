###############################################
# NBA Player Pizza Plot (Dark Theme)
# Source: hoopR (ESPN)  -- or a CSV, see section 1
# Note: bars are weighted percentiles WITHIN the player's position group.
#       The shaded ghost bar is that position's weighted average.
#       Turnovers are inverted (1 - pctile) so outward always means better.
#       Output: PNG
#
# Ported from 04_wnba/scripts/r/01_wnba_pizza_plot.R
# Differences from the WNBA original:
#   - hoopR instead of wehoop; NBA headshot CDN path
#   - the ~200 lines of metric computation are gone: they now live in
#     nba_player_metrics.R, shared with build_nba_player_dataset.R
#   - positions collapse to G/F/C. ESPN's NBA labels put only 7-10 players in
#     SG/PG/PF/SF, and a percentile inside a 7-player group can only land on
#     1/7ths -- confident-looking noise. See nba_position_group().
#   - the accent colour is pulled from the team's own ESPN colour, not hardcoded
###############################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tidyr)
  library(cowplot)
  library(magick)
  library(hoopR)
  library(knitr)
})

find_script_dir <- function() {
  for (d in c(".", "scripts/r", "../r",
              "03_nba/player_pizza_plot/scripts/r", "03_nba/scripts/r")) {
    if (file.exists(file.path(d, "nba_player_metrics.R"))) return(normalizePath(d))
  }
  stop("Cannot find nba_player_metrics.R from ", getwd(), call. = FALSE)
}
script_dir <- find_script_dir()
source(file.path(script_dir, "nba_player_metrics.R"))
source(file.path(script_dir, "nba_metrics.R"))
source(file.path(script_dir, "nba_data.R"))

# Where everything this script writes is saved -- the chart and the CSV.
# Change this one line to send them somewhere else, e.g. "~/Documents/scouting".
save_dir <- "~/Desktop"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

output_dir <- save_dir   # charts
data_dir   <- save_dir   # CSVs

###############################################
# 1. CONFIG -- set the player, and the input
###############################################

MAIN_PLAYER <- "Shai Gilgeous-Alexander"
SEASON      <- 2025            # end year: 2025 = the 2024-25 season
SEASON_TYPE <- 2               # 2 = regular season
LABEL       <- "2024-25 Season"

# Leave NULL to pull from hoopR; set a path to use a player-box CSV instead.
INPUT_CSV   <- NULL

if (is.null(INPUT_CSV)) {
  message("Pulling player box scores from hoopR ...")
  player_box <- load_nba_player_box(seasons = SEASON)
} else {
  message("Reading ", INPUT_CSV, " ...")
  player_box <- read_csv(INPUT_CSV, show_col_types = FALSE)
}

if (!is.null(SEASON_TYPE) && "season_type" %in% names(player_box)) {
  player_box <- player_box %>% filter(season_type == SEASON_TYPE)
}

# season_type == 2 still contains the All-Star tournament and the NBA Cup
# final -- see build_nba_player_dataset.R for what that costs if left in.
if (identical(SEASON_TYPE, 2) && is.null(INPUT_CSV)) {
  player_box <- nba_drop_nonstandard(player_box)
}

###############################################
# 2. Build player-season metrics
###############################################

player_season <- nba_player_season(player_box, collapse_positions = TRUE,
                                   include_pos_avgs = TRUE)

player_row <- player_season %>% filter(athlete_display_name == MAIN_PLAYER)

if (nrow(player_row) == 0) {
  close <- player_season$athlete_display_name[
    agrepl(MAIN_PLAYER, player_season$athlete_display_name, max.distance = 0.3)]
  stop("Player not found: ", MAIN_PLAYER,
       if (length(close)) paste0("\nDid you mean: ", paste(head(close, 5), collapse = ", ")) else "",
       call. = FALSE)
}
player_row <- player_row[1, ]

POS <- player_row$position[1]
pos_label <- c(G = "Guards", F = "Forwards", C = "Centers")[POS]
if (is.na(pos_label)) pos_label <- paste0(POS, "s")

player_team <- player_row$team_short_display_name[1]

###############################################
# 2.5 Print the player against their position average
###############################################

pos_avg <- player_season %>%
  filter(position == POS) %>%
  summarise(across(c(mpg, ppg, rpg, apg, spg, bpg, tovpg),
                   ~ round(mean(.x, na.rm = TRUE), 2))) %>%
  mutate(player = paste0(POS, " Avg"))

bind_rows(
  player_row %>%
    transmute(player = athlete_display_name, mpg, ppg, rpg, apg, spg, bpg, tovpg),
  pos_avg %>% select(player, mpg, ppg, rpg, apg, spg, bpg, tovpg)
) %>%
  kable(format = "simple", digits = 2) %>%
  print()

###############################################
# 3. Theme colours
###############################################
# Accent comes from the team's own ESPN colour rather than a hardcoded hex, so
# the plot re-themes itself when you change the player.

team_hex <- player_box %>%
  filter(team_id == player_row$team_id[1], !is.na(team_color)) %>%
  slice(1) %>% pull(team_color)

BACKGROUND_COLOR <- "#1a1a1a"
LINE_COLOR   <- if (length(team_hex) && !is.na(team_hex)) paste0("#", team_hex) else "#C8102E"
BAR_FILL     <- "#0C2340"
LETTER_COLOR <- "white"
AVG_BAR_COLOR <- "white"

###############################################
# 4. Metrics on the pizza
###############################################

key_metrics_pctile <- c(
  "ts_pctile_pos", "ast_pctile_pos", "dreb_pctile_pos", "oreb_pctile_pos",
  "usage_pctile_pos", "tov_pctile_pos", "fg3_pctile_pos", "ft_pctile_pos"
)
key_metrics_avg_pctile <- c(
  "ts_pos_avg_pctile", "ast_pos_avg_pctile", "dreb_pos_avg_pctile",
  "oreb_pos_avg_pctile", "usage_pos_avg_pctile", "tov_pos_avg_pctile",
  "fg3_pos_avg_pctile", "ft_pos_avg_pctile"
)
metric_labels <- c(
  ts = "True Shooting", ast = "Assists", dreb = "Def. Rebound",
  oreb = "Off. Rebound", usage = "Usage", tov = "Turnovers",
  fg3 = "3PT", ft = "Free Throw"
)
short_name <- function(x) sub("_(pctile_pos|pos_avg_pctile)$", "", x)

# Player bars. Turnovers inverted so that further out is always better.
plot_data <- player_row %>%
  select(player = athlete_display_name, all_of(key_metrics_pctile)) %>%
  mutate(across(all_of(key_metrics_pctile), ~ replace_na(.x, 0))) %>%
  mutate(tov_pctile_pos = 1 - tov_pctile_pos) %>%
  pivot_longer(-player, names_to = "Metric", values_to = "Percentile") %>%
  mutate(Metric = unname(metric_labels[short_name(Metric)]))

# Position-average ghost bars, inverted to match
avg_ring <- player_row %>%
  select(player = athlete_display_name, all_of(key_metrics_avg_pctile)) %>%
  mutate(tov_pos_avg_pctile = 1 - tov_pos_avg_pctile) %>%
  pivot_longer(-player, names_to = "Metric", values_to = "AvgPctile") %>%
  mutate(Metric = unname(metric_labels[short_name(Metric)]))

plot_data <- plot_data %>%
  left_join(avg_ring %>% select(Metric, AvgPctile), by = "Metric") %>%
  mutate(Metric = factor(Metric, levels = unique(Metric)))

###############################################
# 5. Build the pizza
###############################################

pizza <- ggplot(plot_data, aes(x = Metric, y = Percentile)) +

  geom_bar(width = 0.85, stat = "identity",
           fill = BAR_FILL, color = LINE_COLOR, linewidth = 1) +

  # Ghost bar: semi-transparent white over the player bar. Where the average
  # is higher the white shows; where the player is higher, their colour does.
  geom_bar(aes(y = AvgPctile), width = 0.85, stat = "identity",
           fill = AVG_BAR_COLOR, alpha = 0.25,
           color = AVG_BAR_COLOR, linewidth = 1) +

  geom_text(aes(y = 1.20, label = Metric), vjust = -0.5,
            color = LETTER_COLOR, size = 4, fontface = "bold") +
  geom_label(aes(y = 1.20, label = sprintf("%.0f%%", Percentile * 100)),
             vjust = 1.5, color = LETTER_COLOR, size = 3.5, fontface = "bold",
             fill = BACKGROUND_COLOR,
             label.r = unit(0.3, "lines"), label.padding = unit(0.2, "lines"),
             linewidth = 0) +

  geom_hline(yintercept = c(0.25, 0.5, 0.75),
             color = LETTER_COLOR, linetype = "dashed", linewidth = 0.25) +

  coord_polar(clip = "off") +
  scale_y_continuous(limits = c(-0.3, 1.35), breaks = c(0.25, 0.5, 0.75),
                     expand = c(0, 0)) +
  theme_minimal() +
  theme(
    legend.position    = "none",
    panel.background   = element_rect(fill = BACKGROUND_COLOR, color = NA),
    plot.background    = element_rect(fill = BACKGROUND_COLOR, color = NA),
    panel.grid         = element_blank(),
    axis.text          = element_blank(),
    axis.title         = element_blank(),
    panel.border       = element_blank()
  )

###############################################
# 6. Headshot + labels
###############################################

player_circle <- tryCatch(
  magick::image_read(player_row$headshot_url[1]),
  error = function(e) {
    message("Headshot not found for ", MAIN_PLAYER, " - skipping image.")
    NULL
  }
)

final_plot <- cowplot::ggdraw() + cowplot::draw_plot(pizza)

if (!is.null(player_circle)) {
  final_plot <- final_plot +
    cowplot::draw_image(player_circle, x = 0.5, y = 0.54,
                        width = 0.09, height = 0.17, hjust = 0.5, vjust = 0.7)
}

final_plot <- final_plot +
  cowplot::draw_label(
    paste0(MAIN_PLAYER, " | ", player_team, " | ", LABEL),
    x = 0.5, y = 0.96, hjust = 0.5, vjust = 1,
    fontface = "bold", color = LETTER_COLOR, size = 14) +
  cowplot::draw_label(
    paste0("Percentile Profile vs. All NBA ", pos_label, " | Per 36 Min"),
    x = 0.5, y = 0.932, hjust = 0.5, vjust = 1,
    color = LETTER_COLOR, size = 11) +
  cowplot::draw_label("●", x = 0.30, y = 0.075, hjust = 0.5, vjust = 1,
                      color = LINE_COLOR, size = 14) +
  cowplot::draw_label(MAIN_PLAYER, x = 0.40, y = 0.07, hjust = 0.5, vjust = 1,
                      color = LETTER_COLOR, size = 9, fontface = "italic") +
  cowplot::draw_label("●", x = 0.55, y = 0.075, hjust = 0.5, vjust = 1,
                      color = AVG_BAR_COLOR, size = 14) +
  cowplot::draw_label(paste0(pos_label, " Average (shaded)"),
                      x = 0.665, y = 0.07, hjust = 0.5, vjust = 1,
                      color = LETTER_COLOR, size = 9, fontface = "italic")

###############################################
# 7. Save
###############################################

out_file <- file.path(
  output_dir,
  paste0("pizza_plot_", gsub("[^A-Za-z0-9]+", "_", MAIN_PLAYER), "_", SEASON, ".png"))

ggsave(out_file, final_plot, width = 8, height = 8, dpi = 300,
       bg = BACKGROUND_COLOR)

message("Saved ", out_file)
