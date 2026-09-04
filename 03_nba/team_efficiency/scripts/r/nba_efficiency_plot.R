###############################################
# NBA Team Efficiency Landscape
# Source: hoopR (ESPN)  -- or a CSV, see section 1
# Note: Offensive / Defensive / Net Rating for all 30 teams,
#       plotted with team logos on a zoomed quadrant chart.
#       Output: CSV, PNG (landscape + 4:5 Instagram, opaque + transparent)
#
# Ported from 04_wnba/team_efficiency/scripts/r/8.8_wnba_efficiency_aug_2026.R
# Differences from the WNBA original:
#   - hoopR instead of wehoop
#   - logos come from nbaplotR rather than hardcoded ESPN CDN URLs
#   - the live "top up" block is gone: it existed because the WNBA season was
#     in progress and the data release lagged by a week. Point this at a
#     completed season and there is nothing to top up.
#   - All-Star and NBA Cup final exclusion is handled by nba_drop_nonstandard()
###############################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(ggtext)
  library(nbaplotR)
  library(rlang)
})

# Resolve our own location so this runs from anywhere
find_script_dir <- function() {
  for (d in c(".", "scripts/r", "../r",
              "03_nba/team_efficiency/scripts/r", "03_nba/scripts/r")) {
    if (file.exists(file.path(d, "nba_metrics.R"))) return(normalizePath(d))
  }
  stop("Cannot find nba_metrics.R from ", getwd(), call. = FALSE)
}
script_dir <- find_script_dir()
source(file.path(script_dir, "nba_metrics.R"))
source(file.path(script_dir, "nba_data.R"))

# Where everything this script writes is saved -- the chart and the CSV.
# Change this one line to send them somewhere else, e.g. "~/Documents/scouting".
save_dir <- "~/Desktop"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

output_dir <- save_dir   # charts
data_dir   <- save_dir   # CSVs

###############################################
# 1. INPUT -- hoopR, or swap in a CSV
###############################################
# Leave INPUT_CSV as NULL to pull from hoopR.
# Set it to a file path to use a CSV instead. The CSV needs one row per team
# per game with these columns (any capitalisation):
#
#   game_id, team_id, pts, fga, fgm, fg3m, fta, oreb, dreb, tov
#
# If the names differ, pass a mapping to nba_standardize() below, e.g.
#   INPUT_MAP <- c(tov = "turnovers", fg3m = "threes_made")

SEASON      <- 2026          # end year: 2025 = the 2024-25 season
SEASON_TYPE <- "regular"     # "regular", "postseason", "play_in", "all"
INPUT_CSV   <- NULL
INPUT_MAP   <- NULL
LABEL       <- "2025-26"     # used in the title/filenames

if (is.null(INPUT_CSV)) {
  message("Pulling from hoopR ...")
  team_games <- nba_team_games(seasons = SEASON, season_type = SEASON_TYPE)
  source_txt <- "hoopR (ESPN)"
} else {
  message("Reading ", INPUT_CSV, " ...")
  team_games <- read_csv(INPUT_CSV, show_col_types = FALSE) %>%
    nba_standardize(INPUT_MAP) %>%
    nba_pair_opponents() %>%
    nba_add_metrics()
  source_txt <- basename(INPUT_CSV)
}

###############################################
# 2. Season efficiency per team
###############################################

# Group by whatever identity columns the source actually carries. A hoopR pull
# has all three; a hand-supplied CSV may only have team_id.
id_cols <- intersect(c("team_id", "team_display_name", "team_abbreviation"),
                     names(team_games))

team_efficiency <- team_games %>%
  nba_aggregate(!!!rlang::syms(id_cols))

# Logos need team_abbreviation. Without it, fall back to labelled points rather
# than failing -- the chart is still readable, just not branded.
USE_LOGOS <- "team_abbreviation" %in% names(team_efficiency)
if (!USE_LOGOS) {
  message("No team_abbreviation column: plotting labelled points instead of logos.")
  team_efficiency <- team_efficiency %>%
    mutate(plot_label = if ("team_display_name" %in% names(.)) team_display_name
                        else as.character(team_id))
}

###############################################
# 3. League averages for the crosshairs
###############################################

avg_ortg <- mean(team_efficiency$ortg, na.rm = TRUE)
avg_drtg <- mean(team_efficiency$drtg, na.rm = TRUE)

# Labels built FROM THE DATA rather than typed by hand, so they cannot drift
# out of sync with what is actually plotted.
gp_range <- range(team_efficiency$games)

subtitle_txt <- paste0(
  LABEL, " Season | <span style='color:#5d5eaa;'>",
  format(nrow(team_efficiency)), " teams, ",
  if (gp_range[1] == gp_range[2]) paste0(gp_range[1], " games each")
  else paste0(gp_range[1], "-", gp_range[2], " games"),
  "</span> | Per 100 Possessions")

# The caption names the estimator because the two sources someone is likely to
# check disagree. Measured against NBA.com's Advanced table on 2024-25, this
# formula reads ORtg and DRtg about 2 points LOW and pace about 1.8 HIGH,
# because NBA.com counts possessions from play-by-play rather than estimating
# them. Net Rating is unaffected -- the bias hits both sides equally.
# Games played reads as a RANGE only when teams differ -- mid-season, or a
# season with postponements. Once every team has played 82 a "82-82" range is
# just the same number twice, so collapse it. (Same rule as the subtitle.)
games_txt <- if (gp_range[1] == gp_range[2]) {
  paste0(gp_range[1], " games played")
} else {
  paste0(gp_range[1], "-", gp_range[2], " games played")
}

caption_txt <- paste0(
  "Data via ", source_txt,
  " · Regular season only; All-Star and NBA Cup final excluded · ",
  games_txt, "\n",
  "Possessions = FGA + 0.44×FTA + TOV − OREB")

###############################################
# 4. Plot
###############################################

# ---- Scale the box to the data -------------------------------------------
# The WNBA original hardcoded zoom_dist <- 12, which is wrong for the NBA: the
# furthest team sits only ~7.9 points from league average in both 2024-25 and
# 2025-26, so a +/-12 box leaves ~34% of the frame empty and squeezes all 30
# teams into the middle. That is what makes the logos read small.
#
# Deriving the box from the data fixes it for any season or league. HEADROOM
# keeps the outermost logo clear of the axis instead of clipped against it.
#
# Zooming in also REDUCES overlap: logo size is a fraction of the panel, so as
# the box tightens the points spread further apart while the logos stay the
# same pixel size.
#
# Set ZOOM_DIST to a number to override (e.g. to hold the scale fixed across
# seasons so two charts can be compared side by side).
ZOOM_DIST  <- NULL
HEADROOM   <- 1.18
LOGO_WIDTH <- 0.070   # was 0.058 in the WNBA original

# NOTE on the size/crowding tradeoff, measured on 2025-26:
#   zoom 12, logo 0.058 (WNBA setting) -> 25 overlapping pairs
#   zoom 9.5, logo 0.075               -> 25  (logos and spacing scale together)
#   zoom 9.5, logo 0.070               -> 23
# Tightening the box alone does NOT reduce overlap -- it enlarges the logos and
# the gaps by the same factor. Only a smaller logo fraction reduces it.
# Seven pairs are unfixable at any readable size: NO/CHI sit 0.43 points apart,
# BOS/NY 0.51, HOU/MIN 0.63. That is genuine data proximity, not a layout bug.

zoom_dist <- if (!is.null(ZOOM_DIST)) {
  ZOOM_DIST
} else {
  reach <- max(abs(c(range(team_efficiency$ortg, na.rm = TRUE) - avg_ortg,
                     range(team_efficiency$drtg, na.rm = TRUE) - avg_drtg)))
  ceiling(reach * HEADROOM * 2) / 2      # round up to the nearest 0.5
}
message(sprintf("Zoom box: +/-%.1f (furthest team %.1f from centre)",
                zoom_dist, zoom_dist / HEADROOM))

x_lims <- c(avg_ortg - zoom_dist, avg_ortg + zoom_dist)
y_lims <- c(avg_drtg + zoom_dist, avg_drtg - zoom_dist)
tick_by <- if (zoom_dist <= 6) 1 else if (zoom_dist <= 14) 2.5 else 5

# ---- Nudge overlapping logos apart, with leader lines ----------------------
# Seven pairs sit inside a logo-width of each other and no size setting fixes
# that. The honest fix is to move the LOGO and draw a thin line back to the
# team's true position, so the reader can still see where it actually sits.
# Nudging without the leader line would simply misplace the team.
#
# Displacement is capped at MAX_DRIFT points so a logo never wanders somewhere
# misleading, and the leader line is only drawn when the shift is visible.
NUDGE_OVERLAPS <- TRUE
MAX_DRIFT      <- 0.9    # points; hard ceiling on how far a logo may move

repel_xy <- function(x, y, min_dist, iters = 400, step = 0.12, max_drift = 0.9) {
  nx <- x; ny <- y
  for (k in seq_len(iters)) {
    moved <- FALSE
    for (i in seq_along(nx)) {
      for (j in seq_along(nx)) {
        if (i >= j) next
        dx <- nx[j] - nx[i]; dy <- ny[j] - ny[i]
        d  <- sqrt(dx^2 + dy^2)
        if (d < min_dist && d > 1e-9) {
          push <- (min_dist - d) / 2 * step
          ux <- dx / d; uy <- dy / d
          nx[i] <- nx[i] - ux * push; ny[i] <- ny[i] - uy * push
          nx[j] <- nx[j] + ux * push; ny[j] <- ny[j] + uy * push
          moved <- TRUE
        } else if (d <= 1e-9) {
          # exactly coincident: break the tie so the vector is defined
          nx[j] <- nx[j] + min_dist * 0.01
        }
      }
    }
    if (!moved) break
  }
  # Clamp total drift so no logo strays far from its real position
  ddx <- nx - x; ddy <- ny - y
  dist <- sqrt(ddx^2 + ddy^2)
  over <- dist > max_drift & dist > 0
  nx[over] <- x[over] + ddx[over] / dist[over] * max_drift
  ny[over] <- y[over] + ddy[over] / dist[over] * max_drift
  list(x = nx, y = ny)
}

team_efficiency$lx <- team_efficiency$ortg
team_efficiency$ly <- team_efficiency$drtg

if (NUDGE_OVERLAPS && USE_LOGOS) {
  r <- repel_xy(team_efficiency$ortg, team_efficiency$drtg,
                min_dist = LOGO_WIDTH * 2 * zoom_dist * 0.92,
                max_drift = MAX_DRIFT)
  team_efficiency$lx <- r$x
  team_efficiency$ly <- r$y
  shifted <- sqrt((r$x - team_efficiency$ortg)^2 + (r$y - team_efficiency$drtg)^2)
  message(sprintf("Nudged %d logos (max %.2f pts, mean %.2f)",
                  sum(shifted > 0.05), max(shifted), mean(shifted[shifted > 0.05])))
}

p <- ggplot(team_efficiency, aes(x = ortg, y = drtg)) +
  geom_vline(xintercept = avg_ortg, linetype = "dashed", color = "grey70") +
  geom_hline(yintercept = avg_drtg, linetype = "dashed", color = "grey70") +

  # Net-rating diagonal, drawn to the corners of the zoomed box.
  # annotate() not geom_segment(aes()) -- the latter inherits the 30-row data
  # and redraws the identical segment 30 times, which ggplot warns about.
  annotate("segment",
           x = avg_ortg - zoom_dist, y = avg_drtg - zoom_dist,
           xend = avg_ortg + zoom_dist, yend = avg_drtg + zoom_dist,
           linetype = "dotted", color = "grey50") +

  # Leader lines: drawn BEFORE the logos so they sit underneath, and only for
  # teams that actually moved.
  {if (USE_LOGOS && NUDGE_OVERLAPS)
     geom_segment(data = subset(team_efficiency,
                    sqrt((lx - ortg)^2 + (ly - drtg)^2) > 0.12),
                  aes(x = ortg, y = drtg, xend = lx, yend = ly),
                  color = "grey55", linewidth = 0.3)
   else NULL} +

  # Logos via nbaplotR -- no hardcoded CDN URLs to go stale
  {if (USE_LOGOS)
     nbaplotR::geom_nba_logos(aes(x = lx, y = ly, team_abbr = team_abbreviation),
                              width = LOGO_WIDTH, alpha = 0.95)
   else list(geom_point(size = 2.5, color = "grey30"),
             geom_text(aes(label = plot_label), size = 2.4,
                       vjust = -1, color = "grey30"))} +

  # Tick spacing scales with the box: every 5 points was fine for a +/-12 WNBA
  # frame but leaves only ~4 ticks on a +/-9 one.
  scale_x_continuous(limits = x_lims, breaks = seq(0, 200, by = tick_by)) +
  scale_y_reverse(limits = y_lims, breaks = seq(0, 200, by = tick_by)) +
  coord_fixed(ratio = 1) +

  annotate("text", x = avg_ortg - zoom_dist * 0.9, y = avg_drtg - zoom_dist * 0.9,
           label = "(+) Net Rating", angle = -45, vjust = -1,
           color = "grey50", size = 2.5, fontface = "italic") +
  annotate("text", x = avg_ortg + zoom_dist * 0.9, y = avg_drtg + zoom_dist * 0.9,
           label = "(-) Net Rating", angle = -45, vjust = 1.5,
           color = "grey50", size = 2.5, fontface = "italic") +

  labs(
    title    = "NBA Efficiency Landscape",
    subtitle = subtitle_txt,
    x = "Offensive Efficiency (higher = better) →",
    y = "Defensive Efficiency (lower = better) → ",
    caption = caption_txt
  ) +
  theme_classic() +
  theme(
    plot.margin   = margin(t = 15, r = 20, b = 10, l = 20, unit = "pt"),
    plot.title    = element_markdown(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5, size = 9),
    axis.title.x  = element_text(margin = margin(t = 10, b = 15)),
    axis.title.y  = element_text(margin = margin(r = 10)),
    plot.caption  = element_text(color = "grey40", size = 6.5, hjust = 0,
                                 lineheight = 1.3, margin = margin(t = 10))
  )

print(p)

###############################################
# 5. Save CSV  (data/) and plots (outputs/)
###############################################

stem <- paste0("NBA_Efficiency_", gsub("-", "_", LABEL))

# any_of() not c(): `pace` is absent when the input is a CSV without game
# length, and hardcoding the list would fail on exactly the path we want to work.
team_efficiency %>%
  mutate(across(any_of(c("ortg", "drtg", "net_rtg", "pace", "efg",
                         "tov_pct", "orb_pct", "ft_rate", "ts_pct")),
                ~ round(.x, 4))) %>%
  write_csv(file.path(data_dir, paste0(stem, ".csv")))
message("Saved CSV: ", file.path(data_dir, paste0(stem, ".csv")))

# 9x8 not 10x7: coord_fixed() holds the panel SQUARE, so on a 10x7 canvas the
# panel is limited by the 7in height and ~5in of width goes to empty margin.
# Squaring up the canvas grows the panel ~44% in area, which is what actually
# makes the logos read larger -- more than any width tweak.
ggsave(file.path(output_dir, paste0(stem, ".png")), p,
       width = 9, height = 8, dpi = 300, bg = "white")

ggsave(file.path(output_dir, paste0(stem, "_transparent.png")),
       p + theme(panel.background = element_rect(fill = "transparent", color = NA),
                 plot.background  = element_rect(fill = "transparent", color = NA)),
       width = 9, height = 8, dpi = 300, bg = "transparent")

###############################################
# 6. Instagram export (4:5 portrait)
###############################################
# A 10x7 landscape is the wrong SHAPE for a 4:5 post -- it leaves empty bands
# and floats the caption into the middle. Rendering at 4:5 directly fills the
# frame. Type has to grow too: sizes tuned for a 10in export are unreadable on
# a phone once the image is 8in wide. Defined once and reused by both exports.
ig_theme <- theme(
  plot.title    = element_markdown(face = "bold", size = 26, hjust = 0.5),
  plot.subtitle = element_markdown(hjust = 0.5, size = 12,
                                   margin = margin(t = 4, b = 12)),
  plot.caption  = element_text(color = "grey40", size = 8.5, hjust = 0.5,
                               lineheight = 1.4, margin = margin(t = 28)),
  axis.title.x  = element_text(size = 11, margin = margin(t = 10, b = 8)),
  axis.title.y  = element_text(size = 11, margin = margin(r = 10)),
  axis.text     = element_text(size = 10),
  plot.margin   = margin(t = 60, r = 24, b = 8, l = 24, unit = "pt")
)

ggsave(file.path(output_dir, paste0(stem, "_ig4x5.png")), p + ig_theme,
       width = 8, height = 10, dpi = 300, bg = "white")

# Transparent 4:5 for a coloured Canva background. NOTE the ink stays dark --
# it is the background being removed, not the text. Fine on white or any pale
# colour; a dark background needs a light-text variant, not this file.
ggsave(file.path(output_dir, paste0(stem, "_ig4x5_transparent.png")),
       p + ig_theme +
         theme(panel.background = element_rect(fill = "transparent", color = NA),
               plot.background  = element_rect(fill = "transparent", color = NA)),
       width = 8, height = 10, dpi = 300, bg = "transparent")

message("Saved 4 PNGs to ", output_dir)
