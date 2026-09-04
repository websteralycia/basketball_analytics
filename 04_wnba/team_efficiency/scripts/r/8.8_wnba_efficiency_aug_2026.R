###############################################
# 2026 WNBA Teams
# Source: wehoop (ESPN)
# Note: This script finds the Offensive Rating, Defensive Rating, and Net Rating of all teams
#       in addition to plotting the results on a graph.
#       Output: CSV, PNG
###############################################

###############################################
# 0. Install packages (run once if needed)
###############################################

# install.packages("wehoop")
library(dplyr)
library(tibble)
library(wehoop)
library(ggplot2)
library(ggimage) # Essential for plotting logos
library(readr)
library(nbaplotR)
library(ggtext)

###############################################
# 0.1 Load WNBA Logo
###############################################
#wnba_logo <- "https://a.espncdn.com/i/teamlogos/leagues/500/wnba.png"

###############################################
# 1. Pull Game Data
###############################################

wnba_player_box <- load_wnba_player_box(seasons = 2026)

###############################################
# 1.2 Top up with games the data release hasn't picked up yet
###############################################
# load_wnba_player_box() does NOT query ESPN -- it downloads a pre-built data
# release from the sportsdataverse repo, which lags live games by up to a week.
# On 2026-08-08 the release ended 2026-08-01 and 19 completed games were missing,
# so a chart labelled "as of August 8" was really as of August 1.
#
# The espn_wnba_* functions DO hit ESPN live, so we fetch the tail ourselves.
# Note load_wnba_schedule() is stale by the same amount and reports these games
# as not completed, so the list of finals has to come from the live scoreboard.
#
# Self-disabling: once the release catches up there are no dates to scan and
# this block is a no-op. Set to FALSE to pin the script to release data only.
TOPUP_LIVE <- TRUE

if (TOPUP_LIVE) {
  last_release <- max(as.Date(wnba_player_box$game_date), na.rm = TRUE)
  scan_dates   <- seq(last_release + 1, Sys.Date(), by = "day")

  # Live scoreboard, one call per day, keeping only games that actually finished
  new_game_ids <- unlist(lapply(scan_dates, function(d) {
    d  <- as.Date(d, origin = "1970-01-01")
    sb <- try(espn_wnba_scoreboard(season = format(d, "%Y%m%d")), silent = TRUE)
    if (inherits(sb, "try-error") || !NROW(sb)) return(NULL)
    as.character(sb$game_id[sb$status_name == "STATUS_FINAL"])
  }))

  if (length(new_game_ids)) {
    extra_box <- bind_rows(lapply(new_game_ids, function(g) {
      r <- try(espn_wnba_player_box(game_id = g), silent = TRUE)
      if (inherits(r, "try-error")) NULL else r
    }))

    # The live JSON and the release parquet type some columns differently
    # (game_date comes back character, for one), so coerce each live column to
    # whatever class the release uses before binding -- otherwise bind_rows errors.
    shared_cols <- intersect(names(wnba_player_box), names(extra_box))
    for (nm in shared_cols) {
      extra_box[[nm]] <- switch(class(wnba_player_box[[nm]])[1],
        "Date"      = as.Date(extra_box[[nm]]),
        "numeric"   = suppressWarnings(as.numeric(extra_box[[nm]])),
        "integer"   = suppressWarnings(as.integer(extra_box[[nm]])),
        "logical"   = as.logical(extra_box[[nm]]),
        "character" = as.character(extra_box[[nm]]),
        extra_box[[nm]])
    }

    wnba_player_box <- bind_rows(wnba_player_box,
                                 extra_box %>% select(all_of(shared_cols)))

    message(sprintf("Topped up %d live games (%s to %s); season now runs to %s",
                    length(new_game_ids), format(last_release + 1),
                    format(Sys.Date()),
                    format(max(as.Date(wnba_player_box$game_date), na.rm = TRUE))))
  }
}

###############################################
# 1.3 As-of cutoff (keep the chart reconcilable with published sources)
###############################################
# Because 1.2 pulls live from ESPN, this script can run AHEAD of wnba.com/stats
# and Basketball Reference, which ingest a game some hours after it finals. That
# is not an error, but someone checking a team's totals against those sites the
# same day would find a mismatch and reasonably conclude the chart is wrong.
#
# Seen 2026-08-08: our Lynx total was 3,054 over 33 games against BBRef's 2,956
# over 32 -- the whole 98-point gap was that day's game, which BBRef had not
# loaded yet. Only the two teams who played that day differed; the other 13 tied
# out exactly.
#
# Yesterday, so every number on the chart can be verified against a public
# source. Set to Sys.Date() to include today's finals instead, and accept that
# the teams who played today will not tie out until tomorrow.
AS_OF <- Sys.Date() - 1

wnba_player_box <- wnba_player_box %>%
  filter(as.Date(game_date) <= AS_OF)

###############################################
# 1.5 Regular season only (drop the All-Star game)
###############################################
# season_type is 2 for EVERY row, All-Star game included, so it cannot be used
# to filter. ESPN instead gives the All-Star sides their own synthetic team_ids
# (2026: TEAM SPOON 133383, TEAM COOP 133384), and the game is self-contained --
# no real franchise plays in it. So keeping only the real franchises removes it.
#
# Left unfiltered it adds two junk rows to the CSV and, worse, pulls the league
# averages that draw the crosshairs in section 6.1.
#
# Keep this list in sync with team_logos in section 5 -- same 15 team_ids.
wnba_franchise_ids <- c(
  3L, 5L, 6L, 8L, 9L, 11L, 14L, 16L, 17L, 18L, 19L, 20L,
  129689L,   # Golden State Valkyries
  132052L,   # Portland Fire
  131935L    # Toronto Tempo
)

wnba_player_box <- wnba_player_box %>%
  filter(team_id %in% wnba_franchise_ids)

###############################################
#. Team-Game Totals (Add Points)
###############################################

team_game_totals <- wnba_player_box %>%
  filter(!is.na(minutes), minutes > 0) %>%
  group_by(game_id, game_date, team_id, team_location, team_short_display_name) %>%
  summarise(
    pts   = sum(points, na.rm = TRUE),
    fga   = sum(field_goals_attempted, na.rm = TRUE),
    fta   = sum(free_throws_attempted, na.rm = TRUE),
    tov   = sum(turnovers, na.rm = TRUE),
    oreb  = sum(offensive_rebounds, na.rm = TRUE),
    .groups = "drop"
)

###############################################
# 2.5 Restrict to each team's first N games
###############################################
# Default: use every game played so far this season.
team_game_first_n <- team_game_totals

# Clear any stale max_games from a previous run so commenting out the filter
# block below actually reverts to the full season (otherwise the old value
# lingers in the R environment and the output keeps the "_firstN" suffix).
if (exists("max_games")) rm(max_games)

# ---- FIRST N GAMES FILTER --------------------------------------------------
# Change max_games to set how many games per team to include (chronological),
# or comment out this whole block to use the full season.
# max_games <- 20

# team_game_first_n <- team_game_totals %>%
#   group_by(team_id) %>%
#   arrange(game_date, game_id, .by_group = TRUE) %>%
#   slice_head(n = max_games) %>%
#   ungroup()
# ----------------------------------------------------------------------------

###############################################
# 3. Calculate Opponent Stats to get Defensive Rating
###############################################

# Left side uses the (optionally) first-N-games set, while the right side keeps
# the full totals so each game's opponent stats are always available.
team_game_w_opp <- team_game_first_n %>%
  left_join(
    team_game_totals,
    by = 'game_id',
    suffix = c("", "_opp"),
    relationship = "many-to-many"
  ) %>%
  filter(team_id != team_id_opp)

###############################################
# 4. Calculate Efficiency Metrics
###############################################

team_efficiency <- team_game_w_opp %>%
  group_by(team_id, team_short_display_name) %>%
  summarise(
    total_pts_scored = sum(pts),
    total_pts_allowed = sum(pts_opp),
    total_fga = sum(fga),
    total_fta = sum(fta),
    total_tov = sum(tov),
    total_oreb = sum(oreb),
    .groups = "drop"
  ) %>%
  mutate(
    # AJ Recommended Estimated Possesion Formula
    # FGA + .44 * FTA + TOV - OREB
    possessions = total_fga + (.44 * total_fta) + total_tov - total_oreb,

    # Offensive Rating: Points scored per 100 possessions
    ortg = 100 * (total_pts_scored / possessions),

    # Defensive Rating: Points allowed per 100 possessions
    drtg = 100 * (total_pts_allowed / possessions),

    # Net Rating
    net_rtg = ortg - drtg
  )

###############################################
# 5. Grab Team Logos from ESPN CDN (Hardcoded)
###############################################

# Hardcoded ESPN logo URLs to avoid the stats.wnba.com timeout error
# from wehoop::wnba_teams(). Covers all 13 standard teams plus the
# 2026 expansion teams (Portland Fire, Toronto Tempo).
# NOTE: ESPN team_ids for Portland Fire and Toronto Tempo are placeholders
# until ESPN finalizes IDs in box-score data. Update once confirmed.
team_logos <- tibble::tribble(
  ~team_id,      ~slug,
  20L,           "atl",     # Atlanta Dream
  19L,           "chi",     # Chicago Sky
  18L,           "conn",    # Connecticut Sun
  3L,            "dal",     # Dallas Wings
  5L,            "ind",     # Indiana Fever
  17L,           "lv",      # Las Vegas Aces
  6L,            "la",      # Los Angeles Sparks
  8L,            "min",     # Minnesota Lynx
  9L,            "ny",      # New York Liberty
  11L,           "phx",     # Phoenix Mercury
  14L,           "sea",     # Seattle Storm
  16L,           "wsh",     # Washington Mystics
  129689L,       "gs",      # Golden State Valkyries (2025 expansion)
  132052L,       "por",     # Portland Fire (2026 expansion) -- update team_id
  131935L,       "tor"      # Toronto Tempo (2026 expansion) -- update team_id
) %>%
  mutate(logo = paste0("https://a.espncdn.com/i/teamlogos/wnba/500/", slug, ".png")) %>%
  select(team_id, logo)

# Clean up the old table (remove the column of NAs if it exists)
team_efficiency <- team_efficiency %>% select(-any_of("logo"))

# Join Logos to efficiency data
team_efficiency <- team_efficiency %>%
  left_join(team_logos, by = "team_id")

###############################################
# 6. Plotting the Graph
###############################################

# 6.1 Calculate League Averages for the crosshairs
avg_ortg <- mean(team_efficiency$ortg, na.rm = TRUE)
avg_drtg <- mean(team_efficiency$drtg, na.rm = TRUE)

# 6.1b Labels built FROM THE DATA, not typed by hand
# The date used to be hardcoded ("Games as of August 8"), which meant it could
# drift from what was actually plotted without complaining -- and it did, by a
# week, when the wehoop release went stale. Deriving it makes it true by
# construction, so it cannot be wrong no matter what AS_OF or the feed do.
data_through <- max(as.Date(team_game_totals$game_date), na.rm = TRUE)
gp_range     <- range(table(team_game_totals$team_id))

subtitle_txt <- paste0(
  "2026 Season | <span style='color:#5d5eaa;'>Games through ",
  format(data_through, "%B "), as.integer(format(data_through, "%d")),
  "</span> | Per 100 Possessions")

# The caption exists for anyone checking these numbers against another source.
# It names the feed and states the possession estimator, because the two public
# sources someone is likely to check DISAGREE with each other:
#   - NBA.com / WNBA.com define possessions as FGA + 0.44*FTA - OREB + TOV.
#     That is the formula used here, so those ratings should line up.
#   - Basketball Reference uses Dean Oliver's instead: both teams' estimates
#     averaged, offensive rebounds weighted by rebound rate, and a 0.4 rather
#     than 0.44 coefficient on free throws.
# Measured on this data, Oliver's gives ORtg 1-3 points HIGHER for every team,
# and not by a constant, so it reorders teams slightly too. Points and games
# played reconcile against both sites; the RATINGS only reconcile with the one
# using the same estimator.
caption_txt <- paste0(
  "Data via wehoop (ESPN) · Regular season only, All-Star game excluded · ",
  gp_range[1], "-", gp_range[2], " games played\n",
  "Possessions = FGA + 0.44×FTA + TOV − OREB ")
  #"Possessions = FGA + 0.44×FTA + TOV − OREB #(not the Oliver estimate used by ",
  #"wnba.com / Basketball Reference)")

# 2. Set a fixed "Zoom" distance (usually 10-15 points is perfect for WNBA)
# This ensures the center is the average, but the box is much smaller.
zoom_dist <- 12

# 6.3 RECALCULATE LIMITS
x_lims <- c(avg_ortg - zoom_dist, avg_ortg + zoom_dist)
y_lims <- c(avg_drtg + zoom_dist, avg_drtg - zoom_dist)

# 6.4 Apply to your plot
ggplot(team_efficiency, aes(x = ortg, y = drtg)) +
  geom_vline(xintercept = avg_ortg, linetype = "dashed", color = "grey70") +
  geom_hline(yintercept = avg_drtg, linetype = "dashed", color = "grey70") +

  # Net Rating Diagonal line drawn perfectly in zoomed-in box
  geom_segment(aes(x = avg_ortg - zoom_dist, y = avg_drtg - zoom_dist,
                   xend = avg_ortg + zoom_dist, yend = avg_drtg + zoom_dist),
               linetype = "dotted", color = "grey50") +

  # 6.5 Add the Team Logos
  geom_image(aes(image = logo), size = 0.07, asp = 1.6) +

  # 6.6 Apply the centered limits
  # breaks at every 5; the wide seq is clipped to the limits automatically,
  # so ticks stay on round numbers no matter where the average lands.
  scale_x_continuous(limits = x_lims, breaks = seq(0, 200, by = 5)) +
  scale_y_reverse(limits = y_lims, breaks = seq(0, 200, by = 5)) +

  # This forces the X and Y units to be visually equal (square quadrants)
  coord_fixed(ratio = 1) +

  # 6.7 Net Rating annotations
  annotate(
    "text",
    x = avg_ortg - (zoom_dist * 0.9),
    y = avg_drtg - (zoom_dist * 0.9),
    label = "(+) Net Rating",
    angle = -45, vjust = - 1, color = "grey50", size = 2.5, fontface = "italic"
  ) +
  annotate(
    "text",
    x = avg_ortg + (zoom_dist * 0.9),
    y = avg_drtg + (zoom_dist * 0.9),
    label = "(-) Net Rating",
    angle = -45, vjust = 1.5, color = "grey50", size = 2.5, fontface = "italic"
  ) +

  # 6.8 Add Labels and Themes
  labs(
    title = "WNBA Efficiency Landscape",
    subtitle = subtitle_txt,
    x = "Offensive Efficiency (higher = better) \u2192",
    y = "Defensive Efficiency (lower = better) \u2192 ",
    caption = caption_txt
  ) +
  theme_classic() +
  theme(
    plot.margin = margin(t = 15, r = 20, b = 10, l = 20, unit = "pt"),
    plot.title = element_markdown(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5, size = 9),
    axis.title.x = element_text(margin = margin(t = 10, b = 15)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.caption = element_text(color = "grey40", size = 6.5, hjust = 0,
                                lineheight = 1.3, margin = margin(t = 10))
  )
###############################################
# 6. Saving the CSV
###############################################
# Build a filename suffix from the games filter so outputs auto-update when
# max_games changes (e.g. "_first20"). If the filter block is commented out,
# max_games won't exist and we fall back to "_fullseason".
games_suffix <- if (exists("max_games")) paste0("_first", max_games) else "_fullseason"

# Force numeric columns to numeric and round to 3 decimals so the CSV writes
# clean numbers (no scientific notation, no character coercion).
team_efficiency_out <- team_efficiency %>%
  mutate(across(c(total_pts_scored, total_pts_allowed, total_fga, total_fta,
                  total_tov, total_oreb, possessions, ortg, drtg, net_rtg),
                as.numeric)) %>%
  mutate(across(c(possessions, ortg, drtg, net_rtg), ~ round(.x, 3)))

# Where files land.
#   Default (nothing to set up): your Desktop.
#   Optional: set BBALL_HOME in ~/.Renviron to an analytics folder and these
#   route to <BBALL_HOME>/<league>/outputs|data/ instead.
league     <- "04_wnba"   # league folder this script belongs to
bball_home <- Sys.getenv("BBALL_HOME", unset = "")
output_dir <- if (nzchar(bball_home)) file.path(bball_home, league, "outputs") else "~/Desktop"
data_dir   <- if (nzchar(bball_home)) file.path(bball_home, league, "data") else "~/Desktop"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(
  team_efficiency_out,
  file = file.path(data_dir, paste0("WNBA_Efficiency_AUG_2026", games_suffix, ".csv"))
)

###############################################
# 6. Saving the Plot
###############################################
ggsave(
  filename = file.path(output_dir, paste0("WNBA_Efficiency_AUG_2026", games_suffix, ".png")),
  plot = last_plot(),
  width = 10, #10
  height = 7, #7
  dpi = 1200, #300
  bg = "white"
)

# Also save a transparent-background version
ggsave(
  filename = file.path(output_dir, paste0("WNBA_Efficiency_AUG_2026", games_suffix, "_transparent.png")),
  plot = last_plot() +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA)
    ),
  width = 10,
  height = 7,
  dpi = 1200,
  bg = "transparent"
)

###############################################
# 6.9 Instagram export (4:5 portrait)
###############################################
# The 10x7 landscape exports above are the wrong SHAPE for a 4:5 post. Canva
# scales them to fit the width, which leaves empty bands top and bottom and
# floats the caption up into the middle of the frame. Rendering at 4:5 directly
# means the image fills the post and the caption sits on the bottom edge.
#
# The type also has to grow: sizes tuned for a 10in-wide export are too small to
# read on a phone once the image is 8in wide. These overrides are applied to a
# COPY of the plot so the landscape versions above keep their original sizing.
#
# 8 x 10 in at 300 dpi = 2400 x 3000 px. Instagram displays 4:5 at 1080 x 1350,
# so this has room to spare and Canva can downscale cleanly.
# Defined ONCE and reused by both exports below. Copy-pasting it into the
# transparent version instead would mean every spacing tweak has to be made
# twice, and the two files would silently drift apart the first time one is
# forgotten.
ig_theme <-
    theme(
      plot.title    = element_markdown(face = "bold", size = 26, hjust = 0.5),
      plot.subtitle = element_markdown(hjust = 0.5, size = 12,
                                       margin = margin(t = 4, b = 12)),
      # Centred, to sit under the centred title rather than fight it.
      #
      # TWO knobs place the caption vertically, and they pull opposite ways:
      #   margin(t) here    = gap ABOVE the caption; raising it pushes the
      #                       caption DOWN (the panel gives up the space)
      #   plot.margin b     = gap BELOW the caption; lowering it lets the
      #                       caption sit closer to the bottom edge
      # Changing only one just resizes the gap and the caption still floats.
      # 28/8 anchors it near the bottom; 40/4 pins it almost to the edge.
      plot.caption  = element_text(color = "grey40", size = 8.5, hjust = 0.5,
                                   lineheight = 1.4, margin = margin(t = 28)),
      axis.title.x  = element_text(size = 11, margin = margin(t = 10, b = 8)),
      axis.title.y  = element_text(size = 11, margin = margin(r = 10)),
      axis.text     = element_text(size = 10),
      # t is the THIRD placement knob: it drops the title, and the panel with
      # it. Because coord_fixed() holds the panel square, height it gives up is
      # not recovered -- past roughly 110 the logos start to visibly shrink.
      # 60 moves everything down while keeping the plot the same size.
      plot.margin   = margin(t = 60, r = 24, b = 8, l = 24, unit = "pt")
    )

ggsave(
  filename = file.path(output_dir, paste0("WNBA_Efficiency_AUG_2026", games_suffix, "_ig4x5.png")),
  plot = last_plot() + ig_theme,
  width = 8,
  height = 10,
  dpi = 300,
  bg = "white"
)

# Transparent 4:5, for dropping onto a coloured Canva background.
# NOTE the text stays dark (grey40 caption, black title, theme_classic's black
# axis lines) because it is the BACKGROUND being removed, not the ink. On a dark
# background none of it will read -- that needs a light-text variant, not this
# file. Fine on white, cream, or any pale brand colour.
ggsave(
  filename = file.path(output_dir, paste0("WNBA_Efficiency_AUG_2026", games_suffix, "_ig4x5_transparent.png")),
  plot = last_plot() + ig_theme +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA)
    ),
  width = 8,
  height = 10,
  dpi = 300,
  bg = "transparent"
)

