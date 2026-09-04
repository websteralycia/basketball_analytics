###############################################
# Title: NCAA MBB Efficiency Landscape Plot (Shaded + Simple Labels)
# Purpose: Visualizes 2025-26 NCAA MBB Team Efficiency
#          Quadrant shading + explanatory subtitle + simplified axis labels
# Data Source: CSV generated from 02_ncaa_mbb_efficiency_stat_grabber.R
# Author: Alycia Webster
###############################################

###############################################
# 0. Load Required Packages
###############################################

library(dplyr)
library(ggplot2)
library(ggimage)
library(readr)
library(ggtext)

###############################################
# 1. Load Data from CSV
###############################################

# Where everything this script writes is saved -- the chart and the CSV.
# Change this one line to send them somewhere else, e.g. "~/Documents/scouting".
save_dir <- "~/Desktop"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

output_dir <- save_dir   # charts
data_dir   <- save_dir   # CSVs

team_efficiency <- read_csv(file.path(data_dir, "ncaa_mbb_efficiency_2026.csv"))

print("Loaded NCAA MBB efficiency data:")
print(team_efficiency)

###############################################
# 2. Calculate League Averages for Crosshairs
###############################################

avg_ortg <- mean(team_efficiency$ortg, na.rm = TRUE)
avg_drtg <- mean(team_efficiency$drtg, na.rm = TRUE)

cat("\nLeague Averages:\n")
cat(sprintf("  Offensive Rating: %.2f\n", avg_ortg))
cat(sprintf("  Defensive Rating: %.2f\n", avg_drtg))

###############################################
# 3. Set Plot Limits
###############################################

zoom_factor <- 0.85

zoom_dist <- (max(
  abs(avg_ortg - min(team_efficiency$ortg)),
  abs(avg_ortg - max(team_efficiency$ortg)),
  abs(avg_drtg - min(team_efficiency$drtg)),
  abs(avg_drtg - max(team_efficiency$drtg))
) + 3) * zoom_factor

x_lims <- c(avg_ortg - zoom_dist, avg_ortg + zoom_dist)
y_lims <- c(avg_drtg + zoom_dist, avg_drtg - zoom_dist)  # reversed

###############################################
# 4. Create the Plot
###############################################

ggplot(team_efficiency, aes(x = ortg, y = drtg)) +

  # Quadrant shading (drawn first so everything else sits on top)
  # Top-right: good offense + good defense (elite)
  annotate("rect",
    xmin = avg_ortg, xmax = avg_ortg + zoom_dist,
    ymin = avg_drtg - zoom_dist, ymax = avg_drtg,
    fill = "#4CAF50", alpha = 0.08) +
  # Top-left: poor offense + good defense
  annotate("rect",
    xmin = avg_ortg - zoom_dist, xmax = avg_ortg,
    ymin = avg_drtg - zoom_dist, ymax = avg_drtg,
    fill = "#FFC107", alpha = 0.08) +
  # Bottom-right: good offense + poor defense
  annotate("rect",
    xmin = avg_ortg, xmax = avg_ortg + zoom_dist,
    ymin = avg_drtg, ymax = avg_drtg + zoom_dist,
    fill = "#FFC107", alpha = 0.08) +
  # Bottom-left: poor offense + poor defense
  annotate("rect",
    xmin = avg_ortg - zoom_dist, xmax = avg_ortg,
    ymin = avg_drtg, ymax = avg_drtg + zoom_dist,
    fill = "#F44336", alpha = 0.08) +

  # Crosshairs at league average
  geom_vline(xintercept = avg_ortg, linetype = "dashed", color = "grey70") +
  geom_hline(yintercept = avg_drtg, linetype = "dashed", color = "grey70") +

  # Net Rating diagonal line
  geom_segment(
    aes(x = avg_ortg - zoom_dist, y = avg_drtg - zoom_dist,
        xend = avg_ortg + zoom_dist, yend = avg_drtg + zoom_dist),
    linetype = "dotted", color = "grey50"
  ) +

  # Team logos
  geom_image(aes(image = team_logo), size = 0.03, asp = 1.6) +

  scale_x_continuous(limits = x_lims) +
  scale_y_reverse(limits = y_lims) +
  coord_fixed(ratio = 1) +

  # Net Rating diagonal annotations
  annotate("text",
    x = avg_ortg - (zoom_dist * 0.9), y = avg_drtg - (zoom_dist * 0.9),
    label = "(+) Net Rating",
    angle = -45, vjust = -0.5, color = "grey50", size = 2.5, fontface = "italic") +
  annotate("text",
    x = avg_ortg + (zoom_dist * 0.9), y = avg_drtg + (zoom_dist * 0.9),
    label = "(-) Net Rating",
    angle = -45, vjust = 2, color = "grey50", size = 2.5, fontface = "italic") +

  # Labels and theme
  labs(
    title    = "NCAA Men's Tournament Efficiency Landscape",
    subtitle = "Teams in the <span style='color:#4CAF50;'>**top-right**</span> have the best offense AND defense — 2025-26 Regular Season",
    x        = "Offensive Efficiency (higher = better) \u2192",
    y        = "Defensive Efficiency (lower = better) \u2192",
    #caption  = "Data via hoopR"
  ) +
  theme_classic() +
  theme(
    plot.margin    = margin(t = 15, r = 20, b = 10, l = 20, unit = "pt"),
    plot.title     = element_text(face = "bold", size = 15, hjust = 0.5, margin = margin(b = 2)),
    plot.subtitle  = element_markdown(hjust = 0.5, size = 9),
    axis.title.x   = element_text(margin = margin(t = 10, b = 15)),
    axis.title.y   = element_text(margin = margin(r = 10)),
    plot.caption   = element_text(color = "grey50", size = 7, hjust = 1)
  )

###############################################
# 5. Save the Plot
###############################################

out_png <- file.path(output_dir, "ncaa_mbb_efficiency_2026_beat_shaded_simplelabels.png")

ggsave(
  filename = out_png,
  plot     = last_plot(),
  width    = 10,
  height   = 7,
  dpi      = 300,
  bg       = NA
)

cat("\n✅ Plot saved to ", out_png, "\n", sep = "")

###############################################
# End of script
###############################################
