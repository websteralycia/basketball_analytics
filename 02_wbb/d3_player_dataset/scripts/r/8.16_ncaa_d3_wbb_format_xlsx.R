###############################################
# Conditional-formatted Excel version of the D3 lab dataset
#
# A .csv is plain text and cannot store colors, so the
# formatting has to live in a .xlsx. This script reads the lab
# CSV and writes a styled workbook -- it does not recompute any
# statistic, so every number matches the CSV exactly.
#
#   Rscript 8.16_ncaa_d3_wbb_format_xlsx.R
#
# TWO SHADED SHEETS, IDENTICAL NUMBERS
#   "By Position" shades each stat by where the player ranks
#     among others at HER POSITION (G / F / C). Fair to bigs,
#     whose rebounding looks strong and 3P% looks poor when
#     measured against guards.
#   "By Column" shades by rank across ALL players in that
#     column -- the literal "is this a big number?" read.
#
#   Comparing the two sheets is the point: the same number can
#   be red on one and yellow on the other purely because the
#   comparison group changed.
#
# COLOR SCHEME
#   Bottom third red, middle third yellow, top third green.
#   The three tints sit at DIFFERENT lightnesses (red 85,
#   green 89, yellow 95) rather than the same one: red and
#   green are the pair ~8% of men cannot distinguish, so the
#   lightness gap keeps the bands separable even when the hue
#   difference is invisible.
#
#   Direction is set per stat:
#     up      more is better  -> green at the top
#     down    more is worse   -> red at the top (turnovers)
#     neutral no good/bad     -> single-hue blue ramp, which
#             reads as "more of this" rather than "better".
#             Used for playing time, shot ATTEMPTS, usage and
#             the two style rates, none of which is an
#             achievement on its own.
#
#   NOTE ON TOTALS: season totals (pts_total, reb_total, ...)
#   reward availability as much as skill -- a player who
#   appeared in 29 games will out-total one who played 14. The
#   per-game and per-40 columns beside them are the fairer
#   comparison; totals are shaded too, but read them with
#   games_played in view.
###############################################

suppressPackageStartupMessages({
  library(openxlsx)
  library(readr)
  library(dplyr)
})

# Where everything this pipeline reads and writes lives -- the four scripts
# hand files to each other through this one folder, so they all agree.
# Change this one line to point somewhere else.
save_dir <- "~/Desktop"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

data_dir   <- save_dir
output_dir <- save_dir

lab <- read_csv(file.path(data_dir, "wbb_player_season_2026_d3_lab.csv"),
                show_col_types = FALSE)

###############################################
# Palette
###############################################

HI  <- "#C5E9C5"   # top third    (green,  L* 89)
MID <- "#FEEFD1"   # middle third (yellow, L* 95)
LO  <- "#F7C9C9"   # bottom third (red,    L* 85)

SEQ <- c("#CDE2FB", "#9EC5F4", "#6DA7EC")   # sequential blue, low -> high

###############################################
# Which way each stat reads
###############################################

up <- c(
  "pts_total", "ppg", "pts_per_40",
  "fgm_total", "fg_pct", "fg3m_total", "fg3_pct", "ftm_total", "ft_pct",
  "reb_total", "rpg", "reb_per_40",
  "oreb_total", "oreb_pg", "oreb_per_40",
  "dreb_total", "dreb_pg", "dreb_per_40",
  "ast_total", "apg", "ast_per_40",
  "stl_total", "spg", "stl_per_40",
  "blk_total", "bpg", "blk_per_40",
  "efg_pct", "ts_pct", "ast_pct", "oreb_pct", "dreb_pct",
  "ts_pctile_pos", "efg_pctile_pos", "ast_pctile_pos",
  "oreb_pctile_pos", "dreb_pctile_pos", "fg3_pctile_pos", "ft_pctile_pos"
)
down <- c("tov_total", "tovpg", "tov_per_40", "tov_pct", "tov_pctile_pos")
# Playing time (games_played, games_started, minutes_total,
# mpg) is deliberately left UNSHADED -- it is context you read
# alongside the stats, not a result to rank, and coloring it
# made the left edge of the sheet noisy.
neutral <- c(
  "fga_total", "fg3a_total", "fta_total",
  "usage", "threepar", "fta_rate",
  "usage_pctile_pos", "threepar_pctile_pos", "fta_rate_pctile_pos"
)

# On the By Position sheet a stat is shaded by its own
# *_pctile_pos column when one exists, so the color agrees with
# the percentile printed in the same row.
pctile_for <- c(
  fg3_pct = "fg3_pctile_pos", ft_pct = "ft_pctile_pos",
  efg_pct = "efg_pctile_pos", ts_pct = "ts_pctile_pos",
  ast_pct = "ast_pctile_pos", tov_pct = "tov_pctile_pos",
  oreb_pct = "oreb_pctile_pos", dreb_pct = "dreb_pctile_pos",
  usage = "usage_pctile_pos", threepar = "threepar_pctile_pos",
  fta_rate = "fta_rate_pctile_pos"
)

shaded <- c(up, down, neutral)
shaded <- shaded[shaded %in% names(lab)]

###############################################
# Percentiles
#
# NA positions form their own group, matching how the lab
# script builds its percentiles.
###############################################

rank_pct <- function(x, grp = NULL) {
  if (is.null(grp)) return(dplyr::percent_rank(x))
  out <- rep(NA_real_, length(x))
  for (g in unique(grp)) {
    i <- if (is.na(g)) which(is.na(grp)) else which(grp == g & !is.na(grp))
    v <- x[i]
    if (sum(!is.na(v)) > 1) out[i] <- dplyr::percent_rank(v)
    else if (sum(!is.na(v)) == 1) out[i][!is.na(v)] <- 0.5  # alone -> middle
  }
  out
}

pct_by_position <- lapply(shaded, function(s) {
  src <- if (s %in% names(pctile_for)) pctile_for[[s]] else NA_character_
  if (!is.na(src) && src %in% names(lab)) lab[[src]]
  else rank_pct(lab[[s]], lab$position)
})
names(pct_by_position) <- shaded

pct_by_column <- lapply(shaded, function(s) rank_pct(lab[[s]]))
names(pct_by_column) <- shaded

###############################################
# Sheet builder
###############################################

add_shaded_sheet <- function(wb, sheet, pct_list) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, lab, headerStyle = createStyle(
    textDecoration = "bold", fgFill = "#E8E7E3",
    border = "bottom", borderStyle = "medium", halign = "left"
  ))
  cn <- names(lab)

  for (s in shaded) {
    p   <- pct_list[[s]]
    dir <- if (s %in% down) "down" else if (s %in% neutral) "neutral" else "up"

    cols_for <- if (dir == "neutral") SEQ                # low, mid, high
                else if (dir == "down") c(HI, MID, LO)   # inverted
                else c(LO, MID, HI)

    bin   <- cut(p, breaks = c(-Inf, 1/3, 2/3, Inf), labels = FALSE)
    col_i <- which(cn == s)

    for (b in 1:3) {
      r <- which(bin == b)
      if (length(r) == 0) next
      addStyle(wb, sheet, createStyle(fgFill = cols_for[b]),
               rows = r + 1, cols = col_i, gridExpand = FALSE, stack = TRUE)
    }
  }

  freezePane(wb, sheet, firstActiveRow = 2, firstActiveCol = 6)
  setColWidths(wb, sheet, cols = 1:ncol(lab), widths = "auto")
  setColWidths(wb, sheet, cols = 3, widths = 24)   # player
  setColWidths(wb, sheet, cols = 4, widths = 18)   # team
}

wb <- createWorkbook()
add_shaded_sheet(wb, "By Position", pct_by_position)
add_shaded_sheet(wb, "By Column",   pct_by_column)

###############################################
# Key sheet
###############################################

addWorksheet(wb, "Key")
key <- data.frame(
  Band = c("Top third", "Middle third", "Bottom third", "",
           "Top third", "Middle third", "Bottom third", "", "Blank cell"),
  Meaning = c(
    "GREEN  - strong (>= 67th percentile)",
    "YELLOW - around average (33rd-67th percentile)",
    "RED    - weak (< 33rd percentile)",
    "",
    "Style stats (playing time, shot attempts, usage, 3PA/FTA rate): most of this",
    "Style stats: middling",
    "Style stats: least of this",
    "",
    "Undefined -- the player never attempted one (e.g. no 3PA)"
  ),
  stringsAsFactors = FALSE
)
writeData(wb, "Key", key,
          headerStyle = createStyle(textDecoration = "bold", fgFill = "#E8E7E3"))
for (r in 2:4) addStyle(wb, "Key", createStyle(fgFill = c(HI, MID, LO)[r - 1]),
                        rows = r, cols = 1)
for (r in 6:8) addStyle(wb, "Key", createStyle(fgFill = SEQ[c(3, 2, 1)[r - 5]]),
                        rows = r, cols = 1)
writeData(wb, "Key", data.frame(Note = c(
  "The two sheets hold the SAME numbers and differ only in what the color compares against.",
  "By Position: rank among players at the same position (G/F/C). Fair to bigs.",
  "By Column: rank across all players in that column. The literal 'is this a big number?' read.",
  "A cell can be red on one sheet and yellow on the other -- that is the comparison group changing, not the stat.",
  "Style stats use a blue ramp because more is not better -- a high 3PA rate is a role, not an achievement.",
  "Turnover columns are inverted on purpose: a high turnover rate is shaded red, not green.",
  "Season totals reward availability -- a 29-game player out-totals a 14-game one. Per-game and per-40 are the fairer read.",
  "Red, yellow and green sit at different lightnesses so the bands stay readable in grayscale or with red-green color blindness.",
  "Numbers are identical to wbb_player_season_2026_d3_lab.csv -- only formatting differs."
)), startRow = 12, headerStyle = createStyle(textDecoration = "bold"))
setColWidths(wb, "Key", cols = 1:2, widths = c(18, 96))

out <- file.path(output_dir, "wbb_player_season_2026_d3_lab.xlsx")
saveWorkbook(wb, out, overwrite = TRUE)
message("Wrote: ", out)
message("  sheets: By Position, By Column, Key")
message("  rows: ", nrow(lab), " | cols: ", ncol(lab),
        " | shaded columns: ", length(shaded),
        " of ", sum(sapply(lab, is.numeric)), " numeric")
message("  unshaded: ",
        paste(setdiff(names(lab),
              c(shaded, "season", "athlete_id", "player", "team", "position")),
              collapse = ", "))
