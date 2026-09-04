# basketball_analytics

Basketball analytics scripts and finished charts, across men's and women's
college basketball, the NBA and the WNBA.

Everything here is built to be run by someone else. Each script has a marked
`CONFIG` block near the top with the handful of things you'd actually want to
change — team, player, conference, season — and sensible defaults for the rest.

## Projects

| Project | What it does |
|---|---|
| [`01_mbb/team_efficiency`](01_mbb/team_efficiency) | NCAA men's offensive/defensive rating quadrant chart, team logos plotted against the national average |
| [`02_wbb/team_efficiency`](02_wbb/team_efficiency) | Same chart for NCAA women's basketball |
| [`02_wbb/win_probability`](02_wbb/win_probability) | Live win probability calculator for NCAA women's basketball — [open it](https://lineup-optimizer-wnba.shinyapps.io/ncaaw-win-probability/) |
| [`02_wbb/team_four_factors`](02_wbb/team_four_factors) | One NCAA women's team against every team in its conference on the Four Factors, offense and defense |
| [`02_wbb/d3_player_dataset`](02_wbb/d3_player_dataset) | Season dataset for NCAA Division III women's basketball, scraped from stats.ncaa.org, with a conditionally-formatted Excel workbook |
| [`02_wbb/player_pizza_plot`](02_wbb/player_pizza_plot) | Eight-slice percentile radar for one NCAA women's player against her conference, within her position group |
| [`03_nba/team_efficiency`](03_nba/team_efficiency) | NBA efficiency landscape for all 30 teams, plus a 4:5 Instagram crop and transparent variants |
| [`03_nba/player_pizza_plot`](03_nba/player_pizza_plot) | NBA player percentile radar against the league, within position |
| [`04_wnba/team_efficiency`](04_wnba/team_efficiency) | WNBA efficiency landscape, with a live top-up for games the data release hasn't picked up yet |
| [`04_wnba/player_pizza_plot`](04_wnba/player_pizza_plot) | WNBA player percentile radar |

## What the two chart types are

**Efficiency landscape.** Every team plotted by points scored per 100
possessions (across) against points allowed per 100 possessions (up, reversed
so better defense is higher). Dashed crosshairs sit at the league average, so
the top-right quadrant is teams good at both ends and the bottom-left is teams
good at neither. The dotted diagonal is net rating zero.

**Pizza plot.** One player, eight metrics, each slice reaching as far as that
player's percentile in the comparison pool. The pool is the player's own
position group — guards against guards — so a center isn't punished for a low
assist rate. Slices are grouped scoring / shooting / playmaking, and a
reference ring marks the position-group average.

## Running a script

**You do not need to download any data.** The scripts pull live from
[`wehoop`](https://wehoop.sportsdataverse.org/) and
[`hoopR`](https://hoopr.sportsdataverse.org/), so a fresh clone runs end to end.

Each project also ships the CSV its scripts generate, under `data/`, so you can
look at the numbers behind a chart without running anything.

### 1. Install R (once)

- R — <https://cran.r-project.org/>
- RStudio Desktop — <https://posit.co/download/rstudio-desktop/> (free)

### 2. Install the packages (once)

Paste this into the R console. It covers every script in the repo and takes a
few minutes the first time:

```r
install.packages(c("dplyr", "readr", "ggplot2", "ggtext", "ggimage", "tidyr",
                   "tibble", "cowplot", "magick", "httr", "jsonlite", "rlang",
                   "wehoop", "hoopR", "nbaplotR"))
```

Individual projects list the subset they actually need in their own README.

### 3. Get the code

Green **Code** button at the top of this page → **Download ZIP** → unzip it.
(Or `git clone` if you use git.)

### 4. Edit the CONFIG block

Every script has a clearly marked `CONFIG` section near the top. That's the
only part you need to touch — for example:

```r
main_player     <- "Jamisyn Heaton"
school          <- "Utah State"
conference_name <- "Mountain West Conference"
season          <- 2026     # END year: 2026 = the 2025-26 season
```

Get a name wrong and the scripts generally print the valid options before
stopping, so you can copy the right one from the list.

### 5. Run it

`Ctrl/Cmd + Shift + Enter` (Source), or click **Source**. The first run takes a
minute or two while the season's box scores download.

### 6. Find your chart

**It saves to your Desktop.** Nothing to configure.

## Saving somewhere other than the Desktop

Every script has one line near where it saves:

```r
save_dir <- "~/Desktop"
```

Change it to any folder you like — `"~/Documents/scouting"`, a shared drive,
wherever. The chart and the CSV both go there, and the folder is created if it
doesn't exist. That's the whole mechanism; there's nothing to install or
configure.

## Layout

```
<league>/<project>/
├── scripts/r/   # the R scripts
├── data/        # the CSV the scripts generate
└── outputs/     # example rendered charts
```

Leagues keep the same numbered prefixes throughout: `01_mbb`, `02_wbb`,
`03_nba`, `04_wnba`.

Only the small generated CSVs are committed. Raw pulls — play-by-play,
full player-box seasons — stay local.
