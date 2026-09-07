# NCAA Division III Player Dataset

A season-long player dataset for D3 women's basketball, and a
conditionally-formatted Excel workbook built from it.

**Why this exists:** ESPN doesn't cover Division III, so `wehoop` can't either.
There is no `load_wbb_player_box()` for these players. The data has to come
from `stats.ncaa.org` directly, which is what most of this project is.

The example covers the **Midwest Conference**, 2025-26: 169 players, 62 columns.

## The workbook is the point

[`outputs/wbb_player_season_2026_d3_lab.xlsx`](outputs/wbb_player_season_2026_d3_lab.xlsx)
has three sheets:

| Sheet | Shading |
|---|---|
| **By Position** | Each stat shaded by where the player ranks **among others at her position** (G / F / C) |
| **By Column** | Each stat shaded by rank across **all players** in that column |
| **Key** | What each column means |

The numbers on the two shaded sheets are **identical**: nothing is
recomputed. Only the comparison group changes. That's what makes them worth
putting side by side: a post's rebounding number can be yellow on one sheet
and deep green on the other, and a guard's 3P% the reverse. It makes the
choice of comparison group visible instead of leaving it as an assumption
buried in a percentile.

A `.csv` is plain text and can't carry colour, which is why this ships as a
workbook rather than another spreadsheet export.

## Running it

Four scripts, in this order. Each hands a file to the next through `save_dir`,
which defaults to your Desktop.

| Script | Does |
|---|---|
| `8.16_ncaa_d3_wbb_pull.R` | The scraper. Sourced by the runner, not run directly. |
| `8.16_ncaa_d3_wbb_run_mwc.R` | Pulls the conference's games, writes a player-box CSV shaped like `wehoop`'s |
| `8.16_ncaa_d3_wbb_build_dataset.R` | Aggregates to player-seasons, adds rates and within-position percentiles |
| `8.16_ncaa_d3_wbb_format_xlsx.R` | Reads that CSV, writes the styled workbook |

```sh
Rscript 8.16_ncaa_d3_wbb_run_mwc.R smoke   # ~20 games, to check it works
Rscript 8.16_ncaa_d3_wbb_run_mwc.R full    # the whole conference
```

Change `MWC_TEAMS` in the runner to point at a different conference.

### The scrape needs a real browser

`stats.ncaa.org` sits behind an Akamai bot challenge. Plain `httr`/`rvest`
gets a 403 or an empty stub: a browser has to execute the challenge and get
cookied for the session. So the pull drives Chrome through `chromote`.

It runs **headful**: a Chrome window opens and should be left alone until the
pull finishes. That's a deliberate workaround, not an oversight: headless
Chrome crashes on the machine this was written on (macOS 13, Intel), so
`chromote`'s default launch never opens its debugging port. The script starts
Chrome normally with `--remote-debugging-port` and attaches instead. The
reasoning is written up at the top of the pull script.

Requests are spaced 1s apart, and partial results checkpoint every 25 games so
a long pull can be resumed rather than restarted.

## Data

`data/wbb_player_season_2026_d3_lab.csv` is the trimmed player-season table the
workbook is built from: the same rows, without the formatting.

The raw play-by-play and full-column tables stay local; they're intermediates,
and the two files here are what you'd actually want to read.

## Packages

```r
install.packages(c("chromote", "rvest", "dplyr", "readr", "tidyr", "openxlsx"))
```
