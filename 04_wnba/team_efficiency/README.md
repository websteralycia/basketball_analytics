# WNBA Team Efficiency Landscape

Every team plotted by offensive rating against defensive rating, crosshairs at
the league average, logos instead of points. Exported four ways: landscape and
a 4:5 Instagram crop, each on white and on transparent.

![Example](outputs/WNBA_Efficiency_AUG_2026_fullseason.png)

## The live top-up

`load_wnba_player_box()` does **not** query ESPN. It downloads a pre-built data
release that lags live games by up to a week, on 2026-08-08 the release ended
2026-08-01 and 19 completed games were missing, so a chart labelled "as of
August 8" was really as of August 1.

Section 1.2 fixes that. It walks the days since the release, pulls the finals
from the live ESPN scoreboard (`espn_wnba_scoreboard`), and fetches those box
scores directly. The schedule endpoint is stale by the same amount and reports
those games as not completed, which is why the list of finals has to come from
the scoreboard rather than the schedule.

It self-disables: once the release catches up there are no dates to scan and
the block is a no-op. Set `TOPUP_LIVE <- FALSE` to pin the script to release
data only.

## The All-Star game

On the WNBA feed, `season_type` is 2 for **every** row, All-Star game included,
so it cannot be used to filter. Section 1.5 removes it by franchise `team_id`
instead. Leave it in and it visibly drags the crosshairs.

## Configuring

**Season**: section 1: `load_wnba_player_box(seasons = 2026)`.

**First N games.** There's an optional block around section 3 that truncates
each team to its first N games, for like-for-like comparison early in a season.
Comment it out for the full season. The filename suffix follows automatically:
`_first20` when it's set, `_fullseason` when it isn't.

## Packages

```r
install.packages(c("wehoop", "nbaplotR", "dplyr", "tibble", "readr",
                   "ggplot2", "ggimage", "ggtext"))
```

## Data

`data/WNBA_Efficiency_AUG_2026_fullseason.csv` is the season efficiency table
behind the example chart. Committed so you can check the numbers without
running the pull.

## Output

The script writes four PNGs, landscape and a 4:5 Instagram crop, each opaque
and transparent, plus the season efficiency CSV. Only the landscape PNG is
checked in here as an example.

The landscape PNG renders at dpi 1200; drop it to 300 in section 6 if you want
smaller files.
