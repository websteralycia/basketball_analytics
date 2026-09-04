# WNBA Player Pizza Plot

The WNBA version of the pizza plot: one player, eight metrics, each slice
scaled to her percentile against the rest of the league within her position
group. A reference ring marks the position-group average.

![Example](outputs/pizza_plot_Olivia_Miles_2025-26.png)

## Configuring

Near the top of `scripts/r/01_wnba_pizza_plot.R`:

```r
main_player <- "Olivia Miles"
player_team <- "Minnesota Lynx"
```

and the season in section 1:

```r
wnba_player_box_2026 <- load_wnba_player_box(seasons = 2026)
```

`wehoop` uses the END year, so 2026 is the 2026 season.

**Regular season only.** By default every game in the pull is included, which
means the All-Star game is in there too. Section 2 has a commented-out
`filter(season_type == 2)` — note that on the WNBA feed the All-Star game also
carries `season_type == 2`, so that filter alone won't remove it. If All-Star
inclusion matters for your comparison, filter on franchise `team_id` instead.

## Data freshness

`load_wnba_player_box()` serves a periodic data release rather than live ESPN,
and can lag completed games by up to a week. For an in-season chart, check the
last game date in the pull before you publish it. The team efficiency script
in [`04_wnba/team_efficiency`](../team_efficiency) shows the live top-up
pattern if you need it here.

## Packages

```r
install.packages(c("wehoop", "dplyr", "readr", "ggplot2", "tidyr",
                   "cowplot", "magick"))
```

## Output

A PNG at 8×8in, 300dpi, named `pizza_plot_<player>_2025-26.png`.
The script also writes two player-season CSVs alongside it.
