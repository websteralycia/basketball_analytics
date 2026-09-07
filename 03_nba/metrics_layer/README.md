# NBA Metrics Layer

The metric code the NBA charts are built on, with its tests and two runnable
tours. **Source-agnostic**: it works the same whether the numbers came from
`hoopR`, a CSV someone handed you, or a fixture.

## Start here if you have a CSV

```sh
Rscript scripts/r/example_csv_workflow.R
```

Four worked cases, in the order you tend to need them:

| | |
|---|---|
| **1** | pull from `hoopR`, what you'd do with no data supplied |
| **2** | write a CSV, read it back, get **identical** numbers |
| **3** | a CSV with completely unfamiliar column names |
| **4** | what happens when a required column is missing |

Part 2 is the check that matters. Round-tripping through a CSV reproduces the
API path to `max ORtg difference: 0`.

## The column contract

One row per team per game. Everything downstream expects these names:

```
ids     game_id, team_id
stats   pts, fga, fgm, fg3m, fta, oreb, dreb, tov
```

Incoming names are lower-cased first, so `PTS` / `FGA` / `OREB` need no
mapping at all. For anything else, `nba_standardize()` takes one, canonical
name on the left, **their name in lower case** on the right:

```r
df |>
  nba_standardize(c(tov = "giveaways", fg3m = "threes", fga = "shotstaken")) |>
  nba_pair_opponents() |>
  nba_add_metrics() |>
  nba_aggregate(team_id)
```

Miss a column and it says which one and how to supply it, rather than
producing a plausible wrong answer:

```
Error: nba_standardize(): missing required column(s): tov
  Supply a mapping, e.g. nba_standardize(df, c(tov = "YOUR_COLUMN"))
```

## The three modules

```sh
Rscript scripts/r/nba_cookbook.R      # worked examples, read along
```

| file | what it holds |
|---|---|
| `nba_data.R` | fetching. The only file that touches the network. |
| `nba_metrics.R` | **team** metrics. Pure functions, one row per team-game. Possessions, ORtg/DRtg/Net, Four Factors, TS%, Pace. |
| `nba_player_metrics.R` | **player** metrics. Player-game in, player-season out. Usage, per-36, AST%/REB%, percentiles within position. |

The split is the point: nothing in the metrics files makes a network call,
reads a file, or prints, which is what makes them testable and what lets the
same code serve an API pull and a handed-over CSV.

## Tests

```sh
Rscript -e 'library(testthat); library(dplyr);
            source("scripts/r/nba_metrics.R"); source("scripts/r/nba_data.R");
            test_file("tests/test-nba-metrics.R")'
```

63 assertions. The network-backed ones reconcile against NBA.com directly and
are skipped on CRAN; set `NOT_CRAN=true` to run them.

## Possessions

Oliver's estimate, averaged across the two teams:

```
FGA + 0.44*FTA + TOV - 1.07 * (OREB / (OREB + OppDREB)) * (FGA - FGM)
```

Validated against NBA.com across all 30 teams, 2025-26: mean error **+0.06
ORtg, +0.05 DRtg**, worst single team 0.64.

The simpler `FGA + 0.44*FTA + TOV - OREB` is widely called the NBA.com formula
but does not reproduce NBA.com, see
[`03_nba/team_efficiency`](../team_efficiency#the-possession-estimate-and-why-it-matters).

## A note on the copies

`nba_metrics.R`, `nba_data.R` and `nba_player_metrics.R` also sit inside
[`team_efficiency`](../team_efficiency) and
[`player_pizza_plot`](../player_pizza_plot). That is deliberate: every project
in this repo downloads and runs on its own, so each carries what it needs.
This folder is where the layer is documented and tested.
