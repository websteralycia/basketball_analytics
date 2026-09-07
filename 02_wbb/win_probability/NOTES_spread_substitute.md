# Note: the spread input works differently in college women's basketball

**Status: written 2026-08-04, before building Phase 3.** Everything below is
measured on the 6,054 completed NCAA WBB games of the 2026 season, not assumed.

Phase 3 exists because Vegas spreads don't exist for most NCAAW games. But the
substitute is not a drop-in: the quantity it replaces behaves differently here
than in the NBA/WNBA that inpredictable's model was built on. Four differences,
each with a consequence for Phase 5.

## 1. The spread scale is roughly twice as wide

| | NCAA WBB 2026 | NBA (typical) |
|---|---|---|
| SD of final margin | **22.0** | ~13-14 |
| median abs. margin | **13** | ~11 |
| 90th pct abs. margin | **39** | ~25 |
| 99th pct abs. margin | **72** | ~35 |
| largest margin | **106** | ~60 |

**31.9%** of games are decided by 20+, **17.0%** by 30+, **9.8%** by 40+.

NBA/WNBA are closed leagues with 30 and 12 teams and enforced parity. NCAA D-I
women's is an open pool: 666 teams appear in the 2026 schedule, spanning
national title contenders to teams that lose by 50. A pregame line here routinely
exceeds ±30, a range that barely exists in the data inpredictable fit on.

**Consequence:** their smoothing parameters cannot be reused, and the CV in
Phase 5 has to run over our own spread distribution. More importantly, a large
share of our training rows sit in regions where win probability is already
saturated at ~0 or ~1 and carries no information. Expect either an explicit cap
on the spread input or a decision to down-weight blowouts; that choice should be
made from the fitted model, not in advance.

## 2. Home court is about three times larger: and neutral sites are not neutral

- Non-neutral games: mean home margin **+8.56**, home win rate **62.3%**.
  (NBA home advantage is ~2.5-3 points.)
- Neutral-site games are **9.5%** of the schedule (573 of 6,054), a share the
  pro leagues have no equivalent for, driven by tournaments.
- At those neutral sites the nominal home team still wins by **+4.57** on
  average.

That last number is the trap. ESPN's home/away designation at a neutral site is
not arbitrary: it tracks seeding and travel distance, so zeroing the home term
on `neutral_site == TRUE` throws away a real ~4.6-point effect. The home term
needs to be **two parameters**, not one flag.

## 3. Early season carries far less information than an early-season market line

Median games played per team, at the season's first date on or before each:

| date | median prior games | min |
|---|---|---|
| 2025-11-15 | 3 | 0 |
| 2025-12-01 | 7 | 0 |
| 2026-01-01 | 13 | 1 |
| 2026-02-01 | 21 | 1 |
| 2026-03-01 | 29 | 24 |

The brief's framing, early-season games use less-informed priors, "mirroring how
a market line firms up closer to tip-off", is directionally right but understates
the gap. A November Vegas line already encodes returning starters, recruiting
classes, transfer portal moves and coaching changes. A results-based rating on
three games encodes none of that. Ours is not a noisier version of the market's
prior; for the first several weeks it is a **weaker kind of estimate**, and it
should not be presented as equivalent.

**Consequence:** early-season win probabilities will be worse than the model's
overall calibration suggests. Phase 5 should report calibration *by month*, not
just in aggregate, and the served calculator should say what it does not know.

## 4. The schedule graph is barely connected when the rating is least informed

Median **11 distinct opponents** per team by January 1, out of a 666-team pool.
NBA teams have played much of the league by then; NCAA teams play regional and
conference-adjacent schedules, so the opponent-adjustment system is close to
unidentifiable in November and December.

**Consequence:** ridge shrinkage toward the league mean is not a refinement here,
it is what keeps the system solvable at all. The penalty does double duty:
opponent adjustment where the graph supports it, regression to the mean where it
does not, which is why Phase 3 uses one penalised fit rather than a raw
efficiency margin plus a separate shrinkage step.

## What this means for the handoff to Phase 5

The Phase 3 output is a **spread-equivalent**, not a spread. It is:

- on a points scale, so it can enter the model where inpredictable's spread does;
- strictly as-of: a game's rating uses only games completed before its date, or
  the model trains on knowledge of its own outcome;
- wider-tailed, more home-weighted, and much less reliable before January than
  the input it stands in for.

Any comparison of this model's accuracy against inpredictable's published NBA
figures is not apples-to-apples, and shouldn't be made without saying so.

---

## Confirmed by the built ratings (added 2026-08-04, re-measured 2026-08-06)

Phase 3 is built over 2017-2026 (50,585 scored games, every game where both
teams have at least one prior). It predicts realised margin at **RMSE 12.67**
against an 18.91 baseline of predicting zero, a 33% reduction, with
correlation 0.725 and calibration slope 1.007.

The resulting spread distribution confirms point 1 above:

| pct | 1% | 10% | 50% | 90% | 99% |
|---|---|---|---|---|---|
| spread (pts) | −23.3 | −9.4 | +5.9 | +22.7 | **+42.7** |

A one-in-a-hundred game carries a 43-point line. Nothing in inpredictable's NBA
training data looks like that, which is the concrete form of "their smoothing
parameters cannot be reused."

### The 2026-08-04 figures in this section were wrong, in a way worth recording

They read RMSE 12.75 / cor 0.740 / slope 0.955, and the early-season table below
read 18.24 in November. Those came from `rating_method_report()`, which routes
through `asof_ratings()`, **and `asof_ratings()` has no `prior0` argument, so
it silently scores a ratings path with the carryover prior turned off.** It is
not the path that builds the cached CSV. Anything measured through it understates
the shipped model, most of all in November, where the prior does nearly all of
its work. See the warning on that function in `03_team_ratings.R`.

### Early-season weakness, re-measured on the shipped path

By month, at λ 3 / decay 0.9, ten seasons:

| | Nov | Dec | Jan | Feb | Mar |
|---|---|---|---|---|---|
| RMSE | 14.46 | 13.17 | 12.07 | 11.71 | 11.72 |
| games | 8,334 | 9,673 | 13,640 | 13,053 | 5,839 |

November is 23% worse than February, not the 55% this note previously claimed.
Phase 5 should still report calibration by month; a 23% spread is not nothing,
but the early season is in better shape than the first pass suggested.

### Tuning status: SETTLED (2026-08-06)

Both constants were re-tuned across the full 2017-2026 range by
`run_03_tune.R`, 35 (λ, decay) cells, selected on 2017-2023 with 2024-2026
held out. λ moved 1 → 3; decay stayed at 0.9. The caveat this section used to
carry is discharged.

The substantive finding is that **λ and decay are two levers on one axis**: λ compresses predictions, decay expands them, so the good cells form a ridge,
and the original tuning found a false optimum by moving one at a time. Along
that ridge RMSE spans under 3% while calibration slope spans 0.93-1.05, so the
choice is very nearly a pure calibration choice. It was made on calibration
because inpredictable's calculator takes a **hand-entered** pregame spread:
a typed −7 and a rating-derived −7 have to mean the same thing, which requires
the scale to be real expected points. Full grid in
`outputs/phase3_tuning_grid.csv`.
