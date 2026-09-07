## ============================================================
## render_pregame_card.R — the one-card pregame scouting report
## ------------------------------------------------------------
## Draws what keys_to_the_game.R selected. Computes nothing about the
## game beyond formatting: every number here comes from the profile or
## from build_keys().
##
## Four bands, top to bottom:
##   1. header      matchup, date, and the percentile POPULATION, which
##                  SPEC_play_profile.md requires on any card
##   2. stat tiles  opponent record / pace / eFG% / TOV% / ORB% / FT rate.
##                  The four factors replaced 3PT rate and Last 5 on
##                  2026-09-03: 3PT rate is largely restated by the
##                  shot-zone rows below, last-5 is form not quality, and
##                  without the four factors the card never mentioned
##                  turnovers, rebounding or free throws at all.
##   3. profile     the bulletable categories, grouped by dimension,
##                  each group carrying its own PPP definition because
##                  the three do not share a denominator
##   4. keys        what they do well / where they struggle
##
## The prose footer that used to explain the bar was replaced 2026-09-03 by
## an inline key under the population line: same .gp-track / .gp-mid visual
## language as the rows, so the convention is taught before the first real
## row rather than described after the last one. Its gradient is generated
## from CARD_RAMP_* so the key cannot drift from the bars it explains.
##
## THE BAR IS DIVERGING, not a magnitude bar. A rank of 12 has a
## meaningful middle — 6th/7th is mid-pack — so the fill grows from the
## centre of the track, right for above average and left for below. A
## plain left-anchored bar would make "6th of 12" look like a half-full
## strength instead of the nothing it is.
##
## Red = the opponent is strong here (a threat), green = weak and
## attackable. Colour never carries this alone: every row prints its rank
## in text, and the two key sections are separately headed.
##
## PALETTE — diverging green<->red, and green/red is the pair roughly 8%
## of men cannot separate by hue. It survives only because the two poles
## sit at very different LIGHTNESS. Measured with scripts/validate_palette.py
## (which this header used to claim existed and did not; written 2026-09-03):
##
##   light  #e34948 red vs #86c98a green   deutan dE 15.4  protan 29.4  L* 54/75
##   dark   #e66767 red vs #9ad9a0 green   deutan dE 14.1  protan 26.1  L* 59/81
##
## Threshold is 8. For contrast, the previous BLUE/red pair scored 46.5
## deutan — this change costs about two thirds of the CVD margin, and the
## lightness gap is what is buying the rest. Do not darken the green:
## #6fbf7d (L* 71, gap 12) FAILS at 6.3. Re-run the script on any change.
##
## Depends on: keys_to_the_game.R, team_play_profile.R
## ============================================================

#' Minimal HTML escape for interpolated text
#'
#' Team names and coach-authored phrases land in markup; an unescaped
#' ampersand in "P&R ball handler" is enough to break a row.
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Three-point rate for one team, from the shot_zone dimension
#'
#' NO LONGER RENDERED on the card -- the four factors took its tile. Kept
#' because it is the only header-shaped figure derivable from the profile
#' alone, with no schedule or box join, so it is the natural fallback if a
#' tile is ever needed without those sources.
three_point_rate <- function(profile, team) {
  z <- profile[profile$team == team & profile$dimension == "shot_zone", ]
  if (nrow(z) == 0) return(NA_real_)
  sum(z$off_freq[z$category %in% c("corner_3", "above_break_3")])
}

## --- bar + colour configuration -------------------------------
# BAR STYLE
#   "magnitude"  fill starts at the left edge and grows with the percentile.
#                Reads as "how good are they" at a glance.
#   "diverging"  fill grows from the CENTRE of the track, right for above
#                average and left for below. The original: a rank of 18 has
#                a meaningful middle, and a left-anchored bar makes "9th of
#                18" look like a half-full strength rather than the nothing
#                it is. Kept because that argument still holds.
#
# COLOUR MEANING -- these are OPPOSITE readings of the same fact:
#   "quality"    green = the opponent ranks WELL here, red = ranks worst.
#                Colour describes the TEAM, and matches the D3 spreadsheet
#                (green top third, red bottom third).
#   "threat"     red = the opponent is strong here (a danger to you),
#                green = weak (attackable). Colour describes YOUR PROBLEM.
#                This was the original, and it is why a 1st-of-18 row used
#                to be red.
#
# Both use the same two validated hexes, only swapped, so the CVD check in
# the header still holds -- no re-validation needed to flip this.
CARD_BAR_STYLE   <- "diverging"
CARD_COLOR_MEANS <- "threat"

# COLOUR SCALE
#   "ramp"    continuous green -> yellow -> orange -> red keyed to the row's
#             percentile. Matches the D3 spreadsheet convention (green top
#             third, red bottom third) so the two artefacts read alike.
#   "poles"   the two-colour version: one hex either side of mid-pack.
CARD_COLOR_SCALE <- "ramp"

# The ramp, worst -> best. Lightness falls MONOTONICALLY from green to red
# (L* 83 / 81 / 65 / 46 light, 86 / 85 / 69 / 53 dark) and that is not
# decoration: green->yellow->orange->red is the textbook colour-blind
# failure, and an earlier attempt with green L* 75 next to orange L* 74
# scored 7.5 protan -- below the threshold of 8. Separating the stops by
# lightness is what carries the ramp when hue collapses.
#
# Verified with scripts/validate_palette.py over ALL SIX pairs in each
# ramp: worst 10.2 light, 9.8 dark. Re-run it on any change; do not
# equalise the lightnesses to make the ramp look smoother.
CARD_RAMP_LIGHT <- c("#c0413f", "#d98c45", "#dfc96e", "#a8dcab")
CARD_RAMP_DARK  <- c("#d75452", "#e0985a", "#e6d484", "#b6e3b9")

## --- palette (validated — see header) -------------------------
CARD_CSS <- '
.gp-card{color-scheme:light;
  --surface-1:#fcfcfb; --surface-2:#f9f9f7;
  --text-primary:#0b0b0b; --text-secondary:#52514e; --text-muted:#898781;
  --border:rgba(11,11,11,0.10); --track:#f0efec;
  --pole-attack:#86c98a; --pole-threat:#e34948;
  background:var(--surface-1); color:var(--text-primary);
  font-family:Inter,system-ui,-apple-system,"Segoe UI",sans-serif;
  max-width:640px; padding:1.5rem; border-radius:12px;
  border:0.5px solid var(--border); line-height:1.45;}
@media (prefers-color-scheme:dark){:root:where(:not([data-theme="light"])) .gp-card{
  color-scheme:dark;
  --surface-1:#1a1a19; --surface-2:#0d0d0d;
  --text-primary:#ffffff; --text-secondary:#c3c2b7; --text-muted:#898781;
  --border:rgba(255,255,255,0.10); --track:#383835;
  --pole-attack:#9ad9a0; --pole-threat:#e66767;}}
:root[data-theme="dark"] .gp-card{color-scheme:dark;
  --surface-1:#1a1a19; --surface-2:#0d0d0d;
  --text-primary:#ffffff; --text-secondary:#c3c2b7; --text-muted:#898781;
  --border:rgba(255,255,255,0.10); --track:#383835;
  --pole-attack:#9ad9a0; --pole-threat:#e66767;}
.gp-card *{box-sizing:border-box;}
.gp-eyebrow{display:flex;justify-content:space-between;gap:1rem;
  font-size:13px;color:var(--text-muted);margin:0 0 4px;}
.gp-title{margin:0 0 2px;font-size:22px;font-weight:600;letter-spacing:-0.01em;}
.gp-pop{margin:0 0 0.75rem;font-size:12px;color:var(--text-muted);}
.gp-legend{display:flex;align-items:center;gap:8px;margin:0 0 1.25rem;}
.gp-legend-l{font-size:11px;color:var(--text-secondary);white-space:nowrap;}
.gp-legend-track{position:relative;width:120px;height:8px;border-radius:4px;
  --lg-c:var(--lg-l);background:var(--lg-c);}
@media (prefers-color-scheme:dark){:root:where(:not([data-theme="light"])) .gp-legend-track{
  --lg-c:var(--lg-d);}}
:root[data-theme="dark"] .gp-legend-track{--lg-c:var(--lg-d);}
.gp-legend-cap{margin:-0.9rem 0 1.25rem;font-size:11px;color:var(--text-muted);}
.gp-facts{display:flex;flex-wrap:wrap;gap:0;margin:0 0 1.5rem;
  border-top:0.5px solid var(--border);border-bottom:0.5px solid var(--border);}
.gp-fact{display:flex;gap:5px;align-items:baseline;padding:9px 10px 9px 0;
  margin-right:10px;font-size:12px;white-space:nowrap;}
.gp-fact:not(:last-child){border-right:0.5px solid var(--border);padding-right:10px;}
.gp-fact-l{color:var(--text-muted);}
.gp-fact-v{font-size:13px;font-weight:650;font-variant-numeric:tabular-nums;}
.gp-section{margin:0 0 10px;font-size:13px;font-weight:600;
  color:var(--text-secondary);}
.gp-priorities{border-top:0.5px solid var(--border);padding-top:14px;margin:0 0 1.5rem;}
.gp-priority{display:grid;grid-template-columns:7px minmax(0,1fr);gap:10px;
  padding:9px 0;border-bottom:0.5px solid var(--border);}
.gp-priority-mark{width:7px;height:100%;min-height:36px;border-radius:4px;}
.gp-priority-status{margin:0 0 2px;font-size:10px;font-weight:700;
  letter-spacing:.06em;color:var(--text-muted);}
.gp-priority-action{margin:0;font-size:16px;font-weight:700;line-height:1.25;}
.gp-priority-evidence{margin:3px 0 0;font-size:12px;color:var(--text-secondary);
  font-variant-numeric:tabular-nums;}
.gp-identity{margin:0 0 1.5rem;padding:12px 0;border-bottom:0.5px solid var(--border);}
.gp-identity-copy{margin:0 0 8px;font-size:14px;line-height:1.4;}
.gp-tags{display:flex;flex-wrap:wrap;gap:6px;}
.gp-tag{margin:0;padding:2px 7px;border:0.5px solid var(--border);border-radius:4px;
  font-size:11px;color:var(--text-secondary);font-variant-numeric:tabular-nums;}
.gp-group{margin:14px 0 6px;font-size:11px;font-weight:600;
  letter-spacing:0.04em;text-transform:uppercase;color:var(--text-muted);}
.gp-row{display:grid;grid-template-columns:1fr 132px 74px;gap:12px;
  align-items:center;padding:5px 0;}
.gp-name{margin:0;font-size:13px;}
.gp-meta{margin:0;font-size:11px;color:var(--text-muted);
  font-variant-numeric:tabular-nums;}
.gp-track{position:relative;height:8px;background:var(--track);
  border-radius:4px;}
.gp-mid{position:absolute;left:50%;top:-2px;bottom:-2px;width:1px;
  background:var(--border);}
.gp-fill{position:absolute;top:0;height:8px;--fill-c:var(--fill-l);}
@media (prefers-color-scheme:dark){:root:where(:not([data-theme="light"])) .gp-fill{
  --fill-c:var(--fill-d);}}
:root[data-theme="dark"] .gp-fill{--fill-c:var(--fill-d);}
.gp-rank{margin:0;font-size:11px;color:var(--text-secondary);
  text-align:right;font-variant-numeric:tabular-nums;}
.gp-keys{border-top:0.5px solid var(--border);margin-top:1.25rem;
  padding-top:14px;}
.gp-bullet{display:flex;gap:9px;align-items:flex-start;margin-bottom:7px;}
.gp-dot{flex:0 0 auto;width:7px;height:7px;border-radius:50%;margin-top:6px;}
.gp-bullet p{margin:0;font-size:12px;color:var(--text-secondary);}
.gp-none{margin:0;font-size:13px;color:var(--text-muted);font-style:italic;}
@media (max-width:480px){
  .gp-card{padding:1rem;}
  .gp-row{grid-template-columns:minmax(0,1fr) 92px 58px;gap:8px;}
  .gp-priority-action{font-size:15px;}
}
'

#' CSS gradient for the inline key, from the same stops the rows use
#'
#' The key has to be the rows in miniature or it teaches the wrong thing, so
#' it is generated from CARD_RAMP_* / the pole variables rather than written
#' out by hand. Left is always "below average", so under "quality" the ramp
#' runs worst -> best left to right and under "threat" it is reversed.
legend_gradient <- function(stops, means, scale) {
  if (!identical(scale, "ramp")) {
    lo <- if (identical(means, "threat")) "var(--pole-attack)" else "var(--pole-threat)"
    hi <- if (identical(means, "threat")) "var(--pole-threat)" else "var(--pole-attack)"
    return(sprintf("linear-gradient(to right, %s, %s)", lo, hi))
  }
  if (identical(means, "threat")) stops <- rev(stops)
  sprintf("linear-gradient(to right, %s)", paste(stops, collapse = ", "))
}


#' Interpolate a hex colour along a ramp
#'
#' @param t Position in [0, 1]; 0 is the first stop (worst), 1 the last.
ramp_color <- function(t, stops) {
  t <- max(0, min(1, t))
  n <- length(stops)
  if (n == 1) return(stops[1])
  pos <- t * (n - 1)
  i   <- min(floor(pos) + 1, n - 1)
  f   <- pos - (i - 1)
  c1  <- grDevices::col2rgb(stops[i])[, 1]
  c2  <- grDevices::col2rgb(stops[i + 1])[, 1]
  mix <- round(c1 + (c2 - c1) * f)
  sprintf("#%02x%02x%02x", mix[1], mix[2], mix[3])
}


#' One profile row, as a diverging bar
#'
#' `dev` runs -1 (worst in population) to +1 (best). The fill is anchored
#' at the centre and rounded only on its outer end — square where it meets
#' the baseline, per the mark spec.
#'
#' `standing` arrives PREFORMATTED from profile_row_standing(). It used to
#' be a rank plus a population size, formatted here as "%s of %d" — which
#' printed "18th of 18" beside a row labelled "Live-ball turnovers", the
#' rank of KEEPING the ball next to the name of losing it. The renderer is
#' the wrong place to know which dimensions inverse-rank, so it no longer
#' does.
gp_row <- function(label, meta, standing, pole_side, dev,
                   bar_style   = CARD_BAR_STYLE,
                   color_means = CARD_COLOR_MEANS) {
  above <- dev >= 0

  # Colour. Under "rank" the ramp runs red (worst in the population) to
  # green (best), matching the D3 spreadsheet. Under "threat" it is
  # inverted, because a thing they are BEST at is your biggest problem.
  if (!color_means %in% c("quality", "threat")) {
    stop("CARD_COLOR_MEANS must be \"quality\" or \"threat\", got \"",
         color_means, "\".", call. = FALSE)
  }
  pctl_t <- max(0, min(1, dev * 0.5 + 0.5))          # dev -1..1 -> 0..1
  t_col  <- if (identical(color_means, "threat")) 1 - pctl_t else pctl_t

  if (identical(CARD_COLOR_SCALE, "ramp")) {
    c_light <- ramp_color(t_col, CARD_RAMP_LIGHT)
    c_dark  <- ramp_color(t_col, CARD_RAMP_DARK)
    colr    <- "var(--fill-c)"
    theme_vars <- sprintf("--fill-l:%s;--fill-d:%s;", c_light, c_dark)
  } else {
    colr <- if (identical(color_means, "threat")) {
      if (above) "var(--pole-threat)" else "var(--pole-attack)"
    } else {
      if (above) "var(--pole-attack)" else "var(--pole-threat)"
    }
    theme_vars <- ""
  }

  style <- paste0(theme_vars, if (identical(bar_style, "magnitude")) {
    # Percentile recovered from dev = (pctl - 50) / 50, so the bar spans the
    # whole track: 0% at worst in the population, 100% at best.
    sprintf("left:0;width:%.1f%%;border-radius:4px;background:%s;",
            max(0, min(100, dev * 50 + 50)), colr)
  } else if (above) {
    sprintf("left:50%%;width:%.1f%%;border-radius:0 4px 4px 0;background:%s;",
            abs(dev) * 50, colr)
  } else {
    sprintf("right:50%%;width:%.1f%%;border-radius:4px 0 0 4px;background:%s;",
            abs(dev) * 50, colr)
  })

  # The centre tick only means something on a diverging bar.
  mid <- if (identical(bar_style, "diverging")) '<div class="gp-mid"></div>' else ""

  sprintf(paste0(
    '<div class="gp-row"><div><p class="gp-name">%s</p><p class="gp-meta">%s</p></div>',
    '<div class="gp-track">%s',
    '<div class="gp-fill" style="%s"></div></div>',
    '<p class="gp-rank">%s</p></div>'),
    esc(label), esc(meta), mid, style, esc(standing))
}

#' Split a deterministic key sentence into its coaching action and evidence
#'
#' `compose_key_sentence()` is the single source of the prose. The renderer
#' only changes its hierarchy: the clause after the em dash is the action a
#' coach should see first, while the preceding metric sentence is evidence.
split_key_sentence <- function(sentence) {
  parts <- strsplit(sentence, " — ", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(list(action = sentence, evidence = ""))
  action <- sub("\\.$", "", parts[length(parts)])
  action <- paste0(toupper(substr(action, 1, 1)), substr(action, 2, nchar(action)))
  list(action = action,
       evidence = paste(parts[-length(parts)], collapse = " — "))
}

#' Three highest-salience coaching points across strengths and weaknesses
#'
#' The selectors already gate noisy rows and calculate comparable key scores.
#' Taking the top three here changes display density, not the basketball rule.
priority_keys <- function(keys, max_keys = 3L) {
  out <- list()
  for (k in keys) {
    n <- min(nrow(k$rows), length(k$bullets))
    if (n == 0) next
    for (i in seq_len(n)) {
      out[[length(out) + 1L]] <- list(
        side = k$side,
        score = k$rows$key_score[i],
        sentence = k$bullets[i]
      )
    }
  }
  if (!length(out)) return(out)
  out <- out[order(vapply(out, `[[`, numeric(1), "score"), decreasing = TRUE)]
  utils::head(out, max_keys)
}

#' A restrained, data-backed identity sentence from shot-volume leaders
opponent_identity <- function(card, display) {
  zones <- card[card$dimension == "shot_zone" & !is.na(card$freq), ]
  if (nrow(zones) == 0) return(list(copy = "No shot-profile identity is available.", tags = character(0)))
  zones <- zones[order(-zones$freq), ]
  zones <- utils::head(zones, 3L)
  labels <- vapply(seq_len(nrow(zones)), function(i) {
    display$display_label[display$source == zones$source[i] &
                          display$dimension == zones$dimension[i] &
                          display$category == zones$category[i]][1]
  }, character(1))
  tags <- sprintf("%s %d%%", labels, round(zones$freq * 100))
  top <- tolower(labels[1])
  list(copy = sprintf("Their shot volume begins with %s.", top), tags = tags)
}

#' Compact supporting evidence for the lower scouting sections
#'
#' Priorities above carry the action. These rows deliberately keep only the
#' named tendency, its volume and its standing, so the lower card does not
#' repeat the full analytic sentence and its coaching clause.
compact_key_text <- function(row, display, pop_n, population) {
  label <- display$display_label[display$source == row$source &
                                 display$dimension == row$dimension &
                                 display$category == row$category][1]
  volume <- sprintf("%d%% %s", round(row$freq * 100), UNIT_DENOMINATOR[[row$unit]])
  # Which dimensions rank inverted is a property of the data layer, not of
  # this card. Naming "ball_security" here meant a second inverted
  # dimension would silently get the wrong wording.
  standing <- profile_row_standing(row$ppp_percentile, pop_n, population,
                                   row$dimension, row$category)
  paste(label, volume, standing, sep = " · ")
}

#' Render the whole card
#'
#' @param stats Named list for the header tiles, normally from
#'   `team_header_stats(team_season_summary(...), opponent)`. Expects
#'   `record` and `pace` as preformatted strings, and `efg`, `tov_pct`,
#'   `orb_pct`, `ft_rate` as PROPORTIONS so the renderer sets precision.
#'   Anything absent renders as an em dash -- never as a stale or invented
#'   value, which is what the hand-typed tiles used to be.
#' @param file Optional path to write to. Returns the HTML either way.
render_pregame_card <- function(profile, team, opponent,
                                date_label = NULL, venue = NULL,
                                stats      = list(),
                                standing   = "auto",
                                file       = NULL,
                                display    = load_category_display(), ...) {

  keys  <- build_keys(profile, team, opponent, standing = standing,
                      display = display, ...)
  pop   <- attr(keys, "population")
  pop_n <- attr(keys, "population_n")
  mode  <- attr(keys, "standing")

  card  <- attack_index_card(profile, opponent, team)
  rows  <- bullet_pool(card, display)
  heads <- load_dimension_headers()

  ## --- tiles ---------------------------------------------------
  # Six tiles: record and pace (kept), then the opponent's four factors.
  #
  # 3PT rate and Last 5 came out. 3PT rate is largely restated by the
  # shot-zone rows below it, and a last-5 record is form rather than
  # quality -- neither earned space against the four factors, which say
  # something the rest of the card cannot: this card is entirely about
  # shot location and possession context, so without these it never
  # mentions turnovers, rebounding or free throws at all.
  #
  # TOV% and FTr are labelled with their own definitions because TOV%
  # uses the 0.44 possession estimator while `Pace` beside it comes from
  # counted possessions. The two differ ~2.7%; the labels stop a reader
  # assuming one divides into the other.
  fact <- function(l, v) sprintf(
    '<span class="gp-fact"><span class="gp-fact-l">%s</span><span class="gp-fact-v">%s</span></span>',
    esc(l), esc(v))
  pct1 <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) "—" else
    sprintf("%.1f%%", 100 * as.numeric(x))

  facts <- paste0(
    fact("Record",     stats$record %||% "—"),
    fact("Pace",       stats$pace   %||% "—"),
    fact("eFG%",       pct1(stats$efg)),
    fact("TOV%",       pct1(stats$tov_pct)),
    fact("ORB%",       pct1(stats$orb_pct)),
    fact("FT rate",    pct1(stats$ft_rate)))

  priorities <- priority_keys(keys)
  priority_html <- ""
  for (k in priorities) {
    content <- split_key_sentence(k$sentence)
    threat <- identical(k$side, "strength")
    marker <- if (threat) "var(--pole-threat)" else "var(--pole-attack)"
    status <- if (threat) "MUST RESPECT" else "COACHING POINT"
    priority_html <- paste0(priority_html, sprintf(paste0(
      '<div class="gp-priority"><span class="gp-priority-mark" style="background:%s"></span><div>',
      '<p class="gp-priority-status">%s</p><p class="gp-priority-action">%s</p>',
      '<p class="gp-priority-evidence">%s</p></div></div>'),
      marker, status, esc(content$action), esc(content$evidence)))
  }
  if (!nzchar(priority_html)) {
    priority_html <- '<p class="gp-none">No opponent tendency clears the coaching threshold.</p>'
  }

  identity <- opponent_identity(card, display)
  identity_tags <- paste(sprintf('<span class="gp-tag">%s</span>', esc(identity$tags)), collapse = "")

  ## --- profile rows, grouped by dimension ----------------------
  body <- ""
  for (dim in unique(rows$dimension)) {
    d  <- rows[rows$dimension == dim, ]
    d  <- d[order(-d$ppp_percentile), ]
    hd <- heads[heads$dimension == dim, ]
    body <- paste0(body, sprintf('<p class="gp-group">%s &middot; %s</p>',
                                 esc(hd$display_label[1]), esc(hd$ppp_definition[1])))
    for (i in seq_len(nrow(d))) {
      r    <- d[i, ]
      lab  <- display$display_label[display$source == r$source &
                                    display$dimension == r$dimension &
                                    display$category == r$category][1]
      # -1 = worst in population, +1 = best. Mid-pack lands on 0.
      dev  <- (r$ppp_percentile - 50) / 50
      # Both of these consult config, not this file: the PPP clause is
      # dropped where the rate is degenerate, and the standing reads in
      # whichever direction the row's own label names.
      meta <- profile_row_meta(r$freq, r$ppp, r$unit, r$dimension, r$source, heads)
      stan <- profile_row_standing(r$ppp_percentile, pop_n, pop, r$dimension,
                                   r$category, style = "compact")
      body <- paste0(body, gp_row(lab, meta, stan,
                                  if (dev >= 0) "strong" else "weak", dev))
    }
  }

  ## --- keys ----------------------------------------------------
  bullets <- ""
  for (k in keys) {
    # Same convention as the bars, so the dot and the row agree.
    colr <- if (identical(CARD_COLOR_MEANS, "quality")) {
      if (k$side == "strength") "var(--pole-attack)" else "var(--pole-threat)"
    } else {
      if (k$side == "strength") "var(--pole-threat)" else "var(--pole-attack)"
    }
    bullets <- paste0(bullets, sprintf('<p class="gp-section" style="margin-top:14px;">%s</p>',
                                       esc(k$heading)))
    if (nrow(k$rows) == 0) {
      bullets <- paste0(bullets,
        '<p class="gp-none">Nothing separates them here.</p>')
    } else {
      for (i in seq_len(nrow(k$rows))) {
        b <- compact_key_text(k$rows[i, ], display, pop_n, pop)
        bullets <- paste0(bullets, sprintf(
          '<div class="gp-bullet"><span class="gp-dot" style="background:%s"></span><p>%s</p></div>',
          colr, esc(b)))
      }
    }
  }

  eyebrow_r <- paste(c(date_label, venue), collapse = " · ")
  html <- paste0(
    "<style>", CARD_CSS, "</style>\n",
    '<div class="gp-card">',
    sprintf('<div class="gp-eyebrow"><span>Pregame report</span><span>%s</span></div>',
            esc(eyebrow_r)),
    sprintf('<h2 class="gp-title">%s vs %s</h2>', esc(team), esc(opponent)),
    sprintf('<p class="gp-pop">Opponent ranked within %s &middot; %d teams</p>',
            esc(pop), pop_n),
    sprintf('<div class="gp-facts">%s</div>', facts),
    '<div class="gp-priorities"><p class="gp-section">Tonight&rsquo;s 3 keys</p>',
    priority_html,
    '</div>',
    '<div class="gp-identity"><p class="gp-section">Opponent identity</p>',
    sprintf('<p class="gp-identity-copy">%s</p>', esc(identity$copy)),
    sprintf('<div class="gp-tags">%s</div></div>', identity_tags),
    sprintf('<p class="gp-section">%s &middot; full profile</p>', esc(opponent)),
    body,
    sprintf('<div class="gp-keys">%s</div>', bullets),
    '</div>')

  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    doc <- paste0(
      "<!doctype html>\n<html lang=\"en\">\n<head>\n",
      "<meta charset=\"utf-8\">\n",
      "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n",
      sprintf("<title>%s vs %s &mdash; pregame</title>\n", esc(team), esc(opponent)),
      "</head>\n<body>\n", html, "\n</body>\n</html>\n")
    # useBytes so R does not re-encode what is already UTF-8
    # base::file() explicitly -- `file` is this function's own parameter, so a
    # bare file(file, ...) would try to call a character vector.
    con <- base::file(file, open = "wb")
    on.exit(close(con), add = TRUE)
    writeLines(enc2utf8(doc), con, useBytes = TRUE)
  }
  invisible(html)
}
