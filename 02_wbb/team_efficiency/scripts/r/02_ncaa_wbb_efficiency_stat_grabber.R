###############################################
# Title: NCAA WBB Efficiency Calculator
# Purpose: Fetches 2025-26 NCAA WBB team stats via wehoop (ESPN)
#          Calculates Offensive Rating, Defensive Rating, and Net Rating
#          Outputs CSV for R visualization
# Author: Alycia Webster
# Data Source: wehoop (ESPN)
# Season: 2025-26 (wehoop uses end year = 2026)
###############################################

# 0. Install packages (run once if needed)
# install.packages("wehoop")
# install.packages("dplyr")
# install.packages("readr")

library(wehoop)
library(dplyr)
library(readr)

###############################################
# 1. Pull team box scores (2025-26 season)
###############################################

# wehoop uses the END year of the season -> 2026 for 2025-26
wbb_team_box_2026 <- load_wbb_team_box(seasons = 2026)

###############################################
# 2. Filter to valid games
###############################################

wbb_team_box_2026_clean <- wbb_team_box_2026 %>%
  filter(
    season_type == 2,                  # regular season only
    !is.na(team_score),                # must have a final score
    !is.na(field_goals_attempted),
    field_goals_attempted > 0          # must have shot attempts (played the game)
  )

###############################################
# 3. Self-join to pair each team with its opponent per game
#    (same pattern as the player box script)
###############################################

team_game_w_opp_2026 <- wbb_team_box_2026_clean %>%
  left_join(
    wbb_team_box_2026_clean,
    by = "game_id",
    suffix = c("", "_opp"),
    relationship = "many-to-many"
  ) %>%
  filter(team_id != team_id_opp)

###############################################
# 3.5 Filter to NCAA Tournament teams only
###############################################

tournament_teams <- c(
  "UConn Huskies", "UCLA Bruins", "Texas Longhorns", "South Carolina Gamecocks",
  "Alabama Crimson Tide", "Arizona State Sun Devils", "Baylor Bears", "BYU Cougars",
  "California Baptist Lancers", "Charleston Cougars", "Clemson Tigers",
  "Colorado Buffaloes", "Colorado State Rams", "Duke Blue Devils",
  "Fairfield Stags", "Fairleigh Dickinson Knights", "Georgia Lady Bulldogs",
  "Gonzaga Bulldogs", "Green Bay Phoenix", "High Point Panthers",
  "Holy Cross Crusaders", "Howard Bison", "Idaho Vandals",
  "Illinois Fighting Illini", "Iowa Hawkeyes", "Iowa State Cyclones",
  "Jacksonville Dolphins", "James Madison Dukes", "Kentucky Wildcats",
  "LSU Tigers", "Louisville Cardinals", "Maryland Terrapins",
  "Miami (OH) RedHawks", "Michigan Wolverines", "Michigan State Spartans",
  "Minnesota Golden Gophers", "Missouri State Lady Bears", "Murray State Racers",
  "NC State Wolfpack", "Nebraska Cornhuskers", "North Carolina Tar Heels",
  "Notre Dame Fighting Irish", "Ohio State Buckeyes", "Oklahoma Sooners",
  "Oklahoma State Cowgirls", "Ole Miss Rebels", "Oregon Ducks",
  "Princeton Tigers", "Rhode Island Rams", "Richmond Spiders",
  "Samford Bulldogs", "South Dakota State Jackrabbits", "Southern Jaguars",
  "Stephen F. Austin Ladyjacks", "Syracuse Orange", "TCU Horned Frogs",
  "Tennessee Lady Volunteers", "Texas Tech Lady Raiders", "UC San Diego Tritons",
  "UTSA Roadrunners", "Vanderbilt Commodores", "Vermont Catamounts",
  "Villanova Wildcats", "Virginia Cavaliers", "Virginia Tech Hokies",
  "USC Trojans", "Washington Huskies", "West Virginia Mountaineers",
  "Western Illinois Leathernecks"
)

team_game_w_opp_2026_filtered <- team_game_w_opp_2026 %>%
  filter(team_display_name %in% tournament_teams)

###############################################
# 4. Aggregate to season totals (team + opponent)
###############################################

team_season_totals_2026 <- team_game_w_opp_2026_filtered %>%
  group_by(
    team_id,
    team_display_name,
    team_short_display_name,
    team_abbreviation,
    team_logo
  ) %>%
  summarise(
    games_played      = n_distinct(game_id),
    total_pts         = sum(team_score,               na.rm = TRUE),
    total_pts_allowed = sum(team_score_opp,           na.rm = TRUE),
    total_fga         = sum(field_goals_attempted,    na.rm = TRUE),
    total_fgm         = sum(field_goals_made,         na.rm = TRUE),
    total_fta         = sum(free_throws_attempted,    na.rm = TRUE),
    total_tov         = sum(turnovers,                na.rm = TRUE),
    total_oreb        = sum(offensive_rebounds,       na.rm = TRUE),
    total_dreb        = sum(defensive_rebounds,       na.rm = TRUE),

    # The opponent side, needed for the possession estimate below
    total_fga_opp     = sum(field_goals_attempted_opp, na.rm = TRUE),
    total_fgm_opp     = sum(field_goals_made_opp,      na.rm = TRUE),
    total_fta_opp     = sum(free_throws_attempted_opp, na.rm = TRUE),
    total_tov_opp     = sum(turnovers_opp,             na.rm = TRUE),
    total_oreb_opp    = sum(offensive_rebounds_opp,    na.rm = TRUE),
    total_dreb_opp    = sum(defensive_rebounds_opp,    na.rm = TRUE),
    .groups = "drop"
  )

###############################################
# 5. Calculate Possessions, OER, DER, Net Rating
###############################################

team_efficiency_2026 <- team_season_totals_2026 %>%
  mutate(
    # Possessions, Oliver's estimate, averaged across the two teams.
    #
    #   FGA + 0.44*FTA + TOV - 1.07 * (OREB/(OREB+OppDREB)) * (FGA - FGM)
    #
    # The simpler FGA + 0.44*FTA + TOV - OREB subtracts the raw offensive
    # rebound count, which undercounts extended possessions: team offensive
    # rebounds and missed-free-throw rebounds never get credited to a player,
    # so they never reach the box score. Oliver's term estimates the true
    # count instead of trusting the tally.
    #
    # Measured on the NBA, where an official figure exists to check against,
    # the simple version overstates possessions by ~1.8 a game and so reads
    # 2.0 points LOW on both ORtg and DRtg. Net rating survives, since both
    # ends shift together, but neither component matches. See
    # 03_nba/team_efficiency/scripts/r/nba_metrics.R for that validation.
    #
    # NOTE: the 1.07 and 0.44 coefficients were fitted on NBA data. Nobody
    # publishes an official possession count for this league to check them
    # against, so treat these as the NBA convention applied here rather than
    # as verified for the women's college game.
    orb_rate_    = ifelse((total_oreb + total_dreb_opp) > 0,
                          total_oreb / (total_oreb + total_dreb_opp), 0),
    orb_rate_opp = ifelse((total_oreb_opp + total_dreb) > 0,
                          total_oreb_opp / (total_oreb_opp + total_dreb), 0),
    poss_tm_  = total_fga + 0.44 * total_fta + total_tov -
                  1.07 * orb_rate_ * (total_fga - total_fgm),
    poss_opp_ = total_fga_opp + 0.44 * total_fta_opp + total_tov_opp -
                  1.07 * orb_rate_opp * (total_fga_opp - total_fgm_opp),
    possessions = (poss_tm_ + poss_opp_) / 2,

    # Efficiency ratings (points per 100 possessions)
    ortg    = 100 * (total_pts         / possessions),
    drtg    = 100 * (total_pts_allowed / possessions),
    net_rtg = ortg - drtg
  ) %>%
  select(-orb_rate_, -orb_rate_opp, -poss_tm_, -poss_opp_) %>%
  arrange(desc(net_rtg))

###############################################
# 6. Preview results
###############################################

cat("NCAA WBB Efficiency Ratings - 2025-26 Season\n")
cat(rep("=", 60), "\n", sep = "")
print(
  team_efficiency_2026 %>%
    select(team_short_display_name, games_played, ortg, drtg, net_rtg) %>%
    head(25)
)

###############################################
# 7. Save to CSV
###############################################

# Where everything this script writes is saved -- the chart and the CSV.
# Change this one line to send them somewhere else, e.g. "~/Documents/scouting".
save_dir <- "~/Desktop"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

output_dir <- save_dir   # charts
data_dir   <- save_dir   # CSVs

out_csv <- file.path(data_dir, "ncaa_wbb_efficiency_2026.csv")
write_csv(team_efficiency_2026, out_csv)

cat("\n✅ CSV saved to ", out_csv, "\n", sep = "")

###############################################
# End of script
###############################################
