###############################################
# Title: NCAA MBB Efficiency Calculator
# Purpose: Fetches 2025-26 NCAA MBB team stats via hoopR (ESPN)
#          Calculates Offensive Rating, Defensive Rating, and Net Rating
#          Outputs CSV for R visualization
# Author: Alycia Webster
# Data Source: hoopR (ESPN)
# Season: 2025-26 (hoopR uses end year = 2026)
###############################################

# 0. Install packages (run once if needed)
# install.packages("hoopR")
# install.packages("dplyr")
# install.packages("readr")

library(hoopR)
library(dplyr)
library(readr)

###############################################
# 1. Pull team box scores (2025-26 season)
###############################################

# hoopR uses the END year of the season -> 2026 for 2025-26
mbb_team_box_2026 <- load_mbb_team_box(seasons = 2026)

###############################################
# 2. Filter to valid games
###############################################

mbb_team_box_2026_clean <- mbb_team_box_2026 %>%
  filter(
    season_type == 2,                  # regular season only
    !is.na(team_score),                # must have a final score
    !is.na(field_goals_attempted),
    field_goals_attempted > 0          # must have shot attempts (played the game)
  )

###############################################
# 3. Self-join to pair each team with its opponent per game
###############################################

team_game_w_opp_2026 <- mbb_team_box_2026_clean %>%
  left_join(
    mbb_team_box_2026_clean,
    by = "game_id",
    suffix = c("", "_opp"),
    relationship = "many-to-many"
  ) %>%
  filter(team_id != team_id_opp)

###############################################
# 3.5 Filter to NCAA Tournament teams only
###############################################

tournament_teams <- c(
  "Duke Blue Devils", "Arizona Wildcats", "Michigan Wolverines",
  "Florida Gators", "Houston Cougars", "UConn Huskies",
  "Iowa State Cyclones", "Purdue Boilermakers", "Michigan State Spartans",
  "Illinois Fighting Illini", "Virginia Cavaliers", "Gonzaga Bulldogs",
  "Kansas Jayhawks", "Alabama Crimson Tide", "Nebraska Cornhuskers",
  "Arkansas Razorbacks", "St. John's Red Storm", "Texas Tech Red Raiders",
  "Vanderbilt Commodores", "Wisconsin Badgers", "Louisville Cardinals",
  "Tennessee Volunteers", "North Carolina Tar Heels", "BYU Cougars",
  "UCLA Bruins", "Kentucky Wildcats", "Saint Mary's Gaels",
  "Miami Hurricanes", "Ohio State Buckeyes", "Georgia Bulldogs",
  "Clemson Tigers", "Villanova Wildcats", "TCU Horned Frogs",
  "Saint Louis Billikens", "Iowa Hawkeyes", "Utah State Aggies",
  "UCF Knights", "Santa Clara Broncos", "Texas A&M Aggies",
  "Missouri Tigers", "South Florida Bulls", "Miami (OH) RedHawks",
  "SMU Mustangs", "VCU Rams", "Texas Longhorns",
  "NC State Wolfpack", "Northern Iowa Panthers", "Akron Zips",
  "McNeese Cowboys", "High Point Panthers", "California Baptist Lancers",
  "Hawai'i Rainbow Warriors", "Hofstra Pride", "Troy Trojans",
  "North Dakota State Bison", "Wright State Raiders", "Kennesaw State Owls",
  "Furman Paladins", "Tennessee State Tigers", "Pennsylvania Quakers",
  "Long Island University Sharks", "Siena Saints", "UMBC Retrievers",
  "Howard Bison", "Prairie View A&M Panthers", "Lehigh Mountain Hawks",
  "Idaho Vandals", "Queens University Royals"
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
    total_fta         = sum(free_throws_attempted,    na.rm = TRUE),
    total_tov         = sum(turnovers,                na.rm = TRUE),
    total_oreb        = sum(offensive_rebounds,       na.rm = TRUE),
    .groups = "drop"
  )

###############################################
# 5. Calculate Possessions, OER, DER, Net Rating
###############################################

team_efficiency_2026 <- team_season_totals_2026 %>%
  mutate(
    # Standard NCAA possession estimate
    possessions = total_fga + 0.44 * total_fta + total_tov - total_oreb,

    # Efficiency ratings (points per 100 possessions)
    ortg    = 100 * (total_pts         / possessions),
    drtg    = 100 * (total_pts_allowed / possessions),
    net_rtg = ortg - drtg
  ) %>%
  arrange(desc(net_rtg))

###############################################
# 6. Preview results
###############################################

cat("NCAA MBB Efficiency Ratings - 2025-26 Season\n")
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

out_csv <- file.path(data_dir, "ncaa_mbb_efficiency_2026.csv")
write_csv(team_efficiency_2026, out_csv)

cat("\n✅ CSV saved to ", out_csv, "\n", sep = "")

###############################################
# End of script
###############################################
