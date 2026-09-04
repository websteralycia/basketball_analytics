###############################################
# Title: Unrivaled Efficiency Calculator
# Purpose: Fetches 2025 Unrivaled team stats from SportRadar API
#          Calculates Offensive Rating, Defensive Rating, and Net Rating
#          Outputs CSV for R visualization
# Author: Alycia Webster
# Data Source: SportRadar Unrivaled API v8
###############################################

import os
import requests
import pandas as pd
import time
import json

# ============================================
# CONFIGURATION
# ============================================

# Load API key from environment variable
API_KEY = os.getenv('SPORTRADAR_API_KEY')

if not API_KEY:
    raise ValueError(
        "API key not found! Please set the SPORTRADAR_API_KEY environment variable.\n"
        "Add this to your ~/.zshrc or ~/.bash_profile:\n"
        "export SPORTRADAR_API_KEY='your_actual_api_key_here'\n"
        "Then run: source ~/.zshrc"
    )

# API Configuration
BASE_URL = "https://api.sportradar.com/unrivaled"
ACCESS_LEVEL = "trial"  # Change to "production" if you have paid access
VERSION = "v8"
LANGUAGE = "en"
SEASON_YEAR = 2026
SEASON_TYPE = "REG"  # Regular season
# Where the CSV lands.
#   Default (nothing to set up): your Desktop.
#   Optional: export BBALL_HOME=/path/to/your/analytics/folder and the file
#   routes to <BBALL_HOME>/05_unrivaled/data/ instead.
LEAGUE      = "05_unrivaled"
BBALL_HOME  = os.getenv("BBALL_HOME", "")
OUTPUT_DIR  = (os.path.join(BBALL_HOME, LEAGUE, "data") if BBALL_HOME
               else os.path.expanduser("~/Desktop"))
os.makedirs(OUTPUT_DIR, exist_ok=True)
OUTPUT_FILE = os.path.join(OUTPUT_DIR, "Unrivaled_Efficiency_2026.csv")

# Rate limiting (be nice to the API)
REQUEST_DELAY = 1.5  # seconds between requests

# ============================================
# HELPER FUNCTIONS
# ============================================

def make_api_request(endpoint_url):
    """
    Makes an API request with proper headers and error handling.
    """
    headers = {
        "accept": "application/json",
        "x-api-key": API_KEY
    }
    
    try:
        print(f"Fetching: {endpoint_url}")
        response = requests.get(endpoint_url, headers=headers)
        response.raise_for_status()  # Raises error for bad status codes
        time.sleep(REQUEST_DELAY)  # Rate limiting
        return response.json()
    except requests.exceptions.HTTPError as e:
        if response.status_code == 401:
            print(f"❌ Authentication Error: Invalid API key")
        elif response.status_code == 403:
            print(f"❌ Access Forbidden: Check your access level and subscription")
        elif response.status_code == 404:
            print(f"❌ Not Found: Endpoint doesn't exist")
        else:
            print(f"❌ HTTP Error: {e}")
        return None
    except Exception as e:
        print(f"❌ Error making request: {e}")
        return None


def get_season_schedule():
    """
    Fetches the full season schedule to get all game IDs.
    """
    url = f"{BASE_URL}/{ACCESS_LEVEL}/{VERSION}/{LANGUAGE}/games/{SEASON_YEAR}/{SEASON_TYPE}/schedule.json"
    data = make_api_request(url)
    
    if not data or 'games' not in data:
        print("❌ Failed to fetch schedule")
        return []
    
    games = data['games']
    print(f"✅ Found {len(games)} games in the {SEASON_YEAR} {SEASON_TYPE} season")
    
    # Filter only completed games
    completed_games = [g for g in games if g.get('status') == 'closed']
    print(f"✅ {len(completed_games)} games have been completed")
    
    return completed_games


def get_game_summary(game_id):
    """
    Fetches detailed game summary with full team statistics.
    """
    url = f"{BASE_URL}/{ACCESS_LEVEL}/{VERSION}/{LANGUAGE}/games/{game_id}/summary.json"
    return make_api_request(url)


def get_league_hierarchy():
    """
    Fetches league hierarchy to get team info and logos.
    """
    url = f"{BASE_URL}/{ACCESS_LEVEL}/{VERSION}/{LANGUAGE}/league/hierarchy.json"
    return make_api_request(url)


def extract_team_game_stats(game_summary):
    """
    Extracts team statistics from a game summary.
    Returns a list of dictionaries with team stats for both home and away teams.
    """
    if not game_summary:
        return []
    
    game_id = game_summary.get('id')
    home_team = game_summary.get('home', {})
    away_team = game_summary.get('away', {})
    
    # Skip games with missing team data
    if not home_team or not away_team:
        return []
    
    team_stats = []
    
    for team in [home_team, away_team]:
        stats = team.get('statistics', {})
        
        # Skip if statistics are missing or all zeros
        if not stats or stats.get('field_goals_att', 0) == 0:
            return []
        
        team_data = {
            'game_id': game_id,
            'team_id': team.get('id'),
            'team_name': team.get('name'),
            'team_alias': team.get('alias'),
            'pts': stats.get('points', 0),
            'fga': stats.get('field_goals_att', 0),
            'fta': stats.get('free_throws_att', 0),
            'tov': stats.get('total_turnovers', 0),
            'oreb': stats.get('offensive_rebounds', 0) + stats.get('team_offensive_rebounds', 0),
        }
        team_stats.append(team_data)
    
    return team_stats


def calculate_efficiency_metrics(team_game_data):
    """
    Calculates ORtg, DRtg, and Net Rating for each team.
    Modified formula for Unrivaled (no FTA multiplier due to different free throw rules):
    Possessions = FGA + TOV - OREB
    ORtg = 100 * (Points Scored / Possessions)
    DRtg = 100 * (Points Allowed / Possessions)
    Net Rating = ORtg - DRtg
    """
    df = pd.DataFrame(team_game_data)
    
    # Create opponent stats by merging on game_id
    df_with_opp = df.merge(
        df, 
        on='game_id', 
        suffixes=('', '_opp')
    )
    
    # Filter to get only the opponent (not self)
    df_with_opp = df_with_opp[df_with_opp['team_id'] != df_with_opp['team_id_opp']]
    
    # Aggregate by team
    team_totals = df_with_opp.groupby(['team_id', 'team_name', 'team_alias']).agg({
        'pts': 'sum',
        'pts_opp': 'sum',
        'fga': 'sum',
        'fta': 'sum',
        'tov': 'sum',
        'oreb': 'sum',
    }).reset_index()
    
    # Rename columns for clarity
    team_totals.rename(columns={
        'pts': 'total_pts_scored',
        'pts_opp': 'total_pts_allowed',
        'fga': 'total_fga',
        'fta': 'total_fta',
        'tov': 'total_tov',
        'oreb': 'total_oreb'
    }, inplace=True)
    
    # Calculate possessions using the modified formula for Unrivaled
    team_totals['possessions'] = (
        team_totals['total_fga'] + 
        team_totals['total_tov'] - 
        team_totals['total_oreb']
    )
    
    # Calculate efficiency ratings
    team_totals['ortg'] = 100 * (team_totals['total_pts_scored'] / team_totals['possessions'])
    team_totals['drtg'] = 100 * (team_totals['total_pts_allowed'] / team_totals['possessions'])
    team_totals['net_rtg'] = team_totals['ortg'] - team_totals['drtg']
    
    return team_totals


def fetch_team_logos(team_efficiency_df):
    """
    Adds team logos using hardcoded URLs from Unrivaled's CDN.
    The API doesn't include logo URLs, so we map them manually.
    """
    # Hardcoded logo mapping based on team names
    # Logo URL pattern: https://pub-ad8cc693759b4b55a181a76af041efa0.r2.dev/teams/{team}/images/logo/primary.png
    
    logo_map_by_name = {
        'Mist': 'https://pub-ad8cc693759b4b55a181a76af041efa0.r2.dev/teams/mist/images/logo/primary.png',
        'Rose': 'https://pub-ad8cc693759b4b55a181a76af041efa0.r2.dev/teams/rose/images/logo/primary.png',
        'Vinyl': 'https://pub-ad8cc693759b4b55a181a76af041efa0.r2.dev/teams/vinyl/images/logo/primary.png',
        'Laces': 'https://pub-ad8cc693759b4b55a181a76af041efa0.r2.dev/teams/laces/images/logo/primary.png',
        'Lunar Owls': 'https://pub-ad8cc693759b4b55a181a76af041efa0.r2.dev/teams/lunar-owls/images/logo/primary.png',
        'Phantom': 'https://pub-ad8cc693759b4b55a181a76af041efa0.r2.dev/teams/phantom/images/logo/primary.png',
        'Hive': 'https://pub-ad8cc693759b4b55a181a76af041efa0.r2.dev/teams/hive/images/logo/primary.png',
        'Breeze': 'https://pub-ad8cc693759b4b55a181a76af041efa0.r2.dev/teams/breeze/images/logo/primary.png',
    }
    
    # Add logo column by mapping team names
    team_efficiency_df['logo'] = team_efficiency_df['team_name'].map(logo_map_by_name)
    
    logos_found = team_efficiency_df['logo'].notna().sum()
    print(f"✅ Mapped logos for {logos_found} teams")
    
    # Print any teams without logos (for debugging)
    missing_logos = team_efficiency_df[team_efficiency_df['logo'].isna()]['team_name'].tolist()
    if missing_logos:
        print(f"⚠️  Teams without logos: {missing_logos}")
    
    return team_efficiency_df


# ============================================
# MAIN EXECUTION
# ============================================

def main():
    print("=" * 60)
    print("UNRIVALED EFFICIENCY CALCULATOR")
    print("=" * 60)
    print()
    
    # Step 1: Get the season schedule
    print("📅 Step 1: Fetching season schedule...")
    games = get_season_schedule()
    
    if not games:
        print("❌ No games found. Exiting.")
        return
    
    print()
    
    # Step 2: Fetch game summaries for all completed games
    print("📊 Step 2: Fetching game summaries...")
    all_team_game_stats = []
    games_processed = 0
    games_skipped = 0
    
    for i, game in enumerate(games, 1):
        game_id = game['id']
        print(f"  [{i}/{len(games)}] Game ID: {game_id}")
        
        game_summary = get_game_summary(game_id)
        
        if game_summary:
            team_stats = extract_team_game_stats(game_summary)
            if team_stats:
                all_team_game_stats.extend(team_stats)
                games_processed += 1
            else:
                games_skipped += 1
                print(f"    ⚠️  Skipped (no valid statistics)")
    
    print(f"✅ Processed {games_processed} games with valid data")
    print(f"⚠️  Skipped {games_skipped} games with missing/incomplete data")
    print(f"✅ Collected stats from {len(all_team_game_stats)} team-game records")
    print()
    
    # Step 3: Calculate efficiency metrics
    print("🧮 Step 3: Calculating efficiency metrics...")
    team_efficiency = calculate_efficiency_metrics(all_team_game_stats)
    print(f"✅ Calculated efficiency for {len(team_efficiency)} teams")
    print()
    
    # Step 4: Fetch team logos
    print("🖼️  Step 4: Adding team logos...")
    team_efficiency = fetch_team_logos(team_efficiency)
    print()
    
    # Step 5: Display results
    print("=" * 60)
    print("RESULTS")
    print("=" * 60)
    print()
    print(team_efficiency[['team_name', 'ortg', 'drtg', 'net_rtg']].to_string(index=False))
    print()
    
    # Step 6: Save to CSV
    print(f"💾 Step 5: Saving results to {OUTPUT_FILE}...")
    team_efficiency.to_csv(OUTPUT_FILE, index=False)
    print(f"✅ CSV saved successfully!")
    print()
    print("=" * 60)
    print("DONE! You can now use this CSV in your R plotting script.")
    print("=" * 60)


if __name__ == "__main__":
    main()