#### Preamble ####
# Purpose: Cleans the downloaded raw data
# Author: Daniel Du
# Date: 24 November 2024
# Contact: danielc.du@mail.utoronto.ca
# License: MIT
# Pre-requisites: Downloaded and saved data in scripts/02-download_data.R
# Any other information needed? 
#   - Make sure you are in the `HockeyShotAnalysis` rproj
#   - Know the relevant variables to clean and prepare for analysis

#### Install necessary packages
library(arrow)
library(tidyverse)
library(zoo)

#### Load, clean, and save codebook
codebook <- read_parquet("data/01-raw_data/codebook.parquet")
codebook <- codebook %>% select(1, 2) %>% drop_na()
write_parquet(codebook, "data/02-analysis_data/codebook.parquet")

#### Loading acquired data
shots2021_22 <- read_parquet("data/01-raw_data/shots2021_22.parquet")
shots2022_23 <- read_parquet("data/01-raw_data/shots2022_23.parquet")
shots2023_24 <- read_parquet("data/01-raw_data/shots2023_24.parquet")
shots2024_25 <- read_parquet("data/01-raw_data/shots2024_25.parquet")
shots2021to2025 <- bind_rows(
  shots2021_22 %>% mutate(season = "2021-22"),
  shots2022_23 %>% mutate(season = "2022-23"),
  shots2023_24 %>% mutate(season = "2023-24"),
  shots2024_25 %>% mutate(season = "2024-25"))

#### Wrap data cleaning in a function to easily clean selected datasets
clean <- function(shot_dataset) {
  shot_dataset %>%
    # Remove shots without a goaltender in net
    filter(shotOnEmptyNet==0) %>%
    mutate(
      # Calculate GSAx (Goals Saved Above Expected)
      GSAx = xGoal - goal,
      
      ### Shot type and location data cleaning
      # Change name of shot types
      shotType = case_when(
        shotType == "DEFL" ~ "Deflection",
        shotType == "TIP" ~ "Tip",
        shotType == "BACK" ~ "Backhand",
        shotType == "WRAP" ~ "Wraparound",
        shotType == "WRIST" ~ "Wrist Shot",
        shotType == "SLAP" ~ "Slap Shot",
        shotType == "SNAP" ~ "Snap Shot",
        is.na(shotType) ~ "Other"),
      
      # Add shot danger types: given boundary lines from the faceoff dot to goal crease
      # Left boundary line (Line 1): y = 0.7 * (x-69) - 22, faceoff dot to goal crease
      lower_bound = 0.7 * (xCordAdjusted - 69) - 22,
      # Right boundary line (Line 2): y = -0.7 * (x-69) + 22, faceoff dot to goal crease
      upper_bound = -0.7 * (xCordAdjusted - 69) + 22,
      danger = case_when(
        shotDistance <= 29 & # High-Danger: Within 29 feet of goal
          yCordAdjusted >= lower_bound & # Within boundary lines
          yCordAdjusted <= upper_bound & 
          xCordAdjusted <= 89 ~ "High-Danger", # Not behind goal line
        shotDistance > 29 & # Mid-Range: 29-43 feet from goal
          shotDistance <= 43 & 
          yCordAdjusted >= lower_bound & 
          yCordAdjusted <= upper_bound & 
          xCordAdjusted <= 89 ~ "Mid-Range",
        shotDistance > 43 & # Long-Range: 43+ feet but still in offensive zone
          xCordAdjusted <= 89 & 
          xCordAdjusted > 25 & 
          yCordAdjusted >= lower_bound & 
          yCordAdjusted <= upper_bound ~ "Long-Range",
        TRUE ~ "Other"
      ),
      
      ### Game state data cleaning
      # Change variable names to reference goalie and shooter team rather than home and away, while preserving home/away status.
      # Get goalieTeam and shootingTeam
      shootingTeam = teamCode, # teamCode is the team of the shooter
      goalieTeam = ifelse(shootingTeam == homeTeamCode, awayTeamCode, homeTeamCode),
      # Update variables for shooter's team and goalie's team
      shootingTeamGoals = ifelse(shootingTeam == homeTeamCode, homeTeamGoals, awayTeamGoals),
      goalieTeamGoals = ifelse(goalieTeam == homeTeamCode, homeTeamGoals, awayTeamGoals),
      shootingTeamSkaters = ifelse(shootingTeam == homeTeamCode, homeSkatersOnIce, awaySkatersOnIce),
      goalieTeamSkaters = ifelse(goalieTeam == homeTeamCode, homeSkatersOnIce, awaySkatersOnIce),
      
      # Calculate score differential of goalie's Team, with extreme values capped.
      goalieTeamScoreDifferential = case_when(
        goalieTeamGoals - shootingTeamGoals >= 4 ~ "4+",
        goalieTeamGoals - shootingTeamGoals <= -4 ~ "-4+",
        TRUE ~ as.character(goalieTeamGoals - shootingTeamGoals)),
      goalieTeamScoreDifferential = factor(
        goalieTeamScoreDifferential,
        levels = c("4+", "3", "2", "1", "0", "-1", "-2", "-3", "-4+")
      ),
      
      # Calculate strength state, skaters on each team.
      strengthState = case_when(
        goalieTeamSkaters == 5 & shootingTeamSkaters == 5 ~ "5v5", # 5v5
        goalieTeamSkaters == 4 & shootingTeamSkaters == 4 ~ "4v4", # 4v4
        goalieTeamSkaters == 3 & shootingTeamSkaters == 3 ~ "3v3", # 3v3
        goalieTeamSkaters > shootingTeamSkaters ~ "Goalie Team Advantage", # Goalie's team has more skaters
        goalieTeamSkaters < shootingTeamSkaters ~ "Shooting Team Advantage" # Shooter's team has more skaters
      ),
      
      # Add `goalieAtHome` variable, remove home/away columns
      goalieAtHome = ifelse(goalieTeam == homeTeamCode, "Yes", "No")) %>%
    select(-homeTeamCode, -homeTeamGoals, -awayTeamCode, -awayTeamGoals, -homeSkatersOnIce, -awaySkatersOnIce) %>%
    ### In-game performance data cleaning
    mutate(
      # Set the period based on time
      period = case_when(
        time <= 1200 ~ "1st Period",                 
        time > 1200 & time <= 2400 ~ "2nd Period",  
        time > 2400 & time <= 3600 ~ "3rd Period",  
        time > 3600 & time <= 4800 ~ "Overtime",
        time > 4800 & time <= 6000 ~ "2nd Overtime",
        time > 6000 & time <= 7200 ~ "3rd Overtime",
        time > 7200 & time <= 8400 ~ "4th Overtime",
        time > 8400 & time <= 9600 ~ "5th Overtime"
      )) %>%
    # Get rolling average stats
    # Calculate shots faced in the last 3 minutes for each goaltender
    arrange(game_id, period, time) %>% # Sort by game and time
    group_by(game_id, goalieNameForShot, period) %>% # Group by game, goalie, and period
    mutate(
      shotslast3min = sapply(
        seq_along(time),
        function(i) sum(
          time[i] - time[1:i] <= 180 & # shot within last 3 min
            time[i] - time[1:i] > 0 & # exclude current shot
            shotWasOnGoal[1:i] == 1)) # shot was on net
    ) %>%
    # Get cumulative shots faced per minute so far in game
    ungroup() %>% group_by(game_id, goalieNameForShot) %>%
    mutate(
      shots_faced = cumsum(shotWasOnGoal), # Cumulative shots faced in the game
      shots_faced_per_min = shots_faced / (time/60), # Shots faced per min
      GSAx_so_far = cumsum(GSAx) # Cumulative GSAx in the game
    ) %>%
    ungroup() %>%
    
    ### In-game performance data cleaning
    # Get total game performances, and performances from recent games
    # Include stats like goals allowed, GSAx per game and in the last 5 games
    arrange(goalieNameForShot, game_id) %>% # Sort and group by goalie and game
    group_by(goalieNameForShot, game_id) %>%
    mutate(
      ga = sum(goal), # Sum goals allowed for each game
      GSAx_game = sum(GSAx), # Sum GSAx for each game
      .groups = "drop") %>%
    group_by(goalieNameForShot) %>%
    mutate(
      # Goals allowed metrics
      last_game_ga = lag(ga, 1, default = NA), # Assign last game's goals
      last_5_gaa = lag( # Calculate rolling average for last 5 games before current game
        zoo::rollapplyr(ga, 5, mean, fill = NA, partial = TRUE),
        1, # Offset by 1 to exclude the current game
        default = NA # For first game
      ),
      # GSAx metrics
      last_game_GSAx = lag(GSAx_game, 1, default = NA),
      last_5_avg_GSAx = lag(zoo::rollapplyr(GSAx_game, 5, mean, fill = NA, partial = TRUE), 1, default = NA
      )) %>% ungroup() %>% arrange(game_id, time)
}

#### Clean and write data to analysis_data
write_parquet(clean(shots2021_22), "data/02-analysis_data/shotdata2021_22.parquet")
write_parquet(clean(shots2022_23), "data/02-analysis_data/shotdata2022_23.parquet")
write_parquet(clean(shots2023_24), "data/02-analysis_data/shotdata2023_24.parquet")
write_parquet(clean(shots2024_25), "data/02-analysis_data/shotdata2024_25.parquet")
write_parquet(clean(shots2021to2025), "data/02-analysis_data/shotdata.parquet")

