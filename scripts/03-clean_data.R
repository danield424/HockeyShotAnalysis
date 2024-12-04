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
  shots2021_22,
  shots2022_23,
  shots2023_24,
  shots2024_25)


#### Wrap data cleaning in a function to easily clean selected datasets
clean <- function(shot_dataset) {
  clean_data <- shot_dataset %>% drop_na(goal, goalieNameForShot, game_id) %>%
    mutate(game_id = paste0(season, game_id)) %>% # Append season to game_id 
    # Remove shots without a goaltender in net
    filter(shotOnEmptyNet==0) %>%
    mutate(
      # Calculate GSAx (Goals Saved Above Expected)
      GSAx = xGoal - goal,
      
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
      
      # Calculate score differential of goalie's Team.
      goalieTeamScoreDifferential = goalieTeamGoals - shootingTeamGoals) %>%
      
    ### In-game performance data cleaning
    mutate(
      # Set the period based on time
      period = case_when(
        time <= 1200 ~ "1st Period",                 
        time > 1200 & time <= 2400 ~ "2nd Period",  
        time > 2400 & time <= 3600 ~ "3rd Period",  
        time > 3600 & time <= 4800 ~ "Overtime",
        time > 4800 ~ "2nd OT or later"
      )) %>%
    # Get cumulative shots faced per minute so far in game
    group_by(game_id, goalieNameForShot) %>%
    mutate(
      shots_faced = cumsum(shotWasOnGoal) - shotWasOnGoal, # Cumulative shots faced prior
      shots_faced_per_min = shots_faced / (time/60), # Shots faced per min
      GSAx_so_far = cumsum(GSAx) - GSAx # Cumulative GSAx in the shots prior 
    ) %>%
    ungroup() %>%
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
    ) %>% ungroup()
    
    ### In-game performance data cleaning
    # Get total game performances, and performances from recent games
    # Include stats like goals allowed, GSAx per game and in the last 5 games
    clean_data2 <- clean_data %>% arrange(goalieNameForShot, game_id) %>% # Sort and group by goalie and game
    group_by(goalieNameForShot, game_id) %>%
    summarise(
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
      )) %>% ungroup() %>% left_join(clean_data, by = c("goalieNameForShot", "game_id")) %>% arrange(game_id, time)
return(clean_data2)
    }

#### Clean and write data to analysis_data
write_parquet(clean(shots2021_22), "data/02-analysis_data/shotdata2021_22.parquet")
write_parquet(clean(shots2022_23), "data/02-analysis_data/shotdata2022_23.parquet")
write_parquet(clean(shots2023_24), "data/02-analysis_data/shotdata2023_24.parquet")
write_parquet(clean(shots2024_25), "data/02-analysis_data/shotdata2024_25.parquet")
allshots <- clean(shots2021to2025)
write_parquet(allshots, "data/02-analysis_data/shotdata.parquet")
