#### Preamble ####
# Purpose: Explores data and variables of interest.
# Author: Daniel Du
# Date: 24 November 2024
# Contact: danielc.du@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data and saved as parquet files in data/02-analysis_data
# Any other information needed? Make sure you are in the `HockeyShotAnalysis` rproj


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(zoo)

#### Read data ####
# Use just 2023-24 and 2024-25 data for smaller sample size.
shots <- read_parquet("data/02-analysis_data/shotdata.parquet")
shots23_24 <- read_parquet("data/02-analysis_data/shotdata2023_24.parquet")
shots24_25 <- read_parquet("data/02-analysis_data/shotdata2024_25.parquet")

#### Exploring GSAx ####

# Looking at GSAx over all shots: average gsax - should be close to 0
mean(shots$GSAx) # 0.0009
mean(shots24_25$GSAx) # 0.0011

# Average GSAx over all shots on net
shots_on_net <- shots %>% filter(shotWasOnGoal==1)
shots24_25on_net <- shots24_25 %>% filter(shotWasOnGoal==1)
mean(shots_on_net$GSAx) # -0.0222
mean(shots24_25on_net$GSAx) # -0.0267

# write a bit about how gsaa should include off-target shots as well. 
# if we include on target shots we avoid missed shots due to the effect of the goaltender 
# putting off the shooter, which could be responsible for this -0.022 difference.

# Summarizing goaltender GSAx
get_goaltender_stats <- function(shots) {
  goaltender_stats <- shots %>%
    group_by(goalieNameForShot) %>%
    summarize(
      games_played = n_distinct(game_id),
      all_shots_against = n(), # Total shots against including missed
      shots_on_net_against = sum(shotWasOnGoal == 1), # Shots on net faced
      goals_against = sum(goal == 1), 
      total_GSAx = sum(GSAx), # Total GSAx for all shots
      GSAx_on_net = sum(ifelse(shotWasOnGoal == 1, GSAx, 0)), # GSAx for shots on net
      GSAx_per_game = total_GSAx/games_played, #GSAx per game
      GSAx_per_100 = total_GSAx/all_shots_against*100, #GSAx per 100 shots
      GSAx_on_net_per_100 = GSAx_on_net/shots_on_net_against*100 #GSAx per 100 shots on net
    ) %>%
    arrange(desc(total_GSAx))
  return(goaltender_stats)
}
get_goaltender_stats(shots)
# histogram here?

# Comparing to vezina voting 2023-24:
GSAx_ranking2023_24 <- get_goaltender_stats(shots23_24 %>% filter(isPlayoffGame==0))
GSAx_ranking2023_24 %>% select(goalieNameForShot, games_played, total_GSAx, GSAx_per_game)
vezina_ranking <- data.frame(
  Place = c(1, 2, 3, 4, 5, 6, 7, 8, 8),
  Player = c("Connor Hellebuyck", "Thatcher Demko", "Sergei Bobrovsky", 
             "Igor Shesterkin", "Juuse Saros", "Linus Ullmark", 
             "Jeremy Swayman", "Charlie Lindgren", "Ilya Sorokin"),
  Voting_Points = c(158, 70, 40, 7, 5, 3, 3, 1, 1))
vezina_ranking

# matches up pretty well


#### Exploring Shot Types/Shot Locations ####

# Shot Types
get_shot_type_stats <- function(shots) {
  shot_type_stats <- shots %>%
    group_by(shotType) %>%
    # Count number of each shot types and GSAx.
    summarize(
      count = n(),
      avg_GSAx = sum(GSAx) / n(),
      GSAx_per_100 = sum(GSAx) / n() * 100
    ) 
  # Add row representing all shots.
  total_row <- shots %>%
    summarize(
      shotType = "All", # Set shot type to 'All' since we are combining all shots
      count = n(),
      avg_GSAx = sum(GSAx) / n(),
      GSAx_per_100 = sum(GSAx) / n() * 100
    )
  shot_type_stats <- bind_rows(shot_type_stats, total_row)
  # Arrange by average GSAx
  return(shot_type_stats %>% arrange(desc(avg_GSAx)))
}
get_shot_type_stats(shots)

#nhl official definition for shot dangers: https://edge.nhl.com/en/glossary#m 
#https://dobberhockey.com/2024/07/29/the-wild-west-high-danger-shooters/ 
#  nhl rulebook for faceoff dots: https://media.nhl.com/site/asset/public/ext/2021-22/2021-22Rules.pdf 
# Shot locations
get_shot_location_stats <- function(shots) {
  shot_location_stats <- shots %>%
    # Count shots of each danger, and get GSAx per each danger type and all dangers.
    group_by(danger) %>%
    summarize(
      count = n(),
      avg_goal = sum(goal) / n(),
      avg_xGoal = sum(xGoal) / n(),
      avg_GSAx = sum(GSAx) / n(),
      GSAx_per_100 = sum(GSAx) / n() * 100
    ) 
  total_row <- shots %>%
    summarize(danger = "All", # We are combining all dangers
              count = n(),
              avg_goal = sum(goal) / n(),
              avg_xGoal = sum(xGoal) / n(),
              avg_GSAx = sum(GSAx) / n(),
              GSAx_per_100 = sum(GSAx) / n() * 100
    )
  shot_location_stats <- bind_rows(shot_location_stats, total_row)
  return(shot_location_stats %>% arrange(desc(avg_GSAx))) 
}
get_shot_location_stats(shots)

#### Exploring Game State/Context ####

# Home vs Away
get_home_away_stats <- function(shots) {
  home_away_stats <- shots %>%
    group_by(goalieAtHome) %>%
    summarize(
      count = n(),
      avg_goal = sum(goal) / n(),
      avg_xGoal = sum(xGoal) / n(),
      avg_GSAx = sum(GSAx) / n(),
      GSAx_per_100 = sum(GSAx) / n() * 100
    ) 
  return(home_away_stats %>% arrange(desc(avg_GSAx)))
}
get_home_away_stats(shots)

# Strength Situation
get_strength_stats <- function(shots) {
  strength_situation_stats <- shots %>%
    group_by(strengthState) %>%
    summarize(
      count = n(),
      avg_goal = sum(goal) / n(),
      avg_xGoal = sum(xGoal) / n(),
      avg_GSAx = sum(GSAx) / n(),
      GSAx_per_100 = sum(GSAx) / n() * 100
    ) 
  return(strength_situation_stats %>% arrange(desc(avg_GSAx)))
}
get_strength_stats(shots)

# Reg Season/Playoffs
get_reg_vs_playoff_stats <- function(shots) {
  regular_post_ssn_stats <- shots %>%
    mutate(playoffs = ifelse(isPlayoffGame==1, "Yes", "No")) %>%
    group_by(playoffs) %>%
    summarize(
      count = n(),
      avg_goal = sum(goal) / n(),
      avg_xGoal = sum(xGoal) / n(),
      avg_GSAx = sum(GSAx) / n(),
      GSAx_per_100 = sum(GSAx) / n() * 100
    ) 
  return(regular_post_ssn_stats %>% arrange(desc(avg_GSAx)))
}
get_reg_vs_playoff_stats(shots)
# scatter plot of goalie reg vs playoff here?


#### Current game performance ####

# Team performance: In-game score differential
get_score_diff_stats <- function(shots) {
  score_diff_stats <- shots %>% mutate(
    goalieTeamScoreDifferential = case_when(
      goalieTeamGoals - shootingTeamGoals >= 4 ~ "4+",
      goalieTeamGoals - shootingTeamGoals <= -4 ~ "-4+",
      TRUE ~ as.character(goalieTeamGoals - shootingTeamGoals)
      ),
    goalieTeamScoreDifferential = factor(
      goalieTeamScoreDifferential,
      levels = c("4+", "3", "2", "1", "0", "-1", "-2", "-3", "-4+")
    ))
    group_by(goalieTeamScoreDifferential) %>%
    summarize(
      count = n(),
      avg_goal = sum(goal) / n(),
      avg_xGoal = sum(xGoal) / n(),
      avg_GSAx = sum(GSAx) / n(),
      GSAx_per_100 = sum(GSAx) / n() * 100
    )
  return(score_diff_stats %>% arrange(desc(goalieTeamScoreDifferential)))
}
get_score_diff_stats(shots)

# Goalie 'warmness': shots faced in last 3 minutes.
get_last_3min_stats <- function(shots) {
  last3min <- shots %>%
    mutate(
      shotslast3min_group = case_when(
        shotslast3min == 0 ~ "0",
        shotslast3min == 1 ~ "1",
        shotslast3min == 2 ~ "2",
        shotslast3min == 3 ~ "3",
        shotslast3min == 4 ~ "4",
        shotslast3min >= 5 ~ "5+"
      )
    ) %>%
    group_by(shotslast3min_group) %>%
    summarize(
      count = n(),
      avg_goal = sum(goal) / n(),
      avg_xGoal = sum(xGoal) / n(),
      avg_GSAx = sum(GSAx) / n(),
      GSAx_per_100 = sum(GSAx) / n() * 100
    ) 
  return(last3min %>% arrange(desc(avg_GSAx)))
}
get_last_3min_stats(shots)

# Goalie game performance: GSAx throughout game
plot_variable_vs_GSAx <- function(shots, variable) {
  plot <- ggplot(shots, aes(x = .data[[variable]], y = GSAx)) +
    geom_point(size = 3, color = "blue") + # Points for the scatterplot
    geom_smooth(method = "lm", color = "red", se = FALSE) + # Add a trend line
    labs(
      title = paste("Relationship Between", variable, "and GSAx"),
      x = variable,
      y = "GSAx"
    ) + theme_minimal()
  return(plot)
}
plot_variable_vs_GSAx(shots, "GSAx_so_far")
summary(lm(GSAx ~ GSAx_so_far, shots))

#### Recent performance ####
# Past games, and past few games
plot_variable_vs_GSAx(shots, "last_game_ga")
plot_variable_vs_GSAx(shots, "last_5_avg_GSAx") 






