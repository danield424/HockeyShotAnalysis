#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

## Part 0: Downloading and Viewing Data
# install.packages("devtools")
# devtools::install_github("JaseZiv/worldfootballR")
# devtools::install_github("statsbomb/StatsBombR")
# library(worldfootballR)
# library(StatsBombR)
library(tidyverse)


# https://moneypuck.com/data.htm data from here.

codebook <- read_csv("../data/01-raw_data/MoneyPuck_Shot_Data_Dictionary.csv")
shots2022_23 <- read_csv("../data/01-raw_data/shots_2022.csv")
shots2023_24 <- read_csv("../data/01-raw_data/shots_2023.csv")
shots2024_25 <- read_csv("../data/01-raw_data/shots_2024.csv")


##! all data}
shots2024_25 <- shots2024_25 %>% filter(isPlayoffGame==0, shotOnEmptyNet==0) %>% mutate(GSAx = xGoal - goal)
filtered_shots2024_25 <- shots2024_25 %>% filter(isPlayoffGame==0, shotOnEmptyNet==0) %>% select(game_id, time, teamCode, homeTeamCode, awayTeamCode, goalieNameForShot, shooterName, shotType, event, goal, xGoal, GSAx, timeSinceLastEvent, lastEventCategory, lastEventTeam, timeUntilNextEvent, shotWasOnGoal)

shots2023_24 <- shots2023_24 %>% filter(shotOnEmptyNet==0) %>% mutate(GSAx = xGoal - goal)
# isPlayoffGame==0,
filtered_shots <- shots2023_24 %>% select(game_id, isPlayoffGame, time, teamCode, homeTeamCode, awayTeamCode, goalieNameForShot, shooterName, shotType, event, goal, xGoal, GSAx, timeSinceLastEvent, lastEventCategory, lastEventTeam, timeUntilNextEvent, shotWasOnGoal)
shots_on_net <- filtered_shots %>% filter(shotWasOnGoal==1)
# filtered_shots %>% filter(game_id==20001)

## Part 1: Exploratory Data Analysis
##! GSAx over all shots}

# gsax over all shots, and average gsax - should be close to 0
sum(filtered_shots2024_25$GSAx)
sum(filtered_shots2024_25$GSAx)/nrow(filtered_shots2024_25)

sum(filtered_shots$GSAx)
sum(filtered_shots$GSAx)/nrow(filtered_shots)

# gsax over all shots on net, and average gsax
sum(shots_on_net$GSAx)
sum(shots_on_net$GSAx)/nrow(shots_on_net)

write a bit about how gsaa should include off-target shots as well. if we include on target shots we avoid missed shots due to the effect of the goaltender putting off the shooter, which could be responsible for this -0.024 to 0.001 difference.


##! GSAx for individual goaltenders}

# Finding average shots per game and shots on net per game.
shots_per_game <- round(nrow(filtered_shots)/length(unique(filtered_shots$game_id)), 1)
shots_on_net_per_game <- round(nrow(shots_on_net)/length(unique(shots_on_net$game_id)), 1)

# Summarizing goaltender stats
calculate_goaltender_stats <- function(shots) {
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
calculate_goaltender_stats(filtered_shots)


{r, label = Vezina Votes}
vezina_ranking <- data.frame(
  Place = c(1, 2, 3, 4, 5, 6, 7, 8, 8),
  Player = c("Connor Hellebuyck", "Thatcher Demko", "Sergei Bobrovsky", 
             "Igor Shesterkin", "Juuse Saros", "Linus Ullmark", 
             "Jeremy Swayman", "Charlie Lindgren", "Ilya Sorokin"),
  Voting_Points = c(158, 70, 40, 7, 5, 3, 3, 1, 1))

vezina_ranking

https://www.hockey-reference.com/awards/voting-2024.html vezina votes

## Part 2: Exploring Shot Types/Shot Locations
{r, label = Shot Type Stats}
shot_type_stats <- filtered_shots %>%
  # Rename shot types.
  mutate(shotType = case_when(
    shotType == "DEFL" ~ "Deflection",
    shotType == "TIP" ~ "Tip",
    shotType == "BACK" ~ "Backhand",
    shotType == "WRAP" ~ "Wraparound",
    shotType == "WRIST" ~ "Wrist Shot",
    shotType == "SLAP" ~ "Slap Shot",
    shotType == "SNAP" ~ "Snap Shot",
    is.na(shotType) ~ "Other")) %>%
  group_by(shotType) %>%
  # Count number of each shot types and GSAx.
  summarize(
    count = n(),
    avg_GSAx = sum(GSAx) / n(),
    GSAx_per_100 = sum(GSAx) / n() * 100
  ) 
# Add row representing all shots.
total_row <- filtered_shots %>%
  summarize(shotType = "All",
            count = n(),
            avg_GSAx = sum(GSAx) / n(),
            GSAx_per_100 = sum(GSAx) / n() * 100
  )
shot_type_stats <- bind_rows(shot_type_stats, total_row)
shot_type_stats %>%
  arrange(desc(avg_GSAx)) 


{r, label = Shot Location Zones Calculation}
shots_locations <- shots2023_24 %>% select(game_id, time, teamCode, homeTeamCode, awayTeamCode, goalieNameForShot, shooterName, shotType, shotDistance, xCordAdjusted, yCordAdjusted, event, goal, xGoal, GSAx, shotWasOnGoal)

shots_locations <- shots_locations %>%
  mutate(
    # Left boundary line (Line 1): y = 0.7 * (x-69) - 22, faceoff dot to goal crease
    lower_bound = 0.7 * (xCordAdjusted - 69) - 22,
    # Right boundary line (Line 2): y = -0.7 * (x-69) + 22, faceoff dot to goal crease
    upper_bound = -0.7 * (xCordAdjusted - 69) + 22
  ) %>%
  mutate(
    danger_zone = case_when(
      shotDistance <= 29 & # Within 29 feet of goal
        yCordAdjusted >= lower_bound & # Bounded by faceoff dot to goal crease lines
        yCordAdjusted <= upper_bound & 
        xCordAdjusted <= 89 ~ "High-Danger", #Not behind goal line
      
      shotDistance > 29 & # 29-43 feet from goal, bounded by 2 lines
        shotDistance <= 43 & 
        yCordAdjusted >= lower_bound & 
        yCordAdjusted <= upper_bound & 
        xCordAdjusted <= 89 ~ "Mid-Range",
      
      shotDistance > 43 & # 43+ feet but still in offensive zone, bounded by 2 lines
        xCordAdjusted <= 89 & 
        xCordAdjusted > 25 & 
        yCordAdjusted >= lower_bound & 
        yCordAdjusted <= upper_bound ~ "Long-Range",
      TRUE ~ "Other"
    )
  )
shots_locations %>% select(time, goalieNameForShot, shooterName, shotType, shotDistance, xCordAdjusted, yCordAdjusted, danger_zone, xGoal)


nhl official definition for shot dangers: https://edge.nhl.com/en/glossary#m 
https://dobberhockey.com/2024/07/29/the-wild-west-high-danger-shooters/ 
  nhl rulebook for faceoff dots: https://media.nhl.com/site/asset/public/ext/2021-22/2021-22Rules.pdf 


{r, label = Shot Location Stats}
shot_location_stats <- shots_locations %>%
  # Count each by zone, and get GSAx per each zone type and all zones.
  group_by(danger_zone) %>%
  summarize(
    count = n(),
    avg_goal = sum(goal) / n(),
    avg_xGoal = sum(xGoal) / n(),
    avg_GSAx = sum(GSAx) / n(),
    GSAx_per_100 = sum(GSAx) / n() * 100
  ) 
total_row2 <- shots_locations %>%
  summarize(danger_zone = "All",
            count = n(),
            avg_goal = sum(goal) / n(),
            avg_xGoal = sum(xGoal) / n(),
            avg_GSAx = sum(GSAx) / n(),
            GSAx_per_100 = sum(GSAx) / n() * 100
  )
shot_location_stats <- bind_rows(shot_location_stats, total_row2)
shot_location_stats %>%
  arrange(desc(avg_GSAx)) 

# exploring high danger * shotslast3min interaction

# shots_locations <- shots_locations %>%
#   mutate(
#     period = case_when( # Set the period.
#       time <= 1200 ~ "1st Period",
#       time > 1200 & time <= 2400 ~ "2nd Period",
#       time > 2400 & time <= 3600 ~ "3rd Period",
#       time > 3600 & time <= 4800 ~ "Overtime",
#       time > 4800 & time <= 6000 ~ "2nd Overtime",
#       time > 6000 & time <= 7200 ~ "3rd Overtime",
#       time > 7200 & time <= 8400 ~ "4th Overtime",
#       time > 8400 & time <= 9600 ~ "5th Overtime"
#     )
#   ) %>%
#     arrange(game_id, period, time) %>% # Sort by game and time
#     group_by(game_id, goalieNameForShot, period) %>% # Group by game, goalie, and period
#     mutate(
#     shotslast3min = sapply(
#       seq_along(time),
#       function(i) sum(
#         time[i] - time[1:i] <= 180 & # shot within last 3 min
#         time[i] - time[1:i] > 0 & # exclude current shot
#           shotWasOnGoal[1:i] == 1) # shot was on net
#     )) %>% ungroup() %>%
#   mutate(
#     high_danger_cold = case_when(
#       danger_zone == "High-Danger" & shotslast3min <= 0 ~ "HighDanger-Cold",
#       danger_zone == "High-Danger" & shotslast3min > 0 ~ "HighDanger-Warm",
#       danger_zone != "High-Danger" ~ "NotHighDanger"
#     )
#   )
# shots_locationstest <- shots_locations %>%
#   group_by(high_danger_cold) %>%
#   summarize(
#     count = n(),
#     avg_goal = sum(goal) / n(),
#     avg_xGoal = sum(xGoal) / n(),
#     avg_GSAx = sum(GSAx) / n(),
#     GSAx_per_100 = sum(GSAx) / n() * 100
#   )
# shots_locationstest
# t.test(
#   GSAx ~ high_danger_cold, 
#   data = shots_locations %>% filter(high_danger_cold %in% c("HighDanger-Cold", "HighDanger-Warm"))
# )


## Part 3: Building 'Game State'

{r, label = Game State Data Cleaning}
game_state_stats <- shots2023_24 %>% select(game_id, isPlayoffGame, time, teamCode, homeTeamCode, homeTeamGoals, awayTeamCode, awayTeamGoals, goalieNameForShot, shooterName, shotType, event, goal, xGoal, GSAx, shotWasOnGoal, homeSkatersOnIce, awaySkatersOnIce)

# Setting goalie and shooter team stats
game_state_stats <- game_state_stats %>%
  # Get goalieTeam and shootingTeam from home and away team data.
  rename(shootingTeam = teamCode) %>%
  mutate(
    goalieTeam = ifelse(shootingTeam == homeTeamCode, awayTeamCode, homeTeamCode)
  ) %>%
  # Create stats for shooter's team and goalie's team
  mutate(
    shootingTeamGoals = ifelse(shootingTeam == homeTeamCode, homeTeamGoals, awayTeamGoals),
    goalieTeamGoals = ifelse(goalieTeam == homeTeamCode, homeTeamGoals, awayTeamGoals),
    shootingTeamSkaters = ifelse(shootingTeam == homeTeamCode, homeSkatersOnIce, awaySkatersOnIce),
    goalieTeamSkaters = ifelse(goalieTeam == homeTeamCode, homeSkatersOnIce, awaySkatersOnIce)
  ) %>%
  # Calculate score differential of goalie's Team, with extreme values capped.
  mutate(
    goalieTeamScoreDifferential = case_when(
      goalieTeamGoals - shootingTeamGoals >= 4 ~ "4+",
      goalieTeamGoals - shootingTeamGoals <= -4 ~ "-4+",
      TRUE ~ as.character(goalieTeamGoals - shootingTeamGoals)),
    
    goalieTeamScoreDifferential = factor(
      goalieTeamScoreDifferential,
      levels = c("4+", "3", "2", "1", "0", "-1", "-2", "-3", "-4+")),
    
    # Calculate strength state, skaters on each team.
    strengthState = case_when(
      goalieTeamSkaters == 5 & shootingTeamSkaters == 5 ~ "5v5", # 5v5
      goalieTeamSkaters == 4 & shootingTeamSkaters == 4 ~ "4v4", # 4v4
      goalieTeamSkaters == 3 & shootingTeamSkaters == 3 ~ "3v3", # 3v3
      goalieTeamSkaters > shootingTeamSkaters ~ "Goalie Team Advantage", # Goalie's team has more skaters
      goalieTeamSkaters < shootingTeamSkaters ~ "Shooting Team Advantage" # Shooter's team has more skaters
    )
  ) %>%
  # Add `goalieAtHome` variable, remove home/away columns, rename playoff column
  mutate(goalieAtHome = ifelse(goalieTeam == homeTeamCode, "Yes", "No"),
         playoffs = ifelse(isPlayoffGame==1, "Yes", "No")) %>%
  select(-homeTeamCode, -homeTeamGoals, -awayTeamCode, -awayTeamGoals, -isPlayoffGame, -homeSkatersOnIce, -awaySkatersOnIce)


game_state_stats %>% select(shootingTeam, shootingTeamGoals, shooterName, goalieTeam, goalieTeamGoals,
                            goalieNameForShot, goalieAtHome, goalieTeamScoreDifferential, event, goal, xGoal, GSAx)


{r, label = Game State: Home/Away}
home_away_stats <- game_state_stats %>%
  group_by(goalieAtHome) %>%
  summarize(
    count = n(),
    avg_goal = sum(goal) / n(),
    avg_xGoal = sum(xGoal) / n(),
    avg_GSAx = sum(GSAx) / n(),
    GSAx_per_100 = sum(GSAx) / n() * 100
  ) 
home_away_stats %>%
  arrange(desc(avg_GSAx)) 


{r, label = Game State: Score Differential}
score_differential_stats <- game_state_stats %>%
  group_by(goalieTeamScoreDifferential) %>%
  summarize(
    count = n(),
    avg_goal = sum(goal) / n(),
    avg_xGoal = sum(xGoal) / n(),
    avg_GSAx = sum(GSAx) / n(),
    GSAx_per_100 = sum(GSAx) / n() * 100
  ) 
score_differential_stats %>%
  arrange(desc(goalieTeamScoreDifferential))


{r, label = Game State: Strength Situations}
strength_situation_stats <- game_state_stats %>%
  group_by(strengthState) %>%
  summarize(
    count = n(),
    avg_goal = sum(goal) / n(),
    avg_xGoal = sum(xGoal) / n(),
    avg_GSAx = sum(GSAx) / n(),
    GSAx_per_100 = sum(GSAx) / n() * 100
  ) 
strength_situation_stats %>%
  arrange(desc(avg_GSAx))


{r, label = Game State: Regular Season vs Playoffs}
regular_post_ssn_stats <- game_state_stats %>%
  group_by(playoffs) %>%
  summarize(
    count = n(),
    avg_goal = sum(goal) / n(),
    avg_xGoal = sum(xGoal) / n(),
    avg_GSAx = sum(GSAx) / n(),
    GSAx_per_100 = sum(GSAx) / n() * 100
  ) 
regular_post_ssn_stats %>%
  arrange(desc(avg_GSAx)) 


{r, label = Goaltender Performance RegSsn vs Playoffs}
calculate_goaltender_stats(filtered_shots %>% filter(isPlayoffGame==0))
calculate_goaltender_stats(filtered_shots %>% filter(isPlayoffGame==1))
#todo: scatterplot



## Part 4: In-game performance
{r, Label = Rolling 3-min average Data}
rolling_avg_stats <- shots2023_24 %>% select(game_id, isPlayoffGame, time, teamCode, goalieNameForShot, shooterName, shotType, event, goal, xGoal, GSAx, shotWasOnGoal) %>% mutate(
  period = case_when( # Set the period.
    time <= 1200 ~ "1st Period",                 
    time > 1200 & time <= 2400 ~ "2nd Period",  
    time > 2400 & time <= 3600 ~ "3rd Period",  
    time > 3600 & time <= 4800 ~ "Overtime",
    time > 4800 & time <= 6000 ~ "2nd Overtime",
    time > 6000 & time <= 7200 ~ "3rd Overtime",
    time > 7200 & time <= 8400 ~ "4th Overtime",
    time > 8400 & time <= 9600 ~ "5th Overtime"
  )
)

rolling_avg_stats <- rolling_avg_stats %>%
  # Calculate shots faced in the last 3 minutes for each goaltender
  arrange(game_id, period, time) %>% # Sort by game and time
  group_by(game_id, goalieNameForShot, period) %>% # Group by game, goalie, and period
  mutate(
    shotslast3min = sapply(
      seq_along(time),
      function(i) sum(
        time[i] - time[1:i] <= 180 & # shot within last 3 min
          time[i] - time[1:i] > 0 & # exclude current shot
          shotWasOnGoal[1:i] == 1) # shot was on net
    )) %>%
  # Get cumulative shots faced per minute so far in game
  ungroup() %>% group_by(game_id, goalieNameForShot) %>%
  mutate(
    shots_faced = cumsum(shotWasOnGoal), # Cumulative shots faced in the game
    shots_faced_per_min = shots_faced / (time/60), # Shots faced per min
    GSAx_so_far = cumsum(GSAx) # Cumulative GSAx in the game
  ) %>%
  ungroup()

rolling_avg_stats %>% select(game_id, time, period, goalieNameForShot, shotWasOnGoal, shots_faced, shots_faced_per_min, shotslast3min, GSAx_so_far) %>% 
  arrange(game_id, goalieNameForShot, time)
# arrange(desc(shots_faced_per_min))


{r, label = Last 3-min avg Stats}
last3min_stats <- rolling_avg_stats %>%
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
last3min_stats %>%
  arrange(desc(avg_GSAx)) 

trend is there but not very significant.
# todo: measure gsax build-up, not just shots.

{r, label = Shots-per-min stats}
shots_faced_stats <- rolling_avg_stats %>%
  mutate(
    shots_faced_group = case_when(
      shots_faced >= 0 & shots_faced <= 10 ~ "0-10",
      shots_faced >= 11 & shots_faced <= 20 ~ "11-20",
      shots_faced >= 21 & shots_faced <= 30 ~ "21-30",
      shots_faced >= 31 & shots_faced <= 40 ~ "31-40",
      shots_faced >= 41 ~ "41+"
    )
  ) %>%
  group_by(shots_faced_group) %>%
  summarize(
    count = n(),
    avg_goal = sum(goal) / n(),
    avg_xGoal = sum(xGoal) / n(),
    avg_GSAx = sum(GSAx) / n(),
    GSAx_per_100 = sum(GSAx) / n() * 100
  ) 
shots_faced_stats %>%
  arrange(desc(avg_GSAx)) 

ggplot(rolling_avg_stats, aes(x = GSAx_so_far, y = GSAx)) +
  geom_point(size = 3, color = "blue") + # Points for the scatterplot
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Add a trend line (optional)
  labs(
    title = "Relationship Between gsax so far  and GSAx",
    x = "gsax so far",
    y = "GSAx"
  ) +
  theme_minimal()

model <- lm(GSAx ~ GSAx_so_far, data = rolling_avg_stats)
summary(model)

no real differences.

## Part 5: Past games performance
{r, Label = Past games results data}
library(zoo)

past_performance <- shots2023_24 %>% select(game_id, time, teamCode, goalieNameForShot, shooterName, homeTeamCode, homeTeamGoals, awayTeamCode, awayTeamGoals, shotType, event, goal, xGoal, GSAx, shotWasOnGoal) %>%
  # Rename to shooting and goalie team
  rename(shootingTeam = teamCode) %>%
  mutate(
    goalieTeam = ifelse(shootingTeam == homeTeamCode, awayTeamCode, homeTeamCode)
  ) %>%
  # Create stats for shooter's team and goalie's team
  mutate(
    shootingTeamGoals = ifelse(shootingTeam == homeTeamCode, homeTeamGoals, awayTeamGoals),
    goalieTeamGoals = ifelse(goalieTeam == homeTeamCode, homeTeamGoals, awayTeamGoals)
  ) %>%
  arrange(goalieNameForShot, game_id) %>% # Sort and group by goalie and game
  group_by(goalieNameForShot, game_id) %>% 
  summarize(
    ga = sum(goal, na.rm = TRUE), # Sum goals allowed for each game
    GSAx_game = sum(GSAx, na.rm = TRUE), # Sum GSAx for each game
    .groups = "drop") %>% 
  group_by(goalieNameForShot) %>%
  mutate(
    # Goals allowed metrics
    last_game_ga = lag(ga, 1, default = NA), # Assign last game's goals
    last_5_gaa = lag( # Calculate rolling average for last 5 games before current game
      zoo::rollapplyr(ga, 5, mean, fill = NA, partial = TRUE),
      1, # Offset by 1 to exclude the current game
      default = NA
    ),
    # GSAx metrics
    last_game_GSAx = lag(GSAx_game, 1, default = NA), # Assign last game's GSAx
    last_5_avg_GSAx = lag( # Calculate average GSAx for the last 5 games before current game
      zoo::rollapplyr(GSAx_game, 5, mean, fill = NA, partial = TRUE),
      1, # Offset by 1 to exclude the current game
      default = NA
    )) %>%
  ungroup()

past_performance %>%
  arrange(goalieNameForShot, game_id)

{r, label = Performance Based On Recent Results}
# Fit a linear model
model <- lm(GSAx_game ~ last_game_ga, data = past_performance)
summary(model)

ggplot(past_performance, aes(x = last_game_ga, y = GSAx_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "GSAx_game vs. Last Game GSAx",
       x = "Last Game GSAx",
       y = "Current Game GSAx")


not much of a relationship between any.

## Part 6: Modelling


