#### Preamble ####
# Purpose: Creates a model to predict the performance of a goaltender, measured with GSAx.
# Author: Daniel Du
# Date: 29 November 2024
# Contact: danielc.du@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data, performed exploratory data analysis
# Any other information needed? Make sure you are in the `HockeyShotAnalysis` rproj

#### Workspace setup ####
library(tidyverse)
library(arrow)
library(caret)

#### Read data ####
shots <- read_parquet("data/02-analysis_data/shotdata.parquet")
shots23_24 <- read_parquet("data/02-analysis_data/shotdata2023_24.parquet")
shots24_25 <- read_parquet("data/02-analysis_data/shotdata2024_25.parquet")

#### Modelling data ####

# Make adjustments to prepare the dataset for the model
prepare_for_model <- function(shot_dataset) {
  shot_dataset <- shot_dataset %>% mutate(
    last_game_GSAx = ifelse(is.na(last_game_GSAx), median(last_game_GSAx, na.rm = TRUE), last_game_GSAx),
    last_5_avg_GSAx = ifelse(is.na(last_5_avg_GSAx), median(last_5_avg_GSAx, na.rm = TRUE), last_5_avg_GSAx),
    time_segment = floor(time / 600))
  
    segment_data <- shot_dataset %>%
    group_by(game_id, goalieNameForShot, time_segment) %>%
    summarise(
      # Average the past-performance metrics over the interval
      isHomeTeam = mean(isHomeTeam),
      isPlayoffGame = mean(isPlayoffGame),
      goalieTeamScoreDifferential = mean(goalieTeamScoreDifferential),
      shotslast3min = mean(shotslast3min),
      shots_faced = mean(shots_faced),
      period = last(period),
      GSAx_so_far = mean(GSAx_so_far),
      last_game_GSAx = mean(last_game_GSAx),
      last_5_avg_GSAx = mean(last_5_avg_GSAx),
      # Response variable: Mean GSAx for the interval
      GSAx_segment = mean(GSAx),  # Mean GSAx over the interval
      .groups = "drop"
    )
  return(segment_data)
}
prepared_data23_24 <- prepare_for_model(shots23_24)
prepared_data <- prepare_for_model(shots)
# Train-Test Split 
set.seed(424)
train_index <- createDataPartition(prepared_data$GSAx_segment, p = 0.8, list = FALSE)
train_data <- prepared_data[train_index, ]
test_data <- prepared_data[-train_index, ]

# Model response variable: GSAx_segment: 10 minute segment
model <- lm(GSAx_segment ~ isHomeTeam + isPlayoffGame + isHomeTeam * isPlayoffGame + goalieTeamScoreDifferential +
              shotslast3min + shots_faced + period + GSAx_so_far +
              last_game_GSAx + last_5_avg_GSAx + goalieTeamScoreDifferential * shotslast3min +
              goalieTeamScoreDifferential * shots_faced + GSAx_so_far * shotslast3min,
            data = train_data)
summary(model)

# Checking residuals for assumptions
plot(model, which = 1)  # Residuals vs. Fitted

residuals_data <- train_data %>% mutate(residuals = residuals(model, type = "deviance"))
# plotting
ggplot(residuals_data, aes(x = as.factor(period), y = residuals)) +
  geom_boxplot() +
  labs(x = "Categorical Variable", y = "Deviance Residuals") +
  theme_minimal()
ggplot(residuals_data, aes(x = last_5_avg_GSAx, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Continuous Variable", y = "Deviance Residuals") +
  theme_minimal()

# vif checking
library(car)
vif(model, type = 'predictor')

# Validate with test data:
test_predictions <- predict(model, newdata = test_data)
test_residuals <- test_data$GSAx_segment - test_predictions
# Mean Squared Error (MSE)
mse <- mean(test_residuals^2)
print(paste("Mean Squared Error (MSE):", mse))
# R-squared for the test set
ss_total <- sum((test_data$GSAx_segment - mean(test_data$GSAx_segment))^2)
ss_residual <- sum(test_residuals^2)
r_squared <- 1 - (ss_residual / ss_total)
print(paste("R-squared on test set:", r_squared))

#### Save model ####
saveRDS(model, file = "models/model.rds")
