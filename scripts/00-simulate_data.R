#### Preamble ####
# Purpose: Simulates a dataset of hockey shots and their associated attributes, 
# creating a model to predict whether or not a goal was scored.
# Author: Daniel Du
# Date: 24 November 2024
# Contact: danielc.du@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` and `caret` packages must be installed
# Any other information needed? Make sure you are in the `HockeyShotAnalysis` rproj

#### Workspace setup ####
# install.packages("caret")
library(tidyverse)
library(caret) # For createDataPartition
set.seed(424)

#### Simulate data ####
# Number of samples
n_samples <- 500

# Simulate predictors
data <- tibble(
  xGoal = pmax(rnorm(n_samples, mean = 0.3, sd = 0.2), 0.001), # Expected goal probability (positive relationship)
  shots_last_3min = rpois(n_samples, lambda = 2),        # Shots faced in last 3 minutes (negative relationship)
  home = rbinom(n_samples, size = 1, prob = 0.5)         # 0 = Away, 1 = Home
)

# Simulate target variable (goalScored) using logistic function
log_odds <- (
  17 * data$xGoal +                # Strong positive relationship with xGoal
    -2.7 * data$shots_last_3min +   # Negative relationship with recent activity
    -1.3 * data$home +              # Less likely to concede at home
    rnorm(n_samples, mean = 0, sd = 0.1)  # Random noise
)
data <- data %>%
  mutate(goalScored = as.integer(runif(n_samples) < plogis(log_odds)))

data

#### Save Dataset ####
write_csv(data, "data/00-simulated_data/sim_data.csv")

#### Train-Test Split ####
train_index <- createDataPartition(data$goalScored, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

#### Train Predictive Model ####
glm_model <- glm(goalScored ~ xGoal + shots_last_3min + home, data = train_data, family = binomial)
summary(glm_model)

# Random Forest
model <- train(
  goalScored ~ xGoal + shots_last_3min + home,
  data = train_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5)
)

#### Evaluate the Model ####
# Predict on test data
predictions <- round(predict(model, test_data), 0)

conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(test_data$goalScored))
print(conf_matrix)

# Add predictions to test_data
test_data_with_predictions <- test_data %>%
  mutate(predicted_goalScored = predictions)

# View the combined table
test_data_with_predictions
