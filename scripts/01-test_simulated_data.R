#### Preamble ####
# Purpose: Tests the structure and validity of the simulated hockey shot dataset.
# Fits a predictive model to the simulated data
# Author: Daniel Du
# Date: 24 November 2024
# Contact: danielc.du@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` and `caret` packages must be installed and loaded
  # - 00-simulate_data.R must have been run
# Any other information needed? Make sure you are in the `HockeyShotAnalysis` rproj

#### Workspace setup ####
library(tidyverse)
library(caret)

analysis_data <- read_csv("data/00-simulated_data/simulated_data.csv")

# Test if the data was successfully loaded
if (exists("analysis_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}

#### Test data properties ####

# Check if the dataset has 500 rows
if (nrow(analysis_data) == 500) {
  message("Test Passed: The dataset has 500 rows.")
} else {
  stop("Test Failed: The dataset does not have 500 rows.")
}

# Check if the dataset has 4 columns (xGoal, shots_last_3min, home, goalScored)
expected_columns <- c("xGoal", "shots_last_3min", "home", "goalScored")
if (all(colnames(analysis_data) %in% expected_columns)) {
  message("Test Passed: The dataset has the expected column names.")
} else {
  stop("Test Failed: The dataset does not have the expected column names.")
}

# Check if 'xGoal' values are >= 0.001
if (all(analysis_data$xGoal >= 0.001)) {
  message("Test Passed: All values in 'xGoal' are >= 0.001.")
} else {
  stop("Test Failed: Some values in 'xGoal' are less than 0.001.")
}

# Check if 'shots_last_3min' values are non-negative integers
if (all(analysis_data$shots_last_3min >= 0 & analysis_data$shots_last_3min == floor(analysis_data$shots_last_3min))) {
  message("Test Passed: All values in 'shots_last_3min' are non-negative integers.")
} else {
  stop("Test Failed: Some values in 'shots_last_3min' are not non-negative integers.")
}

# Check if 'home' column contains only 0 or 1
if (all(analysis_data$home %in% c(0, 1))) {
  message("Test Passed: The 'home' column contains only 0s and 1s.")
} else {
  stop("Test Failed: The 'home' column contains values other than 0 or 1.")
}

# Check if 'goalScored' column contains only 0 or 1
if (all(analysis_data$goalScored %in% c(0, 1))) {
  message("Test Passed: The 'goalScored' column contains only 0s and 1s.")
} else {
  stop("Test Failed: The 'goalScored' column contains values other than 0 or 1.")
}

# Check if there are any missing values in the dataset
if (all(!is.na(analysis_data))) {
  message("Test Passed: The dataset contains no missing values.")
} else {
  stop("Test Failed: The dataset contains missing values.")
}

# Check if 'xGoal' has a mean close to 0.3
xGoal_mean <- mean(analysis_data$xGoal)
if (abs(xGoal_mean - 0.3) < 0.05) {
  message("Test Passed: The mean of 'xGoal' is approximately 0.3.")
} else {
  stop(paste("Test Failed: The mean of 'xGoal' is not close to 0.3. Actual mean:", xGoal_mean))
}

#### Example of fitting logistic regression model on simulated data

# Train-Test Split 
set.seed(424)
train_index <- createDataPartition(data$goalScored, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
train_data$goalScored <- as.factor(train_data$goalScored)
test_data$goalScored <- as.factor(test_data$goalScored)

# Train logistic reg predictive Model
glm_model <- glm(goalScored ~ xGoal + shots_last_3min + home, data = train_data, family = binomial)
summary(glm_model)

# Train random Forest
rf_model <- train(
  goalScored ~ xGoal + shots_last_3min + home,
  data = train_data,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5)
)

# Evaluate models

# Evaluate the logistic regression model
glm_predictions <- ifelse(predict(glm_model, test_data, type = "response") >= 0.5, 1, 0)
glm_conf_matrix <- table(Predicted = glm_predictions, Actual = test_data$goalScored)
print(glm_conf_matrix)

# Evaluate the Random Forest Model
rf_predictions <- predict(rf_model, test_data)
rf_conf_matrix <- confusionMatrix(as.factor(rf_predictions), as.factor(test_data$goalScored))
print(rf_conf_matrix)

# Add predictions to test_data
test_data_with_glm_predictions <- test_data %>%
  mutate(predicted_goalScored = glm_predictions)

test_data_with_rf_predictions <- test_data %>%
  mutate(predicted_goalScored = rf_predictions)

# View the combined table
test_data_with_glm_predictions
test_data_with_rf_predictions

# Number of errors model made:
print(nrow(test_data_with_glm_predictions %>% filter(goalScored != predicted_goalScored)))
print(nrow(test_data_with_rf_predictions %>% filter(goalScored != predicted_goalScored)))
