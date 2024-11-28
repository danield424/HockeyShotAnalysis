#### Preamble ####
# Purpose: Downloads and saves the data from MoneyPuck
# Author: Daniel Du
# Date: 24 November 2024
# Contact: danielc.du@mail.utoronto.ca
# License: MIT
# Pre-requisites: Simulated data, installed `tidyverse` and `arrow`
# Any other information needed? Make sure you are in the `HockeyShotAnalysis` rproj

#### Workspace setup ####
# install.packages("arrow")
library(tidyverse)
library(arrow)

#### Download data ####
# All csv files can be downloaded from https://moneypuck.com/data.htm
codebook <- read_csv("data/01-raw_data/MoneyPuck_Shot_Data_Dictionary.csv")
shots2021_22 <- read_csv("data/01-raw_data/shots_2021.csv")
shots2022_23 <- read_csv("data/01-raw_data/shots_2022.csv")
shots2023_24 <- read_csv("data/01-raw_data/shots_2023.csv")
shots2024_25 <- read_csv("data/01-raw_data/shots_2024.csv")

#### Save data as parquet files
write_parquet(codebook, "data/01-raw_data/codebook.parquet")
write_parquet(shots2021_22, "data/01-raw_data/shots2021_22.parquet")
write_parquet(shots2022_23, "data/01-raw_data/shots2022_23.parquet")
write_parquet(shots2023_24, "data/01-raw_data/shots2023_24.parquet")
write_parquet(shots2024_25, "data/01-raw_data/shots2024_25.parquet")
