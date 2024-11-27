#### Preamble ####
# Purpose: Downloads and saves the data from [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
# install.packages("devtools")
# devtools::install_github("JaseZiv/worldfootballR")
# devtools::install_github("statsbomb/StatsBombR")
# library(worldfootballR)
# library(StatsBombR)
library(tidyverse)

#### Download data ####
codebook <- read_csv("data/01-raw_data/MoneyPuck_Shot_Data_Dictionary.csv")
shots2023_24 <- read_csv("data/01-raw_data/shots_2023.csv")
shots2024_25 <- read_csv("data/01-raw_data/shots_2024.csv")
#### Save data ####
# [...UPDATE THIS...]
# change the_raw_data to whatever name you assigned when you downloaded it.
# write_csv(the_raw_data, "inputs/data/raw_data.csv") 

todo: SAVE AS PARQUET FILE
         
