
# COMP1013 Analytics Programming – Assignment
# Play Pulse Studios – Online Game Data Analysis

# Required libraries
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(lubridate)

# Load data
players  <- read.csv("players.csv",  stringsAsFactors = FALSE)
sessions <- read.csv("sessions.csv", stringsAsFactors = FALSE)
games    <- read.csv("games.csv",    stringsAsFactors = FALSE)

# Quick sanity checks
cat("players rows:", nrow(players),  "| NAs:", sum(is.na(players)),  "\n")
cat("sessions rows:", nrow(sessions), "| NAs:", sum(is.na(sessions)), "\n")
cat("games rows:",    nrow(games),    "| NAs:", sum(is.na(games)),    "\n")

# PART 1 – Step 1: Parse dates and assign experience levels
players$signup_date <- as.Date(players$signup_date)
players$signup_year <- year(players$signup_date)

players$experience_level <- case_when(
  players$signup_year <  2023 ~ "Veteran",
  players$signup_year == 2023 ~ "Intermediate",
  players$signup_year >  2023 ~ "New",
  TRUE                        ~ NA_character_
)

# Join sessions to players, drop any unclassifiable players
sessions_exp <- sessions %>%
  inner_join(players %>% select(player_id, experience_level),
             by = "player_id") %>%
  filter(!is.na(experience_level))