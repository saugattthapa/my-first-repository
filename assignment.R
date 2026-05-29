
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
