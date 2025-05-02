## ----load packages------------------------------------------------------------
# Load the FastBreakR package
library(FastBreakR)
library(dplyr, warn.conflicts = FALSE)

## ----team stats, cache=TRUE---------------------------------------------------
# Fetch team stats for the 2022-2023 & 2023-2024 regular season
team_stats <- nba_team_stats(2023:2024, season_type = "Regular Season")

# Display a glimpse of the team stats data
glimpse(team_stats)
