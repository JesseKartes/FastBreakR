% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nba_player_stats.R
\name{nba_player_stats}
\alias{nba_player_stats}
\title{Get NBA Player Stats}
\usage{
nba_player_stats(
  seasons,
  season_type = "Regular Season",
  measure_types = NULL,
  return_nested = FALSE
)
}
\arguments{
\item{seasons}{A numeric vector of seasons (e.g., 2024) for which to fetch
NBA player stats.}

\item{season_type}{A character string specifying the type of season
(default = "Regular Season"). Valid options include:
\itemize{
\item \strong{"Pre Season"} - Pre Season games.
\item \strong{"Regular Season"} - Regular Season games.
\item \strong{"Playoffs"} - Playoff games.
\item \strong{"All Star"} - All Star games.
\item \strong{"IST"} - NBA Cup games.
\item \strong{"PlayIn"} - Play In games.
}}

\item{measure_types}{A character vector specifying the types of stats
(default = NULL (all measure types)). Valid options include:
\itemize{
\item \strong{"Base"} - Traditional stats.
\item \strong{"Advanced"} - Advanced stats.
\item \strong{"Usage"} - Usage stats.
\item \strong{"Misc"} - Misc stats.
\item \strong{"Scoring"} - Scoring stats.
}}

\item{return_nested}{A logical value. If FALSE (default), returns a single
combined data frame for all seasons.If TRUE, returns a list of data frames,
one for each season.}
}
\value{
A  data frame containing player level stats for specified seasons and
measure types.
}
\description{
This function gets NBA player stats for the specified seasons returning a
data frame. Function pauses for five seconds after each season to prevent
timeout issues.
}
