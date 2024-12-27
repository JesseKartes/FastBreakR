#' Get NBA Player Dictionary
#'
#' This function gets NBA player dictionary data and returns a tibble..
#'
#' @return A tibble containing the player dictionary.
#' @export
nba_player_dictionary <- function() {
  player_dictionary <- fetch_player_dictionary_data()

  return(player_dictionary)
}

#' Fetch Player Dictionary from API
#'
#' @return A list containing the raw player dictionary data.
fetch_player_dictionary_data <- function() {
  headers <- generate_headers_stats()
  params <- generate_parameters_player_dict()

  url <- "https://stats.nba.com/stats/playerindex"

  data <- get_data(url, headers, params)

  column_names <- data$resultSets$headers[[1]] %>%
    as.character()

  dt <- data$resultSets$rowSet[[1]] %>%
    data.frame(stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    set_names(column_names) %>%
    clean_names()

  return(dt)
}

#' Get NBA Team Dictionary
#'
#' This function gets NBA team dictionary data and returns a tibble.
#'
#' @return A tibble containing the team dictionary.
#' @export
nba_team_dictionary <- function() {
  team_dictionary <- fetch_team_dictionary_data()

  return(team_dictionary)
}

#' Fetch Team Dictionary from API
#'
#' @return A list containing the raw team dictionary data.
fetch_team_dictionary_data <- function() {
  headers <- generate_headers_stats()
  params <- generate_parameters_team_dict()

  url <- "https://stats.nba.com/stats/franchisehistory"

  data <- get_data(url, headers, params)

  column_names <- data$resultSets$headers[[1]] %>%
    as.character()

  dt <- data$resultSets$rowSet[[1]] %>%
    data.frame(stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    set_names(column_names) %>%
    clean_names()

  return(dt)
}

#' Get NBA Player Headshot Photo
#'
#' Gets the URL for a player's headshot based on their NBA player ID.
#'
#' @param player_id A numeric or character vector representing the player's ID.
#' @return A character vector containing the headshot URLs.
#' @export
nba_player_headshot <- function(player_id) {
  if (missing(player_id)) {
    stop("Error: 'player_id' is required to retrieve the player headshot URL.")
  }

  if (!is.numeric(player_id) && !is.character(player_id)) {
    stop("Error: 'player_id' must be a numeric or character vector.")
  }

  get_player_headshot(player_id)
}

#' Get NBA Team Logo
#'
#' Gets the URL for a team's logo based on their NBA team ID.
#'
#' @param team_id A numeric or character vector representing the team's ID.
#' @return A character vector containing the logo URLs.
#' @export
nba_team_logo <- function(team_id) {
  if (missing(team_id)) {
    stop("Error: 'team_id' is required to retrieve the team logo URL.")
  }

  if (!is.numeric(team_id) && !is.character(team_id)) {
    stop("Error: 'team_id' must be a numeric or character vector.")
  }

  get_team_logo(team_id)
}
