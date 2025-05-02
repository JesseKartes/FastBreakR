#' Get NBA Standings Data
#'
#' This function gets NBA standings as a data frame for a single or multiple seasons.
#'
#' @param seasons A numeric vector of seasons for which to get the standings data.
#' @param return_nested A logical value. If FALSE (default), returns a single
#' combined data frame for all seasons. If TRUE, returns a list of data frames, one for each season.
#' @return A data frame containing the NBA standings data for the specified seasons.
#' @export
nba_standings <- function(seasons, return_nested = FALSE) {
  if (!is.numeric(seasons) || length(seasons) == 0) {
    stop("The 'seasons' parameter must be a non-empty numeric vector.")
  }

  results <- map_dfr(seasons, function(year) {
    all_standings <- fetch_standings(year)
    return(all_standings)
  })

  # Return nested results (list of data frames)
  if (return_nested) {
    results_list <- split(results, results$season_year)
    names(results_list) <- glue("season_{seasons}") %>% as.character()

    return(results_list)
  }

  # If return_nested is FALSE (default), return a combined data frame
  return(results)
}

#' Fetch and Process NBA Standings Data
#'
#' This function fetches the NBA standings data for a specific season, processes it into a tidy
#' format, and returns it as a data frame.
#'
#' @param seasons A numeric value representing the year for which to fetch the standings data.
#' @return A data frame containing the processed NBA standings data for the specified season.
#' @export
fetch_standings <- function(seasons) {
  headers <- generate_headers_stats()

  url <- "https://stats.nba.com/stats/leaguestandingsv3"

  all_data <- map_dfr(seasons, function(year) {
    params <- generate_params_standings(year)

    data <- get_data(url, headers, params)

    column_names <- data$resultSets$headers[[1]] %>%
      as.character()

    dt <- data$resultSets$rowSet[[1]] %>%
      data.frame(stringsAsFactors = FALSE) %>%
      as_tibble() %>%
      set_names(column_names) %>%
      clean_names() %>%
      mutate(season_year = year)

    return(dt)
  })

  return(all_data)
}
