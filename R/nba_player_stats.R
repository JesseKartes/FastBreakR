#' Get NBA Player Stats
#'
#' This function gets NBA player stats for the specified seasons returning a
#' data frame. Function pauses for five seconds after each season to prevent
#' timeout issues.
#'
#' @param seasons A numeric vector of seasons (e.g., 2024) for which to fetch
#' NBA player stats.
#' @param season_type A character string specifying the type of season
#' (default = "Regular Season"). Valid options include:
#' \itemize{
#'   \item \strong{"Pre Season"} - Pre Season games.
#'   \item \strong{"Regular Season"} - Regular Season games.
#'   \item \strong{"Playoffs"} - Playoff games.
#'   \item \strong{"All Star"} - All Star games.
#'   \item \strong{"IST"} - NBA Cup games.
#'   \item \strong{"PlayIn"} - Play In games.
#' }
#' @param measure_types A character vector specifying the types of stats
#' (default = NULL (all measure types)). Valid options include:
#' \itemize{
#'   \item \strong{"Base"} - Traditional stats.
#'   \item \strong{"Advanced"} - Advanced stats.
#'   \item \strong{"Usage"} - Usage stats.
#'   \item \strong{"Misc"} - Misc stats.
#'   \item \strong{"Scoring"} - Scoring stats.
#' }
#' @param return_nested A logical value. If FALSE (default), returns a single
#' combined data frame for all seasons.If TRUE, returns a list of data frames,
#' one for each season.
#' @return A  data frame containing player level stats for specified seasons and
#' measure types.
#' @export
nba_player_stats <- function(seasons,
                             season_type = "Regular Season",
                             measure_types = NULL,
                             return_nested = FALSE) {
  if (!is.numeric(seasons) || length(seasons) == 0) {
    stop("The 'seasons' parameter must be a non-empty numeric vector.")
  }

  measure_types <- if (is.null(measure_types)) {
    player_measure_types
  } else {
    measure_types
  }

  results <- map_dfr(seq_along(seasons), function(i) {
    season <- seasons[i]
    message(glue::glue("Fetching season {season} ({i}/{length(seasons)})"))

    # Try to fetch and process data for the season
    nba_final <- tryCatch(
      {
        all_data_list <- map(measure_types, function(measure_type) {
          fetch_player_stats(season, season_type, measure_type)
        })

        if (!is.null(all_data_list)) {
          names(all_data_list) <- measure_types
        }

        all_data_list %>%
          consolidate_stats() %>%
          process_player_measures() %>%
          add_schedule_details()
      },
      error = function(e) {
        message(glue::glue("Error fetching season {season}: {e$message}"))
        return(NULL) # Return NULL if an error occurs
      }
    )

    # Pause after processing each season unless it's the last
    if (i < length(seasons)) {
      message(glue::glue(
        "Pausing for 5 seconds before fetching the next season..."
      ))
      Sys.sleep(5)
    }

    return(nba_final)
  })

  # Return nested results (list of data frames)
  if (return_nested) {
    results_list <- split(results, results$season_year)
    names(results_list) <- glue::glue("season_{seasons}") %>% as.character()

    return(results_list)
  }

  # If return_nested is FALSE (default), return a combined data frame
  return(results)
}

#' Fetch NBA Player Stats from API
#'
#' This function fetches and cleans NBA player stats for a specified season and
#' all measure types.
#'
#' @param season A numeric value representing the season (e.g., 2024).
#' @param season_type A character string specifying the type of season
#' (e.g., "Regular Season").
#' @param measure_type A character string specifying the measure type
#' (e.g., "Base").
#' @return A data frame with cleaned NBA player stats.
fetch_player_stats <- function(season, season_type, measure_type) {
  headers <- generate_headers_stats()

  url <- "https://stats.nba.com/stats/playergamelogs"

  params <- generate_params_stats(season, season_type, measure_type)

  data <- get_data(url, headers, params)

  column_names <- data$resultSets$headers[[1]] %>%
    as.character()

  dt <- data$resultSets$rowSet[[1]] %>%
    data.frame(stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    set_names(column_names) %>%
    clean_names()

  return(clean_stats_cols(dt))
}

#' Process Player Data
#'
#' This helper function processes player data by converting the `game_date`
#' column to a Date object and converting necessary columns to numeric values.
#'
#' @param player_data A data frame containing player stats to be converted to
#' numeric.
#' @return A data frame with the `game_date` column converted to Date and other
#' relevant columns converted to numeric.
#' @export
process_player_measures <- function(player_data) {
  player_data <- player_data %>%
    mutate(
      game_date = as_date(game_date),
      across(
        -min_sec & min:last_col(),
        as.numeric
      )
    ) %>%
    arrange(game_date, game_id, player_id)

  return(player_data)
}
