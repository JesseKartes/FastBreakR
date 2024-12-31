#' Get NBA Shots
#'
#' This function gets NBA shot data for the specified seasons and
#' returns a data frame. Function pauses for five seconds after each season to
#' prevent timeout issues.
#'
#' @param seasons A numeric vector of seasons (e.g., 2024) for which to fetch
#' NBA shot data.
#' @param season_type A character string specifying the type of season. Valid
#' options include:
#' \itemize{
#'   \item \strong{"Pre Season"} - Pre Season games.
#'   \item \strong{"Regular Season"} - Regular Season games.
#'   \item \strong{"Playoffs"} - Playoff games.
#'   \item \strong{"All Star"} - All Star games.
#'   \item \strong{"IST"} - NBA Cup games.
#'   \item \strong{"PlayIn"} - Play In games.
#' }
#' @param return_nested A logical value. If FALSE (default), returns a single
#' combined data frame for all seasons.If TRUE, returns a list of data frames,
#' one for each season.
#' @return A named a data frame containing NBA shots data for the specified
#' seasons.
#' @export
nba_shots <- function(seasons,
                      season_type = "Regular Season",
                      return_nested = FALSE) {
  if (!is.numeric(seasons) || length(seasons) == 0) {
    stop("The 'seasons' parameter must be a non-empty numeric vector.")
  }

  results <- map_dfr(seq_along(seasons), function(i) {
    season <- seasons[i]
    message(glue::glue("Fetching season {season} ({i}/{length(seasons)})"))

    # Try to fetch and process data for the season
    shots_data <- tryCatch(
      {
        data <- fetch_shots_data(season, season_type)

        data <- data$shots_data
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

    return(shots_data)
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

#' Get League Average NBA Shots
#'
#' This function gets league average NBA shot data for the specified seasons and
#' returns a data frame. Function pauses for five seconds after each season to
#' prevent timeout issues.
#'
#' @param seasons A numeric vector of seasons (e.g., 2024) for which to fetch
#' NBA shot data.
#' @param season_type A character string specifying the type of season. Valid
#' options include:
#' \itemize{
#'   \item \strong{"Pre Season"} - Pre Season games.
#'   \item \strong{"Regular Season"} - Regular Season games.
#'   \item \strong{"Playoffs"} - Playoff games.
#'   \item \strong{"All Star"} - All Star games.
#'   \item \strong{"IST"} - NBA Cup games.
#'   \item \strong{"PlayIn"} - Play In games.
#' }
#' @param return_nested A logical value. If FALSE (default), returns a single
#' combined data frame for all seasons.If TRUE, returns a list of data frames,
#' one for each season.
#' @return A named a data frame containing league average NBA shots data for the
#' specified seasons.
#' @export
nba_shots_league_avg <- function(seasons,
                                 season_type = "Regular Season",
                                 return_nested = FALSE) {
  if (!is.numeric(seasons) || length(seasons) == 0) {
    stop("The 'seasons' parameter must be a non-empty numeric vector.")
  }

  results <- map_dfr(seq_along(seasons), function(i) {
    season <- seasons[i]
    message(glue::glue("Fetching season {season} ({i}/{length(seasons)})"))

    # Try to fetch and process data for the season
    shots_data <- tryCatch(
      {
        data <- fetch_shots_data(season, season_type)

        data <- data$league_data
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

    return(shots_data)
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

#' Fetch NBA Shots from API
#'
#' This function fetches raw NBA shot data and league average data for a given
#' season.
#'
#' @param season A numeric value representing the season (e.g., 2024).
#' @param season_type A character string specifying the type of season
#' (e.g., "Regular Season").
#' @return A list containing raw shots data and league average data.
fetch_shots_data <- function(season, season_type) {
  headers <- generate_headers_stats()

  url <- "https://stats.nba.com/stats/shotchartdetail"

  params <- generate_params_shots(season, season_type)

  data <- get_data(url, headers, params)

  shots_column_names <- data$resultSets$headers[[1]] %>%
    as.character()

  shots_dt <- data$resultSets$rowSet[[1]] %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    set_names(shots_column_names) %>%
    clean_names() %>%
    mutate(
      season_year = season,
      game_date = as_date(game_date, format = "%Y%m%d")
    )

  league_column_names <- data$resultSets$headers[[2]] %>%
    as.character()

  league_dt <- data$resultSets$rowSet[[2]] %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    set_names(league_column_names) %>%
    clean_names() %>%
    mutate(season_year = season)

  list(
    shots_data = shots_dt,
    league_data = league_dt
  )
}

#' Process NBA Shots Data (function under development)
#'
#' This function is currently in development and not exported.
#'
#' To be used on the output of `nba_shots()`. Processes shot and league average
#' data frames within each NBA season. Applies transformations to prepare the
#' data for analysis.
#'
#' @param data A named list where each element represents a season. Each season
#' contains a list with two data frames: `shots` and `league_avg`.
#' @return A named list of processed data frames for each season. Transforms the
#' data to prepare for analysis.
process_shots <- function(data) {
  # Check if input is a list
  if (!is.list(data)) {
    stop("Input must be a list of seasons, where each season contains a
         'shots' and 'league_avg' data frame.")
  }

  data %>%
    map(function(season_data) {
      # Validate that the season contains both required data frames
      if (!all(c("shots", "league_avg") %in% names(season_data))) {
        warning("Skipping season due to missing 'shots' or 'league_avg' data.")
        return(NULL)
      }

      # Extract the shots and league_avg data frames
      shots <- season_data$shots
      league_avg <- season_data$league_avg

      # Process shots data
      processed_shots <- shots %>%
        mutate(
          loc_x = -as.numeric(loc_x) / 10,
          loc_y = as.numeric(loc_y) / 10 + 5.25,
          shot_distance = as.numeric(shot_distance),
          shot_made_numeric = as.numeric(shot_made_flag),
          shot_made_flag = factor(shot_made_flag,
            levels = c("1", "0"),
            labels = c("made", "missed")
          ),
          shot_attempted_flag = as.numeric(shot_attempted_flag),
          shot_value = if_else(str_detect(shot_type, "3PT"), 3, 2),
          game_date = as_date(game_date, format = "%Y%m%d")
        )

      # Process league_avg data
      processed_league_avg <- league_avg %>%
        mutate(
          fga = as.numeric(fga),
          fgm = as.numeric(fgm),
          fg_pct = as.numeric(fg_pct),
          shot_value = if_else(
            str_detect(shot_zone_basic, "3") | shot_zone_basic == "Backcourt",
            3, 2
          )
        )

      # Return a list of processed data frames
      list(
        processed_shots = processed_shots,
        processed_league_avg = processed_league_avg
      )
    }) %>%
    # Remove NULL entries and name list elements
    discard(is.null)
}
