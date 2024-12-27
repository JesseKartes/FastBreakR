#' Box Score Options for API Endpoints
#'
#' A list of box score options available for fetching data from the NBA Stats API. Each entry in the list corresponds
#' to a different type of box score data, with the associated API endpoint and the name of the list in the returned data.
#'
#' @format A list where each element is a named list with:
#' \describe{
#'   \item{\code{endpoint}}{A character string representing the API endpoint to fetch data for that box score type.}
#'   \item{\code{list_name}}{A character string representing the list name in the response data. This is used to access the corresponding data from the API response.}
#' }
#' @export
box_score_types <- list(
  Traditional = list(endpoint = "boxscoretraditionalv3", list_name = "boxScoreTraditional"),
  Advanced = list(endpoint = "boxscoreadvancedv3", list_name = "boxScoreAdvanced"),
  Misc = list(endpoint = "boxscoremiscv3", list_name = "boxScoreMisc"),
  Scoring = list(endpoint = "boxscorescoringv3", list_name = "boxScoreScoring"),
  Usage = list(endpoint = "boxscoreusagev3", list_name = "boxScoreUsage"),
  Factors = list(endpoint = "boxscorefourfactorsv3", list_name = "boxScoreFourFactors"),
  Tracking = list(endpoint = "boxscoreplayertrackv3", list_name = "boxScorePlayerTrack"),
  Defensive = list(endpoint = "boxscoredefensivev2", list_name = "boxScoreDefensive"),
  Hustle = list(endpoint = "boxscorehustlev2", list_name = "boxScoreHustle")
)

#' Get NBA Box Scores
#'
#' This function gets NBA box score data for the specified `game_ids`, returning
#' a data frame with all selected `measure_types`.
#' Creates batches of `game_ids` and pauses between batches to avoid timeout issues.
#'
#' @param measure_types A character vector of selected box score options. Each option corresponds to an entry in
#' `box_score_types`. Valid options include:
#' \itemize{
#'   \item \strong{"Traditional"} - Box score with traditional statistics.
#'   \item \strong{"Advanced"} - Box score with advanced statistics.
#'   \item \strong{"Misc"} - Box score with miscellaneous statistics.
#'   \item \strong{"Scoring"} - Box score with scoring statistics.
#'   \item \strong{"Usage"} - Box score with usage statistics.
#'   \item \strong{"Factors"} - Box score with four factors statistics.
#'   \item \strong{"Tracking"} - Box score with player tracking statistics.
#'   \item \strong{"Defensive"} - Box score with defensive statistics.
#'   \item \strong{"Hustle"} - Box score with hustle statistics.
#' }
#' @param game_ids A character vector of game IDs.
#' @param batch_size Number of requests before pausing (default: 100)
#' @param pause_seconds Number of seconds to pause between batches (default: 15)
#'
#' @return A data frame containing box score data for the selected games and
#' measure types.
#' @export
nba_box_scores <- function(measure_types, game_ids, batch_size = 100, pause_seconds = 15) {
  # Validate user choices
  invalid_choices <- setdiff(measure_types, names(box_score_types))
  if (length(invalid_choices) > 0) {
    stop(paste("Invalid choice(s):", paste(invalid_choices, collapse = ", "), "\nValid options are:\n", paste(names(box_score_types), collapse = ", ")))
  }

  # Filter the selected options
  selected_types <- box_score_types[measure_types]

  # Call fetch_box_score to process data in batches
  fetch_box_score(selected_types, game_ids, batch_size, pause_seconds)
}

#' Get NBA Box Score Matchups
#'
#' This function gets NBA box score matchups data for the specified `game_ids`,
#' returning a data frame.
#' Creates batches of game_ids and pauses between batches to avoid timeout issues.
#'
#' @param game_ids A character vector of game IDs.
#' @param batch_size Number of requests before pausing (default: 100)
#' @param pause_seconds Number of seconds to pause between batches (default: 15)
#' @return A tibble containing combined box score matchup data for all game IDs.
#' @export
nba_box_scores_matchups <- function(game_ids, batch_size = 100, pause_seconds = 15) {
  unique_games <- unique(game_ids)
  total_games <- length(unique_games)

  # Divide game IDs into batches
  batched_games <- split(unique_games, ceiling(seq_along(unique_games) / batch_size))
  num_batches <- length(batched_games)

  message(glue::glue("Processing {total_games} games in {num_batches} batches"))

  future::plan(future::multisession)

  # Process each batch sequentially with parallel handling within batches
  results <- map_dfr(seq_along(batched_games), function(batch_num) {
    batch_games <- batched_games[[batch_num]]
    message(glue::glue("Processing batch {batch_num}/{num_batches}: games {batch_games[1]} to {batch_games[length(batch_games)]}"))

    # Fetch data in parallel for the current batch
    batch_results <- future_map_dfr(batch_games, ~ {
      tryCatch(
        {
          headers <- generate_headers_stats()
          url <- "https://stats.nba.com/stats/boxscorematchupsv3"
          params <- generate_parameters_box_scores(.x)

          data <- get_data(url, headers, params)

          matchups_away <- list(data$boxScoreMatchups$awayTeam) %>%
            map_df(flatten) %>%
            unnest(cols = c(matchups), names_sep = "_") %>%
            unnest(cols = c(matchups_statistics), names_sep = "_") %>%
            rename_with(
              ~ gsub("^matchups_statistics_", "", .x),
              starts_with("matchups_statistics_")
            ) %>%
            clean_names() %>%
            mutate(
              game_id = data$boxScoreMatchups$gameId,
              location = "away"
            )

          matchups_home <- list(data$boxScoreMatchups$homeTeam) %>%
            map_df(flatten) %>%
            unnest(cols = c(matchups), names_sep = "_") %>%
            unnest(cols = c(matchups_statistics), names_sep = "_") %>%
            rename_with(
              ~ gsub("^matchups_statistics_", "", .x),
              starts_with("matchups_statistics_")
            ) %>%
            clean_names() %>%
            mutate(
              game_id = data$boxScoreMatchups$gameId,
              location = "home"
            )

          matchups_df <- bind_rows(matchups_away, matchups_home) %>%
            select(game_id, location, team_id:shooting_fouls)

          return(matchups_df)
        },
        error = function(e) {
          message(glue::glue("Error fetching data for Game ID {.x}: {e$message}"))
          return(tibble())
        }
      )
    })

    # Pause after processing a batch unless it's the last batch
    if (batch_num < num_batches) {
      message(glue::glue("Pausing for {pause_seconds} seconds..."))
      Sys.sleep(pause_seconds)
    }

    return(batch_results)
  })

  return(results)
}

#' Fetch Box Score Data from API
#'
#' This function retrieves box score data from the NBA Stats API for given game IDs
#' using a specified endpoint. It creates batches of game_ids and pauses between
#' batches to avoid timeout issues.
#'
#' @param measure_type A vector of measure types for various box score data.
#' @param game_ids A character vector of game IDs.
#' @param batch_size Number of requests before pausing (default: 100)
#' @param pause_seconds Number of seconds to pause between batches (default: 15)
#'
#' @return A data frame containing the box score data for the specified game IDs.
#'
#' @importFrom future plan
#' @importFrom future multisession
#' @importFrom dplyr bind_rows mutate select left_join
#' @importFrom janitor clean_names
fetch_box_score <- function(measure_type, game_ids, batch_size = 100, pause_seconds = 15) {
  unique_games <- unique(game_ids)
  total_games <- length(unique_games)

  # Divide game IDs into batches
  batched_games <- split(unique_games, ceiling(seq_along(unique_games) / batch_size))
  num_batches <- length(batched_games)

  message(glue::glue("Processing {total_games} games in {num_batches} batches"))

  future::plan(future::multisession)

  # Process each batch sequentially
  results <- map_dfr(seq_along(batched_games), function(batch_num) {
    batch_games <- batched_games[[batch_num]]
    message(glue::glue("Processing batch {batch_num}/{num_batches}: games {batch_games[1]} to {batch_games[length(batch_games)]}"))

    # Process each game in the current batch
    batch_results <- map_dfr(batch_games, function(game_id) {
      tryCatch(
        {
          # Process each measure type for the current game and collect in a list
          data_frames <- map(measure_type, function(measure) {
            endpoint <- measure$endpoint
            list_name <- measure$list_name

            # message(glue::glue("Fetching data from {endpoint} for game ID {game_id}"))

            headers <- generate_headers_stats()
            url <- paste0("https://stats.nba.com/stats/", endpoint)
            params <- generate_parameters_box_scores(game_id)
            data <- get_data(url, headers, params)

            # Process team data function
            process_team_data <- function(team_data, game_id, team_id, location) {
              team_data %>%
                unnest_wider(statistics) %>%
                clean_names() %>%
                mutate(
                  game_id = game_id,
                  team_id = team_id,
                  location = location
                )
            }

            # Process both teams' data
            away_df <- process_team_data(
              data[[list_name]]$awayTeam$players,
              data[[list_name]]$gameId,
              data[[list_name]]$awayTeamId,
              "away"
            )

            home_df <- process_team_data(
              data[[list_name]]$homeTeam$players,
              data[[list_name]]$gameId,
              data[[list_name]]$homeTeamId,
              "home"
            )

            # Combine home and away data
            bind_rows(away_df, home_df) %>%
              select(game_id, location, team_id, everything())
          })

          # Define the join columns
          actual_join_columns <- c("game_id", "person_id")

          # Consolidate all measures using reduce
          base_data <- join_data_frames(data_frames, actual_join_columns)

          base_data
        },
        error = function(e) {
          message(glue::glue("Error fetching data for Game ID {game_id}: {e$message}"))
          return(tibble())
        }
      )
    })

    # Pause after processing a batch unless it's the last batch
    if (batch_num < num_batches) {
      message(glue::glue("Pausing for {pause_seconds} seconds..."))
      Sys.sleep(pause_seconds)
    }

    batch_results
  })

  results
}
