#' Calculate Game Count for a Season
#'
#' This function generates a count of games for each team in a season.
#'
#' @return A numeric vector with game counts.
calc_game_count <- function() {
    row_number()
}

#' Calculate Days Rest
#'
#' This function calculates the number of days of rest between games.
#'
#' @param game_count The count of games for the season.
#' @param game_date The date of the current game.
#' @return A numeric value representing the days of rest.
calc_days_rest <- function(game_count, game_date) {
    case_when(
        game_count > 1 ~ as.numeric(game_date - lag(game_date) - 1),
        TRUE ~ 120
    )
}

#' Calculate Days Next Game
#'
#' This function calculates the number of days until the next game for a team.
#'
#' @param game_count The count of games for the season.
#' @param game_date The date of the current game.
#' @return A numeric value representing the days until the next game.
calc_days_next_game <- function(game_count, game_date) {
    case_when(
        game_count < 82 ~ as.numeric(lead(game_date) - game_date - 1),
        TRUE ~ 120
    )
}

#' Determine If Back-to-Back
#'
#' This function determines if the current game is part of a back-to-back (B2B) scenario.
#'
#' @param days_next_game The number of days until the next game.
#' @param days_rest The number of days of rest before the current game.
#' @return A logical value indicating if it's a back-to-back.
calc_is_b2b <- function(days_next_game, days_rest) {
    days_next_game == 0 | days_rest == 0
}

#' Determine If B2B First
#'
#' This function determines if the current game is the first game of a back-to-back (B2B) scenario.
#'
#' @param days_next_game The number of days until the next game.
#' @return A logical value indicating if it's the first game of a back-to-back.
calc_is_b2b_first <- function(days_next_game) {
    days_next_game == 0
}

#' Determine If B2B Second
#'
#' This function determines if the current game is the second game of a back-to-back (B2B) scenario.
#'
#' @param days_rest The number of days of rest before the current game.
#' @return A logical value indicating if it's the second game of a back-to-back.
calc_is_b2b_second <- function(days_rest) {
    days_rest == 0
}

#' Process Game Details
#'
#' This function processes location, game date, and season
#'
#' @param data A data frame containing raw data.
#' @return A data frame with processed location data.
add_game_details <- function(data) {
    data %>%
        arrange(game_date, game_id) %>%
        mutate(
            location = if_else(grepl("@", matchup), "away", "home"),
            game_date = as_date(game_date),
            season_year = as.numeric(substr(season_year, 1, 4)) + 1
        )
}

#' Join Multiple Data Frames by Removing Duplicate Columns
#'
#' This function takes a list of data frames and joins them using `full_join`, ensuring that
#' only unique columns from each data frame are included in the final result. It removes any
#' columns that already exist in the preceding data frames.
#'
#' @param data_frames A list of data frames to be joined.
#' @param join_columns A character vector of column names to join the data frames by.
#'
#' @return A single data frame with all the data frames joined by the specified columns.
join_data_frames <- function(data_frames, join_columns) {
    # Use `reduce` to join all data frames in the list
    reduce(data_frames, function(x, y) {
        # Determine columns in y that are not in x
        y_cols <- setdiff(names(y), names(x))

        # Select the join columns and the new columns from y
        y_subset <- select(y, all_of(c(join_columns, y_cols)))

        # Perform the full join
        full_join(x, y_subset, by = join_columns)
    })
}

#' Consolidate NBA Statistics
#'
#' To be used on the output of `nba_team_stats()` or `nba_player_stats()`.
#' Consolidates data frames within each NBA season by performing joins using
#' `full_join`, ensuring that only unique columns from each data frame are
#' included in the final result. It removes any columns that already exist in
#' the preceding data frames.
#'
#' @param data A named list where each element represents a season. Each season
#'   contains a list of data frames for various statistical categories.
#' @return A named list of consolidated data frames, one for each season. Seasons
#'   with no valid data or common join columns are excluded.
#' @export
consolidate_stats <- function(data) {
    # Check if input is a list
    if (!is.list(data)) {
        stop("Input must be a list of seasons, where each season contains a data frame.")
    }

    data %>%
        # Use map to process each season
        map(function(season_data) {
            # Identify column types that have data
            column_types <- names(season_data)

            # Filter and extract non-null data frames
            data_frames <- keep(
                lapply(column_types, function(type) season_data[[type]]),
                ~ !is.null(.)
            )

            # If no data frames, return NULL
            if (length(data_frames) == 0) return(NULL)

            # Potential join columns
            potential_join_columns <- c("game_id", "team_id", "player_id")

            # Find actual join columns present in all data frames
            actual_join_columns <- potential_join_columns[
                potential_join_columns %in%
                    Reduce(intersect, lapply(data_frames, names))
            ]

            # If no join columns found, return NULL
            if (length(actual_join_columns) == 0) return(NULL)

            # Reduce data frames using full_join
            join_data_frames(data_frames, actual_join_columns)

        }) %>%
        # Remove NULL entries and name list elements
        discard(is.null)
}

#' Add Schedule Details Across Seasons
#'
#' To be used only after `consolidate_stats()`.
#' This function calculates schedule details (such as back-to-back games, rest days, etc.)
#' for each team across multiple NBA seasons.
#' @param data A list where each element is a season's data frame containing NBA stats for teams (must include columns such as `season_year`, `team_id`, `game_date`, etc.).
#' @return A list of data frames, each containing the calculated stats for a specific season.
#' @export
add_schedule_details <- function(data) {
    # Check if input is a list
    if (!is.list(data)) {
        stop("Input must be a list of seasons, where each season contains a data frame.")
    }

    # Calculate stats for each season
    calc_metrics <- map(data, function(season_data) {
        # Check if input is a data frame
        if (!is.data.frame(season_data)) {
            stop("Please run consolidate_stats() before adding schedule details.")
        }

        # Determine grouping columns
        group_by_columns <- if ("player_id" %in% names(season_data)) {
            c("season_year", "player_id")
        } else {
            c("season_year", "team_id")
        }

        season_data %>%
            arrange(game_date) %>%
            group_by(across(all_of(group_by_columns))) %>%
            mutate(
                game_count = calc_game_count(),
                days_rest = calc_days_rest(game_count, game_date),
                days_next_game = calc_days_next_game(game_count, game_date),
                is_b2b = calc_is_b2b(days_next_game, days_rest),
                is_b2b_first = calc_is_b2b_first(days_next_game),
                is_b2b_second = calc_is_b2b_second(days_rest)
            ) %>%
            ungroup() %>%
            mutate(across(where(is.logical), ~ replace_na(., FALSE))) %>%
            add_game_details() %>%
            select(season_year:matchup, location, wl:ncol(.))
    })

    # Return the calculated stats
    return(calc_metrics)
}

#' Clean NBA Stats Data
#'
#' This function cleans the fetched NBA team and player stats data by
#' removing excess columns.
#'
#' @param data A data frame containing raw NBA stats data.
#' @return A cleaned data frame.
clean_stats_cols <- function(data) {
    data %>% select(-starts_with(c("e_", "sp_")),
                  -ends_with(c("_rank", "_flag")), -contains("fantasy"))
}

#' Get Available Measure Types for Team Stats
#'
#' Returns a vector of available measure types for team stats.
#'
#' @return A character vector of measure types.
get_team_measure_types <- function() {
    return(team_measure_types)
}

#' Get Available Measure Types for Player Stats
#'
#' Returns a vector of available measure types for player stats.
#'
#' @return A character vector of measure types.
get_player_measure_types <- function() {
    return(player_measure_types)
}

#' Get Data from a URL
#'
#' Makes an HTTP GET request to the specified URL and retrieves the content.
#'
#' @param url A character string specifying the URL to fetch data from.
#' @param headers A named character vector of HTTP headers to include in the request. Default is `NULL`.
#' @param params Parameters to include in the request.
#' @return The content of the HTTP response. The type of content depends on the response from the server.
get_data <- function(url, headers = NULL, params = NULL) {
    res <- httr::GET(url = url,
                     httr::add_headers(.headers = headers),
                     query = params)

    if (httr::http_status(res)$category != "Success") {
        stop("Request failed with status: ", httr::http_status(res)$message)
    }

    data <- res$content %>% rawToChar() %>% jsonlite::fromJSON(simplifyVector = T)

    return(data)
}

#' Get Data from a URL with no Parameters
#'
#' Makes an HTTP GET request to the specified URL and retrieves the content.
#'
#' @param url A character string specifying the URL to fetch data from.
#' @param headers A named character vector of HTTP headers to include in the request. Default is `NULL`.
#' @return The content of the HTTP response. The type of content depends on the response from the server.
get_data_no_params <- function(url, headers = NULL) {
    res <- httr::GET(url = url,
                     httr::add_headers(.headers = headers))

    if (httr::http_status(res)$category != "Success") {
        stop("Request failed with status: ", httr::http_status(res)$message)
    }

    data <- res$content %>% rawToChar() %>% jsonlite::fromJSON(simplifyVector = T)

    return(data)
}

#' Generate NBA Player Headshot URL
#'
#' Generates the URL for a player's headshot based on their NBA player ID.
#'
#' @param player_id A numeric or character vector representing the player's ID.
#' @return A character vector containing the headshot URLs.
get_player_headshot <- function(player_id) {
    glue::glue("https://cdn.nba.com/headshots/nba/latest/1040x760/{player_id}.png")
}

#' Generate NBA Team Logo URL
#'
#' Generates the URL for a team's logo based on their NBA team ID.
#'
#' @param team_id A numeric or character vector representing the team's ID.
#' @return A character vector containing the logo URLs.
get_team_logo <- function(team_id) {
    glue::glue("https://cdn.nba.com/logos/nba/{team_id}/primary/L/logo.svg")
}
