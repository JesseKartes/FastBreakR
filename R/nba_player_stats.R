#' Get NBA Player Stats
#'
#' This function gets NBA player stats for the specified seasons returning a
#' list where each season contains a data frame for each measure type. Function
#' pauses for five seconds after each season to prevent timeout issues.
#'
#' @param seasons A numeric vector of seasons (e.g., 2024) for which to scrape NBA player stats.
#' @param season_type The season type for the API request.
#' @return A named list where each element is a data frame containing player level stats for that season.
#' @export
nba_player_stats <- function(seasons, season_type = "Regular Season") {
    if (!is.numeric(seasons) || length(seasons) == 0) {
        stop("The 'seasons' parameter must be a non-empty numeric vector.")
    }

    results <- map(seq_along(seasons), function(i) {
        season <- seasons[i]
        message(glue::glue("Processing season {season} ({i}/{length(seasons)})"))

        # Try to fetch and process data for the season
        nba_final <- tryCatch({
            all_data_list <- map(player_measure_types, function(measure_type) {
                fetch_player_stats(season, measure_type, season_type)
            })

            all_data_list <- map(all_data_list, process_player_measures)

        }, error = function(e) {
            message(glue::glue("Error processing season {season}: {e$message}"))
            return(NULL)  # Return NULL if an error occurs
        })

        # Pause after processing each season unless it's the last
        if (i < length(seasons)) {
            message(glue::glue("Pausing for 5 seconds before processing the next season..."))
            Sys.sleep(5)
        }

        if (!is.null(nba_final)) {
            names(nba_final) <- get_player_measure_types()
        }

        return(nba_final)
    })

    names(results) <- glue::glue("season_{seasons}") %>% as.character()
    return(results)
}

#' Fetch NBA Player Stats from API
#'
#' This function fetches and cleans NBA player stats for a specified season and all measure types.
#'
#' @param season A numeric value representing the season (e.g., 2024).
#' @param measure_type A character string specifying the measure type (e.g., "Base").
#' @param season_type A character string specifying the type of season (e.g., "Regular Season").
#' @return A data frame with cleaned NBA player stats.
fetch_player_stats <- function(season, measure_type, season_type) {
    headers <- generate_headers_stats()

    url <- "https://stats.nba.com/stats/playergamelogs"

    params <- generate_parameters_stats(season, measure_type, season_type)

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
#' This helper function processes player data by converting the `game_date` column
#' to a Date object and converting necessary columns to numeric values.
#'
#' @param player_data A data frame containing player stats to be converted to numeric.
#' @return A data frame with the `game_date` column converted to Date and other
#' relevant columns converted to numeric.
#' @export
process_player_measures <- function(player_data) {
    player_data <- player_data %>%
        mutate(game_date = as_date(game_date)) %>%
        arrange(game_date, game_id, player_id)

    min_column_index <- which(names(player_data) == "min")
    cols_to_convert <- min_column_index:(ncol(player_data) - 1)

    player_data <- player_data %>%
        mutate(across(cols_to_convert, as.numeric))

    return(player_data)
}
