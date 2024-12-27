#' Get NBA Play-by-Play Data
#'
#' This function gets play-by-play data for a vector of game IDs and returns a
#' combined tibble.
#' Creates batches of `game_ids` and pauses between batches to avoid timeout issues.
#'
#' @param game_ids A character vector of game IDs.
#' @param batch_size Number of requests before pausing (default: 100)
#' @param pause_seconds Number of seconds to pause between batches (default: 15)
#' @return A tibble containing combined play-by-play data for all game IDs.
#' @export
nba_play_by_play <- function(game_ids, batch_size = 100, pause_seconds = 15) {
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
            tryCatch(fetch_play_by_play_data(.x),
                     error = function(e) {
                         message(glue::glue("Error fetching data for Game ID {.x}: {e$message}"))
                         return(tibble())
                     })
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

#' Fetch Play-by-Play Data from API
#'
#' This function fetches play-by-play data for a given game ID.
#'
#' @param game_id The ID of the game for which to fetch data.
#' @return A list containing the raw play-by-play data and column names.
fetch_play_by_play_data <- function(game_id) {
    headers <- generate_headers_stats()

    all_data <- map_dfr(game_id, function(game) {
        url <- paste0("https://stats.nba.com/stats/playbyplayv2?GameID=", game,
                      "&StartPeriod=0&EndPeriod=12")

        data <- get_data_no_params(url, headers)

        column_names <- data$resultSets$headers[[1]] %>%
            as.character()

        data <- data$resultSets$rowSet[[1]] %>%
            data.frame(stringsAsFactors = FALSE) %>%
            as_tibble() %>%
            set_names(column_names) %>%
            clean_names()

        return(data)
    })

    return(all_data)
}
