#' Get NBA Win Probability Play-by-Play Data
#'
#' This function gets win probability play-by-play data for a vector of game IDs and returns a
#' combined tibble.
#' Creates batches of `game_ids` and pauses between batches to avoid timeout issues.
#'
#' @param game_ids A character vector of game IDs.
#' @param batch_size Number of requests per batch (default: 100).
#' @param pause_seconds Number of seconds to pause between batches (default: 15).
#' @return A tibble containing combined win probability play-by-play data for all game IDs.
#' @export
nba_win_probability <- function(game_ids,
                                batch_size = 100,
                                pause_seconds = 15) {
  unique_games <- unique(game_ids)
  total_games <- length(unique_games)

  # Divide game IDs into batches
  batched_games <- split(
    unique_games,
    ceiling(seq_along(unique_games) / batch_size)
  )
  num_batches <- length(batched_games)

  message(glue("Fetching {total_games} games in {num_batches} batches"))

  plan(multisession)

  # Process each batch sequentially with rate limiting
  results <- map_dfr(seq_along(batched_games), function(batch_num) {
    batch_games <- batched_games[[batch_num]]
    message(glue("Fetching batch {batch_num}/{num_batches}: games
                       {batch_games[1]} to {batch_games[length(batch_games)]}"))

    # Fetch data in parallel for the current batch
    batch_results <- future_map_dfr(batch_games, ~ {
      tryCatch(
        {
          raw_data <- fetch_win_probability_data(.x)
          process_win_probability_data(raw_data$data, raw_data$metadata)
        },
        error = function(e) {
          message(glue("Error fetching data for Game ID {.x}:
                             {e$message}"))
          return(tibble())
        }
      )
    })

    # Pause after processing a batch unless it's the last batch
    if (batch_num < num_batches) {
      message(glue("Pausing for {pause_seconds} seconds..."))
      Sys.sleep(pause_seconds)
    }

    return(batch_results)
  })

  return(results)
}


#' Fetch Win Probability Play-by-Play Data from API
#'
#' This function fetches win probability play-by-play data for a given game ID.
#'
#' @param game_id The ID of the game for which to fetch data.
#' @return A list containing the raw play-by-play data and metadata as tibbles.
fetch_win_probability_data <- function(game_id) {
  headers <- generate_headers_stats()
  url <- glue(
    "https://stats.nba.com/stats/winprobabilitypbp?GameID={game_id}",
    "&StartPeriod=0&EndPeriod=12&StartRange=0&EndRange=12&RangeType=1",
    "&Runtype=each%20second"
  )

  data <- get_data_no_params(url, headers)

  # Extract and clean play-by-play data
  data_columns <- data$resultSets$headers[[1]] %>% as.character()
  data_dt <- data$resultSets$rowSet[[1]] %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    set_names(data_columns) %>%
    clean_names()

  # Extract and clean metadata
  metadata_columns <- data$resultSets$headers[[2]] %>% as.character()
  metadata_dt <- data$resultSets$rowSet[[2]] %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    set_names(metadata_columns) %>%
    clean_names()

  return(list(data = data_dt, metadata = metadata_dt))
}

#' Process Win Probability Play-by-Play Data
#'
#' This function processes the raw win probability play-by-play data.
#'
#' @param data Raw win probability play-by-play data.
#' @param metadata Metadata associated with the data.
#' @return A tibble with processed win probability play-by-play data.
process_win_probability_data <- function(data, metadata) {
  df_metadata <- metadata %>%
    mutate(game_date = mdy(game_date)) %>%
    select(-matches("pts"))

  processed_data <- data %>%
    left_join(df_metadata, by = "game_id") %>%
    select(any_of(names(df_metadata)), everything())

  return(processed_data)
}
