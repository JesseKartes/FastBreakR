#' PBP Column Name Map
#'
#' This vector is used to rename data.nba.com PBP columns.
pbp_column_map <- c(
  "game_id" = "game_id",
  "period" = "period",
  "evt" = "eventnum",
  "wallclk"= "wall_clock",
  "cl" = "clock",
  "de" = "desc",
  "loc_x" = "loc_x",
  "loc_y" = "loc_y",
  "opt1" = "opt1",
  "opt2" = "opt2",
  "opt3" = "opt3",
  "opt4" = "opt4",
  "mtype" = "eventmsgactiontype",
  "etype" = "eventmsgtype",
  "opid" = "player3_id",
  "tid" = "team_id",
  "pid" = "player1_id",
  "hs" = "home_score",
  "vs" = "away_score",
  "epid" = "player2_id",
  "oftid" = "offense_team",
  "ord" = "order",
  "pts" = "pts"
)

#' Get NBA Play-by-Play Data from data.nba.com API
#'
#' This function gets play-by-play data for a vector of game IDs and returns a combined tibble.
#' Creates batches of `game_ids` and pauses between batches to avoid timeout issues.
#'
#' @param game_ids A character vector of game IDs.
#' @param batch_size Number of requests before pausing (default: 100)
#' @param pause_seconds Number of seconds to pause between batches (default: 15)
#' @return A tibble containing combined play-by-play data for all game IDs.
#' @export
nba_data_play_by_play <- function(game_ids, batch_size = 100, pause_seconds = 15) {
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

  # Process each batch sequentially with parallel handling within batches
  results <- map_dfr(seq_along(batched_games), function(batch_num) {
    batch_games <- batched_games[[batch_num]]
    message(glue("Fetching batch {batch_num}/{num_batches}: games
                       {batch_games[1]} to {batch_games[length(batch_games)]}"))

    # Fetch data in parallel for the current batch
    batch_results <- future_map_dfr(batch_games, ~ {
      tryCatch(fetch_data_play_by_play(.x),
               error = function(e) {
                 message(glue("Error fetching data for Game ID {.x}: {e$message}"))
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

#' Get NBA Play-by-Play Data from stats.nba.com API
#'
#' This function gets play-by-play data for a vector of game IDs and returns a combined tibble.
#' Creates batches of `game_ids` and pauses between batches to avoid timeout issues.
#'
#' @param game_ids A character vector of game IDs.
#' @param batch_size Number of requests before pausing (default: 100)
#' @param pause_seconds Number of seconds to pause between batches (default: 15)
#' @return A tibble containing combined play-by-play data for all game IDs.
#' @export
nba_stats_play_by_play <- function(game_ids, batch_size = 100, pause_seconds = 15) {
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

  # Process each batch sequentially with parallel handling within batches
  results <- map_dfr(seq_along(batched_games), function(batch_num) {
    batch_games <- batched_games[[batch_num]]
    message(glue("Fetching batch {batch_num}/{num_batches}: games
                       {batch_games[1]} to {batch_games[length(batch_games)]}"))

    # Fetch data in parallel for the current batch
    batch_results <- future_map_dfr(batch_games, ~ {
      tryCatch(fetch_stats_play_by_play(.x),
               error = function(e) {
                 message(glue("Error fetching data for Game ID {.x}: {e$message}"))
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

#' Fetch Play-by-Play Data from data.nba.com API
#'
#' This function fetches play-by-play data for a given game ID.
#'
#' @param game_id The ID of the game for which to fetch data.
#' @return A list containing the raw play-by-play data and column names.
fetch_data_play_by_play <- function(game_id) {

  all_data <- map_dfr(game_id, function(game) {
    season <- get_season_start_year(game)

    url <- paste0(
      "https://data.nba.com/data/10s/v2015/json/mobile_teams/nba/", season,
      "/scores/pbp/", game, "_full_pbp.json"
    )

    data <- get_data_no_params(url)

    data <- data$g$pd$pla %>%
      bind_rows(.id = "period") %>%
      as_tibble() %>%
      mutate(game_id = game,
             pid = as.character(pid)) %>%
      clean_names()

    return(data)
  })

  all_data <- all_data %>%
    rename_with(
      ~ pbp_column_map[.x],
      .cols = names(pbp_column_map)
    ) %>%
    mutate(
      period = as.numeric(period),
      team_id = as.character(team_id),
      offense_team = as.character(offense_team)
    )

  return(all_data)
}

#' Fetch Play-by-Play Data from stats.nba.com API
#'
#' This function fetches play-by-play data for a given game ID.
#'
#' @param game_id The ID of the game for which to fetch data.
#' @return A list containing the raw play-by-play data and column names.
fetch_stats_play_by_play <- function(game_id) {
  headers <- generate_headers_stats()

  all_data <- map_dfr(game_id, function(game) {
    url <- paste0(
      "https://stats.nba.com/stats/playbyplayv2?GameID=", game, "&StartPeriod=0&EndPeriod=12"
    )

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

#' Get Season Start Year
#'
#' This function extracts the season start year from a game_id
#'
#' @param game_id The ID of the game for which to extract.
#' @return A string of the season start year.
get_season_start_year <- function(game_id) {
  game_id <- as.character(game_id)

  d4 <- substr(game_id, 4, 4)
  d5 <- substr(game_id, 5, 5)
  two_digits <- as.integer(paste0(d4, d5))


  start_year <- if_else(d4 == "9", paste0("19", two_digits), paste0("20", two_digits))

  return(start_year)
}
