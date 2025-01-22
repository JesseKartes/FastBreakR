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
#' This function determines if the current game is part of a back-to-back (B2B)
#' scenario.
#'
#' @param days_next_game The number of days until the next game.
#' @param days_rest The number of days of rest before the current game.
#' @return A logical value indicating if it's a back-to-back.
calc_is_b2b <- function(days_next_game, days_rest) {
  days_next_game == 0 | days_rest == 0
}

#' Determine If B2B First
#'
#' This function determines if the current game is the first game of a
#' back-to-back (B2B) scenario.
#'
#' @param days_next_game The number of days until the next game.
#' @return A logical value indicating if it's the first game of a back-to-back.
calc_is_b2b_first <- function(days_next_game) {
  days_next_game == 0
}

#' Determine If B2B Second
#'
#' This function determines if the current game is the second game of a
#' back-to-back (B2B) scenario.
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
#' This function takes a list of data frames and joins them using `full_join`,
#' ensuring that only unique columns from each data frame are included in the
#' final result. It removes any columns that already exist in the preceding data
#' frames.
#'
#' @param data_frames A list of data frames to be joined.
#' @param join_columns A character vector of column names to join the data
#' frames by.
#'
#' @return A single data frame with all the data frames joined by the specified
#' columns.
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
#' To be used on a list of data frames representing various NBA seasons.
#' Consolidates the data frames by performing joins using `full_join`,
#' ensuring that only unique columns from each data frame are included
#' in the final result. It removes any columns that already exist in
#' the preceding data frames.
#'
#' @param data A list of data frames. Each data frame corresponds to a season
#' and contains various statistical categories.
#' @return A consolidated data frame containing all the input data,
#' or NULL if no valid join columns are found.
consolidate_stats <- function(data) {
  # Check if input is a list of data frames
  if (!all(map_lgl(data, ~ is.data.frame(.)))) {
    stop("Input must be a list of data frames.")
  }

  # Potential join columns
  potential_join_columns <- c("game_id", "team_id", "player_id")

  # Find actual join columns present in all data frames
  actual_join_columns <- potential_join_columns[
    potential_join_columns %in%
      Reduce(intersect, lapply(data, names))
  ]

  # If no join columns found, return NULL
  if (length(actual_join_columns) == 0) {
    return(NULL)
  }

  # Use the existing join_data_frames() function
  consolidated_data <- join_data_frames(data, actual_join_columns)

  return(consolidated_data)
}

#' Add Schedule Details
#'
#' This function calculates schedule details (such as back-to-back games,
#' rest days, etc.) for NBA data.
#'
#' @param data A data frame containing NBA stats for teams (must include columns
#' such as `season_year`, `team_id`, `game_date`, etc.).
#' @return A data frame containing the details for the corresponding season.
add_schedule_details <- function(data) {
  # Determine grouping columns
  group_by_columns <- if ("player_id" %in% names(data)) {
    c("season_year", "player_id")
  } else {
    c("season_year", "team_id")
  }

  data %>%
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
}

#' Clean NBA Stats Data
#'
#' This function cleans the fetched NBA team and player stats data by
#' removing excess columns.
#'
#' @param data A data frame containing raw NBA stats data.
#' @return A cleaned data frame.
clean_stats_cols <- function(data) {
  data %>% select(
    -starts_with(c("e_", "sp_")),
    -ends_with(c("_rank", "_flag")), -contains("fantasy")
  )
}

#' Get Data from a URL
#'
#' Makes an HTTP GET request to the specified URL and retrieves the content.
#'
#' @param url A character string specifying the URL to fetch data from.
#' @param headers A named character vector of HTTP headers to include in the
#' request. Default is `NULL`.
#' @param params Parameters to include in the request.
#' @return The content of the HTTP response. The type of content depends on the
#' response from the server.
get_data <- function(url, headers = NULL, params = NULL) {
  res <- httr::GET(
    url = url,
    httr::add_headers(.headers = headers),
    query = params
  )

  if (httr::http_status(res)$category != "Success") {
    stop("Request failed with status: ", httr::http_status(res)$message)
  }

  data <- res$content %>%
    rawToChar() %>%
    jsonlite::fromJSON(simplifyVector = TRUE)

  return(data)
}

#' Get Data from a URL with no Parameters
#'
#' Makes an HTTP GET request to the specified URL and retrieves the content.
#'
#' @param url A character string specifying the URL to fetch data from.
#' @param headers A named character vector of HTTP headers to include in the
#' request. Default is `NULL`.
#' @return The content of the HTTP response. The type of content depends on the
#' response from the server.
get_data_no_params <- function(url, headers = NULL) {
  res <- httr::GET(
    url = url,
    httr::add_headers(.headers = headers)
  )

  if (httr::http_status(res)$category != "Success") {
    stop("Request failed with status: ", httr::http_status(res)$message)
  }

  data <- res$content %>%
    rawToChar() %>%
    jsonlite::fromJSON(simplifyVector = TRUE)

  return(data)
}

#' Generate NBA Player Headshot URL
#'
#' Generates the URL for a player's headshot based on their NBA player ID.
#'
#' @param player_id A numeric or character vector representing the player's ID.
#' @return A character vector containing the headshot URLs.
get_player_headshot <- function(player_id) {
  glue::glue(
    "https://cdn.nba.com/headshots/nba/latest/1040x760/{player_id}.png"
  )
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

#' Get Current Season
#'
#' This function returns the current season in the format "YYYY-YY".
#' The season is determined based on the current date:
#' - If the current date is before July 1st of the current year,
#' the season is "previous year - current year".
#' - If the current date is after or on July 1st,
#' the season is "current year - next year".
#'
#' @return A character string representing the current season.
get_current_season <- function() {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  current_date <- Sys.Date()

  # Logic to determine the season
  if (current_date < as.Date(paste(current_year, "-07-01", sep = ""))) {
    season <- paste(current_year - 1, "-", substr(current_year, 3, 4), sep = "")
  } else {
    season <- paste(current_year, "-", substr(current_year + 1, 3, 4), sep = "")
  }

  return(season)
}

#' Convert Time String to Seconds
#'
#' Converts a time string (in MM:SS format) into seconds.
#'
#' @param time_str A string in "MM:SS" format.
#' @return A numeric value representing the time in seconds.
convert_to_seconds <- function(time_str) {
  time_parts <- do.call(rbind, strsplit(time_str, ":"))
  minutes <- as.numeric(time_parts[, 1])
  seconds <- as.numeric(time_parts[, 2])
  return((minutes * 60) + seconds)
}

#' Convert Minutes to Minutes:Seconds
#'
#' Converts a time numeric into minutes and seconds (in MM:SS format).
#'
#' @param time_numeric A numeric value.
#' @return A string value in "MM:SS" format.
convert_to_minutes_seconds <- function(time_numeric) {
    minutes <- floor(time_numeric)
    seconds <- round((time_numeric - minutes) * 60)
    sprintf("%d:%02d", minutes, seconds)
}

#' Calculate Seconds Passed in Game
#'
#' Calculates the total seconds passed in the game based on time and period.
#'
#' @param time_str A string representing the time in the current period.
#' @param period An integer representing the period.
#' @return A numeric value representing the total seconds passed in the game.
seconds_passed <- function(time_str, period) {
  period_seconds <- 720
  time_remaining <- convert_to_seconds(time_str)
  secs_passed_current_period <- ifelse(period %in% 1:4,
    period_seconds - time_remaining,
    300 - time_remaining
  )
  seconds_in_previous_periods <- (as.numeric(period) - 1) * period_seconds
  return(seconds_in_previous_periods + secs_passed_current_period)
}

#' NBA Player Lookup
#'
#' This function retrieves a simplified player dictionary from the NBA API,
#' including player IDs and full names.
#'
#' @return A tibble with person_id and player_name.
nba_player_lookup <- function() {
  nba_player_dictionary() %>%
    mutate(player_name = paste(player_first_name,
      player_last_name,
      sep = " "
    )) %>%
    select(person_id, player_name)
}

#' Get Team Colors for Active NBA Teams
#'
#' Fetches and processes team color information for all active NBA teams
#'
#' @return A tibble with columns for team ID, abbreviation, city, name, and
#' primary/secondary/tertiary colors.
#' @export
nba_team_colors <- function() {

    url <- "https://stats.nba.com/js/data/ptsd/stats_ptsd.js"

    content <- tryCatch(
        {
            suppressWarnings(readLines(url))
        },
        error = function(e) {
            stop("Failed to retrieve data from the URL. Please check the URL or your connection.")
        }
    )

    json <- tryCatch(
        {
            jsonlite::fromJSON(
                gsub("var stats_ptsd =|;", "", paste(content, collapse = "\n")),
                flatten = TRUE,
                simplifyDataFrame = TRUE
            )
        },
        error = function(e) {
            stop("Failed to parse JSON data. Please check the structure of the content.")
        }
    )

    team_colors <- map_dfr(
        json$data$teams,
        ~ .x %>%
            set_names(c(
                "team_id",
                "team_abbreviation",
                "team_slug",
                "team_city",
                "team_name",
                "conference_id",
                "division_id",
                "is_non_nba_team",
                "end_year",
                "league_id",
                "team_color"
            )) %>%
            as_tibble()
    ) %>%
    filter(division_id != 0) %>%
    select(team_id, team_abbreviation, team_city, team_name, team_color) %>%
    group_by(team_id, team_abbreviation, team_city, team_name) %>%
    mutate(color_index = dplyr::row_number()) %>%
    pivot_wider(
        names_from = color_index,
        values_from = team_color,
        names_prefix = "team_color_"
    ) %>%
    rename(
        primary_color = team_color_1,
        secondary_color = team_color_2,
        tertiary_color = team_color_3
    ) %>%
    arrange(team_id)

    return(team_colors)
}


