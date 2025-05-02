#### NBA Player Profile Descriptive Data ----
#' Get NBA Player Profile Descriptive Information
#'
#' Retrieves and processes profile information for a single or multiple players
#'
#' @param player_id A single or vector of player IDs to fetch profile
#' information for.
#' @return A tibble containing the processed profile information for the
#' player(s).
#' @export
nba_profile_info <- function(player_id) {
  results <- map_dfr(player_id, function(id) {
    all_player_data <- fetch_profile_info(id)

    all_player_data %>%
      select(
        person_id, display_first_last, birthdate, school, country, height,
        weight, season_exp, jersey, position, team_id, team_name,
        team_abbreviation, team_city, from_year, to_year, draft_year,
        draft_round, draft_number
      ) %>%
      mutate(
        height_inches = convert_height_to_inches(height),
        height_meters = convert_inches_to_meters(height_inches),
        weight_kg = convert_weight_to_kg(weight),
        birthdate = as_date(birthdate),
        age = calculate_age(birthdate),
        draft = paste0(draft_year, " R", draft_round, " Pick ", draft_number),
        headshot_url = nba_player_headshot(id)
      )
  })

  return(results)
}

#' Fetch Player Profile Information from API
#'
#' This function fetches player profile info for given player ID's.
#'
#' @param player_id A numeric or character vector representing the player's ID
#' @return A data frame containing the raw player profile info.
fetch_profile_info <- function(player_id) {
  headers <- generate_headers_stats()

  all_data <- map_dfr(player_id, function(player) {
    url <- paste0(
      "https://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID=",
      player
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

# Conversion functions
#' Convert Height from Feet-Inches to Inches
#'
#' Converts a height given in the format "feet-inches" (e.g., "6-2") to total
#' inches.
#'
#' @param height A character string representing height in "feet-inches" format.
#' @return A numeric value representing height in total inches.
convert_height_to_inches <- function(height) {
  parts <- strsplit(height, "-")[[1]]
  feet <- as.numeric(parts[1])
  inches <- as.numeric(parts[2])
  total_inches <- (feet * 12) + inches
  return(total_inches)
}

#' Convert Height from Inches to Meters
#'
#' Converts a height in inches to meters.
#'
#' @param height_inches A numeric value representing height in inches.
#' @return A numeric value representing height in meters.
convert_inches_to_meters <- function(height_inches) {
  height_meters <- height_inches * 0.0254
  return(height_meters)
}

#' Convert Weight from Pounds to Kilograms
#'
#' Converts a weight in pounds (lbs) to kilograms (kg).
#'
#' @param weight_lbs A numeric value representing weight in pounds.
#' @return A numeric value representing weight in kilograms.
convert_weight_to_kg <- function(weight_lbs) {
  weight_lbs <- as.numeric(weight_lbs)
  weight_kg <- weight_lbs * 0.453592
  return(weight_kg)
}

#' Calculate Age from Birthdate
#'
#' Calculates the current age of a person based on their birthdate.
#'
#' @param birthdate A date string in the format "YYYY-MM-DD" or a Date object.
#' @return A numeric value representing the age in years.
calculate_age <- function(birthdate) {
  birthdate <- as.Date(birthdate)

  # Calculate age
  age <- floor(interval(start = birthdate, end = Sys.Date()) / years(1))
  return(age)
}


#### NBA Player Profile Data ----

#' Get Player Profile Stats
#'
#' This function gets the player's various stats by measure type and
#' aggregation method.
#'
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
#' @param player_id A numeric or character vector representing the player's ID
#' @param per_mode A character string representing the aggregation method
#' (default = "PerGame") Valid options include:
#' \itemize{
#'   \item \strong{"PerGame"} - Stats on per game basis.
#'   \item \strong{"Totals"} - Stat totals.
#' }
#' @return A list of tibbles with all profile stats
#' @export
nba_profile_stats <- function(season_type = "Regular Season",
                              player_id,
                              per_mode = "PerGame") {
  profile_stats_data <- fetch_player_profile_data(
    season_type,
    player_id,
    per_mode
  )

  profile_stats_list <- process_profile_data(profile_stats_data)

  return(profile_stats_list)
}

#' Fetch Player Profile Stats Data from API
#'
#' @param season_type A character string specifying the type of season
#' @param player_id A numeric or character vector representing the player's ID
#' @param per_mode A character string representing the aggregation method
#' @return A list containing the raw player profile stats data.
fetch_player_profile_data <- function(season_type,
                                      player_id,
                                      per_mode) {
  headers <- generate_headers_stats()
  params <- generate_params_profile(
    season_type,
    player_id,
    per_mode
  )

  url <- "https://stats.nba.com/stats/playerdashboardbyyearoveryearcombined"

  data <- get_data(url, headers, params)

  return(data)
}

#' Process Profile Stats List
#'
#' This function processes a nested list structure containing splits data.
#'
#' @param data A nested list structure with splits data.
#' @return A named list where each element is a tibble.
process_profile_data <- function(data) {
  # Define dashboard groups
  dashboard_groups <- list(
    base = c("ByYearBasePlayerDashboard"),
    advanced = c("ByYearAdvancedPlayerDashboard"),
    misc = c("ByYearMiscPlayerDashboard"),
    scoring = c("ByYearScoringPlayerDashboard"),
    usage = c("ByYearUsagePlayerDashboard")
  )

  # Process and combine stats by dashboard groups
  profile_stats <- imap(
    dashboard_groups,
    ~ map_dfr(
      imap(data$resultSets$rowSet, function(row_set, i) {
        column_names <- data$resultSets$headers[[i]] %>%
          as.character()

        row_set %>%
          data.frame(stringsAsFactors = FALSE) %>%
          as_tibble() %>%
          set_names(column_names) %>%
          clean_names() %>%
          clean_stats_cols()
      })[.x],
      ~.x,
      .id = "season_type"
    )
  )

  return(profile_stats)
}

#### NBA Player Splits Data ----

#' Get Player Splits
#'
#' This function gets the player's various splits by measure type and season.
#'
#' @param season A numeric value representing the season (e.g., 2024).
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
#' @param measure_type A character vector specifying the types of stats
#' (default = "Base"). Valid options include:
#' \itemize{
#'   \item \strong{"Base"} - Traditional stats.
#'   \item \strong{"Advanced"} - Advanced stats.
#'   \item \strong{"Usage"} - Usage stats.
#'   \item \strong{"Misc"} - Misc stats.
#'   \item \strong{"Scoring"} - Scoring stats.
#' }
#' @param player_id A numeric or character vector representing the player's ID
#' @param per_mode A character string representing the aggregation method
#' (default = "PerGame") Valid options include:
#' \itemize{
#'   \item \strong{"PerGame"} - Stats on per game basis.
#'   \item \strong{"Totals"} - Stat totals.
#' }
#' @return A tibble with all splits totals
#' @export
nba_career_splits <- function(season,
                              season_type = "Regular Season",
                              measure_type = "Base",
                              player_id,
                              per_mode = "PerGame") {
  results <- map_dfr(player_id, function(id) {
    all_player_data <- fetch_player_splits_data(
      season,
      season_type,
      measure_type,
      id,
      per_mode
    )

    all_player_data %>%
      process_splits_data() %>%
      mutate(player_id = as.character(id)) %>%
      left_join(nba_player_lookup(), by = c("player_id" = "person_id"))
  })

  return(results)
}

#' Fetch Player Splits Data from API
#'
#' @param season A numeric value representing the season (e.g., 2024).
#' @param season_type A character string specifying the type of season
#' @param measure_type A character string specifying the measure type
#' @param player_id A numeric or character vector representing the player's ID
#' @param per_mode A character string representing the aggregation method
#' @return A list containing the raw player career data.
fetch_player_splits_data <- function(season,
                                     season_type,
                                     measure_type,
                                     player_id,
                                     per_mode) {
  headers <- generate_headers_stats()
  params <- generate_params_splits(
    season,
    season_type,
    measure_type,
    player_id,
    per_mode
  )

  url <- "https://stats.nba.com/stats/playerdashboardbygeneralsplits"

  data <- get_data(url, headers, params)

  return(data)
}

#' Process Splits List
#'
#' This function processes a nested list structure containing splits data.
#'
#' @param data A nested list structure with splits data.
#' @return A named list where each element is a tibble.
process_splits_data <- function(data) {
  map_dfr(seq_along(data$resultSets$rowSet),
    function(i) {
      data$resultSets$rowSet[[i]] %>%
        data.frame(stringsAsFactors = FALSE) %>%
        as_tibble() %>%
        set_names(as.character(data$resultSets$headers[[i]])) %>%
        clean_names() %>%
        clean_stats_cols()
    },
    .id = "name"
  ) %>%
    mutate(name = data$resultSets$name[as.numeric(name)])
}

#### NBA Player Career Data ----

#' Get Player Highs
#'
#' This function gets the player's current season and career highs.
#'
#' @param player_id A numeric or character vector representing the player's ID
#' @return A tibble with season and career highs.
#' @export
nba_player_highs <- function(player_id) {
  results <- map_dfr(player_id, function(id) {
    career_data <- fetch_player_career_data(id, "PerGame")
    player_career_list <- process_career_data(career_data)

    map_dfr(
      player_career_list[c("SeasonHighs", "CareerHighs")],
      ~.x,
      .id = "season_type"
    )
  })

  results <- results %>%
    left_join(nba_player_lookup(), by = c("player_id" = "person_id"))

  return(results)
}

#' Get Player Career Totals
#'
#' This function gets the player's regular season, post-season, all-star, and
#' preseason career totals.
#'
#' @param player_id A numeric or character vector representing the player's ID
#' @param per_mode A character string representing the aggregation method
#' @return A tibble with all career totals
#' @export
nba_career_stats <- function(player_id, per_mode = "PerGame") {
  results <- map_dfr(player_id, function(id) {
    career_data <- fetch_player_career_data(id, per_mode)
    player_career_list <- process_career_data(career_data)

    map_dfr(
      player_career_list[c(
        "SeasonTotalsRegularSeason",
        "CareerTotalsRegularSeason",
        "SeasonTotalsPostSeason",
        "CareerTotalsPostSeason",
        "SeasonTotalsAllStarSeason",
        "CareerTotalsAllStarSeason",
        "SeasonTotalsPreseason",
        "CareerTotalsPreseason"
      )],
      ~.x,
      .id = "season_type"
    )
  })

  results <- results %>%
    left_join(nba_player_lookup(), by = c("player_id" = "person_id"))

  return(results)
}

#' Get Player Career Rankings
#'
#' This function gets the player's season rankings for regular and
#' post-season.
#'
#' @param player_id A numeric or character vector representing the player's ID
#' @param per_mode A character string representing the aggregation method
#' @return A tibble with all season rankings
#' @export
nba_career_rankings <- function(player_id, per_mode = "PerGame") {
  results <- map_dfr(player_id, function(id) {
    career_data <- fetch_player_career_data(id, per_mode)
    player_career_list <- process_career_data(career_data)

    map_dfr(
      player_career_list[c(
        "SeasonRankingsRegularSeason",
        "SeasonRankingsPostSeason"
      )],
      ~.x,
      .id = "season_type"
    )
  })

  results <- results %>%
    left_join(nba_player_lookup(), by = c("player_id" = "person_id"))

  return(results)
}

#' Fetch Player Career Data from API
#'
#' @param player_id A numeric or character vector representing the player's ID
#' @param per_mode A character string representing the aggregation method
#' @return A list containing the raw player career data.
fetch_player_career_data <- function(player_id, per_mode) {
  headers <- generate_headers_stats()
  params <- generate_params_career(player_id, per_mode)

  url <- "https://stats.nba.com/stats/playerprofilev2"

  data <- get_data(url, headers, params)

  return(data)
}

#' Process Career List
#'
#' This function processes a nested list structure containing career data.
#'
#' @param data A nested list structure with career data.
#' @return A named list where each element is a tibble.
process_career_data <- function(data) {
  profile_career_list <- list()

  for (i in seq_along(data$resultSets$rowSet)) {
    column_names <- data$resultSets$headers[[i]] %>%
      as.character()

    dt <- data$resultSets$rowSet[[i]]

    if (length(dt) > 0) {
      dt <- dt %>%
        data.frame(stringsAsFactors = FALSE) %>%
        as_tibble() %>%
        set_names(column_names) %>%
        clean_names() %>%
        clean_stats_cols()

      profile_career_list[[as.character(data$resultSets$name[i])]] <- dt
    }
  }

  return(profile_career_list)
}

#### NBA Player Awards Data ----

#' Get NBA Player Awards History
#'
#' Gets the award history for a player based on their NBA player ID.
#'
#' @param player_id A numeric or character vector representing the player's ID.
#' @return A character vector containing the headshot URLs.
#' @export
nba_player_awards <- function(player_id) {
  results <- map_dfr(player_id, function(id) {
    fetch_player_awards_data(id)
  })

  return(results)
}

#' Fetch Player Awards from API
#'
#' @param player_id A numeric or character vector representing the player's ID
#' @return A list containing the raw player awards data.
fetch_player_awards_data <- function(player_id) {
  headers <- generate_headers_stats()
  params <- generate_params_career(player_id)

  url <- "https://stats.nba.com/stats/playerawards"

  data <- get_data(url, headers, params)

  column_names <- data$resultSets$headers[[1]] %>%
    as.character()

  dt <- data$resultSets$rowSet[[1]] %>%
    data.frame(stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    set_names(column_names) %>%
    clean_names()

  return(dt)
}
