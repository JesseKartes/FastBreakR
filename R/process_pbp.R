#' Process Play-by-Play Data for Lineups and Possessions
#'
#' This function processes raw play-by-play (pbp) data, performs corrections to
#' remove errors, adds missing player information, and generates lineups for
#' each play-by-play event. It also marks the end of each possession and assigns
#' the possessing team. The resulting data can be used to analyze player
#' performance and lineup efficiency.
#'
#' @param data A data frame containing raw play-by-play data. This should
#'             include information on each play event (e.g., players involved,
#'             teams, event type, time, etc.).
#' @return A data frame containing:
#'         - Corrected play-by-play data
#'         - Four lineup columns:
#'           - Two columns for calculating player statistics
#'           - Two columns for calculating lineup playtime
#'         - Possession end markers and team ownership of each possession.
#'
#' @details The function performs the following steps:
#'
#' 1. **Data Corrections**: It applies necessary corrections to the raw
#'    play-by-play data, including removing errors (e.g., missing player data)
#'    and adding missing players based on the available context.
#'
#' 2. **Lineup Creation**: The function creates four lineup-related columns:
#'    - Two columns (`lineup_home`, `lineup_away`) are used for calculating
#'    player stats
#'    - Two columns (`lineup_home_pt`, `lineup_away_pt`) are used for
#'    calculating lineup playtime based on when players are on and off the
#'    floor.
#'
#' 3. **Possession Tracking**: It tracks the end of each possession, marking
#'    when the possession ends and assigning the team that currently possesses
#'    the ball. This is done by analyzing the sequence of events and
#'    determining when a team gains control of the ball.
#'
#' 4. **Output**: The resulting data frame includes the processed play-by-play
#'    events, lineup information, and possession data.
#'
#' @export
pbp_lineups <- function(data) {
  # Apply corrections to the play-by-play data
  pbp_final <- tryCatch(
    {
      data <- data %>%
        make_pbp_corrections() %>%
        add_event_info() %>%
        add_team_names()

      # Track substitutions made during the game
      subs_made <- subs_in_quarter(data)

      # Identify players involved in events and track their presence in quarters
      other_players <- players_in_events(data, subs_made)

      # Combine substitutions and players into a comprehensive lineup data frame
      quarter_start_lineups <- combine_and_add_players(subs_made, other_players)

      # Identify field goals (FG) and "and-one" events
      fgs_data <- identify_fg_and_one(data)

      # Track substitutions and lineups during the game
      all_subs <- track_subs_and_lineups(data, quarter_start_lineups)

      # Track play-by-play lineups
      pbp_lineups <- track_pbp_lineups(
        data,
        quarter_start_lineups,
        all_subs,
        fgs_data
      )

      # Add points to the play-by-play data
      pbp_lineups <- add_pbp_points(pbp_lineups)

      # Add possession data to the play-by-play data
      pbp_final <- add_pbp_possessions(pbp_lineups)

      # Return the final data after processing
      return(pbp_final)
    },
    error = function(e) {
      message("Error during data processing ", e$message)
      return(data) # Return the original data if this step fails
    }
  )
}

#' Play-by-Play Corrections
#'
#' This function handles data corrections in play-by-play data.
#'
#' @param data The raw play-by-play data.
#' @return A data frame with corrections made to events and descriptions
make_pbp_corrections <- function(data) {
  # Load corrections data from sysdata.rda
  corrections_file <- system.file("internal", "sysdata.rda",
    package = "FastBreakR"
  )

  # Using tryCatch to handle potential loading errors
  load_status <- tryCatch(
    {
      load(corrections_file)
      TRUE
    },
    error = function(e) {
      message("Error loading corrections file: ", e$message)
      return(FALSE) # Return FALSE if loading fails
    }
  )

  if (!load_status) {
    return(data) # Return the original data if loading fails
  }

  data <- data %>%
    left_join(corrections$home_corrections, by = c("game_id", "eventnum")) %>%
    mutate(
      eventmsgtype = coalesce(eventmsgtype.y, eventmsgtype.x),
      eventmsgactiontype = coalesce(
        eventmsgactiontype.y,
        eventmsgactiontype.x
      ),
      homedescription = coalesce(homedescription.y, homedescription.x)
    ) %>%
    select(-ends_with(".y"), -ends_with(".x")) %>%
    left_join(corrections$visitor_corrections,
      by = c("game_id", "eventnum")
    ) %>%
    mutate(
      eventmsgtype = coalesce(eventmsgtype.y, eventmsgtype.x),
      eventmsgactiontype = coalesce(
        eventmsgactiontype.y,
        eventmsgactiontype.x
      ),
      visitordescription = coalesce(
        visitordescription.y,
        visitordescription.x
      )
    ) %>%
    select(-ends_with(".y"), -ends_with(".x")) %>%
    anti_join(corrections$delete_rows, by = c("game_id", "eventnum"))

  return(data)
}

#' Add Season, Location, Time, and Event Index to Play-by-Play
#'
#' This function processes a given dataset by adding season year, team location,
#' time, and an event index.
#'
#' @param data A data frame containing game data with the following columns:
#' @return A data frame with additional columns for season year, team location,
#' and event index.
add_event_info <- function(data) {
  data <- data %>%
    distinct() %>%
    filter(eventmsgtype != 18) %>%
    mutate(
      # Calculate season year based on game_id
      season_year = if_else(
        as.integer(substr(game_id, 4, 5)) > 90,
        1900 + as.integer(substr(game_id, 4, 5)) + 1,
        2000 + as.integer(substr(game_id, 4, 5)) + 1
      ),
      # Determine team location based on player type
      team_location = case_when(
        person1type == "4" ~ "home", # home player
        person1type == "5" ~ "away", # away player
        person1type == "2" ~ "home", # home team
        person1type == "3" ~ "away", # away team
        TRUE ~ NA_character_
      ),
      # Calculate seconds left in quarter and seconds passed in the game
      secs_left_quarter = convert_to_seconds(pctimestring),
      secs_passed_quarter = if_else(period %in% 1:4,
        720 - secs_left_quarter,
        300 - secs_left_quarter
      ),
      secs_passed_game = seconds_passed(pctimestring, period)
    ) %>%
    arrange(game_id, secs_passed_game) %>%
    group_by(game_id) %>%
    mutate(
      # Event index based on the game
      event_index = row_number()
    ) %>%
    ungroup() %>%
    select(
      -c(
        ends_with("_city"), ends_with("_nickname"),
        wctimestring, score, scoremargin, video_available_flag
      )
    )

  return(data)
}

#' Add Team Names to Play-by-Play
#'
#' This function adds home and away team names to the provided data.
#'
#' @param data A data frame containing game event data.
#' @return A data frame with the original data and two new columns:
#' `home_team_name` and `away_team_name`.
add_team_names <- function(data) {
  # Extract home and away team names
  home_teams <- data %>%
    filter(eventmsgtype == 10, eventmsgactiontype == 0, period == 1) %>%
    select(game_id, home_team_name = player1_team_abbreviation) %>%
    distinct()

  away_teams <- data %>%
    filter(eventmsgtype == 10, eventmsgactiontype == 0, period == 1) %>%
    select(game_id, away_team_name = player2_team_abbreviation) %>%
    distinct()

  # Join home and away teams
  data <- data %>%
    left_join(home_teams, by = "game_id") %>%
    left_join(away_teams, by = "game_id")

  return(data)
}

#' Track Player Substitutions in Each Quarter
#'
#' This function processes the input data to identify player substitutions
#' to show who was substituted in and out during each quarter.

#' @param data A data frame containing game event data with substitution events.
#' @return A data frame with the players subbed during a quarter
subs_in_quarter <- function(data) {
  # Subs in quarter
  subs_made <- data %>%
    filter(eventmsgtype == 8) %>%
    select(
      game_id, period, secs_passed_game, team_location,
      team_player = player1_team_abbreviation,
      player_out = player1_name,
      player_in = player2_name
    ) %>%
    pivot_longer(
      cols = starts_with("player_"),
      names_to = "sub",
      names_prefix = "player_",
      values_to = "name_player"
    ) %>%
    group_by(game_id, period, team_player, name_player) %>%
    filter(row_number() == 1) %>%
    ungroup()

  return(subs_made)
}

#' Track Players Involved in Events But Not Substitutions
#'
#' This function processes the input data to identify players involved in
#' events (excluding substitutions) and ensures that only players who are not
#' substituted are included in the final output.
#'
#' @param data A data frame containing game event data.
#' @param subs_made A data frame containing subs made
#' @return A data frame with players involved in events but not subbed
players_in_events <- function(data, subs_made) {
  other_players <- data %>%
    filter(
      eventmsgtype != 8,
      !(eventmsgtype == 6 & eventmsgactiontype %in% c(
        10, 11, 16,
        18, 25
      )),
      !(eventmsgtype == 11 & eventmsgactiontype %in% c(1, 4))
    ) %>%
    pivot_longer(
      cols = c(player1_name, player2_name, player3_name),
      names_to = "player_number",
      names_prefix = "name_player",
      values_to = "name_player"
    ) %>%
    mutate(
      team_player = case_when(
        player_number == "player1_name" ~ player1_team_abbreviation,
        player_number == "player2_name" ~ player2_team_abbreviation,
        player_number == "player3_name" ~ player3_team_abbreviation,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(name_player), !is.na(team_player)) %>%
    anti_join(
      subs_made %>%
        select(game_id, period, team_player, name_player),
      by = c("game_id", "period", "team_player", "name_player")
    ) %>%
    distinct(game_id, period, name_player, team_player) %>%
    left_join(
      subs_made %>%
        select(game_id, team_player, team_location) %>%
        distinct(),
      by = c("game_id", "team_player")
    )

  return(other_players)
}

#' Combine and Add Missing Players
#'
#' This function combines player data from substitutions and other events,
#' then adds missing player data to the resulting data frame.
#'
#' @param subs_made A data frame containing substitution event data
#' @param other_players A data frame containing players involved in events
#' but not substitutions.
#'
#' @return A data frame with the lineups to start each quarter
combine_and_add_players <- function(subs_made, other_players) {
  # Load corrections data from sysdata.rda
  corrections_file <- system.file("internal", "sysdata.rda",
    package = "FastBreakR"
  )

  # Using tryCatch to handle potential loading errors
  load_status <- tryCatch(
    {
      load(corrections_file)
      TRUE
    },
    error = function(e) {
      message("Error loading corrections file: ", e$message)
      return(FALSE) # Return FALSE if loading fails
    }
  )

  if (!load_status) {
    return(data) # Return the original data if loading fails
  }

  # Combine players
  quarter_start_lineups <- bind_rows(
    subs_made %>% filter(sub == "out"),
    other_players
  ) %>%
    arrange(game_id, period, team_player)

  # Add missing players
  quarter_start_lineups <- bind_rows(
    corrections$missing_starters %>%
      mutate(period = as.character(period)) %>%
      left_join(
        subs_made %>%
          distinct(game_id, team_player, team_location),
        by = c("game_id", "team_player")
      ),
    quarter_start_lineups
  ) %>%
    filter(
      !(game_id == "0022200140" &
        name_player == "Dennis Schroder" & period == 4)
    )

  return(quarter_start_lineups)
}

#' Identify Field Goals of an And-One
#'
#' This function identifies field goals made as part of an "and-one".
#'
#' @param data A data frame with raw play-by-play data
#' @return A data frame with the only the fgs of an and-one.
identify_fg_and_one <- function(data) {
  fgs_data <- data %>%
    mutate(
      possession = case_when(
        eventmsgtype %in% c(1, 2, 5) ~ 1,
        eventmsgtype == 3 & eventmsgactiontype %in% c(
          10, 12, 15,
          19, 20, 29
        ) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    filter(
      eventmsgtype == 1 |
        (eventmsgtype == 6 & !eventmsgactiontype %in% c(
          4, 10, 11,
          12, 16, 18
        )) |
        (eventmsgtype == 3 & eventmsgactiontype == 10)
    ) %>%
    group_by(game_id, secs_passed_game) %>%
    filter(
      eventmsgtype == 1 &
        lead(eventmsgtype) == 6 &
        player1_team_id != lead(player1_team_id)
    ) %>%
    ungroup() %>%
    mutate(fg_and_one = 1)

  return(fgs_data)
}

#' Track Player Substitutions and Lineups
#'
#' This function processes play-by-play data to track player substitutions
#' and the lineups before and after each substitution. It provides a breakdown
#' of player substitutions by game and period, along with the updated lineups
#' for both home and away teams.
#'
#' @param data A data frame with raw play-by-play data
#' @param quarter_start_lineups A data frame with lineups to begin each quarter
#' @return A data frame with all subs made
track_subs_and_lineups <- function(data, quarter_start_lineups) {
  # All subs
  all_subs <- data %>%
    filter(eventmsgtype == 8) %>%
    select(
      game_id, period, event_index, team_location,
      team_player = player1_team_abbreviation,
      player_out = player1_name,
      player_in = player2_name
    ) %>%
    left_join(
      quarter_start_lineups %>%
        group_by(game_id, period, team_player) %>%
        summarise(
          lineup_initial = paste(sort(unique(name_player)),
            collapse = ", "
          ),
          .groups = "drop"
        ),
      by = c("game_id", "period", "team_player")
    ) %>%
    mutate(lineup_initial = str_split(lineup_initial, ", ")) %>%
    group_by(game_id, period, team_player) %>%
    mutate(
      lineup_after = accumulate2(
        .x = player_in,
        .y = player_out,
        .f = ~ setdiff(c(..1, ..2), ..3),
        .init = lineup_initial[[1]]
      )[-1],
      lineup_before = coalesce(lineup_initial, lag(lineup_after))
    ) %>%
    ungroup() %>%
    mutate(across(
      c(lineup_initial, lineup_after),
      ~ map_chr(.x, ~ paste(.x, collapse = ", "))
    )) %>%
    mutate(
      lineup_before_home = ifelse(team_location == "home",
        lineup_initial, NA
      ),
      lineup_after_home = ifelse(team_location == "home",
        lineup_after, NA
      ),
      lineup_before_away = ifelse(team_location == "away",
        lineup_initial, NA
      ),
      lineup_after_away = ifelse(team_location == "away",
        lineup_after, NA
      )
    ) %>%
    select(game_id, event_index, contains("home"), contains("away"))

  return(all_subs)
}

#' Process Play-by-Play Data to Track Lineups for Each Play
#'
#' This function processes play-by-play data to track lineups. It adds details
#' about the starting lineups, substitutions, and final lineups for each
#' game period. The function also identifies free throw sequences and
#' tracks the players involved in those events.
#'
#' The resulting data frame includes four additional columns:
#' \describe{
#'   \item{\code{lineup_away_pt}}{Tracks the total playtime for away lineups.}
#'   \item{\code{lineup_home_pt}}{Tracks the total playtime for home lineups.}
#'   \item{\code{lineup_away}}{Tracks the away lineup for calculating stats.}
#'   \item{\code{lineup_home}}{Tracks the home lineup for calculating stats.}
#' }
#'
#' @param data A data frame with raw play-by-play data
#' @param quarter_start_lineups A data frame with lineups to begin each quarter
#' @param all_subs A data frame with all subs made
#' @param fgs_data A data frame with fgs before and-one
#' @return A data frame with the lineups for all play-by-play data
track_pbp_lineups <- function(data, quarter_start_lineups, all_subs, fgs_data) {
  # Final lineups
  pbp_lineups <- data %>%
    left_join(
      quarter_start_lineups %>% # adds starters at beginning of quarter
        group_by(game_id, period, team_location) %>%
        summarize(
          lineup_initial = paste(sort(unique(name_player)),
            collapse = ", "
          ),
          .groups = "drop"
        ) %>%
        pivot_wider(
          names_from = team_location,
          names_prefix = "lineup_initial_",
          values_from = lineup_initial
        ) %>%
        mutate(eventmsgtype = "12"),
      by = c("game_id", "period", "eventmsgtype"),
    ) %>%
    left_join(
      all_subs,
      by = c("game_id", "event_index") # adds subs in quarter
    ) %>%
    mutate(
      lineup_before_home = coalesce(
        lineup_before_home,
        lineup_initial_home
      ),
      lineup_after_home = coalesce(
        lineup_after_home,
        lineup_initial_home
      ),
      lineup_before_away = coalesce(
        lineup_before_away,
        lineup_initial_away
      ),
      lineup_after_away = coalesce(
        lineup_after_away,
        lineup_initial_away
      )
    ) %>%
    select(-starts_with("lineup_initial")) %>%
    group_by(eventmsgtype != 9) %>%
    mutate(
      across(lineup_before_home:lineup_after_away, ~ {
        last_na <- lead(is.na(.), default = TRUE)
        replace(., !last_na & !is.na(.), NA)
      })
    ) %>%
    ungroup() %>%
    select(-"eventmsgtype != 9") %>%
    group_by(game_id, period) %>%
    mutate(
      lineup_home = zoo::na.locf(lineup_after_home, na.rm = FALSE),
      lineup_away = zoo::na.locf(lineup_after_away, na.rm = FALSE),
      lineup_home = coalesce(
        lineup_home,
        zoo::na.locf(lineup_before_home,
          fromLast = TRUE,
          na.rm = FALSE
        )
      ),
      lineup_away = coalesce(
        lineup_away,
        zoo::na.locf(lineup_before_away,
          fromLast = TRUE,
          na.rm = FALSE
        )
      ),
      lineup_home = str_split(lineup_home, ", ") %>%
        map_chr(~ paste(sort(.), collapse = ", ")),
      lineup_away = str_split(lineup_away, ", ") %>%
        map_chr(~ paste(sort(.), collapse = ", "))
    ) %>%
    ungroup() %>%
    select(-starts_with("lineup_before"), -starts_with("lineup_after")) %>%
    left_join(fgs_data %>% select(game_id, eventnum, fg_and_one),
      by = c("game_id", "eventnum")
    ) %>%
    mutate(
      lineup_home_pt = lineup_home,
      lineup_away_pt = lineup_away
    ) %>%
    group_by(game_id) %>%
    mutate(
      ft_start_id = if_else(
        (eventmsgtype == 3 & eventmsgactiontype %in% c(
          11, 13, 18,
          21, 25, 27
        )) |
          fg_and_one == 1,
        row_number(),
        NA_integer_
      )
    ) %>%
    ungroup() %>%
    mutate(
      ft_start_id = if_else(
        !is.na(ft_start_id),
        cumsum(!is.na(ft_start_id)),
        NA_integer_
      )
    ) %>%
    group_by(game_id) %>%
    mutate(
      ft_end_id = if_else(
        eventmsgtype == 3 & eventmsgactiontype %in% c(
          12, 15, 19, 22,
          26, 29, 10, 20
        ),
        row_number(),
        NA_integer_
      )
    ) %>%
    ungroup() %>%
    mutate(
      ft_end_id = if_else(
        !is.na(ft_end_id),
        cumsum(!is.na(ft_end_id)),
        NA_integer_
      )
    ) %>%
    group_by(game_id) %>%
    mutate(
      ft_id = ft_start_id,
      group_id = cumsum(!is.na(ft_start_id))
    ) %>%
    group_by(game_id, group_id) %>%
    mutate(
      ft_id = if_else(
        is.na(ft_id) & group_id > 0 & row_number() <=
          which.max(!is.na(ft_end_id)),
        first(ft_start_id[!is.na(ft_start_id)]),
        ft_id
      )
    ) %>%
    ungroup() %>%
    group_by(game_id, ft_id) %>%
    mutate(
      lineup_away = if_else(!is.na(ft_id),
        first(lineup_away[!is.na(lineup_away) & !is.na(ft_id)]),
        lineup_away
      ),
      lineup_home = if_else(!is.na(ft_id),
        first(lineup_home[!is.na(lineup_home) & !is.na(ft_id)]),
        lineup_home
      )
    ) %>%
    ungroup() %>%
    select(-c(group_id, ft_start_id, ft_end_id, ft_id))

  return(pbp_lineups)
}

#' Add Points to Play-by-Play Data
#'
#' This function processes play-by-play data to add shot points and cumulative
#' points to lineup data.
#'
#' ' The resulting data frame includes four additional columns:
#' \describe{
#'   \item{\code{shot_pts_away}}{Tracks the away team points in a single
#'   possession.}
#'   \item{\code{shot_pts_home}}{Tracks the home team points in a single
#'   possession.}
#'   \item{\code{pts_away}}{Tracks the away team cumulative points.}
#'   \item{\code{pts_home}}{Tracks the home team cumulative points.}
#' }
#'
#' @param data A data frame with raw play-by-play data
#' @return A data frame with points added to lineups for all play-by-play data
add_pbp_points <- function(data) {
  data <- data %>%
    mutate(
      shot_pts_home = case_when(
        eventmsgtype == 3 & !str_detect(homedescription, "MISS") ~ 1,
        eventmsgtype == 1 & str_detect(homedescription, "3PT") ~ 3,
        eventmsgtype == 1 & !str_detect(homedescription, "3PT") ~ 2,
        TRUE ~ 0
      ),
      shot_pts_away = case_when(
        eventmsgtype == 3 & !str_detect(visitordescription, "MISS") ~ 1,
        eventmsgtype == 1 & str_detect(visitordescription, "3PT") ~ 3,
        eventmsgtype == 1 & !str_detect(visitordescription, "3PT") ~ 2,
        TRUE ~ 0
      )
    ) %>%
    group_by(game_id) %>%
    mutate(
      pts_home = cumsum(shot_pts_home),
      pts_away = cumsum(shot_pts_away)
    ) %>%
    ungroup()

  return(data)
}


#' Add Possessions to Play-by-Play Data
#'
#' This function processes play-by-play data to add possessions to lineup data.
#'
#' The resulting data frame includes four additional columns:
#' \describe{
#'   \item{\code{poss}}{Tracks the end of the possession.}
#'   \item{\code{start_poss}}{Tracks the time remaining in the quarter when the
#'   possession started.}
#'   \item{\code{poss_team}}{Tracks the possessing team.}
#'   \item{\code{heave}}{Tracks end of quarter heaves.}
#' }
#'
#' @param data A data frame with raw play-by-play data
#' @return A data frame with possessions added to lineups for all play-by-play
add_pbp_possessions <- function(data) {
  possession_base <- data %>%
    mutate(
      possession = case_when(
        fg_and_one == 1 ~ 0,
        eventmsgtype %in% c(1, 2, 5) ~ 1,
        eventmsgtype == 3 & eventmsgactiontype %in% c(
          10, 12, 15, 19,
          20, 29
        ) ~ 1,
        TRUE ~ 0
      )
    )

  # Identify and change consecutive possessions
  possession_consecutive <- possession_base %>%
    distinct() %>%
    filter(
      possession == 1 |
        (eventmsgtype == 6 & eventmsgactiontype == 30)
    ) %>%
    group_by(game_id, period) %>%
    filter(
      possession == lead(possession) &
        (player1_team_id == lead(player1_team_id) |
          player1_team_id == lead(player1_id)) &
        !(eventmsgtype == 3 & eventmsgactiontype %in% c(19, 20, 29))
    ) %>%
    ungroup() %>%
    mutate(possession = 0)

  # Remove consecutive possessions and identify heaves
  possession_final <- possession_base %>%
    left_join(possession_consecutive %>% select(game_id, eventnum, possession),
      by = c("game_id", "eventnum")
    ) %>%
    mutate(possession = coalesce(possession.y, possession.x)) %>%
    select(-possession.y, -possession.x, -fg_and_one) %>%
    mutate(
      start_poss = case_when(
        possession == 1 & eventmsgtype != 4 ~ lag(secs_left_quarter)
      ),
      poss_team = case_when(
        possession == 1 &
          team_location == "home" ~ "home",
        possession == 1 &
          team_location == "away" ~ "away"
      ),
      heave = ifelse(
        eventmsgtype %in% c(2, 5) &
          possession == 1 &
          start_poss <= 2 &
          (lead(shot_pts_home) + lead(shot_pts_away) == 0),
        1, 0
      ),
      possession = ifelse(heave == 1, 0, possession)
    )

  return(possession_final)
}

#' Calculate Play-by-Play Box Scores for Multiple Games
#'
#' This function processes play-by-play box scores for multiple games.
#' IMPORTANT: This function can only be used after \code{pbp_lineups()} has
#' been applied to the play-by-play data to add lineup columns.
#'
#' @param game_df A data frame of play-by-play data processed with
#' \code{pbp_lineups()}
#' @return A tibble containing box scores for all lineups across multiple games
#' @export
pbp_box_scores <- function(game_df) {
  # Check if lineups have been processed
  if (!"lineup_home_pt" %in% names(game_df)) {
    stop("Error: pbp_lineups() must be run on the data frame before calling
             pbp_box_scores()")
  }

  # Split the dataframe by game_id and process each game
  final_results <- game_df %>%
    split(.$game_id) %>%
    map(function(single_game_df) {
      # Calculate lineup playtime
      lineup_playtime <- calculate_lineup_playtime(single_game_df)

      # Calculate lineup box score
      lineup_box_score <- calculate_lineup_stats(
        single_game_df,
        lineup_playtime
      )

      # Calculate advanced box score
      advanced_box_score <- calculate_advanced_stats(lineup_box_score)

      # Combine the two
      final_box_score <- lineup_playtime %>%
        left_join(
          advanced_box_score,
          by = c(
            "game_id", "lineup", "period", "team_name",
            "team_location"
          )
        ) %>%
        arrange(team_location, period, desc(total_minutes)) %>%
        group_by(team_name, period) %>%
        mutate(lineup_index = row_number()) %>%
        ungroup()

      return(final_box_score)
    }) %>%
    bind_rows()

  return(final_results)
}

#' Calculate Initial Lineup Playtime
#'
#' This function creates the initial playtime dataframe that calculates the
#' playtime for each lineup.
#'
#' @param data A data frame containing game event data, including lineup
#' information, periods, and time.
#' @param location A string indicating the location ("home" or "away") to
#' specify which team's lineup to calculate.
#' @return A tibble with the initial dataframe containing playtime data.
initial_playtime <- function(data, location) {
  lineup_col <- paste0("lineup_", location, "_pt")
  team_col <- paste0(location, "_team_name")

  last_secs <- tail(data$secs_passed_game, 1)
  last_lineup <- tail(data[[lineup_col]], 1)

  data %>%
    mutate(
      prev_lineup = lag(!!sym(lineup_col), default = ""),
      lineup_changed = !!sym(lineup_col) != prev_lineup
    ) %>%
    filter(lineup_changed) %>%
    select(
      secs_passed_game = !!sym("secs_passed_game"),
      lineup = !!sym(lineup_col),
      period,
      game_id
    ) %>%
    bind_rows(
      tibble(
        secs_passed_game = last_secs,
        lineup = last_lineup
      )
    ) %>%
    arrange(secs_passed_game) %>%
    mutate(
      time_played = lead(secs_passed_game,
        default = last_secs
      ) - secs_passed_game
    ) %>%
    filter(time_played != 0) %>%
    group_by(game_id, period) %>%
    mutate(lineup_index = cumsum(lineup != lag(lineup, default = first(lineup))
                                 | row_number() == 1)) %>%
    ungroup() %>%
    mutate(team_name = head(data[[team_col]], 1))
}

#' Calculate Lineup Playtime
#'
#' This function calculates the playtime for each lineup to be used in
#' play-by-play box scores.
#'
#' @param game_df A data frame containing play-by-play data with lineups.
#' @return A tibble containing playtime data.
calculate_lineup_playtime <- function(game_df) {
  home_lineups <- initial_playtime(game_df, "home") %>%
    mutate(team_location = "home")

  away_lineups <- initial_playtime(game_df, "away") %>%
    mutate(team_location = "away")

  lineup_playtime <- bind_rows(home_lineups, away_lineups) %>%
    filter(!is.na(period)) %>%
    mutate(
      starter = if_else(lineup_index == 1, 1, 0)
    ) %>%
    group_by(game_id, team_location, team_name, lineup, period, starter) %>%
    summarize(
      total_seconds = sum(time_played, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(team_location) %>%
    mutate(
      total_minutes = total_seconds / 60,
      minutes_played = convert_to_minutes_seconds(total_minutes)
    ) %>%
    select(
      game_id, team_location, period, lineup, team_name,
      total_minutes, minutes_played, starter
    )

  return(lineup_playtime)
}

#' Calculate Lineup Base Stats
#'
#' This function calculates the base stats for each lineup to be used in
#' calculating play-by-play box scores.
#'
#' @param game_df A data frame containing play-by-play data with lineups.
#' @param lineup_playtime A data frame containing lineups playtime.
#' @return A tibble containing base stats data.
calculate_lineup_stats <- function(game_df, lineup_playtime) {
  lineup_stats <- game_df %>%
    mutate(
      lineup = case_when(
        team_location == "away" ~ lineup_away,
        team_location == "home" ~ lineup_home,
        TRUE ~ NA_character_
      ),
      opp_lineup = case_when(
        team_location == "away" ~ lineup_home,
        team_location == "home" ~ lineup_away,
        TRUE ~ NA_character_
      ),
      lineup_pts = case_when(
        team_location == "away" ~ shot_pts_away,
        team_location == "home" ~ shot_pts_home,
        TRUE ~ NA_integer_
      ),
      team_name = case_when(
        team_location == "away" ~ away_team_name,
        team_location == "home" ~ home_team_name,
        TRUE ~ NA_character_
      ),
      fga = if_else(eventmsgtype %in% c(1, 2), 1, NA_integer_),
      fgm = if_else(eventmsgtype == 1, 1, NA_integer_),
      fg3a = if_else(eventmsgtype %in% c(1, 2) &
        (str_detect(homedescription, "3PT") |
          str_detect(visitordescription, "3PT")),
      1, NA_integer_
      ),
      fg3m = if_else(eventmsgtype == 1 &
        (str_detect(homedescription, "3PT") |
          str_detect(visitordescription, "3PT")),
      1, NA_integer_
      ),
      fta = if_else(eventmsgtype == 3, 1, NA_integer_),
      ftm = if_else(eventmsgtype == 3 &
        (!str_detect(homedescription, "MISS") |
          !str_detect(visitordescription, "MISS")),
      1, NA_integer_
      ),
      oreb = if_else(eventmsgtype == 4 &
        player1_team_id == lag(player1_team_id),
      1, NA_integer_
      ),
      dreb = if_else(eventmsgtype == 4 &
        player1_team_id != lag(player1_team_id),
      1, NA_integer_
      ),
      reb = if_else(eventmsgtype == 4, 1, NA_integer_),
      ast = if_else(eventmsgtype == 1 & person2type != 0, 1, NA_integer_),
      stl = if_else(eventmsgtype == 5 &
        eventmsgactiontype %in% c(1, 2), 1, NA_integer_),
      blk = if_else(eventmsgtype == 2, 1, NA_integer_),
      tov = if_else(eventmsgtype == 5, 1, NA_integer_),
      foul = if_else(eventmsgtype == 6, 1, NA_integer_),
      poss = if_else(possession == 1, 1, NA_integer_)
    ) %>%
    filter(lineup %in% lineup_playtime$lineup &
      period %in% lineup_playtime$period) %>%
    group_by(
      game_id,
      team_location,
      period,
      lineup,
      opp_lineup,
      team_name
    ) %>%
    summarize(
      fgm = sum(fgm, na.rm = TRUE),
      fga = sum(fga, na.rm = TRUE),
      fg3m = sum(fg3m, na.rm = TRUE),
      fg3a = sum(fg3a, na.rm = TRUE),
      ftm = sum(ftm, na.rm = TRUE),
      fta = sum(fta, na.rm = TRUE),
      oreb = sum(oreb, na.rm = TRUE),
      dreb = sum(dreb, na.rm = TRUE),
      reb = sum(reb, na.rm = TRUE),
      ast = sum(ast, na.rm = TRUE),
      stl = sum(stl, na.rm = TRUE),
      blk = sum(blk, na.rm = TRUE),
      tov = sum(tov, na.rm = TRUE),
      foul = sum(foul, na.rm = TRUE),
      poss = sum(poss, na.rm = TRUE),
      pts = sum(lineup_pts, na.rm = TRUE),
      .groups = "drop"
    )

  return(lineup_stats)
}

#' Calculate Lineup Advanced Stats
#'
#' This function calculates the advanced stats for each lineup to be used in
#' calculating play-by-play box scores.
#'
#' @param lineup_stats A data frame containing play-by-play data with lineups
#' and stats.
#' @return A tibble containing advanced stats data.
calculate_advanced_stats <- function(lineup_stats) {
  advanced_stats <- lineup_stats %>%
    filter(!is.na(lineup)) %>%
    group_by(game_id, team_location, team_name, period, lineup) %>%
    summarize(across(fgm:pts, sum), .groups = "drop") %>%
    left_join(
      lineup_stats %>%
        filter(!is.na(lineup)) %>%
        group_by(opp_lineup) %>%
        summarize(across(fgm:pts, sum), .groups = "drop"),
      by = c("lineup" = "opp_lineup"), suffix = c("", "_opp")
    ) %>%
    mutate(
      plus_minus = pts - pts_opp,
      fg_pct = fgm / fga,
      fg3_pct = fg3m / fg3a,
      ft_pct = ftm / fta,
      ortg = (pts / poss) * 100,
      drtg = (pts_opp / poss_opp) * 100,
      nrtg = ortg - drtg,
      ast_pct = ast / fgm,
      ast_tov = if_else(tov > 0, ast / tov, ast / 1),
      ast_ratio = ast / poss,
      oreb_pct = oreb / (oreb + dreb_opp),
      dreb_pct = dreb / (dreb + oreb_opp),
      tov_pct = tov / poss,
      efg_pct = (fgm + 0.5 * fg3m) / fga,
      ts_pct = pts / (2 * (fga + 0.44 * fta)),
      usage_pct = poss / (sum(poss + poss_opp, na.rm = TRUE))
    ) %>%
    mutate(
      across(everything(), ~ replace(., is.nan(.) | is.na(.), 0)),
      across(matches("pct$|ratio$|rtg$"), ~ round(., 3))
    ) %>%
    filter(!is.na(team_name)) %>%
    select(
      game_id:fga, fg_pct, fg3m:fg3a, fg3_pct, ftm:fta, ft_pct,
      oreb:foul, pts, poss, ortg:usage_pct
    ) %>%
    arrange(team_location, period)

  return(advanced_stats)
}
