#' Process Play-by-Play Data for Lineups and Possessions
#'
#' This function processes raw play-by-play (pbp) data, performs corrections to remove errors,
#' adds missing player information, and generates lineups for each play-by-play event.
#' It also marks the end of each possession and assigns the possessing team.
#' The resulting data can be used to analyze player performance and lineup efficiency.
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

#' Calculate Play-by-Play Box Scores by Lineup
#'
#' This function processes play-by-play box scores for multiple games.
#' IMPORTANT: This function can only be used after \code{pbp_lineups()} has
#' been applied to the play-by-play data to add lineup columns.
#'
#' @param game_df A data frame of play-by-play data processed with \code{pbp_lineups()}
#' @return A tibble containing box scores for all lineups across multiple games
#' @export
pbp_box_scores_lineup <- function(game_df) {
  # Check if lineups have been processed
  if (!"lineup_home_pt" %in% names(game_df)) {
    stop("Error: pbp_lineups() must be run on the data frame before calling pbp_box_scores()")
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

      # Combine the two
      final_box_score <- lineup_playtime %>%
        left_join(
          lineup_box_score,
          by = c("game_id", "lineup", "period", "team_name", "team_location")
        ) %>%
        arrange(lineup_role, period, team_location, lineup_index)

      return(final_box_score)
    }) %>%
    bind_rows()

  return(final_results)
}

#' Calculate Play-by-Play Box Scores by Lineup with Matchups
#'
#' This function processes play-by-play box scores for multiple games.
#' IMPORTANT: This function can only be used after \code{pbp_lineups()} has
#' been applied to the play-by-play data to add lineup columns.
#'
#' @param game_df A data frame of play-by-play data processed with \code{pbp_lineups()}
#' @return A tibble containing box scores for all lineups across multiple games
#' @export
pbp_box_scores_lineup_matchups <- function(game_df) {
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
      lineup_playtime <- calculate_lineup_playtime_matchups(single_game_df)

      # Calculate lineup box score
      lineup_box_score <- calculate_matchup_stats(
        single_game_df,
        lineup_playtime
      )

      # Combine the two
      final_box_score <- lineup_playtime %>%
        left_join(
          lineup_box_score,
          by = c(
            "game_id", "lineup", "opp_lineup", "period", "team_name",
            "team_location"
          )
        ) %>%
        mutate(
          across(
            where(is.numeric),
            ~ replace(., is.na(.) | is.nan(.), 0)
          )
        )

      return(final_box_score)
    }) %>%
    bind_rows()

  return(final_results)
}

#' Calculate Play-by-Play Box Scores by Player
#'
#' Combines raw stats, minutes played, and possessions to produce per-player box scores.
#'
#' IMPORTANT: This function can only be used after \code{pbp_lineups()} has
#' been applied to the play-by-play data to add lineup columns.
#'
#' @param game_df A data frame processed with `pbp_lineups()`, including
#' points, lineup, and possession columns.
#' @return A tibble with per-player box scores for offensive possessions.
#' @export
pbp_box_scores_player <- function(game_df) {
  tagged <- tag_pbp_events(game_df)
  base_box <- summarize_player_box(tagged)
  mins <- calculate_minutes_played(game_df)
  poss <- calculate_possessions_played(game_df)

  player_box_final <- base_box %>%
    full_join(mins, by = c("game_id", "period", "team_location", "team_name",  "player")) %>%
    left_join(poss, by = c("period", "player")) %>%
    left_join(
      get_game_starters(game_df) %>% mutate(starter = TRUE),
      by = c("game_id", "period", "player")
    ) %>%
    replace_na(list(starter = FALSE)) %>%
    filter(player_role == "offense") %>%
    select(
      game_id, period, team_location, team_name, player, starter,
      total_minutes, minutes_played, fgm:pfd, poss, pts
    ) %>%
    mutate(
      across(fgm:pts, ~ coalesce(.x, 0))
    ) %>%
    arrange(game_id, period, team_location, desc(starter), player)

  return(player_box_final)
}

#' Tag Play-by-Play Events with Stat Columns
#'
#' Maps each play-by-play event to a set of stat indicator columns (e.g., FGA, FGM, etc.).
#'
#' @param game_df A data frame of play-by-play events
#' @return A tibble with logical/stat columns for each event.
tag_pbp_events <- function(game_df) {
  df <- game_df %>%
    mutate(
      desc = paste0(
        coalesce(homedescription, ""),
        if_else(!is.na(homedescription) & !is.na(visitordescription), "; ", ""),
        coalesce(visitordescription, "")
      )
    )

  primary_events <- df %>%
    transmute(
      game_id,
      secs_passed_game,
      player = player1_name,
      team_name = player1_team_abbreviation,
      team_location,
      period,

      # shooting
      fgm = eventmsgtype == 1,
      fga = eventmsgtype %in% c(1, 2),
      fg3m = fgm & str_detect(desc, regex("3PT", ignore_case = TRUE)),
      fg3a = fga & str_detect(desc, regex("3PT", ignore_case = TRUE)),

      # free throws
      ftm = eventmsgtype == 3 & !str_detect(desc, regex("MISS", ignore_case = TRUE)),
      fta = eventmsgtype == 3,

      # rebounds
      oreb = eventmsgtype == 4 & player1_team_id == lag(player1_team_id),
      dreb = eventmsgtype == 4 & player1_team_id != lag(player1_team_id),
      reb = eventmsgtype == 4,

      # assists
      ast = FALSE,

      # misc
      stl = FALSE,
      blk = FALSE,
      blka = eventmsgtype == 2 & str_detect(desc, regex("BLOCK", ignore_case = TRUE)),
      tov = eventmsgtype == 5,
      pf = eventmsgtype == 6,
      pfd = FALSE,

      # points
      pts = shot_pts_home + shot_pts_away
    )

  # 2) Assist rows (only for made shots that name a helper)
  assist_events <- df %>%
    filter(eventmsgtype == 1, !is.na(player2_name)) %>%
    transmute(
      game_id,
      secs_passed_game,
      player = player2_name,
      team_name = player2_team_abbreviation,
      team_location,
      period,

      # all other stats are zero/false
      fgm = FALSE,
      fga = FALSE,
      fg3m = FALSE,
      fg3a = FALSE,
      ftm = FALSE,
      fta = FALSE,
      oreb = FALSE,
      dreb = FALSE,
      reb = FALSE,
      ast = TRUE,
      reb = FALSE,
      stl = FALSE,
      blk = FALSE,
      blka = FALSE,
      tov = FALSE,
      pf = FALSE,
      pfd = FALSE,
      pts = 0
    )

  # 3) Block events
  block_events <- df %>%
    filter(
      eventmsgtype == 2, str_detect(desc, regex("BLOCK", ignore_case = TRUE))
    ) %>%
    transmute(
      game_id,
      secs_passed_game,
      player = player3_name,
      team_name = player3_team_abbreviation,
      team_location = if_else(team_location == "away", "home", "away"),
      period,

      # all other stats are zero/false
      fgm = FALSE,
      fga = FALSE,
      fg3m = FALSE,
      fg3a = FALSE,
      ftm = FALSE,
      fta = FALSE,
      oreb = FALSE,
      dreb = FALSE,
      reb = FALSE,
      ast = FALSE,
      reb = FALSE,
      stl = FALSE,
      blk = TRUE,
      blka = FALSE,
      tov = FALSE,
      pf = FALSE,
      pfd = FALSE,
      pts = 0
    )

  # 4) Steal events
  steal_events <- df %>%
    filter(
      eventmsgtype == 5, str_detect(desc, regex("STEAL", ignore_case = TRUE))
    ) %>%
    mutate(
      team_location2 = if_else(team_location == "away", "home", "away")
    ) %>%
    transmute(
      game_id,
      secs_passed_game,
      player = player2_name,
      team_name = player2_team_abbreviation,
      team_location = if_else(team_location == "away", "home", "away"),
      period,

      # all other stats are zero/false
      fgm = FALSE,
      fga = FALSE,
      fg3m = FALSE,
      fg3a = FALSE,
      ftm = FALSE,
      fta = FALSE,
      oreb = FALSE,
      dreb = FALSE,
      reb = FALSE,
      ast = FALSE,
      reb = FALSE,
      stl = TRUE,
      blk = FALSE,
      blka = FALSE,
      tov = FALSE,
      pf = FALSE,
      pfd = FALSE,
      pts = 0
    )

  # 5) Foul events
  foul_events <- df %>%
    filter(
      eventmsgtype == 6
    ) %>%
    mutate(
      team_location2 = if_else(team_location == "away", "home", "away")
    ) %>%
    transmute(
      game_id,
      secs_passed_game,
      player = player2_name,
      team_name = player2_team_abbreviation,
      team_location = if_else(team_location == "away", "home", "away"),
      period,

      # all other stats are zero/false
      fgm = FALSE,
      fga = FALSE,
      fg3m = FALSE,
      fg3a = FALSE,
      ftm = FALSE,
      fta = FALSE,
      oreb = FALSE,
      dreb = FALSE,
      reb = FALSE,
      ast = FALSE,
      reb = FALSE,
      stl = FALSE,
      blk = FALSE,
      blka = FALSE,
      tov = FALSE,
      pf = FALSE,
      pfd = TRUE,
      pts = 0
    )

  # 6) Combine and sort
  tagged_events <- bind_rows(
    primary_events,
    assist_events, block_events, steal_events, foul_events
  ) %>%
    arrange(game_id, secs_passed_game)

  return(tagged_events)
}

#' Summarize Player Box Score from Tagged Events
#'
#' Aggregates per-player stats across all events.
#'
#' @param tagged_events A tibble from `tag_pbp_events()`.
#' @return A tibble with one row per player and their summed stats.
summarize_player_box <- function(tagged_events) {
  box_score <- tagged_events %>%
    group_by(game_id, period, player, team_name, team_location) %>%
    summarize(
      across(fgm:pts, sum, .names = "{.col}"),
      .groups = "drop"
    )

  return(box_score)
}

#' Calculate Minutes Played per Player
#'
#' Computes total minutes each player was on court based on lineup playtime.
#'
#' @param game_df A data frame with `lineup_home_pt` and `lineup_away_pt`
#' columns.
#' @return A tibble with player names and total minutes played.
calculate_minutes_played <- function(game_df) {
  intervals <- game_df %>%
    arrange(secs_passed_game) %>%
    mutate(
      next_sec = lead(secs_passed_game, default = max(secs_passed_game)),
      interval = next_sec - secs_passed_game,
      home_list = str_split(lineup_home_pt, ",\\s*"),
      away_list = str_split(lineup_away_pt, ",\\s*")
    )

  home_min <- intervals %>%
    select(game_id, interval, home_list, period, home_team_name) %>%
    unnest(home_list) %>%
    group_by(game_id, player = home_list, period, home_team_name) %>%
    summarize(sec_played = sum(interval), .groups = "drop") %>%
    mutate(
      team_location = "home",
      team_name = home_team_name
    )

  away_min <- intervals %>%
    select(game_id, interval, away_list, period, away_team_name) %>%
    unnest(away_list) %>%
    group_by(game_id, player = away_list, period, away_team_name) %>%
    summarize(sec_played = sum(interval), .groups = "drop") %>%
    mutate(
      team_location = "away",
      team_name = away_team_name
    )

  minutes_played <- bind_rows(home_min, away_min) %>%
    group_by(game_id, player, period, team_location, team_name) %>%
    summarize(
      total_sec = sum(sec_played),
      total_minutes = total_sec / 60,
      minutes_played = convert_to_minutes_seconds(total_minutes),
      .groups = "drop"
    ) %>%
    select(-total_sec)

  return(minutes_played)
}

#' Calculate Possessions Played per Player
#'
#' Computes total offensive/defensive possessions for each player.
#'
#' @param game_df A data frame with `lineup_home`, `lineup_away`, and `possession`.
#' @return A tibble with player, their role (offense/defense), and possessions count.
calculate_possessions_played <- function(game_df) {
  intervals <- game_df %>%
    arrange(secs_passed_game) %>%
    mutate(
      home_list = str_split(lineup_home, ",\\s*"),
      away_list = str_split(lineup_away, ",\\s*")
    )

  home_poss <- intervals %>%
    filter(possession > 0) %>%
    select(possession, home_list, poss_team, period) %>%
    mutate(player_team = "home") %>%
    unnest(home_list) %>%
    group_by(player = home_list, period, player_team, poss_team) %>%
    summarize(possessions = sum(possession), .groups = "drop") %>%
    mutate(player_role = if_else(player_team == poss_team, "offense", "defense"))

  away_poss <- intervals %>%
    filter(possession > 0) %>%
    select(possession, away_list, poss_team, period) %>%
    mutate(player_team = "away") %>%
    unnest(away_list) %>%
    group_by(player = away_list, player_team, poss_team, period) %>%
    summarize(possessions = sum(possession), .groups = "drop") %>%
    mutate(player_role = if_else(player_team == poss_team, "offense", "defense"))

  possessions_played <- bind_rows(home_poss, away_poss) %>%
    group_by(player, period, player_role) %>%
    summarize(poss = sum(possessions), .groups = "drop")

  return(possessions_played)
}

#' Get Starters for Each Game
#'
#' Extracts the five home and away starters (the first lineup) from the PBP.
#'
#' @param game_df A data frame processed with `pbp_lineups()`, must include
#'   `game_id`, `secs_passed_game`, `lineup_home`, and `lineup_away`.
#' @return A tibble with one row per starter: game_id, player, and team_location.
get_game_starters <- function(game_df) {
  game_df %>%
    group_by(game_id, period) %>%
    arrange(secs_passed_game) %>%
    slice(1) %>%
    transmute(
      game_id,
      period,
      home_list = str_split(lineup_home, ",\\s*"),
      away_list = str_split(lineup_away, ",\\s*")
    ) %>%
    # turn each 5‐element list into 5 rows
    pivot_longer(
      cols = c(home_list, away_list),
      names_to = "team_location",
      values_to = "starters"
    ) %>%
    unnest(cols = starters) %>%
    rename(player = starters) %>%
    select(game_id, player, period)
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
#' @return A data frame with additional columns for season year, team location, and event index.
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
      secs_passed_game = seconds_passed(pctimestring, period),
      # Fix NA player_team_id
      player1_team_id = if_else(is.na(player1_team_id), player1_id, player1_team_id)
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
#' This function processes the input data to identify players involved in events
#' (excluding substitutions) and ensures that only players who are not substituted are included in
#' the final output.
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
      !(game_id == "0022200140" & name_player == "Dennis Schroder" & period == 4),
      !(game_id == "0021600668" & name_player == "Ian Clark" & period == 4),
      !(game_id == "0021600655" & name_player == "Derrick Williams" & period == 1),
      !(game_id == "0021600235" & name_player == "Malik Beaseley" & period == 4)
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
      lineup_home = na.locf(lineup_after_home, na.rm = FALSE),
      lineup_away = na.locf(lineup_after_away, na.rm = FALSE),
      lineup_home = coalesce(
        lineup_home,
        na.locf(lineup_before_home,
          fromLast = TRUE,
          na.rm = FALSE
        )
      ),
      lineup_away = coalesce(
        lineup_away,
        na.locf(lineup_before_away,
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
      lineup_away = if_else(
        !is.na(ft_id),
        first(lineup_away[!is.na(lineup_away) & !is.na(ft_id)]),
        lineup_away
      ),
      lineup_home = if_else(
        !is.na(ft_id),
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
        (player1_team_id == lead(player1_team_id) | player1_team_id == lead(player1_id)) &
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
          eventmsgtype == 4 &
          team_location == "home" ~ "away",
        possession == 1 &
          eventmsgtype == 4 &
          team_location == "away" ~ "home",
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
      prev_period = lag(period, default = first(period)),
      lineup_changed = (!!sym(lineup_col) != prev_lineup | period != prev_period)
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
    mutate(
      lineup_index = cumsum(
        lineup != lag(lineup, default = first(lineup)) | row_number() == 1
      )
    ) %>%
    ungroup() %>%
    mutate(team_name = head(data[[team_col]], 1))
}

#' Calculate Initial Lineup Playtime with Matchups
#'
#' This function creates the initial playtime dataframe that calculates the
#' playtime for each lineup and the opponent lineup.
#'
#' @param data A data frame containing game event data, including lineup
#' information, matchup, periods, and time.
#' @param location A string indicating the location ("home" or "away") to
#' specify which team's lineup to calculate.
#' @return A tibble with the initial dataframe containing playtime and matchup
#' data.
initial_playtime_matchups <- function(data, location) {
  opponent <- if (location == "away") "home" else "away"

  lineup_col <- paste0("lineup_", location, "_pt")
  opp_lineup_col <- paste0("lineup_", opponent, "_pt")
  team_col <- paste0(location, "_team_name")

  last_secs <- tail(data$secs_passed_game, 1)
  last_lineup <- tail(data[[lineup_col]], 1)
  last_opp_lineup <- tail(data[[opp_lineup_col]], 1)

  data %>%
    mutate(
      prev_lineup = lag(!!sym(lineup_col), default = ""),
      prev_opp_lineup = lag(!!sym(opp_lineup_col),
        default = first(!!sym(opp_lineup_col))
      ),
      prev_period = lag(period, default = first(period)),
      lineup_changed = (
        (!!sym(lineup_col) != prev_lineup) |
          (!!sym(opp_lineup_col) != prev_opp_lineup) |
          (period != prev_period)
      )
    ) %>%
    filter(lineup_changed) %>%
    select(
      secs_passed_game = !!sym("secs_passed_game"),
      lineup = !!sym(lineup_col),
      opp_lineup = !!sym(opp_lineup_col),
      period,
      game_id
    ) %>%
    bind_rows(
      tibble(
        secs_passed_game = last_secs,
        lineup = last_lineup,
        opp_lineup = last_opp_lineup
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
    mutate(
      matchup_index = cumsum(
        lag(lineup, default = first(lineup)) != lineup |
          lag(opp_lineup, default = first(opp_lineup)) != opp_lineup |
          row_number() == 1
      )
    ) %>%
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
      starter = if_else(lineup_index == 1, TRUE, FALSE)
    ) %>%
    group_by(
      game_id, team_location, team_name, lineup, period, lineup_index, starter
    ) %>%
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
      game_id, period, team_location, team_name, lineup,
      lineup_index, starter, total_minutes, minutes_played
    ) %>%
    arrange(
      period, team_location, lineup_index
    )

  return(lineup_playtime)
}

#' Calculate Lineup Playtime with Matchups
#'
#' This function calculates the playtime for each lineup and matchup to be used
#' in play-by-play box scores.
#'
#' @param game_df A data frame containing play-by-play data with lineups.
#' @return A tibble containing playtime data.
calculate_lineup_playtime_matchups <- function(game_df) {
  home_lineups <- initial_playtime_matchups(game_df, "home") %>%
    mutate(team_location = "home")

  away_lineups <- initial_playtime_matchups(game_df, "away") %>%
    mutate(team_location = "away")

  lineup_playtime <- bind_rows(home_lineups, away_lineups) %>%
    filter(!is.na(period)) %>%
    mutate(
      starter = if_else(matchup_index == 1, TRUE, FALSE)
    ) %>%
    group_by(
      game_id, team_location, team_name, lineup, opp_lineup,
      period, matchup_index, starter
    ) %>%
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
      game_id, period, team_location, team_name, lineup, opp_lineup,
      matchup_index, starter, total_minutes, minutes_played
    ) %>%
    arrange(
      period, team_location, matchup_index
    )

  return(lineup_playtime)
}

#' Calculate Lineup Base Stats
#'
#' Calculates base stat totals (e.g., FGM, FGA, AST, REB, etc.) for each lineup,
#' combining both offensive and defensive contributions. Useful for building
#' lineup-level play-by-play box scores.
#'
#' @param game_df A data frame of play-by-play events that includes lineup
#' information.
#' @param lineup_playtime A data frame of lineup playtime.
#'
#' @return A tibble containing summed stat columns by lineup, including
#' separate rows for lineup (\code{lineup_role = "lineup"}) and opponent
#' lineup (\code{lineup_role = "opp_lineup"}) performance.
calculate_lineup_stats <- function(game_df, lineup_playtime) {
  # Pull out a minimal table of home/away names for later
  teams <- game_df %>%
    select(game_id, home_team_name, away_team_name) %>%
    distinct()

  # 1) Lineup
  off_stats <- tag_lineup_events(game_df, mode = "lineup") %>%
    filter(
      lineup %in% lineup_playtime$lineup,
      period %in% lineup_playtime$period
    ) %>%
    group_by(game_id, team_location, team_name, period, lineup) %>%
    summarize(
      across(fgm:pts, sum, .names = "{.col}"),
      .groups = "drop"
    ) %>%
    mutate(lineup_role = "lineup")

  # 2) Opponent lineup
  # We take the same “matchup” tags, then re‐attribute each event to the
  # defending lineup:
  def_stats <- tag_lineup_events(game_df, mode = "matchup") %>%
    filter(
      opp_lineup %in% lineup_playtime$lineup,
      period %in% lineup_playtime$period
    ) %>%
    # bring in home/away team names so we can re‐derive the defender’s team_name
    left_join(teams, by = "game_id") %>%
    mutate(
      # switch to “defender” as our grouping lineup
      lineup = opp_lineup,
      # flip offense/defense location
      team_location = if_else(team_location == "away", "home", "away"),
      # assign the correct team_name for that side
      team_name = if_else(
        team_location == "away",
        away_team_name,
        home_team_name
      )
    ) %>%
    group_by(game_id, team_location, team_name, period, lineup) %>%
    summarize(
      across(fgm:pts, sum, .names = "{.col}"),
      .groups = "drop"
    ) %>%
    mutate(lineup_role = "opp_lineup")

  # 3) Combine
  bind_rows(off_stats, def_stats)
}

#' Calculate Matchup Base Stats
#'
#' Calculates base stat totals (e.g., FGM, FGA, AST, REB, etc.) for each
#' lineup vs. opponent lineup matchup, to be used in play-by-play box score
#' calculations.
#'
#' @param game_df A data frame of play-by-play events that includes tagged
#' lineup data.
#' @param lineup_playtime A data frame of lineup playtime.
#'
#' @return A tibble containing summed stat columns grouped by game, period,
#' team, lineup, and opposing lineup.
calculate_matchup_stats <- function(game_df, lineup_playtime) {
  tag_lineup_events(game_df, mode = "matchup") %>%
    filter(
      (lineup %in% lineup_playtime$lineup | opp_lineup %in% lineup_playtime$lineup) &
        period %in% lineup_playtime$period
    ) %>%
    group_by(game_id, team_location, team_name, period, lineup, opp_lineup) %>%
    summarize(across(fgm:pts, sum), .groups = "drop")
}

#' Tag Lineup Play-by-Play Events with Stat Indicators
#'
#' Adds stat indicator columns (e.g., FGA, FGM, REB, AST, etc.) to each
#' play-by-play event and aligns events with the corresponding lineup. Also
#' adjusts lineup context for STEAL and BLOCK events by flipping possession.
#'
#' @param game_df A data frame of play-by-play events.
#' @param mode A string indicating the tagging mode: \code{"lineup"} (default)
#' or \code{"matchup"}. If \code{"matchup"}, the returned data will include the
#' opposing lineup (\code{opp_lineup}).
#'
#' @return A tibble containing the original events with appended logical
#' stat columns, along with lineup identifiers and team metadata.
tag_lineup_events <- function(game_df, mode = c("lineup", "matchup")) {
  mode <- match.arg(mode)
  df <- game_df %>%
    mutate(
      desc = paste0(
        coalesce(homedescription, ""),
        if_else(!is.na(homedescription) & !is.na(visitordescription), "; ", ""),
        coalesce(visitordescription, "")
      ),
      ## pick the right columns based on mode
      lineup = if_else(team_location == "away",
        lineup_away, lineup_home
      ),
      opp_lineup = if_else(team_location == "away",
        lineup_home, lineup_away
      ),
      lineup_pts = if_else(team_location == "away",
        shot_pts_away, shot_pts_home
      ),
      team_name = if_else(team_location == "away",
        away_team_name, home_team_name
      ),
      period = period,
      game_id = game_id
    )

  block_events <- df %>%
    filter(str_detect(desc, regex("BLOCK", ignore_case = TRUE))) %>%
    mutate(
      lineup_tmp = lineup,
      lineup = opp_lineup,
      opp_lineup = lineup_tmp,
      team_name = if_else(team_name == away_team_name, home_team_name, away_team_name),
      team_location = if_else(team_location == "away", "home", "away"),
      possession = 0,
      block = TRUE,
      eventmsgtype = "0"
    ) %>%
    select(-lineup_tmp)

  steal_events <- df %>%
    filter(str_detect(desc, regex("STEAL", ignore_case = TRUE))) %>%
    mutate(
      lineup_tmp = lineup,
      lineup = opp_lineup,
      opp_lineup = lineup_tmp,
      team_name = if_else(team_name == away_team_name, home_team_name, away_team_name),
      team_location = if_else(team_location == "away", "home", "away"),
      possession = 0,
      steal = TRUE,
      eventmsgtype = "0"
    ) %>%
    select(-lineup_tmp)

  foul_events <- df %>%
    filter(eventmsgtype == 6) %>%
    mutate(
      lineup_tmp = lineup,
      lineup = opp_lineup,
      opp_lineup = lineup_tmp,
      team_name = if_else(team_name == away_team_name, home_team_name, away_team_name),
      team_location = if_else(team_location == "away", "home", "away"),
      possession = 0,
      foul_drawn = TRUE,
      eventmsgtype = "0"
    ) %>%
    select(-lineup_tmp)

  df <- bind_rows(df, block_events, steal_events, foul_events) %>%
    arrange(game_id, secs_passed_game) %>%
    replace_na(list(
      steal = FALSE,
      block = FALSE,
      foul_drawn = FALSE
    ))

  df %>%
    transmute(
      game_id, period, team_location, team_name, lineup,
      # only for matchups:
      opp_lineup = if (mode == "matchup") opp_lineup else NULL,
      fgm = eventmsgtype == 1,
      fga = eventmsgtype %in% c(1, 2),
      fg3m = fgm & str_detect(desc, regex("3PT", ignore_case = TRUE)),
      fg3a = fga & str_detect(desc, regex("3PT", ignore_case = TRUE)),
      ftm = eventmsgtype == 3 & !str_detect(desc, regex("MISS",
        ignore_case = TRUE
      )),
      fta = eventmsgtype == 3,
      oreb = eventmsgtype == 4 & player1_team_id == lag(player1_team_id),
      dreb = eventmsgtype == 4 & player1_team_id != lag(player1_team_id),
      reb = eventmsgtype == 4,
      ast = eventmsgtype == 1 & !is.na(player2_name),
      stl = steal == TRUE,
      blk = block == TRUE,
      blka = eventmsgtype == 2 & str_detect(desc, regex("BLOCK", ignore_case = TRUE)),
      tov = eventmsgtype == 5,
      pf = eventmsgtype == 6,
      pfd = foul_drawn == TRUE,
      poss = possession == 1,
      pts = lineup_pts
    )
}

#' Compute Advanced Lineup Stats
#'
#' Calculates a suite of offensive and defensive advanced metrics for a single
#' lineup role by joining base box‑score counts with the opponent’s stats,
#' cleaning, and rounding results.
#'
#' @param pbp_bs A tibble of lineup box‑score counts as returned by
#'   \code{pbp_box_scores_lineup()}, containing columns
#'   \code{game_id}, \code{period}, \code{lineup}, \code{poss}, \code{pts},
#'   etc.
#' @param base_role Character; the \code{lineup_role} (e.g. \code{"lineup"}) to
#'   treat as the “offense” for which metrics will be computed.
#' @param opp_role Character; the \code{lineup_role} (e.g. \code{"opp_lineup"})
#'   to treat as the “defender,” whose counts will be prefixed with \code{opp_}.
#' @return A tibble with one row per \code{game_id} × \code{period} ×
#'   \code{lineup}, containing all standard advanced metrics:
#'   \code{efg_pct}, \code{ts_pct}, \code{ortg}, \code{drtg}, \code{nrtg},
#'   \code{ast_pct}, \code{ast_tov}, \code{ast_ratio}, \code{oreb_pct},
#'   \code{dreb_pct}, \code{trb_pct}, \code{tov_pct}, \code{usage_pct},
#'   \code{fg2a_pct}, \code{fg3a_pct}, \code{ftr}, \code{stl_pct},
#'   \code{blk_pct}, \code{ppp}, \code{pm}, \code{pace}, plus the original
#'   counting stats and a \code{lineup_role} column set to \code{base_role}.
compute_lineup_advanced_stats <- function(pbp_bs, base_role, opp_role) {
  base_stats <- pbp_bs %>%
    filter(lineup_role == base_role) %>%
    select(-lineup_role)

  opp_stats <- pbp_bs %>%
    filter(lineup_role == opp_role) %>%
    select(game_id, period, lineup, fgm:pts) %>%
    rename_with(~ paste0("opp_", .x), -c(game_id, period, lineup))

  adv_cols <- c(
    "efg_pct","ts_pct","ortg","drtg","nrtg",
    "ast_pct","ast_tov","ast_ratio",
    "oreb_pct","dreb_pct","trb_pct",
    "tov_pct","usage_pct","fg2a_pct","fg3a_pct",
    "ftr","stl_pct","blk_pct","ppp","pm","pace"
  )

  base_stats %>%
    left_join(opp_stats, by = c("game_id","period","lineup")) %>%
    mutate(across(where(is.numeric), ~ replace(., is.na(.) | is.nan(.), 0))) %>%
    mutate(
      efg_pct = (fgm + 0.5 * fg3m) / fga,
      ts_pct = pts / (2 * (fga + 0.44 * fta)),
      ortg = (pts / poss) * 100,
      drtg = (opp_pts / opp_poss) * 100,
      nrtg = ortg - drtg,
      ast_pct = if_else(fgm > 0, ast / fgm, 0),
      ast_tov = if_else(tov > 0, ast / tov, 0),
      ast_ratio = ast / poss,
      oreb_pct = oreb / (oreb + opp_dreb),
      dreb_pct = dreb / (dreb + opp_oreb),
      trb_pct = (oreb + dreb) / (oreb + dreb + opp_oreb + opp_dreb),
      tov_pct = tov / poss,
      usage_pct = ((fga + 0.5*fta + tov) / poss) * 100, # NBA.com uses 0.5 / NBA Stuffer uses 0.44
      fg2a_pct = (fgm - fg3m) / (fga - fg3a),
      fg3a_pct = fg3a / fga,
      ftr = fta / fga,
      stl_pct = stl / opp_poss,
      blk_pct = blk / opp_poss,
      ppp = pts / poss,
      pm = pts - opp_pts,
      pace = poss / total_minutes * 48
    ) %>%
    replace_na(as.list(set_names(rep(0, length(adv_cols)), adv_cols))) %>%
    mutate(across(matches("pct$|rtg$|ratio$|ftr|ppp|pace"), \(x) round(x, 3))) %>%
    select(game_id:minutes_played, fgm:pts, efg_pct:pace) %>%
    mutate(lineup_role = base_role)
}

#' Calculate Lineup Advanced Stats Box Score
#'
#' Wrapper that takes play‑by‑play data (processed by \code{pbp_lineups()})
#' and returns a box‑score–style tibble of advanced metrics for each lineup
#' and its opponent.
#'
#' @param game_df A tibble of play‑by‑play events, as returned by
#'   \code{pbp_lineups()}.
#' @return A tibble with one row per \code{lineup_role} per \code{lineup},
#'   containing all advanced metrics for both offense (\code{lineup}) and
#'   defense (\code{opp_lineup}).
#' @export
pbp_advanced_lineup <- function(game_df) {
  pbp_bs <- pbp_box_scores_lineup(game_df)

  # Define the two role pairs
  roles <- list(
    list(base_role = "lineup", opp_role = "opp_lineup"),
    list(base_role = "opp_lineup", opp_role = "lineup")
  )

  # Map over each pair, calling your helper
  stats_list <- map(
    roles,
    ~ compute_lineup_advanced_stats(
      base_role = .x$base_role,
      opp_role = .x$opp_role,
      pbp_bs = pbp_bs
    )
  )

  # Bind the two results into one tibble
  list_rbind(stats_list)
}

#' Summarize Opponent Points and Possessions by Defender
#'
#' Processes lineup‑aware play‑by‑play data to compute, for each defender,
#' the total opponent field goals, free throws, rebounds, assists, steals,
#' blocks, turnovers, fouls, possessions, and points they faced.
#'
#' @param game_df A tibble of play‑by‑play events (from \code{pbp_lineups()})
#'   which must include \code{possession}, \code{poss_team},
#'   \code{lineup_home_pt}, \code{lineup_away_pt}, \code{shot_pts_home},
#'   and \code{shot_pts_away}.
#' @return A tibble with one row per \code{game_id} × \code{period} ×
#'   \code{player}, containing:
#'   \itemize{
#'     \item \code{opp_fgm}, \code{opp_fga}, \code{opp_fg3m}, \code{opp_fg3a},
#'           \code{opp_ftm}, \code{opp_fta}
#'     \item \code{opp_oreb}, \code{opp_dreb}, \code{opp_reb}
#'     \item \code{opp_ast}, \code{opp_stl}, \code{opp_blk},
#'           \code{opp_tov}, \code{opp_pf}
#'     \item \code{opp_poss}: total opponent possessions faced
#'     \item \code{opp_pts}: total points scored against the defender
#'   }
summarize_player_def_stats <- function(game_df) {
  df <- game_df %>%
    # 1) make sure every row “knows” which team had the ball & who was on court
    group_by(game_id, period) %>%
    fill(poss_team, lineup_home, lineup_away, .direction = "up") %>%
    ungroup()

  block_events <- game_df %>%
    group_by(game_id, period) %>%
    fill(poss_team, lineup_home, lineup_away, .direction = "up") %>%
    ungroup() %>%
    mutate(
      desc = paste0(
        coalesce(homedescription, ""),
        if_else(!is.na(homedescription) & !is.na(visitordescription), "; ", ""),
        coalesce(visitordescription, "")
      )
    ) %>%
    filter(str_detect(desc, regex("BLOCK", ignore_case = TRUE))) %>%
    mutate(
      poss_team = if_else(poss_team == "away", "home", "away"),
      possession = 0,
      block = TRUE,
      eventmsgtype = "0"
    )

  steal_events <- game_df %>%
    group_by(game_id, period) %>%
    fill(poss_team, lineup_home, lineup_away, .direction = "up") %>%
    ungroup() %>%
    mutate(
      desc = paste0(
        coalesce(homedescription, ""),
        if_else(!is.na(homedescription) & !is.na(visitordescription), "; ", ""),
        coalesce(visitordescription, "")
      )
    ) %>%
    filter(str_detect(desc, regex("STEAL", ignore_case = TRUE))) %>%
    mutate(
      poss_team = if_else(poss_team == "away", "home", "away"),
      possession = 0,
      steal = TRUE,
      eventmsgtype = "0"
    )

  df <- bind_rows(df, block_events, steal_events) %>%
    arrange(game_id, secs_passed_game) %>%
    replace_na(list(
      steal = FALSE,
      block = FALSE
    ))

  def_stats <- df %>%
    # 2) define all your event‐level defensive stats
    mutate(
      desc = paste0(
        coalesce(homedescription, ""),
        if_else(!is.na(homedescription) & !is.na(visitordescription), "; ", ""),
        coalesce(visitordescription, "")
      ),
      fgm = eventmsgtype == 1,
      fga = eventmsgtype %in% c(1, 2),
      fg3m = fgm & str_detect(desc, regex("3PT", ignore_case = TRUE)),
      fg3a = fga & str_detect(desc, regex("3PT", ignore_case = TRUE)),
      ftm = eventmsgtype == 3 & !str_detect(desc, regex("MISS", ignore_case = TRUE)),
      fta = eventmsgtype == 3,
      oreb = eventmsgtype == 4 & player1_team_id == lag(player1_team_id),
      dreb = eventmsgtype == 4 & player1_team_id != lag(player1_team_id),
      reb = eventmsgtype == 4,
      ast = eventmsgtype == 1 & !is.na(player2_name),
      stl = steal == TRUE,
      blk = block == TRUE,
      tov = eventmsgtype == 5,
      pf = eventmsgtype == 6
    ) %>%
    # 3) split out the five defenders
    mutate(
      defenders = if_else(
        poss_team == "home",
        str_split(lineup_away, ",\\s*"),
        str_split(lineup_home, ",\\s*")
      )
    ) %>%
    unnest(defenders) %>%
    # 4) roll up per defender
    group_by(game_id, period, player = defenders) %>%
    summarize(
      opp_fgm = sum(fgm, na.rm = TRUE),
      opp_fga = sum(fga, na.rm = TRUE),
      opp_fg3m = sum(fg3m, na.rm = TRUE),
      opp_fg3a = sum(fg3a, na.rm = TRUE),
      opp_ftm = sum(ftm, na.rm = TRUE),
      opp_fta = sum(fta, na.rm = TRUE),
      opp_oreb = sum(oreb, na.rm = TRUE),
      opp_dreb = sum(dreb, na.rm = TRUE),
      opp_reb = sum(reb, na.rm = TRUE),
      opp_ast = sum(ast, na.rm = TRUE),
      opp_stl = sum(stl, na.rm = TRUE),
      opp_blk = sum(blk, na.rm = TRUE),
      opp_tov = sum(tov, na.rm = TRUE),
      opp_pf = sum(pf, na.rm = TRUE),
      opp_poss = sum(possession, na.rm = TRUE),
      opp_pts = sum(shot_pts_home + shot_pts_away, na.rm = TRUE),
      .groups = "drop"
    )

  return(def_stats)
}

#' Compute Advanced Player Stats
#'
#' Combines offensive box scores, defensive summaries, and lineup totals to
#' generate per‑player advanced metrics (e.g.\ efficiency and rating stats).
#'
#' @param game_df A tibble of play‑by‑play events, as returned by
#'   \code{pbp_lineups()}, including lineup and possession columns.
#' @return A tibble with one row per \code{player} containing advanced stats:
#'   \code{efg_pct}, \code{ts_pct}, \code{ortg}, \code{drtg}, \code{nrtg},
#'   \code{ast_pct}, \code{ast_tov}, \code{ast_ratio}, \code{oreb_pct},
#'   \code{dreb_pct}, \code{trb_pct}, \code{tov_pct}, \code{usage_pct},
#'   \code{fg2a_pct}, \code{fg3a_pct}, \code{ftr}, \code{stl_pct},
#'   \code{blk_pct}, \code{ppp}, \code{pm}, \code{pace}, plus raw counts
#'   and minutes played.
compute_player_advanced_stats <- function(game_df) {
  if (!"lineup_home_pt" %in% names(game_df)) {
    stop("pbp_lineups() must be run before defensive player box scores")
  }

  player_box <- pbp_box_scores_player(game_df)
  opp_stats <- summarize_player_def_stats(game_df)
  lineup_playtime <- calculate_lineup_playtime(game_df)

  all_stats <- player_box %>%
    full_join(opp_stats, by = c("game_id", "period", "player"))

  # Get lineup‐level offense counts (team FGA, FTA, TOV) via your existing function
  lineup_off <- calculate_lineup_stats(game_df, lineup_playtime) %>%
    filter(lineup_role == "lineup") %>%
    select(game_id, period, lineup, fga, fta, tov, pts)

  # Expand each lineup string into its five players
  player_team_totals <- lineup_off %>%
    mutate(player = str_split(lineup, ",\\s*")) %>%  # list‑column of 5 names
    unnest(player) %>%                                # one row per player
    group_by(game_id, period, player) %>%
    summarize(
      team_fga = sum(fga, na.rm = TRUE),
      team_fta = sum(fta, na.rm = TRUE),
      team_tov = sum(tov, na.rm = TRUE),
      team_pts = sum(pts, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      team_min = if_else(period > 4, 5, 12)
    )

  all_stats <- all_stats %>%
    left_join(player_team_totals, by = c("game_id", "period", "player"))

  adv_cols <- c(
    "efg_pct","ts_pct","ortg","drtg","nrtg",
    "ast_pct","ast_tov","ast_ratio",
    "oreb_pct","dreb_pct","trb_pct",
    "tov_pct","usage_pct","fg2a_pct","fg3a_pct",
    "ftr","stl_pct","blk_pct","ppp","pm","pace"
  )

  player_adv_box <- all_stats %>%
    mutate(
      efg_pct = (fgm + 0.5 * fg3m) / fga,
      ts_pct = pts / (2 * (fga + 0.44 * fta)),
      ortg = (pts / poss) * 100,
      drtg = (opp_pts / opp_poss) * 100,
      nrtg = ortg - drtg,
      ast_pct = if_else(fgm > 0, ast / fgm, 0),
      ast_tov = if_else(tov > 0, ast / tov, 0),
      ast_ratio = ast / poss,
      oreb_pct = oreb / (oreb + opp_dreb),
      dreb_pct = dreb / (dreb + opp_oreb),
      trb_pct = (oreb + dreb) / (oreb + dreb + opp_oreb + opp_dreb),
      tov_pct = tov / poss,
      usage_pct = ((fga + 0.5*fta + tov)) / # NBA.com uses 0.5 / NBA Stuffer uses 0.44
                      ((team_fga + 0.5*team_fta + team_tov)) * 100,
      fg2a_pct = (fgm - fg3m) / (fga - fg3a),
      fg3a_pct = fg3a / fga,
      ftr = fta / fga,
      stl_pct = stl / opp_poss,
      blk_pct = blk / opp_poss,
      ppp = pts / poss,
      pm = team_pts - opp_pts,
      pace = poss / total_minutes * 48
    ) %>%
    replace_na(as.list(set_names(rep(0, length(adv_cols)), adv_cols))) %>%
    mutate(across(matches("pct$|rtg$|ratio$|ftr|ppp|pace"), \(x) round(x, 3))) %>%
    select(game_id:minutes_played, fgm:pts, efg_pct:pace)

  return(player_adv_box)
}

#' Calculate Player Advanced Stats Box Score
#'
#' Wrapper around \code{compute_player_advanced_stats()} to produce a per-player
#' advanced metrics tibble.
#'
#' @param game_df A tibble of play‑by‑play events, as returned by
#'   \code{pbp_lineups()}.
#' @return A tibble with per‑player advanced stats.
#' @export
pbp_advanced_player <- function(game_df) {
  compute_player_advanced_stats(game_df)
}

#' Compute Player Usage Stats
#'
#' Calculates usage rates and the breakdown of each player’s shot, rebound, assist, turnover,
#' block, steal, and point usage as a percentage of team totals.
#'
#' @param game_df A tibble of play‑by‑play events, as returned by
#'   \code{pbp_lineups()}.
#' @return A tibble with one row per \code{player}, containing:
#'   \code{usage_pct} and detailed usage metrics
#'   \code{usage_fgm}, \code{usage_fga}, \code{usage_fg3m}, \dots,
#'   \code{usage_pts}.
compute_player_usage_stats <- function(game_df) {
  if (!"lineup_home_pt" %in% names(game_df)) {
    stop("pbp_lineups() must be run before defensive player box scores")
  }

  player_box <- pbp_box_scores_player(game_df)
  lineup_playtime <- calculate_lineup_playtime(game_df)

  # Get lineup‐level offense counts (team FGA, FTA, TOV) via your existing function
  lineup_off <- calculate_lineup_stats(game_df, lineup_playtime) %>%
    filter(lineup_role == "lineup") %>%
    select(game_id, period, lineup, fgm:pts)

  # Expand each lineup string into its five players
  player_team_totals <- lineup_off %>%
    mutate(player = str_split(lineup, ",\\s*")) %>%  # list‑column of 5 names
    unnest(player) %>%                                # one row per player
    group_by(game_id, period, player) %>%
    summarize(
      team_fgm = sum(fgm, na.rm = TRUE),
      team_fga = sum(fga, na.rm = TRUE),
      team_fg3m = sum(fg3m, na.rm = TRUE),
      team_fg3a = sum(fg3a, na.rm = TRUE),
      team_ftm = sum(ftm, na.rm = TRUE),
      team_fta = sum(fta, na.rm = TRUE),
      team_oreb = sum(oreb, na.rm = TRUE),
      team_dreb = sum(dreb, na.rm = TRUE),
      team_reb = sum(reb, na.rm = TRUE),
      team_ast = sum(ast, na.rm = TRUE),
      team_stl = sum(stl, na.rm = TRUE),
      team_blk = sum(blk, na.rm = TRUE),
      team_blka = sum(blka, na.rm = TRUE),
      team_tov = sum(tov, na.rm = TRUE),
      team_pf = sum(pf, na.rm = TRUE),
      team_poss = sum(poss, na.rm = TRUE),
      team_pts = sum(pts, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      team_min = if_else(period > 4, 5, 12)
    )

  all_stats <- player_box %>%
    left_join(player_team_totals, by = c("game_id", "period", "player"))

  player_usage_box <- all_stats %>%
    mutate(
      usage_pct = ((fga + 0.5*fta + tov)) / # NBA.com uses 0.5 / NBA Stuffer uses 0.44
        ((team_fga + 0.5*team_fta + team_tov)) * 100,
      usage_fgm = fgm / team_fgm,
      usage_fga = fga / team_fga,
      usage_fg3m = fg3m  / team_fg3m ,
      usage_fg3a = fg3a / team_fg3a,
      usage_ftm = ftm / team_ftm,
      usage_fta = fta / team_fta,
      usage_oreb = oreb / team_oreb,
      usage_dreb = dreb / team_dreb,
      usage_reb = reb / team_reb,
      usage_ast = ast / team_ast,
      usage_stl = stl / team_stl,
      usage_blk = blk / team_blk,
      usage_blka = blka / team_blka,
      usage_tov = tov / team_tov,
      usage_pf = pf / team_pf,
      usage_pts = pts / team_pts
    ) %>%
    mutate(
      across(where(is.numeric), ~ replace(., is.na(.) | is.nan(.), 0))
    )%>%
    mutate(across(usage_pct:usage_pts, \(x) round(x, 3))) %>%
    select(game_id:minutes_played, usage_pct:usage_pts)

  return(player_usage_box)
}

#' Calculate Player Usage Stats Box Score
#'
#' Wrapper around \code{compute_player_usage_stats()} to output per-player usage statistics.
#'
#' @param game_df A tibble of play‑by‑play events, as returned by
#'   \code{pbp_lineups()}.
#' @return A tibble with per‑player usage stats.
#' @export
pbp_usage_player <- function(game_df) {
  compute_player_usage_stats(game_df)
}
