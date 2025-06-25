season = "2024"
game = "0022401073" # 0022400287
url <- paste0(
  "https://data.nba.com/data/10s/v2015/json/mobile_teams/nba/", season,
  "/scores/pbp/", game, "_full_pbp.json"
)



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



#' Add Season, Time, and Event Index to Play-by-Play
#'
#' This function processes a given dataset by adding season year, team location,
#' time, and an event index.
#'
#' @param data A data frame containing game data.
#' @return A data frame with additional columns for season year, team location, and event index.
add_pbp_event_info <- function(data) {
  data <- data %>%
    distinct() %>%
    filter(eventmsgtype != 18) %>%
    mutate(
      season_year = if_else(
        as.integer(substr(game_id, 4, 5)) > 90,
        1900 + as.integer(substr(game_id, 4, 5)) + 1,
        2000 + as.integer(substr(game_id, 4, 5)) + 1
      ),
      secs_remain_quarter = round(convert_to_seconds(clock),0),
      secs_passed_game = round(seconds_passed(clock, period),0)
    ) %>%
    arrange(game_id, secs_passed_game) %>%
    group_by(game_id) %>%
    mutate(
      # Event index based on the game
      event_index = row_number()
    ) %>%
    ungroup()

  return(data)
}

#' Prepare PBP Data for Processing
#'
#' Function to prep data for pbp processing by making data corrections,
#' adding event info, and adding team name columns.
#'
#' @param data A data frame containing raw play-by-play data. This should
#'             include information on each play event (e.g., players involved,
#'             teams, event type, time, etc.).
#' @return A tibble with prepped pbp data.
prepare_data_pbp <- function(data) {
  data %>%
    add_pbp_event_info() %>%
    group_by(game_id) %>%
    mutate(
      prev_idx = lag(row_number()),
      next_idx = lead(row_number())
    ) %>%
    ungroup()
}

#' Compute Possession Start and End Time
#'
#' Function to compute the possession start and end times.
#'
#' @param data A data frame containing the possession base data.
#' @return A tibble with the base possession with possession start and end time added.
compute_possession_start_bounds <- function(data) {
  bounds <- data %>%
    group_by(game_id, period, poss_id) %>%
    summarise(
      poss_end = min(secs_remain_quarter),
      poss_start = max(secs_remain_quarter),
      .groups = "drop"
    ) %>%
    arrange(game_id, period, poss_id) %>%
    group_by(game_id, period) %>%
    mutate(
      poss_start = lag(poss_end, default = first(poss_start))
    ) %>%
    select(game_id, period, poss_id, poss_start)

  data %>%
    left_join(bounds, by = c("game_id","period","poss_id"))
}

#' Builds Base for PBP Possession Data
#'
#' Function to build the base for pbp possession data.
#'
#' @param data A data frame containing prepped play-by-play data.
#' @return A tibble with the base possession data needed for processing.
build_poss_base <- function(data) {
  possession_base <- data %>%
    group_by(game_id, period) %>%
    mutate(
      is_fgm = eventmsgtype == 1,
      is_tov = eventmsgtype == 5,
      is_dreb = eventmsgtype == 4 & offense_team != lag(offense_team),
      is_ft_end = eventmsgtype == 3 & eventmsgactiontype %in% c(10, 12, 15) &
        !str_detect(desc, regex("Missed", ignore_case = TRUE)),
      is_jump = eventmsgtype == 10 & lag(eventmsgtype) != 12,
      is_jump_start = eventmsgtype == 10 & lag(eventmsgtype) == 12,
      is_period_end = eventmsgtype == 13,
      oftid_changed = offense_team != lead(offense_team),
      oftid_is_0 = offense_team == 0 | lead(offense_team) == 0,
      is_poss_end = oftid_changed & !oftid_is_0 & !eventmsgtype == 12,
      is_jump_ball_tov = eventmsgtype == 10 & offense_team != lag(offense_team) & !lag(is_jump_start)
    ) %>%
    select(
      -c(oftid_changed, oftid_is_0)
    ) %>%
    ungroup() %>%
    mutate(poss_id = cumsum(lag(is_poss_end, default = TRUE)))

  return(possession_base)
}

#' Builds Player to Team Map
#'
#' Function to build the map between player and team.
#'
#' @param data A data frame containing prepped play-by-play data.
#' @return A lookup vector to return the team of each player.
build_player_team_map <- function(data) {
  player_team_map <- data %>%
    filter(!is_jump_start) %>%
    transmute(player_id = player1_id, team_id) %>%
    filter(player_id != 0 & player_id != team_id) %>%
    distinct() %>%
    deframe()

  return(player_team_map)
}

#' Track Player Substitutions in Each Period
#'
#' This function processes the input data to identify player substitutions
#' to show who was substituted in and out during each period

#' @param data A data frame containing game event data with substitution events.
#' @return A data frame with the players subbed during a period
get_subbed_players <- function(data) {
  # Subs in period
  subbed_players <- data %>%
    filter(eventmsgtype == 8) %>%
    select(
      game_id, period, secs_passed_game,
      team_id = team_id,
      player_out = player1_id,
      player_in = player2_id
    ) %>%
    pivot_longer(
      cols = starts_with("player"),
      names_to = "sub",
      names_prefix = "player_",
      values_to = "player_id"
    ) %>%
    group_by(game_id, period, player_id) %>%
    filter(row_number() == 1) %>%
    ungroup()

  return(subbed_players)
}

#' Track Players Involved in Events But Not Substitutions
#'
#' This function processes the input data to identify players involved in events
#' (excluding substitutions) and ensures that only players who are not substituted are included in
#' the final output.
#'
#' @param data A data frame containing game event data.
#' @param subbed_players A data frame containing subs made
#' @return A data frame with players involved in events but not subbed
get_event_players <- function(data, subbed_players) {
  player_team_map <- build_player_team_map(data)

  event_players <- data %>%
    filter(
      eventmsgtype != 8,
      !(eventmsgtype == 6 & eventmsgactiontype %in% c(
        10, 11, 16,
        18, 25
      )),
      !(eventmsgtype == 11 & eventmsgactiontype %in% c(1, 4))
    ) %>%
    pivot_longer(
      cols = c(player1_id, player2_id, player3_id),
      names_to = "player_number",
      names_prefix = "player_id",
      values_to = "player_id"
    ) %>%
    rename(
      team_id_original = team_id
    ) %>%
    mutate(
      team_id = player_team_map[as.character(player_id)]
    ) %>%
    filter(!player_id == 0, !player_id == "", !is.na(team_id)) %>%
    anti_join(
      subbed_players %>%
        select(game_id, period, player_id, team_id),
      by = c("game_id", "period", "player_id", "team_id")
    ) %>%
    distinct(game_id, period, player_id, team_id)

  return(event_players)
}

#' Fetch Box Score Data from API for PBP Fix
#'
#' This function retrieves box score data from the NBA Stats API for given game IDs using a
#' specified endpoint. It creates batches of game_ids and pauses between batches to avoid timeout
#' issues.
#'
#' @param measure_type A vector of measure types for various box score data.
#' @param game_ids A character vector of game IDs.
#' @param period The period with missing players.
#' @param start_range The start time for which to generate parameters.
#' @param end_range The end time for which to generate parameters.
#' @param batch_size Number of requests before pausing (default: 100)
#' @param pause_seconds Number of seconds to pause between batches (default: 15)
#'
#' @return A data frame containing the box score data for the specified game IDs.
fetch_box_score_fix <- function(measure_type,
                                game_ids,
                                period,
                                start_range,
                                end_range,
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

  # Process each batch sequentially
  results <- map_dfr(seq_along(batched_games), function(batch_num) {
    batch_games <- batched_games[[batch_num]]
    message(glue("Fetching batch {batch_num}/{num_batches}: games
                       {batch_games[1]} to {batch_games[length(batch_games)]}"))

    # Process each game in the current batch
    batch_results <- map_dfr(batch_games, function(game_id) {
      tryCatch(
        {
          # Process each measure type for the current game and collect in a list
          data_frames <- map(measure_type, function(measure) {
            endpoint <- measure$endpoint
            list_name <- measure$list_name

            # endpoint <- "boxscoretraditionalv2"

            headers <- generate_headers_stats()
            url <- paste0("https://stats.nba.com/stats/", endpoint)
            params <- generate_params_box_scores_fix(game_id, start_range, end_range)
            data <- get_data(url, headers, params)

            # Process team data function
            process_team_data <- function(team_data,
                                          game_id,
                                          team_id,
                                          location) {
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
          base_data <- join_data_frames(data_frames, actual_join_columns) %>%
            mutate(period = period)

          base_data
        },
        error = function(e) {
          message(glue("Error fetching data for Game ID {game_id}:
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

    batch_results
  })

  results
}

#' Get NBA Box Scores for PBP Fix
#'
#' This function gets NBA box score data for the specified `game_ids`, returning a data frame with
#' all selected `measure_types`.
#' Creates batches of `game_ids` and pauses between batches to avoid timeout issues.
#'
#' @param game_ids A character vector of game IDs.
#' @param measure_types A character vector of selected box score options.
#' @param period The period with missing players.
#' @param start_range The start time for which to generate parameters.
#' @param end_range The end time for which to generate parameters.
#' @param batch_size Number of requests before pausing (default: 100)
#' @param pause_seconds Number of seconds to pause between batches (default: 15)
#'
#' @return A data frame containing box score data for the selected games and measure types.
nba_box_scores_fix <- function(game_ids,
                               measure_types,
                               period,
                               start_range,
                               end_range,
                               batch_size = 100,
                               pause_seconds = 15) {
  # Validate user choices
  invalid_choices <- setdiff(measure_types, names(box_score_types))
  if (length(invalid_choices) > 0) {
    stop(paste(
      "Invalid choice(s):", paste(invalid_choices, collapse = ", "),
      "\nValid options are:\n",
      paste(names(box_score_types), collapse = ", ")
    ))
  }

  # Filter the selected options
  selected_types <- box_score_types[measure_types]

  # Call fetch_box_score to process data in batches
  fetch_box_score_fix(
    selected_types, game_ids, period, start_range, end_range, batch_size, pause_seconds
  )
}


#' Get Starters from Box Sxore
#'
#' This function gets starters from box score using boxscoretraditionalv3
#'
#' @param lineups_errors A data frame of missing players.
#' @param quarter_start_lineups A data frame containing the starters determined by subs or events.
#'
#' @return Starters that were not determined by subs or events
get_starters_from_boxscore <- function(lineups_errors, quarter_start_lineups) {
  missing_starters_params <- lineups_errors %>%
    select(game_id, period) %>%
    mutate(
      start_range = case_when(
        period == "1" ~ "0",
        period <= "4" ~ as.character(7200 * (as.numeric(period) - 1)),
        TRUE ~ as.character(4 * 7200 + 3000 * (as.numeric(period) - 5))
      ),
      end_range = as.character(as.numeric(start_range) + 300 * 10)
    ) %>%
    distinct()

  all_starters <- missing_starters_params %>%
    pmap_dfr(function(game_id, period, start_range, end_range) {
      nba_box_scores_fix(
        measure_type = "Traditional",
        game_ids = game_id,
        period = period,
        start_range = start_range,
        end_range = end_range
      )
    })

  missing_starters <- all_starters %>%
    mutate(
      sec_played = convert_to_seconds(minutes),
      secs_passed_game = NA_integer_,
      sub = NA_character_,
      person_id = as.character(person_id),
      keep = case_when(
        period <= "4" & sec_played >= 720 ~ TRUE,
        period > "4" & sec_played >= 300 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(if_all(field_goals_made:points, ~ . == 0) & keep) %>%
    select(game_id, period, secs_passed_game, team_id, sub, player_id = person_id)

  missing_starters <- missing_starters %>%
    anti_join(quarter_start_lineups, by = c("player_id", "period"))

  quarter_start_lineups <- bind_rows(quarter_start_lineups, missing_starters)
}

#' Get Starters for each Quarter
#'
#' This function combines player data from substitutions and other events,
#' then adds missing player data to the resulting data frame.
#'
#' @param subbed_players A data frame containing substitution event data
#' @param event_players A data frame containing players involved in events
#' but not substitutions.
#'
#' @return A data frame with the lineups to start each quarter
get_period_starters <- function(data) {

  # Track substitutions made during the game
  subbed_players <- get_subbed_players(data)

  # Identify players involved in events and track their presence in quarters
  event_players <- get_event_players(data, subbed_players)

  period_starters_lineup <- bind_rows(
    subbed_players %>% filter(sub == "out"),
    event_players
  ) %>%
    arrange(game_id, period, team_id)

  # Add missing players
  lineups_errors <- period_starters_lineup %>%
    count(game_id, period, team_id) %>%
    filter(n != 5)

  if (nrow(lineups_errors) > 0) {

    period_starters_lineup <- get_starters_from_boxscore(lineups_errors, period_starters_lineup)
  }

  return(period_starters_lineup)

}





#' Process Data Play-by-Play
#'
#' This function processes play-by-play data to add possessions.
#'
#' @param data A data frame with raw play-by-play data
#' @return A data frame with possessions added to lineups for all play-by-play
#' @export
pbp_data_processing <- function(data) {

  pbp_final <- tryCatch(
    {
      data <- prepare_data_pbp(data)

      possession_base <- build_poss_base(data)

      possession_base <- possession_base %>%
        compute_possession_start_bounds()

      quarter_start_lineups <- get_period_starters(possession_base)

      return(quarter_start_lineups)
    },
    error = function(e) {
      message("Error during data processing ", e$message)
      return(data) # Return the original data if this step fails
    }
  )
}









#' Build Starters Lookup
#'
#' Given a tibble of period‐start lineups, constructs a named list
#' where each key is “game_id__period” and each value is a named list of length 2
#' (team_id → 5‐player integer vector).
#'
#' @param quarter_start_lineups A tibble with columns:
#'   - `game_id` (chr/int)
#'   - `period` (int)
#'   - `team_id` (chr/int)
#'   - `player_id` (int)
#'   Exactly 10 rows per `(game_id,period)`: five players per team.
#' @return A named list. Names are “game_id__period”. Each element is a named list:
#'   `{ team_id → c(player1,…,player5) }`.
build_starters_lookup <- function(quarter_start_lineups) {
  starters_by_team <- quarter_start_lineups %>%
    group_by(game_id, period, team_id) %>%
    summarize(players = list(player_id), .groups = "drop")

  starters_nested <- starters_by_team %>%
    group_by(game_id, period) %>%
    summarize(
      starters_list = list(
        setNames(players, as.character(team_id))
      ),
      .groups = "drop"
    )

  starters_lookup <- starters_nested %>%
    mutate(key = paste0(game_id, "__", period)) %>%
    select(key, starters_list) %>%
    deframe()

  starters_lookup
}


#' Tag Period Starts in PBP
#' Attaches a `starters_list` column to a play‐by‐play tibble,
#' where `starters_list` is non‐NULL exactly on the “start of period” rows
#' (`eventmsgtype == 12`). Requires a `starters_lookup` from
#' `build_starters_lookup()`.
#'
#' @param pbp A tibble of play‐by‐play events with columns at minimum:
#'   - `game_id`, `period`, `eventmsgtype` (12=start)
#'   - `player1_id`, `player2_id`, `team_id`, plus any others.
#' @param starters_lookup A named list from `build_starters_lookup()`.
#' @return The original `pbp` tibble with two new columns:
#'   - `is_start_of_period` (logical)
#'   - `starters_list` (list‐column; named list of length‐2 on start rows, else NULL)
tag_period_starts <- function(pbp, starters_lookup) {
  # 1) First, assemble a “key” column so we can pull from starters_lookup:
  pbp_key <- pbp %>%
    mutate(
      key = paste0(game_id, "__", period)
    )

  # 2) For every row, look up the full five‐man list for that game/period:
  #    (This gives a list‐column, but we’ll zero it out except on event_type == 12)
  pbp_starters <- pbp_key %>%
    mutate(
      all_starters = map(key, ~ starters_lookup[[.x]])
    )

  # 3) Flag “start of period” and keep starters_list only on that row:
  pbp_starters <- pbp_starters %>%
    mutate(
      is_start_of_period = if_else(eventmsgtype == 12, TRUE, FALSE)
    ) %>%
    mutate(
      starters_list = map2(
        all_starters,
        is_start_of_period,
        function(st, flag) {
          if (flag && !is.null(st)) st else NULL
        }
      )
    ) %>%
    select(-key, -all_starters)

  return(pbp_starters)
}

#' Update Current Players
#' @description Internal helper. Given a previous “current_players” named list
#'   and one PBP row, returns the updated named list of two 5‐player vectors.
#'   - Resets if `row$starters_list` is non‐NULL.
#'   - If `eventmsgtype == 8` (sub), removes `player1_id` and appends `player2_id`.
#'   - Otherwise returns `prev_players` unchanged.
#' @param prev_players Named list of length 2: `{team_id → integer[5]}`.
#' @param row A one‐row tibble containing at least:
#'   - `starters_list` (list‐column)
#'   - `eventmsgtype` (int; 8=sub, 12=start)
#'   - `player1_id`, `player2_id`, `team_id`
#' @return Named list of length 2 (team_id → updated integer[5]).
#' @noRd
update_current_players <- function(prev_players, row) {
  # Reset at start of period
  if (!is.null(row$starters_list[[1]])) {
    return(row$starters_list[[1]])
  }

  # Handle substitution
  if (!is.na(row$eventmsgtype) && row$eventmsgtype == 8) {
    sub_out <- row$player1_id
    sub_in <- row$player2_id

    if (!is.na(sub_out) && !is.na(sub_in)) {
      cp <- map(prev_players, ~ .x)
      tid <- as.character(row$team_id)

      if (!tid %in% names(cp)) {
        stop(
          "Team ID ", tid,
          " not found among keys: ", paste(names(cp), collapse = ", ")
        )
      }

      new_vec <- cp[[tid]][cp[[tid]] != sub_out]

      if (length(new_vec) != 4) {
        warning(
          "Expected to remove exactly one player (", sub_out,
          ") from team ", tid,
          ".  Length after removal = ", length(new_vec),
          " (was ", paste(cp[[tid]], collapse = ","), ")."
        )
      }

      new_vec <- c(new_vec, sub_in)

      if (length(new_vec) != 5) {
        stop(
          "After substituting in ", sub_in,
          " for ", sub_out, " on team ", tid,
          ", the resulting roster has length ", length(new_vec),
          " (expected 5)."
        )
      }

      cp[[tid]] <- new_vec
      return(cp)
    }
  }

  # No change
  prev_players
}

#' Build Current Players Column
#' @description Walks through each row of `pbp_starters` (output of `tag_period_starts`)
#'   and constructs a `current_players` list‐column where each element is a
#'   named list of two 5‐player vectors (team_id → on‐court players).
#' @param pbp2 A tibble from `tag_period_starts()`, containing at least:
#'   - `game_id`, `period`, `eventmsgtype`, `player1_id`, `player2_id`, `team_id`
#'   - `starters_list` (non‐NULL exactly on start‐of‐period rows)
#' @return The original tibble with a new column `current_players`.
#' @export
build_current_players <- function(pbp_starters) {
  n <- nrow(pbp_starters)
  cp_list <- vector("list", n)

  for (i in seq_len(n)) {
    rowi <- pbp2[i, ]

    # If first row of new game/period, must reset
    if (
      i == 1 ||
      pbp_starters$game_id[i] != pbp_starters$game_id[i - 1] ||
      pbp_starters$period[i] != pbp_starters$period[i - 1]
    ) {
      init <- rowi$starters_list[[1]]
      if (is.null(init)) {
        stop(
          "build_current_players(): Row ", i,
          " is the first row of game ", pbp_starters$game_id[i],
          " period ", pbp_starters$period[i],
          " but starters_list is NULL. Make sure eventmsgtype==12 rows have starters_list."
        )
      }
      cp_list[[i]] <- init
    } else {
      cp_list[[i]] <- update_current_players(cp_list[[i - 1]], rowi)
    }
  }

  pbp_starters %>% mutate(current_players = cp_list)
}

#' Build Lineup ID Strings
#' @description Given a tibble with `current_players` (named list of two vectors)
#'   and `team_id`, creates `lineup_id_offense` and `lineup_id_defense` as
#'   hyphen‐joined, sorted player IDs.
#' @param pbp_lineups A tibble with columns:
#'   - `current_players` (list‐column; `{team_id → integer[5]}`)
#'   - `team_id` (must match names of `current_players` elements)
#' @return The same tibble with two new columns:
#'   - `lineup_id_offense` (chr)
#'   - `lineup_id_defense` (chr)
#' @export
build_lineup_ids <- function(pbp_lineups) {
  mutate(
    lineup_id_offense = map2_chr(
      current_players, team_id,
      ~ {
        tid     <- as.character(.y)
        # If the offense team_id isn’t in current_players, return NA
        if (!tid %in% names(.x)) return(NA_character_)
        players <- sort(.x[[tid]])
        stringr::str_c(players, collapse = "-")
      }
    ),
    lineup_id_defense = map2_chr(
      current_players, team_id,
      ~ {
        teams <- names(.x)
        off   <- as.character(.y)
        # Make sure 'off' is actually on-court
        if (!off %in% teams) return(NA_character_)
        # Take the other team
        def_team <- teams[teams != off]
        # If there isn’t exactly one “other” team, return NA
        if (length(def_team) != 1) return(NA_character_)
        players <- sort(.x[[def_team]])
        stringr::str_c(players, collapse = "-")
      }
    )
  )
}








# starters_nested$starters_list[[3]]
# starters_lookup[["0022401162__3"]]



tag_period_starts <- function(pbp, starters_lookup) {
  # 1) First, assemble a “key” column so we can pull from starters_lookup:
  pbp_key <- pbp %>%
    mutate(
      key = paste0(game_id, "__", period)
    )

  # 2) For every row, look up the full five‐man list for that game/period:
  #    (This gives a list‐column, but we’ll zero it out except on event_type == 12)
  pbp_starters <- pbp_key %>%
    mutate(
      all_starters = map(key, ~ starters_lookup[[.x]])
    )

  # 3) Flag “start of period” and keep starters_list only on that row:
  pbp_starters <- pbp_starters %>%
    mutate(
      is_start_of_period = if_else(eventmsgtype == 12, TRUE, FALSE)
    ) %>%
  mutate(
    starters_list = map2(
      all_starters,
      is_start_of_period,
      function(st, flag) {
        if (flag && !is.null(st)) st else NULL
      }
      )
    ) %>%
    select(-key, -all_starters)

  return(pbp_starters)
}

update_current_players <- function(prev_players, row) {
  # 1) Reset if this row has a non‐NULL starters_list (start of period)
  if (!is.null(row$starters_list[[1]])) {
    return(row$starters_list[[1]])
  }

  # 2) Substitution logic: eventmsgtype == 8
  if (!is.na(row$eventmsgtype) && row$eventmsgtype == 8) {
    # On a sub row, player1_id is the one going out; player2_id is the one coming in.
    sub_out <- row$player1_id
    sub_in  <- row$player2_id

    # Both should be non-NA to carry out a swap
    if (!is.na(sub_out) && !is.na(sub_in)) {
      # Deep copy prev_players so we don’t modify it in place
      cp <- map(prev_players, ~ .x)
      tid <- as.character(row$team_id)

      # Sanity check: the team must exist in prev_players
      if (!tid %in% names(cp)) {
        stop(
          "Team ID ", tid,
          " not found among keys: ", paste(names(cp), collapse = ", ")
        )
      }

      # Remove the outgoing player
      new_vec <- cp[[tid]][cp[[tid]] != sub_out]

      # After removal, length should be 4 (one down from 5)
      if (length(new_vec) != 4) {
        warning(
          "Expected to remove exactly one player (", sub_out,
          ") from team ", tid,
          ".  Length after removal = ", length(new_vec),
          " (was ", paste(cp[[tid]], collapse = ","), ")."
        )
      }

      # Add the incoming player
      new_vec <- c(new_vec, sub_in)

      # Now length should be back to 5
      if (length(new_vec) != 5) {
        stop(
          "After substituting in ", sub_in,
          " for ", sub_out, " on team ", tid,
          ", the resulting roster has length ", length(new_vec),
          " (expected 5)."
        )
      }

      cp[[tid]] <- new_vec
      return(cp)
    }
  }

  # 3) Otherwise, no change
  prev_players
}

build_current_players <- function(pbp2) {
  n <- nrow(pbp2)
  cp_list <- vector("list", n)

  for (i in seq_len(n)) {
    rowi <- pbp2[i, ]

    # If this is the very first row of the tibble, or the game/period changed,
    # initialize from starters_list.
    if (
      i == 1 ||
      pbp2$game_id[i]   != pbp2$game_id[i - 1] ||
      pbp2$period[i]    != pbp2$period[i - 1]
    ) {
      init <- rowi$starters_list[[1]]
      if (is.null(init)) {
        stop(
          "build_current_players(): Row ", i,
          " is the first row of game ", pbp2$game_id[i],
          " period ", pbp2$period[i],
          " but starters_list is NULL. Make sure eventmsgtype==12 rows have starters_list."
        )
      }
      cp_list[[i]] <- init
    } else {
      cp_list[[i]] <- update_current_players(cp_list[[i - 1]], rowi)
    }
  }

  pbp2 %>% mutate(current_players = cp_list)
}


# # Example:
# pbp2 <- tag_period_starts(df, starters_lookup)
# pbp3 <- build_current_players(pbp2)




# pbp4 <- pbp3 %>%
#   mutate(
#     lineup_id_offense = map2_chr(
#       current_players, team_id,
#       ~ {
#         tid     <- as.character(.y)
#         # If the offense team_id isn’t in current_players, return NA
#         if (!tid %in% names(.x)) return(NA_character_)
#         players <- sort(.x[[tid]])
#         stringr::str_c(players, collapse = "-")
#       }
#     ),
#     lineup_id_defense = map2_chr(
#       current_players, team_id,
#       ~ {
#         teams <- names(.x)
#         off   <- as.character(.y)
#         # Make sure 'off' is actually on-court
#         if (!off %in% teams) return(NA_character_)
#         # Take the other team
#         def_team <- teams[teams != off]
#         # If there isn’t exactly one “other” team, return NA
#         if (length(def_team) != 1) return(NA_character_)
#         players <- sort(.x[[def_team]])
#         stringr::str_c(players, collapse = "-")
#       }
#     )
#   )

