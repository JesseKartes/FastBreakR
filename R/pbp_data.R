
# Shot cutoffs
heave_distance_cutoff <- 40
heave_time_cutoff <- 2
at_rim_cutoff <- 4
short_mid_range_cutoff <- 14

# Shot type strings
corner_3_string <- "corner 3"
arc_3_string <- "arc 3"
unknown_shot_distance_string <- "unknown distance"
at_rim_string <- "at rim"
short_mid_range_string <- "short mid range"
long_mid_range_string <- "long mid range"
heave_string <- "heave"

# Foul type strings
personal_foul_type_string <- "personal foul"
shooting_foul_type_string <- "shooting foul"
loose_ball_foul_type_string <- "loose ball foul"
offensive_foul_type_string <- "offensive foul"
away_from_play_foul_type_string <- "away from play foul"
charge_foul_type_string <- "charge foul"
personal_block_type_string <- "personal block foul"
personal_take_type_string <- "personal take foul"
shooting_block_type_string <- "shooting block foul"
clear_path_foul_type_string <- "clear path foul"
defensive_3_seconds_foul_type_string <- "defensive 3 seconds violation"
flagrant_1_foul_type_string <- "flagrant 1 foul"
flagrant_2_foul_type_string <- "flagrant 2 foul"
double_foul_type_string <- "double foul"
inbound_foul_type_string <- "inbound foul"
transition_take_type_string <- "transition take foul"

offensive_possession_string <- "OffPoss"
defensive_possession_string <- "DefPoss"
second_chance_string <- "SecondChance"
penalty_string <- "Penalty"


seconds_played_offense_string <- "seconds_played_offense"
seconds_played_defense_string <- "seconds_played_defense"

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


get_all_events_at_current_time <- function(i, df) {
  # grab the key fields for row i
  gid <- df$game_id[i]
  per <- df$period[i]
  secs <- df$secs_remain_quarter[i]

  # filter for same game, period, and clock time; then sort by your existing order
  df %>%
    filter(
      game_id == gid,
      period == per,
      secs_remain_quarter == secs
    ) %>%
    arrange(game_id, period, event_index)
}


# Ejection
data_ejection <- function(data) {

  data %>%
    mutate(
      is_ejection = eventmsgtype == 11
    )

}


# End of period
data_end_of_period <- function(data) {

  data %>%
    mutate(
      end_of_period = eventmsgtype == 13
    )

}


# Timeout
data_timeout <- function(data) {

  data %>%
    mutate(
      is_timeout = eventmsgtype == 9
    )

}


# Replay
data_replay <- function(data) {

  data %>%
    mutate(
      is_replay = eventmsgtype == 18,
      support_ruling = is_replay & eventmsgactiontype == 4,
      overturn_ruling = is_replay & eventmsgactiontype == 5,
      ruling_stands = is_replay & eventmsgactiontype == 6
    )

}


# Violation
data_violation <- function(data) {

  data %>%
    mutate(
      is_violation = eventmsgtype == 7,
      is_delay_of_game = is_violation & eventmsgactiontype == 1,
      is_goaltend_violation = is_violation & eventmsgactiontype == 2,
      is_lane_violation = is_violation & eventmsgactiontype == 3,
      is_jumpball_violation = is_violation & eventmsgactiontype == 4,
      is_kicked_ball_violation = is_violation & eventmsgactiontype == 5,
      is_double_lane_violation = is_violation & eventmsgactiontype == 6
    )

}


# Fouls
data_fouls <- function(data) {

  data %>%
    mutate(
      is_foul = eventmsgtype == 6,
      number_of_fta_for_foul = case_when(
        str_detect(desc, "\\(1 FTA\\)") ~ 1,
        str_detect(desc, "\\(2 FTA\\)") ~ 2,
        str_detect(desc, "\\(3 FTA\\)") ~ 3,
        TRUE ~ NA_integer_
      ),
      is_personal_foul = is_foul & eventmsgactiontype == 1,
      is_shooting_foul = is_foul & eventmsgactiontype == 2,
      is_loose_ball_foul = is_foul & eventmsgactiontype == 3,
      is_offensive_foul = is_foul & eventmsgactiontype == 4,
      is_inbound_foul = is_foul & eventmsgactiontype == 5,
      is_away_from_play_foul = is_foul & eventmsgactiontype == 6,
      is_clear_path_foul = is_foul & eventmsgactiontype == 9,
      is_double_foul = is_foul & eventmsgactiontype == 10,
      is_technical = is_foul & eventmsgactiontype %in% c(11, 12, 13, 18, 19, 25, 30),
      is_flagrant1 = is_foul & eventmsgactiontype == 14,
      is_flagrant2 = is_foul & eventmsgactiontype == 15,
      is_flagrant = is_flagrant1 | is_flagrant2,
      is_double_technical = is_foul & eventmsgactiontype == 16,
      is_defensive_3_seconds = is_foul & eventmsgactiontype == 17,
      is_delay_of_game = is_foul & eventmsgactiontype == 18,
      is_charge = is_foul & eventmsgactiontype == 26,
      is_personal_block_foul = is_foul & eventmsgactiontype == 27,
      is_personal_take_foul = is_foul & eventmsgactiontype == 28,
      is_shooting_block_foul = is_foul & eventmsgactiontype == 29,
      is_transition_take_foul = is_foul & eventmsgactiontype == 31,
      counts_as_personal_foul = is_personal_foul | is_shooting_foul | is_loose_ball_foul |
        is_offensive_foul | is_inbound_foul | is_away_from_play_foul |
        is_clear_path_foul | is_double_foul | is_flagrant |
        is_charge | is_personal_block_foul | is_personal_take_foul |
        is_shooting_block_foul | is_transition_take_foul,
      counts_towards_penalty = is_personal_foul | is_shooting_foul | is_loose_ball_foul |
        is_inbound_foul | is_away_from_play_foul | is_clear_path_foul |
        is_flagrant | is_personal_block_foul | is_personal_take_foul |
        is_shooting_block_foul | is_transition_take_foul,
      foul_type_string = case_when(
        is_personal_foul ~ personal_foul_type_string,
        is_shooting_foul ~ shooting_foul_type_string,
        is_loose_ball_foul ~ loose_ball_foul_type_string,
        is_offensive_foul ~ offensive_foul_type_string,
        is_inbound_foul ~ inbound_foul_type_string,
        is_away_from_play_foul ~ away_from_play_foul_type_string,
        is_clear_path_foul ~ clear_path_foul_type_string,
        is_double_foul ~ double_foul_type_string,
        is_flagrant1 ~ flagrant_1_foul_type_string,
        is_flagrant2 ~ flagrant_2_foul_type_string,
        is_defensive_3_seconds ~ defensive_3_seconds_foul_type_string,
        is_charge ~ charge_foul_type_string,
        is_personal_block_foul ~ personal_block_type_string,
        is_personal_take_foul ~ personal_take_type_string,
        is_shooting_block_foul ~ shooting_block_type_string,
        is_transition_take_foul ~ transition_take_type_string,
        TRUE ~ NA_character_
      )
    )

}


# Free throws
free_throw_type_helper <- function(i, df) {
  row <- df[i, ]

  if (!isTRUE(row$is_fta)) {
    return(NA_character_)
  }

  # 1) special override for that one known mis-logged game/event
  if (row$game_id == "0022301195" && row$eventnum == 138) {
    return("Penalty")
  }
  # 2) technical free throws
  if (isTRUE(row$is_technical_ft)) {
    return("Technical")
  }

  # 3) how many FTs did this trip award?
  num_fts <- row$num_ft_for_trip
  if (is.na(num_fts)) num_fts <- row$shot_value

  # 4) single-shot branch: look for an And-1
  if (num_fts == 1) {
    current_secs <- row$secs_remain_quarter
    shot_idx     <- NA_integer_
    # walk backwards at the same clock time
    for (j in seq(i - 1, 1)) {
      if (df$secs_remain_quarter[j] != current_secs) break
      # find a made non-FT shot
      if (isTRUE(df$is_fgm[j]) && !isTRUE(df$is_fta[j])) {
        shot_idx <- j
        break
      }
    }
    if (!is.na(shot_idx)) {
      if (row$player1_id == df$player1_id[shot_idx]) {
        return(stringr::str_glue("{df$shot_value[shot_idx]}pt And 1"))
      } else {
        return("1 Shot Away From Play")
      }
    } else {
      return("1 Shot Away From Play")
    }
  }

  # 5) otherwise, find the foul that led to these FTs
  prev_fouls <- which(df$is_foul[1:(i-1)])
  if (length(prev_fouls) == 0) {
    return("Penalty")
  }
  foul_idx <- tail(prev_fouls, 1)
  foul     <- df[foul_idx, ]

  # shooting foul?
  if (isTRUE(foul$is_shooting_foul) || isTRUE(foul$is_shooting_block_foul)) {
    return(stringr::str_glue("{num_fts}pt Shooting Foul"))
  }
  # flagrant?
  if (isTRUE(foul$is_flagrant)) {
    if (is.na(num_fts)) num_fts <- 2
    return(stringr::str_glue("{num_fts} Shot Flagrant"))
  }
  # away-from-play?
  if (isTRUE(foul$is_away_from_play_foul)) {
    return(stringr::str_glue("{num_fts} Shot Away From Play"))
  }
  # inbound?
  if (isTRUE(foul$is_inbound_foul)) {
    return(stringr::str_glue("{num_fts} Shot Inbound Foul"))
  }
  # clear-path?
  if (isTRUE(foul$is_clear_path_foul)) {
    return(stringr::str_glue("{num_fts} Shot Clear Path"))
  }
  # 3-FTA flagged as a 3-pt shooting foul
  if (num_fts == 3) {
    return("3pt Shooting Foul")
  }

  # fallback
  "Penalty"
}

data_free_throws <- function(data) {

  data %>%
    mutate(
      is_fta = eventmsgtype == 3,
      is_ftm = is_fta & !str_detect(desc, " Missed"),
      is_ft_1_of_1 = eventmsgactiontype == 10 | eventmsgactiontype == 20,
      is_ft_1_of_2 = eventmsgactiontype == 11,
      is_ft_2_of_2 = eventmsgactiontype == 12,
      is_ft_1_of_3 = eventmsgactiontype == 13 | eventmsgactiontype == 27,
      is_ft_2_of_3 = eventmsgactiontype == 14,
      is_ft_3_of_3 = eventmsgactiontype == 15,
      is_technical_ft = is_fta & str_detect(desc, " Technical"),
      is_flagrant_ft = is_fta & str_detect(desc, " Flagrant"),
      is_first_ft = is_ft_1_of_1 | is_ft_1_of_2 | is_ft_1_of_3,
      is_end_ft = !is_flagrant_ft & (is_ft_1_of_1 | is_ft_2_of_2 | is_ft_3_of_3),
      num_ft_for_trip = case_when(
        str_detect(desc, "of 1") ~ 1,
        str_detect(desc, "of 2") ~ 2,
        str_detect(desc, "of 3") ~ 3,
        TRUE ~ NA_real_
      )
    )

}

data_free_throw_types <- function(data) {

  data %>%
    mutate(
      free_throw_type = map_chr(
        event_index,
        ~ free_throw_type_helper(.x, pick(everything()))
      )
    )

}


# Field goals
extract_distance_from_desc <- function(desc) {
  if(is.na(desc)) return(NA_real_)

  tryCatch({
    # Split by single quote, take first part, split by space, take last element
    parts <- str_split(desc, "'", simplify = TRUE)[1]
    if(!is.na(parts) && parts != "") {
      words <- str_split(parts, " ", simplify = TRUE)
      last_word <- words[length(words)]
      # Only try to convert if it looks like a number
      if(str_detect(last_word, "^\\d+$")) {
        return(as.numeric(last_word))
      }
    }
    return(NA_real_)
  }, error = function(e) {
    return(NA_real_)
  })
}

find_putback_event <- function(i, df) {
  row <- df[i, ]

  # 1) exclude assisted or 3-pt attempts, guard NAs -- only include fgms
  if ( isTRUE(row$is_assisted) ||
       isTRUE(row$shot_value == 3) ||
       !isTRUE(row$is_fgm)) {
    return(FALSE)
  }

  # 2) need at least one prior event
  if (i <= 1) return(FALSE)

  prev <- df[i - 1, ]
  prev_is_shoot_foul <- isTRUE(prev$is_shooting_foul) ||
    isTRUE(prev$is_shooting_block_foul)
  prev_is_goaltend   <- isTRUE(prev$is_offensive_goaltending)

  # skip if foul/violation at same second (guard NAs again)
  if ( (prev_is_shoot_foul || prev_is_goaltend) &&
       isTRUE(row$secs_remain_quarter == prev$secs_remain_quarter) ) {
    if (i <= 2) return(FALSE)
    prev <- df[i - 2, ]
  }

  # 3) must be a real OREB by same player
  if (!isTRUE(prev$is_real_rebound) ||
      !isTRUE(prev$is_oreb) ||
      prev$player1_id != row$player1_id) {
    return(FALSE)
  }

  # 4) time gap ≤ 2 seconds (guard NA)
  gap <- prev$secs_remain_quarter - row$secs_remain_quarter
  return(!is.na(gap) && gap <= 2)
}

check_make_ends_possession_when_1_foul <- function(row, events, foul) {
  shooter_team <- row$team_id
  foul_team    <- foul$team_id

  # 1a) flagrant on the opponent → possession ends
  if (isTRUE(foul$is_flagrant) && foul_team != shooter_team) {
    return(TRUE)
  }

  # 1b) other made FGs by shooter at that second
  other_makes <- events %>%
    filter(
      is_fga, is_fgm,
      team_id   == shooter_team,
      eventnum != row$eventnum
    )

  # only proceed if the foul wasn’t on the shooter’s own team
  if (shooter_team != foul_team) {

    # 1c) any 1-of-1 free throws (non‐tech) at that same second?
    ft1s <- events %>%
      filter(
        is_fta,
        (is_ft_1_of_1), # removed | is_ft_1pt
        !isTRUE(is_technical_ft)
      )

    # 1d) exactly one make + one FT?
    if (nrow(other_makes) == 1 && nrow(ft1s) == 1) {
      # if the FT went to the OTHER shooter, that ends possession
      if (ft1s$player1_id[1] == other_makes$player1_id[1]) {
        return(TRUE)
      }
      # if it went to *this* shooter, THEN possession continues
      if (ft1s$player1_id[1] == row$player1_id) {
        return(FALSE)
      }
    }

    # 1e) more than one FT: if *any* FT belonged to shooter’s team, possession ends
    if (nrow(ft1s) > 0 && any(ft1s$team_id == shooter_team)) {
      return(TRUE)
    }

    # 1f) no FTs: check lane‐violation or offensive‐goaltend → possession ends
    if (any(events$is_turnover &
            (events$is_lane_violation | events$is_offensive_goaltending))) {
      return(TRUE)
    }
    # double‐lane‐violation also ends
    if (any(events$is_violation & events$is_double_lane_violation)) {
      return(TRUE)
    }
  }

  # default: possession continues
  FALSE
}

check_make_ends_possession_when_not_1_foul <- function(row, events, fouls) {
  shooter_team <- row$team_id

  # 2a) if shooter’s team wasn’t among the fouls at all
  if (!(shooter_team %in% fouls$team_id)) {
    ft1s <- events %>%
      filter(
        is_fta,
        (is_ft_1_of_1), # removed | is_ft_1pt
        !isTRUE(is_technical_ft)
      )

    # exactly one FT1 by shooter’s team → possession ends
    if (nrow(ft1s) == 1 && ft1s$team_id[1] == shooter_team) {
      return(TRUE)
    }
    # multiple FT1s: if any went to this shooter → possession ends
    if (nrow(ft1s) > 1 && any(ft1s$player1_id == row$player1_id)) {
      return(TRUE)
    }

  } else {
    # 2b) shooter’s team was fouled: if opponent got exactly 1 FTA → poss ends
    opp_fouls <- fouls %>% filter(team_id != shooter_team)
    if (any(opp_fouls$number_of_fta_for_foul == 1)) {
      return(TRUE)
    }
  }

  # default: possession continues
  FALSE
}

is_make_not_end_possession_helper <- function(i, df) {
  row <- df[i, ]
  # only care about made FGs
  if (!isTRUE(row$is_fgm)) return(FALSE)

  # 3a) fetch all events at this second
  evts <- get_all_events_at_current_time(i, df)
  # 3b) pick out non‐delay, non‐tech fouls
  fouls <- evts %>% filter(is_foul, !is_delay_of_game, !is_technical)

  # zero non-tech, non-delay fouls → make ends possession
  if (nrow(fouls) == 0) {
    return(FALSE)
  }

  if (nrow(fouls) == 1) {
    return(
      check_make_ends_possession_when_1_foul(row, evts, fouls[1, ])
    )
  } else {
    return(
      check_make_ends_possession_when_not_1_foul(row, evts, fouls)
    )
  }
}

is_and1_helper <- function(i, df) {
  # 1) must be a non-ending make
  if (!isTRUE(df$is_make_that_does_not_end_possession[i])) {
    return(FALSE)
  }
  # 2) collect all FTs at the same second
  sec <- df$secs_remain_quarter[i]
  fts  <- df %>%
    filter(
      secs_remain_quarter == sec,
      is_fta == TRUE
    )
  # 3) check for any And-1 belonging to the same player
  any(
    str_detect(fts$free_throw_type, stringr::fixed("And 1")) &
      fts$player1_id == df$player1_id[i]
  )
}


data_field_goals <- function(data) {

  data %>%
    mutate(
      is_fga = eventmsgtype %in% c(1, 2),
      is_fgm = eventmsgtype == 1,
      shot_value = case_when(
        is_fga & str_detect(desc, regex("3pt shot", ignore_case = TRUE)) ~ 3,
        is_fga & !str_detect(desc, regex("3pt shot", ignore_case = TRUE)) ~ 2,
        TRUE ~ NA_integer_
      ),
      is_blocked = if_else(is_fga & !player3_id == "", TRUE, FALSE),
      is_assisted = if_else(is_fgm & !player2_id == "", TRUE, FALSE),
      shot_distance = case_when(
        is_fga & !loc_x == 0 & !loc_y == 0 ~ round(sqrt(loc_x^2 + loc_y^2) / 10, 1),
        is_fga ~ map_dbl(desc, extract_distance_from_desc),
        TRUE ~ NA_real_
      ),
      is_heave = if_else(
        shot_distance > heave_distance_cutoff &
          secs_remain_quarter < heave_time_cutoff, TRUE, FALSE
      ),
      is_corner_3 = if_else(
        is_fga & shot_value == 3 & loc_y <= 87, TRUE, FALSE
      ),
      shot_type = case_when(
        is_corner_3 ~ corner_3_string,
        is_heave ~ heave_string,
        shot_value == 3 & !is_corner_3 ~ arc_3_string,
        !is_fga ~ NA_character_,
        is.na(shot_distance) ~ unknown_shot_distance_string,
        shot_distance < at_rim_cutoff ~ at_rim_string,
        shot_distance < short_mid_range_cutoff ~ short_mid_range_string,
        TRUE ~ long_mid_range_string
      )
    )

}

data_putbacks <- function(data) {

  data %>%
    mutate(
      is_putback = map_lgl(
        event_index,
        ~ find_putback_event(.x, pick(everything()))
      )
    )

}

data_make_not_end_possession <- function(data) {
  data %>%
    mutate(
      is_make_that_does_not_end_possession =
        map_lgl(
          row_number(),
          ~ is_make_not_end_possession_helper(.x, pick(everything()))
        )
    )
}

data_and1 <- function(data) {

  data %>%
    mutate(
      is_and1 =
        map_lgl(
          row_number(),
          ~ is_and1_helper(.x, pick(everything()))
        )
    )

}


# Jump ball
data_jump_ball <- function(data) {

  data %>%
    mutate(
      is_jump_ball = eventmsgtype == 10,
      jump_ball_winner = if_else(is_jump_ball, team_id, NA_character_)
    )

  # data %>%
  #   mutate(
  #     is_jump_ball = eventmsgtype == 10,
  #     jump_ball_winner = if_else(is_jump_ball, team_id, NA_character_),
  #     jump_ball = map2(
  #       is_jump_ball, jump_ball_winner, ~list(is_jump_ball = .x, jump_ball_winner = .y)
  #     )
  #   ) %>%
  #   select(-is_jump_ball, -jump_ball_winner)

}


# Turnover
data_turnover <- function(data) {

  data %>%
    mutate(
      is_turnover = eventmsgtype == 5,
      is_no_turnover = is_turnover & eventmsgactiontype == 0,
      is_steal = is_turnover & !player3_id == "",
      is_bad_pass = is_turnover & eventmsgactiontype == 1  & is_steal,
      is_lost_ball = is_turnover & eventmsgactiontype == 2  & is_steal,
      is_travel = is_turnover & eventmsgactiontype == 4,
      is_3_second_violation = is_turnover & eventmsgactiontype == 8,
      is_shot_clock_violation = is_turnover & eventmsgactiontype == 11,
      is_offensive_goaltending = is_turnover & eventmsgactiontype == 15,
      is_lane_violation = is_turnover & eventmsgactiontype == 17,
      is_kicked_ball = is_turnover & eventmsgactiontype == 19,
      is_step_out_of_bounds = is_turnover & eventmsgactiontype == 39,
      is_lost_ball_out_of_bounds = is_turnover & (
        eventmsgactiontype == 40 |
          (eventmsgactiontype == 2 & !is_steal)
      ),
      is_bad_pass_out_of_bounds  = is_turnover & (
        eventmsgactiontype == 45 |
          (eventmsgactiontype == 1 & !is_steal)
      )
    )

}


# Rebound
missed_shot_event <- function(i, df) {
  if (i <= 1) {
    return(0)
  }

  prev <- df[i - 1, ]
  # a) direct miss
  if ((prev$is_fga && !prev$is_fgm) ||
      (prev$is_fta && !prev$is_ftm)) {
    return(i - 1)
  }

  # b) shot-clock violation turnover
  if (prev$is_turnover && prev$is_shot_clock_violation && i > 2) {
    cand <- df[i - 2, ]
    if ((cand$is_fga && !cand$is_fgm) ||
        (cand$is_fta && !cand$is_ftm)) {
      return(i - 2)
    }
  }

  # c) jump ball + skip subs/timeouts
  if (prev$is_jump_ball) {
    j <- i - 2
    while (j > 1 && df$eventmsgtype[j] %in% c(8, 9)) {
      j <- j - 1
    }
    if (j >= 1 && ((df$is_fga[j] && !df$is_fgm[j]) ||
                   (df$is_fta[j] && !df$is_ftm[j]))) {
      return(j)
    }
  }

  # nothing matched → indicate “not found”
  return(0)
}

data_turnover_placeholder <- function(data) {

  # 1) find all times where there's a turnover of the two types
  placeholder_times <- data %>%
    data_turnover() %>%
    filter(
      is_turnover,
      is_shot_clock_violation | is_kicked_ball
    ) %>%
    distinct(game_id, period, secs_remain_quarter) %>%
    mutate(placeholder_flag = TRUE)

  # 2) join back and flag the team‐rebounds at those times
  data %>%
    left_join(
      placeholder_times,
      by = c("game_id", "period", "secs_remain_quarter")
    ) %>%
    mutate(
      is_rebound = (eventmsgtype == 4),
      is_team_rebound = is_rebound & player1_id == 0,
      is_turnover_placeholder = is_team_rebound & coalesce(placeholder_flag, FALSE)
    ) %>%
    select(-placeholder_flag)
} ## is this needed?

data_rebound <- function(data) {

  data %>%
    mutate(
      is_rebound = eventmsgtype == 4,
      missed_shot = map(event_index, ~ {
        idx <- missed_shot_event(.x, pick(everything()))
        if (idx == 0) {
          list(team_id = NA_character_,
               player1_id = NA_character_,
               secs_remain_quarter = NA_integer_)
        } else {
          evt <- pick(everything())[idx, ]
          list(team_id = evt$team_id,
               player1_id = evt$player1_id,
               secs_remain_quarter = evt$secs_remain_quarter)
        }
      }),
      is_oreb = is_rebound & team_id == map_chr(missed_shot, "team_id"),
      is_placeholder = is_rebound & eventmsgactiontype != 0 & player1_id == 0,
      is_turnover_placeholder = map2_lgl(
        event_index,
        player1_id,
        ~ {
          if (!(eventmsgtype[.x] == 4 && .y == 0)) return(FALSE)
          events_at_time <- get_all_events_at_current_time(.x, pick(everything()))
          any(
            events_at_time$is_turnover &
              (events_at_time$is_shot_clock_violation | events_at_time$is_kicked_ball)
          )
        }
      ),
      is_non_live_ft_placeholder = (is_fta & !is_ftm) & !is_end_ft,
      is_buzzer_beater_placeholder =
        is_rebound & clock %in% c("00:00.0", "0:00") & player1_id == "0" & (
          # a) if the very next event is a replay, require the *second* next to be end_of_period
          (lead(is_replay) & lead(end_of_period, 2)) |
            # b) else require the very next to be end_of_period
            !lead(is_replay) & lead(end_of_period) |
            # c) or if there is no “next” event here (i.e. at end of group), treat it as placeholder
            (is.na(lead(is_replay)) & is.na(lead(end_of_period))) ## check this
        ),
      is_buzzer_beater_rebound_at_shot_time =
        # must be a team rebound
        is_rebound & player1_id == "0"
      # at exactly the same second as the missed shot
      & secs_remain_quarter == map_int(missed_shot, "secs_remain_quarter")
      # and that second is <= 3
      & map_int(missed_shot, "secs_remain_quarter") <= 3
      # and the next non-replay event is EndOfPeriod
      & (
        (lead(is_replay) == TRUE  & lead(end_of_period, 2) == TRUE) |
          (lead(is_replay) == FALSE & lead(end_of_period) == TRUE)
      ),
      self_reb = is_rebound & player1_id == map_chr(missed_shot, "player1_id"),
      is_real_rebound = is_rebound &
        !( is_placeholder
           | is_buzzer_beater_placeholder
           | is_turnover_placeholder
           | is_non_live_ft_placeholder
           | is_buzzer_beater_rebound_at_shot_time
        )
    )

}


# Substitution
data_substitution <- function(data) {

  data %>%
    mutate(
      is_sub = eventmsgtype == 8,
      outgoing_player_id = if_else(is_sub, player1_id, NA_character_),
      incoming_player_id = if_else(is_sub, player2_id, NA_character_),
    )

}


# Start of period
build_player_team_map <- function(data) {
  player_team_map <- data %>%
    filter(!(is_jump_ball & lag(start_of_period))) %>%
    transmute(player_id = player1_id, team_id) %>%
    filter(player_id != 0 & player_id != team_id) %>%
    distinct() %>%
    deframe()

  return(player_team_map)
}

get_subbed_players <- function(data) {
  # Subs in period
  subbed_players <- data %>%
    filter(is_sub) %>%
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

get_event_players <- function(data, subbed_players) {
  player_team_map <- build_player_team_map(data)

  event_players <- data %>%
    filter(
      !(is_sub | is_technical | is_ejection)
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


fetch_lineups_box_score <- function(measure_type,
                                    game_id,
                                    period,
                                    start_range,
                                    end_range) {
  message(glue("Fetching game {game_id}"))

  tryCatch(
    {
      # for each measure type, pull & process the box score
      data_frames <- map(measure_type, function(measure) {
        endpoint  <- measure$endpoint
        list_name <- measure$list_name

        headers <- generate_headers_stats()
        url     <- paste0("https://stats.nba.com/stats/", endpoint)
        params  <- generate_params_box_scores_fix(game_id, start_range, end_range)
        data    <- get_data(url, headers, params)

        # unpack & clean both teams
        process_team_data <- function(team_data, game_id, team_id, location) {
          team_data %>%
            unnest_wider(statistics) %>%
            clean_names() %>%
            mutate(
              game_id = game_id,
              team_id = team_id,
              location = location
            )
        }

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

        bind_rows(away_df, home_df) %>%
          select(game_id, location, team_id, everything())
      })

      # join all measures & tag the period
      join_cols <- c("game_id", "person_id")
      join_data_frames(data_frames, join_cols) %>%
        mutate(period = period)
    },
    error = function(e) {
      message(glue("Error fetching data for Game ID {game_id}: {e$message}"))
      tibble()
    }
  )
}

nba_lineups_box_score <- function(game_ids,
                                  measure_types,
                                  period,
                                  start_range,
                                  end_range) {
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
  fetch_lineups_box_score(
    selected_types, game_ids, period, start_range, end_range
  )
}



get_starters_from_boxscore <- function(invalid_lineups, period_starters) {
  missing_starters_params <- invalid_lineups %>%
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
      nba_lineups_box_score(
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
      team_id = as.character(team_id),
      keep = case_when(
        period <= "4" & sec_played >= 720 ~ TRUE,
        period > "4" & sec_played >= 300 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(if_all(field_goals_made:points, ~ . == 0) & keep) %>%
    select(game_id, period, secs_passed_game, team_id, sub, player_id = person_id)

  missing_starters <- missing_starters %>%
    anti_join(period_starters, by = c("player_id", "period"))

  period_starters <- bind_rows(period_starters, missing_starters)
}

get_period_starters <- function(data) {

  # Track substitutions made during the game
  subbed_players <- get_subbed_players(data)

  # Identify players involved in events and track their presence in quarters
  event_players <- get_event_players(data, subbed_players)

  period_starters <- bind_rows(
    subbed_players %>% filter(sub == "out"),
    event_players
  ) %>%
    arrange(game_id, period, team_id)

  # Add missing players
  invalid_lineups <- period_starters %>%
    count(game_id, period, team_id) %>%
    filter(n != 5)

  if (nrow(invalid_lineups) > 0) {

    period_starters <- get_starters_from_boxscore(invalid_lineups, period_starters)
  }

  return(period_starters)

}

build_starters_lookup <- function(period_starters) {
  starters_by_team <- period_starters %>%
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

  return(starters_lookup)
}

tag_period_starts <- function(data, starters_lookup) {
  # 1) First, assemble a “key” column so we can pull from starters_lookup:
  pbp_key <- data %>%
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
      starters_list = map2(
        all_starters,
        start_of_period,
        function(st, flag) {
          if (flag && !is.null(st)) st else NULL
        }
      )
    ) %>%
    select(-key, -all_starters)

  return(pbp_starters)
}

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
} #

build_current_players <- function(pbp_starters) {
  n <- nrow(pbp_starters)
  cp_list <- vector("list", n)

  for (i in seq_len(n)) {
    rowi <- pbp_starters[i, ]

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
} #

build_lineup_ids <- function(pbp_lineups) {
  pbp_lineups %>%
    mutate(
      lineup_id_offense = map2_chr(
        current_players, offense_team,
        ~ {
          tid <- as.character(.y)
          # If the offense team_id isn’t in current_players, return NA
          if (!tid %in% names(.x)) return(NA_character_)
          players <- sort(.x[[tid]])
          stringr::str_c(players, collapse = "-")
        }
      ),
      lineup_id_defense = map2_chr(
        current_players, offense_team,
        ~ {
          teams <- names(.x)
          off <- as.character(.y)
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
} #

get_team_starting_with_ball <- function(i, df) {
  n <- nrow(df)
  gid <- df$game_id[i]
  per <- df$period[i]

  for (j in seq(i + 1, n)) {
    gid_j <- df$game_id[j]
    per_j <- df$period[j]

    # break if we’ve left this game/period or hit an NA
    if (is.na(gid_j) || is.na(per_j) || gid_j != gid || per_j != per) {
      break
    }

    # coerce NAs → FALSE so `if()` never sees an NA
    is_shot <- isTRUE(df$is_fga[j])
    is_to <- isTRUE(df$is_turnover[j])
    is_ft <- isTRUE(df$is_fta[j]) && !isTRUE(df$is_technical_ft[j])

    if (is_shot || is_to || is_ft) {
      return(df$team_id[j])
    }
  }

  NA_character_
}

data_start_of_period <- function(data) {
  data %>%
    mutate(
      start_of_period = eventmsgtype == 12
    ) %>%
    tag_period_starts(
      get_period_starters(.) %>% build_starters_lookup()
    ) %>%
    build_current_players() %>%
    build_lineup_ids() %>%
    mutate(
      team_starting_with_ball = case_when(
        (period == 1 | period >= 5) &
          lead(is_jump_ball) &
          lead(start_of_period) ~ team_id,
        !start_of_period ~ NA_character_,
        TRUE ~ map_chr(
          event_index,
          ~ get_team_starting_with_ball(.x, pick(everything()))
        )
      )
    )
}


# Possessions
count_as_possession_helper <- function(i, df) {
  # Only possession-ending events are ever “poss_count” candidates
  if (!isTRUE(df$is_possession_ending_event[i])) {
    return(FALSE)
  }

  # 1) If the current possession ends with more than 2 seconds left, count it
  curr_secs <- df$secs_remain_quarter[i]
  if (!is.na(curr_secs) && curr_secs > 2) {
    return(TRUE)
  }

  # 2) Otherwise, find the index of the *previous* possession end (if any)
  prior_ends <- which(df$is_possession_ending_event[1:(i-1)])
  if (length(prior_ends) == 0) {
    # no prior end → this is the *first* possession of the period, so count it
    return(TRUE)
  }
  prev_idx   <- max(prior_ends)
  prev_secs  <- df$secs_remain_quarter[prev_idx]

  # 2a) If that prior end happened with >2 secs remaining, count this possession
  if (!is.na(prev_secs) && prev_secs > 2) {
    return(TRUE)
  }

  # 3) Otherwise (pos started in final 2 secs), only count if any scoring
  #    event (FT or made FG) occurs *after* that prior end
  n <- nrow(df)
  for (j in seq(prev_idx + 1, n)) {
    # guard possible NAs
    made_ft <- isTRUE(df$is_fta[j])
    made_fg <- isTRUE(df$is_fga[j]  ) && isTRUE(df$is_fgm[j])
    if (made_ft || made_fg) {
      return(TRUE)
    }
    # you could break if you hit end_of_period here, but grouping by period
    # will naturally keep you in–period only
  }

  # no scoring found → do *not* count
  FALSE
}

data_possession <- function(data) {

  data %>%
    group_by(game_id, period) %>%
    mutate(
      oftid_changed = offense_team != lead(offense_team),
      oftid_is_0 = offense_team == 0 | lead(offense_team) == 0,
      is_possession_ending_event = oftid_changed & !oftid_is_0 & !start_of_period,
      count_as_possession = map_lgl(
        row_number(),
        ~ count_as_possession_helper(.x, pick(everything()))
      ),
      poss_id = cumsum(lag(count_as_possession, default = TRUE)),
      seconds_since_previous_event = case_when(
        secs_remain_quarter == 720 ~ 0,
        secs_remain_quarter == 300 & period > 4 ~ 0,
        TRUE ~ lag(secs_remain_quarter) - secs_remain_quarter
      ),
      # detect when the home score increments
      home_team_id = if_else(
        home_score != lag(home_score, default = first(home_score)),
        team_id,
        NA_character_
      ),
      # detect when the away score increments
      away_team_id = if_else(
        away_score != lag(away_score, default = first(away_score)),
        team_id,
        NA_character_
      )
    ) %>%
    ungroup() %>%
    fill(away_team_id, home_team_id, .direction = "downup") %>%
    group_by(game_id, period, poss_id) %>%
    mutate(
      is_off_reb = is_real_rebound & is_oreb,
      is_second_chance_event = cumany(lag(is_off_reb, default = FALSE))
    ) %>%
    ungroup() %>%
    mutate(
      score_margin = case_when(
        offense_team == home_team_id ~ home_score - away_score,
        offense_team == away_team_id ~ away_score - home_score,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup()

}



### NEW
event_for_efficiency_stats <- function(i, df) {
  # if not a free throw, just return this row’s snapshot
  if (!isTRUE(df$is_fta[i])) {
    return(list(
      current_players = df$current_players[[i]],
      lineup_id_offense = df$lineup_id_offense[i],
      lineup_id_defense = df$lineup_id_defense[i]
    ))
  }

  clk <- df$secs_remain_quarter[i]
  n <- nrow(df)

  # 1) scan backwards at the same clock for a foul
  j <- i
  while (
    j > 1 &&
    df$secs_remain_quarter[j] == clk &&
    !isTRUE(df$is_foul[j])
  ) {
    j <- j - 1L
  }
  if (
    j > 0 &&
    isTRUE(df$is_foul[j]) &&
    df$secs_remain_quarter[j] == clk
  ) {
    return(list(
      current_players = df$current_players[[j]],
      lineup_id_offense = df$lineup_id_offense[j],
      lineup_id_defense = df$lineup_id_defense[j]
    ))
  }

  # 2) scan forwards (rare bug case)
  j <- i
  while (
    j < n &&
    df$secs_remain_quarter[j] == clk &&
    !isTRUE(df$is_foul[j])
  ) {
    j <- j + 1L
  }
  if (
    j <= n &&
    isTRUE(df$is_foul[j]) &&
    df$secs_remain_quarter[j] == clk
  ) {
    return(list(
      current_players   = df$current_players[[j]],
      lineup_id_offense = df$lineup_id_offense[j],
      lineup_id_defense = df$lineup_id_defense[j]
    ))
  }

  # 3) fallback → nothing found
  list(
    current_players   = df$current_players[[i]],
    lineup_id_offense = df$lineup_id_offense[i],
    lineup_id_defense = df$lineup_id_defense[i]
  )
}

add_foul_snapshots <- function(data) {

  data %>%
    mutate(
      eff = map(event_index, ~ event_for_efficiency_stats(.x, data)),
      foul_current_players = map(eff, "current_players"),
      foul_lineup_id_offense = map_chr(eff, "lineup_id_offense"),
      foul_lineup_id_defense = map_chr(eff, "lineup_id_defense")
    ) %>%
    select(-eff)
} # not needed




get_possessions_played_stats_items <- function(i, df) {
  row <- df[i, ]
  # quick exit if it doesn’t count as a possession
  if (!isTRUE(row$count_as_possession)) {
    return(tibble(
      player_id            = character(),
      team_id              = character(),
      opponent_team_id     = character(),
      lineup_id            = character(),
      opponent_lineup_id   = character(),
      key                  = character(),
      value                = integer()
    ))
  }

  # pick the right snapshot—FTs get the foul’s lineup, everything else uses the event’s own
  if (isTRUE(row$is_fta)) {
    eff <- event_for_efficiency_stats(i, df)
    cp  <- eff$current_players
    off_lid <- eff$lineup_id_offense
    def_lid <- eff$lineup_id_defense
  } else {
    cp      <- row$current_players[[1]]
    off_lid <- row$lineup_id_offense
    def_lid <- row$lineup_id_defense
  }

  team_chrs   <- names(cp)
  offense_chr <- as.character(row$offense_team)
  # is_2nd      <- isTRUE(row$is_second_chance_event)
  # is_pen      <- isTRUE(row$is_penalty_event)
  is_2nd      <- FALSE
  is_pen      <- FALSE

  out <- list()
  idx <- 1L

  for (team_chr in team_chrs) {
    players <- cp[[team_chr]]
    if (team_chr == offense_chr) {
      base_key <- offensive_possession_string
      lid      <- off_lid
      opp_lid  <- def_lid
      opp_chr  <- setdiff(team_chrs, team_chr)
    } else {
      base_key <- defensive_possession_string
      lid      <- def_lid
      opp_lid  <- off_lid
      opp_chr  <- setdiff(team_chrs, team_chr)
    }
    opp_id <- as.character(opp_chr)

    keys <- base_key
    if (is_2nd) keys <- c(keys, paste0(second_chance_string, base_key))
    if (is_pen) keys <- c(keys, paste0(penalty_string,      base_key))

    for (stat_key in keys) {
      out[[idx]] <- tibble(
        player_id           = as.character(players),
        team_id             = as.character(team_chr),
        opponent_team_id    = opp_id,
        lineup_id           = lid,
        opponent_lineup_id  = opp_lid,
        key            = stat_key,
        value          = 1L
      )
      idx <- idx + 1L
    }
  }

  bind_rows(out)
}

pbp_with_stats <- function(data) {

  data %>%
    mutate(
      possessions_list = map(event_index, ~ get_possessions_played_stats_items(.x, data))
    ) %>%
    # explode it out so each stat‐row gets its own row
    unnest(possessions_list, names_sep = "_") %>%       # prefix collisions
    rename_with(
      .cols = starts_with("possessions_list_"),
      .fn   = ~ sub("^possessions_list_", "stat_", .)
    )
}


get_seconds_played_stats_items <- function(i, df) {
  secs <- df$seconds_since_previous_event[i]
  # nothing to do if zero or NA
  if (is.na(secs) || secs == 0) {
    return(tibble(
      player_id           = character(),
      team_id             = character(),
      opponent_team_id    = character(),
      lineup_id           = character(),
      opponent_lineup_id  = character(),
      key                 = character(),
      value               = numeric()
    ))
  }

  # snapshot from the *previous* event
  prev <- df[i - 1, ]
  cp   <- prev$current_players[[1]]

  team_chrs  <- names(cp)
  off_chr    <- as.character(df$offense_team[i])
  # is_2nd     <- df$is_second_chance_event[i]
  # is_pen     <- df$is_penalty_event[i]
  is_2nd     <- FALSE
  is_pen     <- FALSE
  per_label  <- if (prev$period <= 4) as.character(prev$period) else "OT"

  # lineups come from that previous event
  lid_off    <- prev$lineup_id_offense
  lid_def    <- prev$lineup_id_defense

  out <- list()
  idx <- 1L

  for (team_chr in team_chrs) {
    players <- cp[[team_chr]]

    # choose offense/defense key
    base_key <- if (team_chr == off_chr) {
      seconds_played_offense_string
    } else {
      seconds_played_defense_string
    }

    opp_chr <- setdiff(team_chrs, team_chr)
    opp_id  <- as.character(opp_chr)

    # foul count for each player from previous_event
    foul_table <- df %>%
      get_player_game_fouls()

    prev_i <- i - 1
    fouls_map <- foul_table[[prev_i]]

    # build the list of stat_keys
    keys <- base_key
    for (pl in players) {
      # add the foul‐tracking key
      pfouls <- fouls_map[as.character(pl)] %||% 0
      foul_key <- paste0("Period", per_label, "Fouls", pfouls, base_key)
      all_keys <- c(
        keys,
        foul_key,
        if (is_2nd) paste0(second_chance_string, base_key),
        if (is_pen) paste0(penalty_string,       base_key)
      )

      # emit one row per stat_key
      for (stat_key in all_keys) {
        out[[idx]] <- tibble(
          player_id           = pl,
          team_id             = as.character(team_chr),
          opponent_team_id    = opp_id,
          lineup_id           = if (team_chr == off_chr) lid_off else lid_def,
          opponent_lineup_id  = if (team_chr == off_chr) lid_def else lid_off,
          key                 = stat_key,
          value               = secs
        )
        idx <- idx + 1L
      }
    }
  }

  bind_rows(out)
}

get_player_game_fouls <- function(data) {

  n               <- nrow(data)
  foul_counts     <- vector("list", n)      # to store the map at each event
  current_counts  <- integer()              # named integer vector: player_id → foul count

  # loop
  for (i in seq_len(n)) {
    # did this event count as a personal foul?
    if (isTRUE(data$counts_as_personal_foul[i])) {
      pid         <- as.character(data$player1_id[i])
      prev_count  <- current_counts[pid]
      if (is.na(prev_count)) prev_count <- 0L
      current_counts[pid] <- prev_count + 1L
    }
    # stash a copy of the current map
    foul_counts[[i]] <- current_counts
  }

  foul_counts

}

seconds_items <- function(data) {

  data %>%
    mutate(
      possessions_list = map(event_index, ~ get_seconds_played_stats_items(.x, data))
    ) %>%
    # explode it out so each stat‐row gets its own row
    unnest(possessions_list, names_sep = "_") %>%       # prefix collisions
    rename_with(
      .cols = starts_with("possessions_list_"),
      .fn   = ~ sub("^possessions_list_", "stat_", .)
    )
}







# data <- prepare_data_pbp(all_pbp %>% filter(game_id == "0022401073"))


# data2 <- data %>%
#   filter(game_id %in% c("0022401073", "0022401074", "0022401075", "0022401076"))


# # Testing
# test <- data %>%
#   data_jump_ball() %>%
#   data_substitution() %>%
#   data_fouls() %>%
#   data_ejection() %>%
#   data_field_goals() %>%
#   data_free_throws() %>%
#   data_turnover() %>%
#   data_start_of_period() %>%
#   data_end_of_period() %>%
#   data_replay() %>%
#   data_rebound() %>%
#   data_putbacks() %>%
#   data_free_throw_types() %>%
#   data_make_not_end_possession() %>%
#   data_and1() %>%
#   data_timeout() %>%
#   data_violation() %>%
#   data_possession() %>%
#   pbp_with_stats()
#   # seconds_items()


# player_possessions <- test %>%
#   group_by(stat_player_id, stat_key) %>%
#   summarise(stat_value = sum(stat_value), .groups = "drop")
#
# player_seconds <- test %>%
#   group_by(stat_player_id, stat_key) %>%
#   summarise(stat_value = sum(stat_value), .groups = "drop")


# test <- data2 %>%
#   group_split(game_id) %>%
#   map_dfr(~ .x %>%
#             data_jump_ball() %>%
#             data_substitution() %>%
#             data_fouls() %>%
#             data_ejection() %>%
#             data_field_goals() %>%
#             data_free_throws() %>%
#             data_turnover() %>%
#             data_start_of_period() %>%
#             data_end_of_period() %>%
#             data_replay() %>%
#             data_rebound() %>%
#             data_putbacks() %>%
#             data_free_throw_types() %>%
#             data_make_not_end_possession() %>%
#             data_and1() %>%
#             data_timeout() %>%
#             data_violation() %>%
#             data_possession()
#   )



fetch_lineups_box_score <- function(measure_type,
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


