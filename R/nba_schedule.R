#' Get NBA Schedule Data
#'
#' This function gets the current NBA schedule, processes it, and returns it.
#' @return A data frame with processed NBA schedule.
#' @export
nba_schedule <- function() {
  schedule_data <- fetch_nba_schedule()

  nba_schedule <- process_nba_schedule(schedule_data)
  return(nba_schedule)
}

#' Fetch NBA Schedule
#'
#' This function fetches NBA schedule data from the NBA API.
#' @return A list containing the NBA schedule data.
fetch_nba_schedule <- function() {
  headers <- generate_headers_cdn()

  url <- "https://cdn.nba.com/static/json/staticData/scheduleLeagueV2_1.json"

  data <- get_data_no_params(url, headers)

  return(data)
}

#' Process NBA Schedule Data
#'
#' This function processes the fetched NBA schedule data by adding game and schedule details.
#' @param data A list containing the fetched NBA schedule data.
#' @return A data frame with processed NBA schedule.
process_nba_schedule <- function(data) {
  games_list <- data$leagueSchedule$gameDates$games

  process_team_schedule <- function(team_data,
                                    game_id,
                                    game_date_est,
                                    location,
                                    game_label) {
    team_schedule <- as_tibble(team_data) %>%
      mutate(
        game_id = game_id,
        game_date_est = game_date_est,
        location = location,
        game_label = game_label
      )
    return(team_schedule)
  }

  schedule_list <- map(games_list, function(game_info) {
    game_id <- game_info$gameId
    game_date_est <- game_info$gameDateEst
    game_label <- game_info$gameLabel

    away_schedule <- process_team_schedule(
      team_data = game_info$awayTeam,
      game_id = game_id,
      game_date_est = game_date_est,
      location = "away",
      game_label = game_label
    )

    home_schedule <- process_team_schedule(
      team_data = game_info$homeTeam,
      game_id = game_id,
      game_date_est = game_date_est,
      location = "home",
      game_label = game_label
    )

    list(away_schedule, home_schedule)
  })

  schedule_df <- bind_rows(flatten(schedule_list))

  nba_schedule <- schedule_df %>%
    clean_names() %>%
    mutate(
      game_date = as_date(game_date_est),
      team_name = paste0(team_city, " ", team_name)
    ) %>%
    select(game_date, game_id, location, team_id, team_name, game_label) %>%
    arrange(game_date, game_id, location) %>%
    group_by(game_id) %>%
    mutate(
      opp_team_id = team_id[c(2, 1)],
      opp_team_name = if_else(location == "home", lag(team_name), lead(team_name))
    ) %>%
    filter(team_id != opp_team_id) %>%
    ungroup() %>%
    group_by(team_id, team_name, location) %>%
    mutate(
      game_count = calc_game_count(),
      days_rest = calc_days_rest(game_count, game_date),
      days_next_game = calc_days_next_game(game_count, game_date),
      is_b2b = calc_is_b2b(days_next_game, days_rest),
      is_b2b_first = calc_is_b2b_first(days_next_game),
      is_b2b_second = calc_is_b2b_second(days_rest)
    ) %>%
    group_by(opp_team_id, opp_team_name, location) %>%
    mutate(
      opp_game_count = calc_game_count(),
      opp_days_rest = calc_days_rest(opp_game_count, game_date),
      opp_days_next_game = calc_days_next_game(opp_game_count, game_date),
      opp_is_b2b = calc_is_b2b(opp_days_next_game, opp_days_rest),
      opp_is_b2b_first = calc_is_b2b_first(opp_days_next_game),
      opp_is_b2b_second = calc_is_b2b_second(opp_days_rest)
    ) %>%
    ungroup() %>%
    mutate(across(where(is.logical), ~ replace_na(., FALSE))) %>%
    select(
      game_date:team_name,
      opp_team_id:opp_team_name,
      is_b2b:is_b2b_second,
      opp_is_b2b:opp_is_b2b_second,
      game_count, opp_game_count,
      days_rest, opp_days_rest,
      game_label
    )

  return(nba_schedule)
}
