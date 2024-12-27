#' Get NBA Scores
#'
#' This function gets NBA scores for the specified seasons. Function pauses for
#' five seconds after each season to prevent timeout issues.
#'
#' @param seasons A numeric vector of seasons (e.g., 2024) for which to scrape NBA scores.
#' @param season_type A character string specifying the type of season (e.g., "Regular Season").
#' @return A data frame containing the NBA scores and related statistics.
#' @export
nba_scores <- function(seasons, season_type = "Regular Season") {
    if (!is.numeric(seasons) || length(seasons) == 0) {
        stop("The 'seasons' parameter must be a non-empty numeric vector.")
    }

    results <- map(seq_along(seasons), function(i) {
        season <- seasons[i]
        message(glue::glue("Processing season {season} ({i}/{length(seasons)})"))

        # Fetch data for the current season
        season_data <- tryCatch({
            all_data <- fetch_nba_scores(season, season_type)

            team_all_stats <- add_game_details(all_data) %>%
                select(season_year:matchup, location, wl:min, pts, plus_minus)

            team_games <- process_team_games(team_all_stats)
            opp_all_stats <- process_opp_all_stats(team_all_stats)
            opp_team_games <- process_opp_team_games(team_games)
            all_stats <- combine_team_and_opp_scores(team_all_stats, opp_all_stats)
            join_scores(all_stats, team_games, opp_team_games)
        }, error = function(e) {
            message(glue::glue("Error processing season {season}: {e$message}"))
            return(tibble())  # Return an empty tibble on error
        })

        # Pause after processing each season unless it's the last
        if (i < length(seasons)) {
            message(glue::glue("Pausing for 5 seconds before processing the next season..."))
            Sys.sleep(5)
        }

        return(season_data)
    })

    names(results) <- glue::glue("season_{seasons}") %>% as.character()
    return(results)
}

#' Fetch NBA Scores from API
#'
#' This function fetches NBA scores data for a specified season.
#'
#' @param season A numeric value representing the season (e.g., 2024).
#' @param season_type A character string specifying the type of season (e.g., "Regular Season").
#' @return A data frame containing raw NBA scores data.
fetch_nba_scores <- function(season, season_type) {
    headers <- generate_headers_stats()

    url <- "https://stats.nba.com/stats/teamgamelogs"

    params <- generate_parameters_stats(season, "Base", season_type)

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

#' Process Team Games
#'
#' This function processes team game data.
#'
#' @param team_all_stats A data frame containing processed team stats.
#' @return A data frame with processed team game data.
process_team_games <- function(team_all_stats) {
    team_all_stats %>%
        distinct(season_year, game_id, game_date, team_id, team_name) %>%
        group_by(season_year, team_id) %>%
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
        select(game_id, team_name, is_b2b_first, is_b2b_second, game_count)
}

#' Process Opponent Team Games
#'
#' This function processes opponent team game data.
#'
#' @param team_games A data frame containing processed team game data.
#' @return A data frame with processed opponent team game data.
process_opp_team_games <- function(team_games) {
    team_games %>%
        select(game_id, team_name, is_b2b_first, is_b2b_second, game_count) %>%
        rename_with(~ paste0("opp_", .), -game_id)
}

#' Process Opponent All Stats
#'
#' This function processes opponent team stats data.
#'
#' @param team_all_stats A data frame containing processed team stats.
#' @return A data frame with processed opponent team stats.
process_opp_all_stats <- function(team_all_stats) {
    team_all_stats %>%
        select(game_id, team_id, team_abbreviation, team_name, pts) %>%
        rename_with(~ paste0("opp_", .), -game_id)
}

#' Combine Team and Opponent Score
#'
#' This function combines team and opponent stats.
#'
#' @param team_all_stats A data frame containing processed team stats.
#' @param opp_all_stats A data frame containing processed opponent team stats.
#' @return A data frame with combined team and opponent stats.
combine_team_and_opp_scores <- function(team_all_stats, opp_all_stats) {
    team_all_stats %>%
        inner_join(opp_all_stats, by = "game_id", relationship = "many-to-many") %>%
        filter(team_name != opp_team_name) %>%
        select(season_year, team_id, team_abbreviation, team_name,
               opp_team_id, opp_team_abbreviation, opp_team_name,
               game_id:min, pts, opp_pts, plus_minus)
}

#' Join Scores
#'
#' This function joins team and opponent stats with game information.
#'
#' @param all_stats A data frame with combined team and opponent stats.
#' @param team_games A data frame with processed team game data.
#' @param opp_team_games A data frame with processed opponent team game data.
#' @return A data frame with joined stats.
join_scores <- function(all_stats, team_games, opp_team_games) {
    all_stats %>%
        left_join(team_games, by = c("game_id", "team_name")) %>%
        left_join(opp_team_games, by = c("game_id", "opp_team_name")) %>%
        select(season_year:opp_pts, plus_minus, game_count, opp_game_count,
               is_b2b_first, is_b2b_second, opp_is_b2b_first, opp_is_b2b_second) %>%
        arrange(game_date, game_id, location) %>%
        mutate(
            across(min:last_col(), as.numeric),
            across(starts_with(c("is_b2b", "opp_is_b2b")), as.logical),
            min = if_else(min < 48, 48, min)
        )
}
