#' Generic helper to call any pbpstats API endpoint
#'
#' A thin wrapper around any pbpstats API endpoint.
#'
#' @param endpoint Character. One of the endpoint names (e.g., `"get-games"`).
#' @param league Character. League slug (e.g., `"nba"`, `"wnba"`, `"gleague"`).
#' @param params Named list of query parameters (e.g., `Season`, `SeasonType`, `GameId`).
#' @return A tibble of the parsed JSON response.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
pbpstats_api <- function(endpoint, league, params = list()) {

  base_url <- "https://api.pbpstats.com"
  url <- paste0(base_url, "/", endpoint, "/", league)

  req <- httr2::request(url) %>%
    httr2::req_headers(
      Accept = "application/json"
    ) %>%
    httr2::req_url_query(!!!params)

  resp <- req %>% httr2::req_perform()
  httr2::resp_check_status(resp)

  raw <- httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' Fetch all players for a league from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-all-players-for-league” endpoint.
#'
#' @param league Character. Which league to query; e.g. `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @return A data frame (`tibble`) of all players in the specified league.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_all_players_for_league <- function(league = "nba") {

  raw <- pbpstats_api("get-all-players-for-league", league)
}

#' Fetch all-season stats from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-all-season-stats” endpoint.
#'
#' @param league Character. Which league to query; e.g. `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param entity_type Character. One of:
#'   \itemize{
#'     \item `"Player"`
#'     \item `"Team"`
#'     \item `"Opponent"`
#'     \item `"Lineup"`
#'     \item `"LineupOpponent"`
#'   }
#' @param entity_id Integer or character. The ID of the entity you want.
#' @return A data frame (`tibble`) of season stats as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_all_season_stats <- function(league = "nba", entity_type, entity_id) {

  entity_type <- match.arg(entity_type,
                           c("Player","Team","Lineup","LineupOpponent", "Opponent"))

  params <- list(
    EntityType = entity_type,
    EntityId = as.character(entity_id)
  )

  raw <- pbpstats_api("get-all-season-stats", league, params)
}

#' Fetch assist combo summary from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-assist-combo-summary” endpoint.
#'
#' @param league Character. Which league to query; e.g. `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g. `"2024-25"`.
#' @param season_type Character. One of `"Regular Season"`, `"Playoffs"`, or `"PlayIn"`;
#' defaults to `"Regular Season"`.
#' @return A tibble of assist combo summary as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_assist_combo_summary <- function(league = "nba", season, season_type = "Regular Season") {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn"))

  params <- list(
    Season = season,
    SeasonType = season_type
  )

  raw <- pbpstats_api("get-assist-combo-summary", league, params)
}

#' Fetch assist networks from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-assist-networks” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`;
#' required for full-season calls; set to `NULL` if using `game_id`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param entity_type Character. One of:
#'   \itemize{
#'     \item `"Team"`
#'     \item `"Lineup"`
#'   }.
#' @param entity_id Integer or character. The pbpstats ID of the entity.
#' @param game_id Integer or character. The pbpstats ID of a specific game; if non-`NULL`, `season`
#' and `season_type` are ignored.
#' @return A tibble of assist network data as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_assist_networks <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    entity_type,
    entity_id,
    game_id = NULL
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))
  entity_type <- match.arg(entity_type,
                           c("Team","Lineup"))

  if (!is.null(game_id)) {
    params <- list(
      GameId = as.character(game_id),
      EntityType = entity_type,
      EntityId = as.character(entity_id)
    )
  } else {
    if (is.null(season)) {
      stop("You must supply either `season` (for a full-season call) or `game_id` (for a single-game call).")
    }
    params <- list(
      Season = season,
      SeasonType = season_type,
      EntityType = entity_type,
      EntityId = as.character(entity_id)
    )
  }

  raw <- pbpstats_api("get-assist-networks", league, params)
}

#' Fetch endgame breakeven two-point percentage from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-endgame-breakeven-2-pct” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param margin Numeric. Point margin from the perspective of the team with the ball.
#' @param seconds_remaining Integer. Time remaining (in seconds) after the shot is made or
#' rebounded.
#' @param three_point_pct Numeric. Three-point field goal percentage as a whole number
#' (e.g., `30` for 30%).
#' @return A tibble of endgame breakeven two-point percentages as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_endgame_breakeven_2_pct <- function(
    league = "nba",
    margin,
    seconds_remaining,
    three_point_pct
) {

  url_addon <- paste0(league, "/", margin, "/", seconds_remaining, "/", three_point_pct)

  raw <- pbpstats_api("get-endgame-breakeven-2-pct", url_addon)
}

#' Fetch four-factor on/off metrics from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-four-factor-on-off” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of `"Regular Season"`, `"Playoffs"`, `"PlayIn"`, or `"All"`;
#' defaults to `"Regular Season"`.
#' @param team_id Integer or character. The pbpstats ID of the team.
#' @param player_id Integer or character. The pbpstats ID of the player.
#' @return A tibble of four-factor on/off metrics as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_four_factor_on_off <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    team_id,
    player_id
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    TeamId = as.character(team_id),
    PlayerId = as.character(player_id)
  )

  raw <- pbpstats_api("get-four-factor-on-off", league, params)
}

#' Fetch game logs from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-game-logs” endpoint.
#'
#' @param league Character. Which league to query; e.g. `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g. `"2024-25"`.
#' @param season_type Character. One of `"Regular Season"`, `"Playoffs"`, `"PlayIn"`, or `"All"`;
#' defaults to `"Regular Season"`.
#' @param entity_type Character. One of:
#'   \itemize{
#'     \item `"Player"` (player logs)
#'     \item `"Team"` or `"Opponent"` (team logs)
#'     \item `"Lineup"` or `"LineupOpponent"` (lineup logs)
#'   }
#' @param entity_id Integer or character. The pbpstats ID of the entity (player, team, or lineup).
#' @return A tibble of game logs as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_game_logs <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    entity_type,
    entity_id
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))
  entity_type <- match.arg(entity_type,
                           c("Player","Team","Opponent","Lineup","LineupOpponent"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    EntityType = entity_type,
    EntityId = as.character(entity_id)
  )

  raw <- pbpstats_api("get-game-logs", league, params)
}

#' Fetch game stats from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-game-stats” endpoint.
#'
#' @param game_id Integer or character. The pbpstats ID of the game.
#' @param type Character. One of:
#'   \itemize{
#'     \item `"Player"`
#'     \item `"Lineup"`
#'     \item `"LineupOpponent"`
#'   }.
#' @return A tibble of game statistics as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_game_stats <- function(game_id, type) {

  type <- match.arg(type,
                    c("Player","Lineup","LineupOpponent"))

  params <- list(
    GameId = as.character(game_id),
    Type = type
  )

  raw <- pbpstats_api("get-game-stats", "", params)
}

#' Fetch games from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-games” endpoint (NBA only).
#'
#' @param season Character or integer. The season to query; e.g. `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @return A tibble of games in the specified season as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_games <- function(season, season_type = "Regular Season") {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    Season = season,
    SeasonType = season_type
  )

  raw <- pbpstats_api("get-games", "nba", params)
}

#' Fetch league year-over-year plots from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-league-year-over-year-plots” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param left_axis Character. One of the statistical metrics to plot on the left axis.
#' Options include:
#'   \itemize{
#'     \item `"PtsPer100Poss"`
#'     \item `"SecondsPerPoss"`
#'     \item `"FG3APct"`
#'     \item `"Fg3Pct"`
#'     \item `"AtRimFrequency"`
#'     \item `"AtRimAccuracy"`
#'     \item `"AtRimPctAssisted"`
#'     \item `"ShortMidRangeFrequency"`
#'     \item `"ShortMidRangeAccuracy"`
#'     \item `"ShortMidRangePctAssisted"`
#'     \item `"LongMidRangeFrequency"`
#'     \item `"LongMidRangeAccuracy"`
#'     \item `"LongMidRangePctAssisted"`
#'     \item `"Corner3Frequency"`
#'     \item `"Corner3Accuracy"`
#'     \item `"Corner3PctAssisted"`
#'     \item `"Arc3Frequency"`
#'     \item `"Arc3Accuracy"`
#'     \item `"Arc3PctAssisted"`
#'     \item `"LiveBallTurnoverPct"`
#'     \item `"EfgPct"`
#'     \item `"DefFTReboundPct"`
#'     \item `"OffFTReboundPct"`
#'     \item `"DefTwoPtReboundPct"`
#'     \item `"OffTwoPtReboundPct"`
#'     \item `"DefThreePtReboundPct"`
#'     \item `"OffThreePtReboundPct"`
#'     \item `"DefFGReboundPct"`
#'     \item `"OffFGReboundPct"`
#'     \item `"OffAtRimReboundPct"`
#'     \item `"OffShortMidRangeReboundPct"`
#'     \item `"OffLongMidRangeReboundPct"`
#'     \item `"OffArc3ReboundPct"`
#'     \item `"OffCorner3ReboundPct"`
#'     \item `"DefAtRimReboundPct"`
#'     \item `"DefShortMidRangeReboundPct"`
#'     \item `"DefLongMidRangeReboundPct"`
#'     \item `"DefArc3ReboundPct"`
#'     \item `"DefCorner3ReboundPct"`
#'     \item `"SecondChancePtsPer100PossSecondChance"`
#'     \item `"PenaltyPtsPer100PossPenalty"`
#'     \item `"SecondChanceOffPossPer100Poss"`
#'     \item `"FirstChancePtsPer100Poss"`
#'     \item `"SecondChancePtsPer100Poss"`
#'     \item `"PenaltyOffPossPer100Poss"`
#'     \item `"Avg2ptShotDistance"`
#'     \item `"Avg3ptShotDistance"`
#'   }
#' @param right_axis Character. One of the same options as `left_axis`, plotted on the right axis.
#' @return A tibble of year-over-year league metrics suitable for plotting, as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_year_over_year_plots <- function(
    league = "nba",
    season_type = "Regular Season",
    left_axis,
    right_axis
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    SeasonType = season_type,
    LeftAxis = left_axis,
    RightAxis = right_axis
  )

  raw <- pbpstats_api("get-league-year-over-year-plots", league, params)
}

#' Fetch leverage data from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-leverage” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param margin Numeric. Point margin (difference in score).
#' @param pre_game_win_prob Numeric. Pre-game win probability as a decimal (e.g., `0.65` for 65%).
#' @param seconds_remaining Integer. Seconds remaining in the game.
#' @param end_of_possession_seconds_remaining Integer. Seconds remaining at the end of the
#' possession.
#' @return A tibble of leverage values as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_leverage <- function(
    league = "nba",
    margin,
    pre_game_win_prob,
    seconds_remaining,
    end_of_possession_seconds_remaining
) {

  url_addon <- paste0(league, "/", margin, "/", pre_game_win_prob, "/", seconds_remaining, "/", end_of_possession_seconds_remaining)

  raw <- pbpstats_api("get-leverage", url_addon)
}

#' Fetch lineup opponent summary from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-lineup-opponent-summary” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param team_id Integer or character. The pbpstats ID of the team.
#' @param lineup_id Character. The pbpstats lineup ID
#' (hyphen-separated player IDs sorted as strings).
#' @return A tibble of lineup opponent summary as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_lineup_opponent_summary <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    team_id,
    lineup_id
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    TeamId = as.character(team_id),
    LineupId = as.character(lineup_id)
  )

  raw <- pbpstats_api("get-lineup-opponent-summary", league, params)
}

#' Fetch lineup player stats from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-lineup-player-stats” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param lineup_id Character. The pbpstats lineup ID
#' (hyphen-separated player IDs sorted as strings).
#' @return A tibble of lineup player stats as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_lineup_player_stats <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    lineup_id
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    LineupId = as.character(lineup_id)
  )

  raw <- pbpstats_api("get-lineup-player-stats", league, params)
}

#' Fetch lineup subunit stats from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-lineup-subunit-stats” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g. `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param team_id Integer or character. The pbpstats ID of the team.
#' @param lineup_id Character. The pbpstats lineup ID
#' (hyphen-separated player IDs sorted as strings).
#' @param sub_unit_size Integer. Number of players in each subunit
#' (2, 3, or 4 players of a 5-player lineup).
#' @return A tibble of lineup subunit stats as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_lineup_subunit_stats <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    team_id,
    lineup_id,
    sub_unit_size
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    TeamId = as.character(team_id),
    LineupId = as.character(lineup_id),
    SubUnitSize = sub_unit_size
  )

  raw <- pbpstats_api("get-lineup-subunit-stats", league, params)
}

#' Fetch number of starter stats by team from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-number-of-starter-stats-by-team” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @return A tibble of the number of starter stats by team as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_number_of_starter_stats_by_team <- function(
    league = "nba",
    season,
    season_type = "Regular Season"
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    Season = season,
    SeasonType = season_type
  )

  raw <- pbpstats_api("get-number-of-starter-stats-by-team", league, params)
}

#' Fetch on/off metrics from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-on-off” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g. `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param type Character. Whether to fetch on/off for a `"team"` or a `"player"`.
#' @param team_id Integer or character. The pbpstats ID of the team (required if `type = "team"`).
#' @param player_id Integer or character. The pbpstats ID of the player
#' (required if `type = "player"`).
#' @param leverage Character. Leverage levels to include; one or more of:
#'   \itemize{
#'     \item `"VeryLow"`
#'     \item `"Low"`
#'     \item `"Medium"`
#'     \item `"High"`
#'     \item `"VeryHigh"`
#'   }; comma-separated for multiple values.
#' @param stat Character or character vector. Which metric(s) to return. Options include:
#'   \itemize{
#'     \item `"Assisted2sPct"`
#'     \item `"NonPutbacksAssisted2sPct"`
#'     \item `"Assisted3sPct"`
#'     \item `"Fg3Pct"`
#'     \item `"NonHeaveFg3Pct"`
#'     \item `"Fg2Pct"`
#'     \item `"EfgPct"`
#'     \item `"TsPct"`
#'     \item `"FG3APct"`
#'     \item `"FG3APctBlocked"`
#'     \item `"FG2APctBlocked"`
#'     \item `"LiveBallTurnoverPct"`
#'     \item `"DefFTReboundPct"`
#'     \item `"OffFTReboundPct"`
#'     \item `"DefTwoPtReboundPct"`
#'     \item `"OffTwoPtReboundPct"`
#'     \item `"DefThreePtReboundPct"`
#'     \item `"OffThreePtReboundPct"`
#'     \item `"DefFGReboundPct"`
#'     \item `"OffFGReboundPct"`
#'     \item `"OffAtRimReboundPct"`
#'     \item `"OffShortMidRangeReboundPct"`
#'     \item `"OffLongMidRangeReboundPct"`
#'     \item `"OffArc3ReboundPct"`
#'     \item `"OffCorner3ReboundPct"`
#'     \item `"DefAtRimReboundPct"`
#'     \item `"DefShortMidRangeReboundPct"`
#'     \item `"DefLongMidRangeReboundPct"`
#'     \item `"DefArc3ReboundPct"`
#'     \item `"DefCorner3ReboundPct"`
#'     \item `"BlocksRecoveredPct"`
#'     \item `"SecondsPerPossOff"`
#'     \item `"SecondsPerPossDef"`
#'     \item `"AtRimFrequency"`
#'     \item `"AtRimAccuracy"`
#'     \item `"AtRimPctAssisted"`
#'     \item `"ShortMidRangeFrequency"`
#'     \item `"ShortMidRangeAccuracy"`
#'     \item `"ShortMidRangePctAssisted"`
#'     \item `"LongMidRangeFrequency"`
#'     \item `"LongMidRangeAccuracy"`
#'     \item `"LongMidRangePctAssisted"`
#'     \item `"Corner3Frequency"`
#'     \item `"Corner3Accuracy"`
#'     \item `"Corner3PctAssisted"`
#'     \item `"Arc3Frequency"`
#'     \item `"Arc3Accuracy"`
#'     \item `"Arc3PctAssisted"`
#'     \item `"AtRimFG3AFrequency"`
#'     \item `"NonHeaveArc3Accuracy"`
#'     \item `"ShotQualityAvg"`
#'     \item `"ShootingFoulsDrawnPct"`
#'     \item `"TwoPtShootingFoulsDrawnPct"`
#'     \item `"ThreePtShootingFoulsDrawnPct"`
#'     \item `"SecondChancePointsPct"`
#'     \item `"PenaltyPointsPct"`
#'     \item `"PenaltyOffPossPct"`
#'     \item `"Avg2ptShotDistance"`
#'     \item `"Avg3ptShotDistance"`
#'     \item `"PenaltyEfficiencyExcludingTakeFouls"`
#'     \item `"DefReboundPct"`
#'     \item `"OffReboundPct"`
#'     \item `"PtsPer100Poss"`
#'     \item `"AssistPointsPer100Poss"`
#'     \item `"FTAPer100Poss"`
#'     \item `"TurnoversPer100Poss"`
#'     \item `"AssistsPer100Poss"`
#'     \item `"PtsPer100PossOpponent"`
#'     \item `"OpponentShotQualityAvg"`
#'     \item `"Fg3PctOpponent"`
#'     \item `"Fg2PctOpponent"`
#'     \item `"EfgPctOpponent"`
#'     \item `"FG3APctOpponent"`
#'     \item `"AtRimFrequencyOpponent"`
#'     \item `"AtRimAccuracyOpponent"`
#'     \item `"ShortMidRangeFrequencyOpponent"`
#'     \item `"ShortMidRangeAccuracyOpponent"`
#'     \item `"LongMidRangeFrequencyOpponent"`
#'     \item `"LongMidRangeAccuracyOpponent"`
#'     \item `"Corner3FrequencyOpponent"`
#'     \item `"Corner3AccuracyOpponent"`
#'     \item `"Arc3FrequencyOpponent"`
#'     \item `"Arc3AccuracyOpponent"`
#'     \item `"Pace"`
#'     \item `"SecondChancePtsPer100PossSecondChance"`
#'     \item `"PenaltyPtsPer100PossPenalty"`
#'     \item `"SecondChanceOffPossPer100Poss"`
#'     \item `"FirstChancePtsPer100Poss"`
#'     \item `"SecondChancePtsPer100Poss"`
#'   }.
#' @return A tibble of on/off metrics as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_on_off <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    type,
    team_id,
    player_id,
    leverage = NULL,
    stat = NULL
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  type <- match.arg(type,
                    c("team","player"))

  leverage <- match.arg(leverage,
                        c("VeryLow","Low","Medium","High","VeryHigh"), several.ok = TRUE)

  params <- list(
    Season = season,
    SeasonType = season_type,
    TeamId = as.character(team_id),
    PlayerId = as.character(player_id),
    Leverage = leverage,
    Stat = stat
  )

  raw <- pbpstats_api("get-on-off", paste0(league, "/", type), params)
}

#' Fetch pace efficiency by season from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-pace-efficiency-by-season” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @return A tibble of pace efficiency metrics by season as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_pace_efficiency_by_season <- function(league = "nba", season_type = "Regular Season") {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    SeasonType = season_type
  )

  raw <- pbpstats_api("get-pace-efficiency-by-season", league, params)
}

#' Fetch pace efficiency summary from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-pace-efficiency-summary” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g. `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param type Character. One of:
#'   \itemize{
#'     \item `"Team"`
#'     \item `"Opponent"`
#'   }.
#' @return A tibble of pace efficiency summary as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_pace_efficiency_summary <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    type
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  type <- match.arg(type,
                    c("Team","Opponent"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    Type = type
  )

  raw <- pbpstats_api("get-pace-efficiency-summary", league, params)
}

#' Fetch playing time distribution from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-playing-time-distribution” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param team_id Integer or character. The pbpstats ID of the team.
#' @param player_id Integer or character. The pbpstats ID of the player.
#' @return A tibble of playing time distribution as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_playing_time_distribution <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    team_id,
    player_id
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    TeamId = as.character(team_id),
    PlayerId = as.character(player_id)
  )

  raw <- pbpstats_api("get-playing-time-distribution", league, params)
}

#' Fetch possession length frequency from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-possession-length-frequency” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param poss_type Character. One of:
#'   \itemize{
#'     \item `"Offense"`
#'     \item `"Defense"`
#'   }.
#' @param time_type Character. One of:
#'   \itemize{
#'     \item `"IncludeSecondChances"`
#'     \item `"ExcludeSecondChances"`
#'   }.
#' @param start_type Character. Defines the event that starts the possession; one of:
#'   \itemize{
#'     \item `"All"`
#'     \item `"OffMissedFG"`
#'     \item `"OffMissed2"`
#'     \item `"OffMissed3"`
#'     \item `"OffMadeFG"`
#'     \item `"OffMade2"`
#'     \item `"OffMade3"`
#'     \item `"OffAtRimMake"`
#'     \item `"OffAtRimMiss"`
#'     \item `"OffAtRimBlock"`
#'     \item `"OffShortMidRangeMake"`
#'     \item `"OffShortMidRangeMiss"`
#'     \item `"OffLongMidRangeMake"`
#'     \item `"OffLongMidRangeMiss"`
#'     \item `"OffArc3Make"`
#'     \item `"OffArc3Miss"`
#'     \item `"OffCorner3Make"`
#'     \item `"OffCorner3Miss"`
#'     \item `"OffFTMake"`
#'     \item `"OffFTMiss"`
#'     \item `"OffLiveBallTurnover"`
#'     \item `"OffDeadball"`
#'     \item `"OffTimeout"`
#'   }.
#' @return A tibble of possession length frequency as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_possession_length_frequency <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    poss_type,
    time_type,
    start_type = "All"
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  poss_type <- match.arg(poss_type,
                           c("Offense","Defense"))

  time_type <- match.arg(time_type,
                           c("IncludeSecondChances","ExcludeSecondChances"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    PossessionType = poss_type,
    TimeType = time_type,
    StartType = start_type
  )

  raw <- pbpstats_api("get-possession-length-frequency", league, params)
}

#' Fetch possessions from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-possessions” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g. `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param team_id Integer or character. The pbpstats ID of the team.
#' @param filter_comparison Character. One of:
#'   \itemize{
#'     \item `"Exactly"`
#'     \item `"GreaterThan"`
#'     \item `"LessThan"`
#'   }.
#' @param filter_event Character. One of:
#'   \itemize{
#'     \item `"OnFloor"`
#'     \item `"OffFloor"`
#'     \item `"PlayedInGame"`
#'     \item `"DidNotPlayInGame"`
#'     \item `"Started"`
#'     \item `"CameOffBench"`
#'   }.
#' @param filter_value Numeric or character. The value to filter with the specified comparison and
#' event.
#' @param off_def Character. One of:
#'   \itemize{
#'     \item `"Offense"`
#'     \item `"Defense"`
#'   }.
#' @param start_type Character. The event that started the possession; see endpoint docs for
#' available values.
#' @param leverage Character. One or more of:
#'   \itemize{
#'     \item `"VeryLow"`
#'     \item `"Low"`
#'     \item `"Medium"`
#'     \item `"High"`
#'     \item `"VeryHigh"`
#'   }; comma-separated for multiple values. Defaults to `NULL`.
#' @param period Character or integer. One of:
#'   \itemize{
#'     \item `"All"`
#'     \item `"1"`
#'     \item `"2"`
#'     \item `"3"`
#'     \item `"4"`
#'     \item `"SecondHalf"`
#'   }; defaults to `NULL` (all periods).
#' @param from_margin Numeric. Optional starting point margin for possessions; defaults to `NULL`.
#' @param to_margin Numeric. Optional ending point margin for possessions; defaults to `NULL`.
#' @param from_time Integer. Optional seconds remaining at the start of a possession;
#' defaults to `NULL`.
#' @param to_time Integer. Optional seconds remaining at the end of a possession;
#' defaults to `NULL`.
#' @param event_type Character. Optional event type filter; one of:
#'   \itemize{
#'     \item `"FG3A"`
#'     \item `"FG2A"`
#'     \item `"Oreb"`
#'   }; defaults to `NULL`.
#' @param event_player_id Integer or character. Optional pbpstats ID for the event player;
#' defaults to `NULL`.
#' @param reb_player_id Integer or character. Optional pbpstats ID for the rebounder;
#' defaults to `NULL`.
#' @param shooter_player_id Integer or character. Optional pbpstats ID for the shooter;
#' defaults to `NULL`.
#' @return A tibble of possession-level data as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_possessions <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    team_id,
    filter_comparison,
    filter_event,
    filter_value,
    off_def,
    start_type,
    leverage,
    period = NULL,
    from_margin = NULL,
    to_margin = NULL,
    from_time = NULL,
    to_time = NULL,
    event_type = NULL,
    event_player_id = NULL,
    reb_player_id = NULL,
    shooter_player_id = NULL
) {

  filter_comparison <- match.arg(
    filter_comparison,
    c("Exactly", "GreaterThan", "LessThan")
  )

  filter_event <- match.arg(
    filter_event,
    c(
      "OnFloor", "OffFloor", "PlayedInGame", "DidNotPlayInGame",
      "Started", "CameOffBench"
    )
  )

  filter_name <- paste0(filter_comparison, filter_event)

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  leverage <- match.arg(leverage,
                        c("VeryLow","Low","Medium","High","VeryHigh"), several.ok = TRUE)

  params <- list(
    Season = season,
    SeasonType = season_type,
    TeamId = as.character(team_id),
    OffDef = off_def,
    StartType = start_type
  )

  params[[filter_name]] <- filter_value

  if (!is.null(period)) params$Period <- as.character(period)
  if (!is.null(from_margin)) params$FromMargin <- from_margin
  if (!is.null(to_margin)) params$ToMargin <- to_margin
  if (!is.null(from_time)) params$FromTime <- from_time
  if (!is.null(to_time)) params$ToTime <- to_time
  if (!is.null(leverage)) params$Leverage <- leverage
  if (!is.null(event_type)) params$EventType <- event_type
  if (!is.null(event_player_id)) params$EventPlayerId <- as.character(event_player_id)
  if (!is.null(reb_player_id)) params$ReboundPlayerId <- as.character(reb_player_id)
  if (!is.null(shooter_player_id)) params$ShooterPlayerId <- as.character(shooter_player_id)

  raw <- pbpstats_api("get-possessions", league, params)
  return(raw)
}

#' Fetch relative offensive/defensive efficiency from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-relative-off-def-efficiency” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @return A tibble of relative offensive and defensive efficiency metrics as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_relative_off_def_efficiency <- function(league = "nba") {

  raw <- pbpstats_api("get-relative-off-def-efficiency", league)
}

#' Fetch scatter plots from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-scatter-plots” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g. `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param x_axis Character. One of:
#'   \itemize{
#'     \item `"PtsPer100Poss"`
#'     \item `"SecondsPerPoss"`
#'     \item `"FG3APct"`
#'     \item `"Fg3Pct"`
#'     \item `"AtRimFrequency"`
#'     \item `"AtRimAccuracy"`
#'     \item `"AtRimPctAssisted"`
#'     \item `"ShortMidRangeFrequency"`
#'     \item `"ShortMidRangeAccuracy"`
#'     \item `"ShortMidRangePctAssisted"`
#'     \item `"LongMidRangeFrequency"`
#'     \item `"LongMidRangeAccuracy"`
#'     \item `"LongMidRangePctAssisted"`
#'     \item `"Corner3Frequency"`
#'     \item `"Corner3Accuracy"`
#'     \item `"Corner3PctAssisted"`
#'     \item `"Arc3Frequency"`
#'     \item `"Arc3Accuracy"`
#'     \item `"Arc3PctAssisted"`
#'     \item `"LiveBallTurnoverPct"`
#'     \item `"EfgPct"`
#'     \item `"DefFTReboundPct"`
#'     \item `"OffFTReboundPct"`
#'     \item `"DefTwoPtReboundPct"`
#'     \item `"OffTwoPtReboundPct"`
#'     \item `"DefThreePtReboundPct"`
#'     \item `"OffThreePtReboundPct"`
#'     \item `"DefFGReboundPct"`
#'     \item `"OffFGReboundPct"`
#'     \item `"OffAtRimReboundPct"`
#'     \item `"OffShortMidRangeReboundPct"`
#'     \item `"OffLongMidRangeReboundPct"`
#'     \item `"OffArc3ReboundPct"`
#'     \item `"OffCorner3ReboundPct"`
#'     \item `"DefAtRimReboundPct"`
#'     \item `"DefShortMidRangeReboundPct"`
#'     \item `"DefLongMidRangeReboundPct"`
#'     \item `"DefArc3ReboundPct"`
#'     \item `"DefCorner3ReboundPct"`
#'     \item `"SecondChancePtsPer100PossSecondChance"`
#'     \item `"PenaltyPtsPer100PossPenalty"`
#'     \item `"SecondChanceOffPossPer100Poss"`
#'     \item `"FirstChancePtsPer100Poss"`
#'     \item `"SecondChancePtsPer100Poss"`
#'     \item `"PenaltyOffPossPer100Poss"`
#'     \item `"Avg2ptShotDistance"`
#'     \item `"Avg3ptShotDistance"`
#'   }.
#' @param y_axis Character. One of the same options as `x_axis`.
#' @param x_axis_type Character. One of:
#'   \itemize{
#'     \item `"Team"`
#'     \item `"Opponent"`
#'   }.
#' @param y_axis_type Character. One of:
#'   \itemize{
#'     \item `"Team"`
#'     \item `"Opponent"`
#'   }.
#' @return A tibble of scatter‐plot data as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_scatter_plots <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    x_axis,
    y_axis,
    x_axis_type,
    y_axis_type
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  x_axis_type <- match.arg(x_axis_type,
                           c("Team","Opponent"))

  y_axis_type <- match.arg(y_axis_type,
                           c("Team","Opponent"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    Xaxis = x_axis,
    Yaxis = y_axis,
    XaxisType = x_axis_type,
    YaxisType = y_axis_type
  )

  raw <- pbpstats_api("get-scatter-plots", league, params)
}

#' Fetch score margin breakdown from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-score-margin-breakdown” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param team_id Integer or character. The pbpstats ID of the team.
#' @param period Character. One of:
#'   \itemize{
#'     \item `"All"`
#'     \item `"1"`
#'     \item `"2"`
#'     \item `"3"`
#'     \item `"4"`
#'     \item `"SecondHalf"`
#'   }.
#' @return A tibble of score margin breakdown as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_score_margin_breakdown <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    team_id,
    period
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    TeamId = as.character(team_id),
    Period = as.character(period)
  )

  raw <- pbpstats_api("get-score-margin-breakdown", league, params)
}

#' Fetch score-time summary from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-score-time-summary” endpoint.
#' Exactly one of `period_gte`, `period_lte`, or `period_equals` must be supplied.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param type Character. One of:
#'   \itemize{
#'     \item `"Team"`
#'     \item `"Lineup"`
#'     \item `"Opponent"`
#'     \item `"LineupOpponent"`
#'   }.
#' @param from_margin Numeric. Starting point margin for the breakdown.
#' @param to_margin Numeric. Ending point margin for the breakdown.
#' @param from_time Integer. Seconds remaining in the period at the start of a possession.
#' @param to_time Integer. Seconds remaining in the period at the end of a possession.
#' @param period_gte Integer. Filter for possessions in periods ≥ this value; defaults to `NULL`.
#' @param period_lte Integer. Filter for possessions in periods ≤ this value; defaults to `NULL`.
#' @param period_equals Integer. Filter for possessions in periods == this value;
#' defaults to `NULL`.
#' @return A tibble of score-time summary data as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_score_time_summary <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    type,
    from_margin,
    to_margin,
    from_time,
    to_time,
    period_gte = NULL,
    period_lte = NULL,
    period_equals = NULL
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  type <- match.arg(type,
                    c("Team","Opponent","Lineup","LineupOpponent"))

  period_args <- list(
    PeriodGte = period_gte,
    PeriodLte = period_lte,
    PeriodEquals = period_equals
  )
  period_args <- Filter(Negate(is.null), period_args)
  if (length(period_args) != 1) {
    stop("Please supply exactly one of `period_gte`, `period_lte`, or `period_equals`.",
         call. = FALSE)
  }

  params <- list(
    Season = season,
    SeasonType = season_type,
    Type = type,
    FromMargin = from_margin,
    ToMargin = to_margin,
    FromTime = from_time,
    ToTime = to_time
  )

  params <- c(params, period_args)

  raw <- pbpstats_api("get-score-time-summary", league, params)
  return(raw)
}

#' Fetch shot query summary from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-shot-query-summary” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param type Character. One of:
#'   \itemize{
#'     \item `"Team"`
#'     \item `"Lineup"`
#'     \item `"Opponent"`
#'     \item `"LineupOpponent"`
#'   }.
#' @param from_margin Numeric. Starting point margin for the breakdown.
#' @param to_margin Numeric. Ending point margin for the breakdown.
#' @param from_time Integer. Seconds remaining in the period at the start of a possession.
#' @param to_time Integer. Seconds remaining in the period at the end of a possession.
#' @param period_gte Integer. Filter for possessions in periods ≥ this value; defaults to `NULL`.
#' @param period_lte Integer. Filter for possessions in periods ≤ this value; defaults to `NULL`.
#' @param period_equals Integer. Filter for possessions in periods == this value;
#' defaults to `NULL`.
#' @return A tibble of shot query summary data as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_shot_query_summary <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    type,
    from_margin,
    to_margin,
    from_time,
    to_time,
    period_gte = NULL,
    period_lte = NULL,
    period_equals = NULL
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  type <- match.arg(type,
                    c("Team","Opponent","Lineup","LineupOpponent"))

  period_args <- list(
    PeriodGte = period_gte,
    PeriodLte = period_lte,
    PeriodEquals = period_equals
  )
  period_args <- Filter(Negate(is.null), period_args)
  if (length(period_args) != 1) {
    stop("Please supply exactly one of `period_gte`, `period_lte`, or `period_equals`.",
         call. = FALSE)
  }

  params <- list(
    Season = season,
    SeasonType = season_type,
    Type = type,
    FromMargin = from_margin,
    ToMargin = to_margin,
    FromTime = from_time,
    ToTime = to_time
  )

  params <- c(params, period_args)

  raw <- pbpstats_api("get-shot-query-summary", league, params)
  return(raw)
}

#' Fetch shots from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-shots” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g. `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param entity_type Character. One of:
#'   \itemize{
#'     \item `"Player"`
#'     \item `"Team"`
#'     \item `"Lineup"`
#'     \item `"LineupOpponent"`
#'   }.
#' @param entity_id Integer or character. The pbpstats ID of the entity.
#' @param start_type Character. The event that started the possession; see endpoint docs for
#' options.
#' @param blocked Logical. Filter by blocked shots (`TRUE`) or unblocked (`FALSE`).
#' @param shot_value Integer. Shot value; `2` for two‐pointers or `3` for three‐pointers;
#' defaults to `NULL`.
#' @param oreb_shot_type Character. One of:
#'   \itemize{
#'     \item `"MissedFG"`
#'     \item `"Missed2"`
#'     \item `"MissedMidRange"`
#'     \item `"Missed3"`
#'     \item `"AtRim"`
#'     \item `"AtRimBlocked"`
#'     \item `"ShortMidRange"`
#'     \item `"LongMidRange"`
#'     \item `"Arc3"`
#'     \item `"Corner3"`
#'     \item `"FT"`
#'   }; defaults to `NULL`.
#' @param sec_since_oreb Integer. Maximum seconds since an offensive
#' rebound (`SecondsSinceOrebLte`); defaults to `NULL`.
#' @return A tibble of shot data as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_shots <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    entity_type,
    entity_id,
    start_type = "All",
    blocked,
    shot_value = NULL,
    oreb_shot_type = NULL,
    sec_since_oreb = NULL
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  entity_type <- match.arg(entity_type,
                    c("Player","Team","Lineup","LineupOpponent"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    EntityType = entity_type,
    EntityId = entity_id,
    StartType = start_type,
    Blocked = blocked,
    ShotValue = shot_value,
    OrebShotType = oreb_shot_type,
    SecondsSinceOrebLte = sec_since_oreb
  )

  raw <- pbpstats_api("get-shots", league, params)
}

#' Fetch team leverage summary from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-team-leverage-summary” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g. `"2024-25"`.
#' @param leverage Character. One or more leverage levels; one of:
#'   \itemize{
#'     \item `"VeryLow"`
#'     \item `"Low"`
#'     \item `"Medium"`
#'     \item `"High"`
#'     \item `"VeryHigh"`
#'   }; comma‐separated for multiple values; defaults to `NULL`.
#' @return A tibble of team leverage summary as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_team_leverage_summary <- function(league = "nba", season, leverage) {

  leverage <- match.arg(leverage,
                        c("VeryLow","Low","Medium","High","VeryHigh"), several.ok = TRUE)

  params <- list(
    Season = season,
    Leverage = leverage
  )

  raw <- pbpstats_api("get-team-leverage-summary", league, params)
}

#' Fetch team players for a season from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-team-players-for-season” endpoint.
#'
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param team_id Integer or character. The pbpstats ID of the team.
#' @return A tibble of players for the specified team and season as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_team_players_for_season <- function(season, season_type = "Regular Season", team_id) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    TeamId = as.character(team_id)
  )

  raw <- pbpstats_api("get-team-players-for-season", "", params)
}

#' Fetch teams from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-teams” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @return A tibble of teams in the specified league as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_teams <- function(league = "nba") {

  raw <- pbpstats_api("get-teams", league)
}

#' Fetch top results from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-top-results” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param entity_type Character. One of:
#'   \itemize{
#'     \item `"Team"`
#'     \item `"Opponent"`
#'     \item `"Player"`
#'     \item `"Lineup"`
#'     \item `"LineupOpponent"`
#'   }.
#' @param stat Character. The statistic to rank by; one of:
#'   \itemize{
#'     \item `"AtRimFGA"`
#'     \item `"AtRimFGM"`
#'     \item `"ShortMidRangeFGA"`
#'     \item `"ShortMidRangeFGM"`
#'     \item `"LongMidRangeFGA"`
#'     \item `"LongMidRangeFGM"`
#'     \item `"Corner3FGA"`
#'     \item `"Corner3FGM"`
#'     \item `"Arc3FGA"`
#'     \item `"Arc3FGM"`
#'     \item `"UnassistedAtRim"`
#'     \item `"AssistedAtRim"`
#'     \item `"UnassistedShortMidRange"`
#'     \item `"AssistedShortMidRange"`
#'     \item `"UnassistedLongMidRange"`
#'     \item `"AssistedLongMidRange"`
#'     \item `"UnassistedCorner3"`
#'     \item `"AssistedCorner3"`
#'     \item `"UnassistedArc3"`
#'     \item `"AssistedArc3"`
#'     \item `"Fg3aBlocked"`
#'     \item `"Fg2aBlocked"`
#'     \item `"LiveBallTurnovers"`
#'     \item `"DeadBallTurnovers"`
#'     \item `"Turnovers"`
#'     \item `"AssistedFG2M"`
#'     \item `"UnassistedFG2M"`
#'     \item `"AssistedFG3M"`
#'     \item `"UnassistedFG3M"`
#'     \item `"Putbacks"`
#'     \item `"FG2A"`
#'     \item `"FG2M"`
#'     \item `"FG3A"`
#'     \item `"FG3M"`
#'     \item `"AtRimAssists"`
#'     \item `"Corner3Assists"`
#'     \item `"Arc3Assists"`
#'     \item `"TwoPtAssists"`
#'     \item `"ThreePtAssists"`
#'     \item `"AssistPoints"`
#'     \item `"OffThreePtRebounds"`
#'     \item `"OffTwoPtRebounds"`
#'     \item `"FTOffRebounds"`
#'     \item `"DefThreePtRebounds"`
#'     \item `"DefTwoPtRebounds"`
#'     \item `"FTDefRebounds"`
#'     \item `"ShootingFouls"`
#'     \item `"FoulsDrawn"`
#'     \item `"TwoPtShootingFoulsDrawn"`
#'     \item `"ThreePtShootingFoulsDrawn"`
#'     \item `"NonShootingFoulsDrawn"`
#'     \item `"Offensive Fouls"`
#'     \item `"Offensive FoulsDrawn"`
#'     \item `"Charge Fouls"`
#'     \item `"Charge FoulsDrawn"`
#'     \item `"Blocked2s"`
#'     \item `"Blocked3s"`
#'     \item `"TotalPoss"`
#'     \item `"Minutes"`
#'     \item `"PtsPer100Poss"`
#'     \item `"AtRimFrequency"`
#'     \item `"AtRimAccuracy"`
#'     \item `"AtRimPctAssisted"`
#'     \item `"ShortMidRangeFrequency"`
#'     \item `"ShortMidRangeAccuracy"`
#'     \item `"ShortMidRangePctAssisted"`
#'     \item `"LongMidRangeFrequency"`
#'     \item `"LongMidRangeAccuracy"`
#'     \item `"LongMidRangePctAssisted"`
#'     \item `"Corner3Frequency"`
#'     \item `"Corner3Accuracy"`
#'     \item `"Corner3PctAssisted"`
#'     \item `"Arc3Frequency"`
#'     \item `"Arc3Accuracy"`
#'     \item `"Arc3PctAssisted"`
#'     \item `"EfgPct"`
#'     \item `"AssistPointsPer100Poss"`
#'     \item `"DefFTReboundPct"`
#'     \item `"OffFTReboundPct"`
#'     \item `"DefTwoPtReboundPct"`
#'     \item `"OffTwoPtReboundPct"`
#'     \item `"DefThreePtReboundPct"`
#'     \item `"OffThreePtReboundPct"`
#'     \item `"DefFGReboundPct"`
#'     \item `"OffFGReboundPct"`
#'   }.
#' @param type Character. Whether to fetch `"season"` or `"game"` results.
#' @param sort_order Character. Sort order; `"Asc"` for ascending or `"Desc"` for descending.
#' @param min_cut_off Numeric. Minimum cutoff value for inclusion.
#' @return A tibble of the top results as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_top_results <- function(
    league = "nba",
    season_type = "Regular Season",
    entity_type,
    stat,
    type,
    sort_order = NULL,
    min_cut_off = NULL
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  entity_type <- match.arg(entity_type,
                           c("Team","Opponent","Lineup","LineupOpponent","Player"))

  type <- match.arg(type,
                    c("season","game"))

  sort_order <- match.arg(sort_order,
                          c("Asc","Desc"))

  params <- list(
    SeasonType = season_type,
    EntityType = entity_type,
    Stat = stat,
    Type = type,
    SortOrder = sort_order,
    MinCutOff = min_cut_off
  )

  raw <- pbpstats_api("get-top-results", league, params)
}

#' Fetch totals from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-totals” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g. `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param type Character. One of:
#'   \itemize{
#'     \item `"Player"`
#'     \item `"Team"`
#'     \item `"Opponent"`
#'     \item `"Lineup"`
#'     \item `"LineupOpponent"`
#'   }.
#' @param from_date Character or Date. Optional start date filter; defaults to `NULL`.
#' @param to_date Character or Date. Optional end date filter; defaults to `NULL`.
#' @param team_id Integer or character. Optional pbpstats team ID; defaults to `NULL`.
#' @param group_by Character. Optional grouping variable; e.g., `"Player"` or `"Team"`;
#' defaults to `NULL`.
#' @param leverage Character. One or more leverage levels; one of:
#'   \itemize{
#'     \item `"VeryLow"`
#'     \item `"Low"`
#'     \item `"Medium"`
#'     \item `"High"`
#'     \item `"VeryHigh"`
#'   }; comma‐separated for multiple values; defaults to `NULL`.
#' @param starter_state Character. One or more starter configurations; one of:
#'   \itemize{
#'     \item `"5v5"`
#'     \item `"4v5"`
#'     \item `"3v5"`
#'     \item `"2v5"`
#'     \item `"1v5"`
#'     \item `"0v5"`
#'   }; comma‐separated for multiple values; defaults to `NULL`.
#' @param start_type Character. One or more possession start types; one of:
#'   \itemize{
#'     \item `"All"`
#'     \item `"OffMissedFG"`
#'     \item `"OffMissed2"`
#'     \item `"OffMissed3"`
#'     \item `"OffMadeFG"`
#'     \item `"OffMade2"`
#'     \item `"OffMade3"`
#'     \item `"OffAtRimMake"`
#'     \item `"OffAtRimMiss"`
#'     \item `"OffAtRimBlock"`
#'     \item `"OffShortMidRangeMake"`
#'     \item `"OffShortMidRangeMiss"`
#'     \item `"OffLongMidRangeMake"`
#'     \item `"OffLongMidRangeMiss"`
#'     \item `"OffArc3Make"`
#'     \item `"OffArc3Miss"`
#'     \item `"OffCorner3Make"`
#'     \item `"OffCorner3Miss"`
#'     \item `"OffFTMake"`
#'     \item `"OffFTMiss"`
#'     \item `"OffLiveBallTurnover"`
#'     \item `"OffDeadball"`
#'     \item `"OffTimeout"`
#'   }; comma‐separated for multiple values; defaults to `NULL`.
#' @return A tibble of total metrics as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_totals <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    type,
    from_date = NULL,
    to_date = NULL,
    team_id = NULL,
    group_by = NULL,
    leverage = NULL,
    starter_state = NULL,
    start_type = NULL
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  type <- match.arg(type,
                    c("Team", "Opponent","Lineup","LineupOpponent","Player"))

  group_by <- match.arg(group_by,
                        c("Team", "Player"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    Type = type
  )

  if (!is.null(from_date)) {
    params$FromDate <- from_date
    params$ToDate <- to_date
  }
  if (!is.null(team_id)) {
    params$TeamId <- as.character(team_id)
  }
  if (!is.null(group_by)) {
    params$GroupBy <- group_by
  }

  if (!is.null(leverage)) {
    allowed_lv <- c("VeryLow","Low","Medium","High","VeryHigh")
    leverage   <- match.arg(leverage, allowed_lv, several.ok = TRUE)
    params$Leverage <- paste(leverage, collapse = ",")
  }

  if (!is.null(starter_state)) {
    allowed_ss <- c("5v5","4v5","3v5","2v5","1v5","0v5")
    starter_state <- match.arg(starter_state,
                               allowed_ss, several.ok = TRUE)
    params$StarterState <- paste(starter_state, collapse = ",")
  }

  if (!is.null(start_type)) {
    allowed_st <- c(
      "All", "OffMissedFG", "OffMissed2", "OffMissed3",
      "OffMadeFG", "OffMade2", "OffMade3",
      "OffAtRimMake", "OffAtRimMiss", "OffAtRimBlock",
      "OffShortMidRangeMake", "OffShortMidRangeMiss",
      "OffLongMidRangeMake", "OffLongMidRangeMiss",
      "OffArc3Make", "OffArc3Miss",
      "OffCorner3Make", "OffCorner3Miss",
      "OffFTMake", "OffFTMiss",
      "OffLiveBallTurnover", "OffDeadball", "OffTimeout"
    )
    start_type <- match.arg(start_type,
                            allowed_st, several.ok = TRUE)
    params$StartType <- paste(start_type, collapse = ",")
  }

  raw <- pbpstats_api("get-totals", league, params)
}

#' Fetch win probability from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-win-probability” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param margin Numeric. Point margin (difference in score).
#' @param pre_game_win_prob Numeric. Pre-game win probability as a decimal (e.g., `0.65` for 65%).
#' @param seconds_remaining Integer. Seconds remaining in the game.
#' @return A tibble of win probability data as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_win_probability <- function(league = "nba", margin, pre_game_win_prob, seconds_remaining) {

  url_addon <- paste0(league, "/", margin, "/", pre_game_win_prob, "/", seconds_remaining)

  raw <- pbpstats_api("get-win-probability", url_addon)
}

#' Fetch WOWY (With Or Without You) combination stats from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-wowy-combination-stats” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param team_id Integer or character. The pbpstats ID of the team.
#' @param player_id Integer, character, or numeric vector. One or more pbpstats player IDs to
#' include in the combination.
#' @param leverage Character. One or more leverage levels; one of:
#'   \itemize{
#'     \item `"VeryLow"`
#'     \item `"Low"`
#'     \item `"Medium"`
#'     \item `"High"`
#'     \item `"VeryHigh"`
#'   }; comma‐separated for multiple values; defaults to `NULL`.
#' @param only_common_games Logical. If `TRUE`, include only games in which all specified
#' `player_id`s played; defaults to `FALSE`.
#' @return A tibble of WOWY combination statistics as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_wowy_combination_stats <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    team_id,
    player_id,
    leverage = NULL,
    only_common_games = NULL
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  leverage <- match.arg(leverage,
                        c("VeryLow","Low","Medium","High","VeryHigh"), several.ok = TRUE)

  params <- list(
    Season = season,
    SeasonType = season_type,
    TeamId = as.character(team_id),
    PlayerIds = as.character(player_id),
    Leverage = leverage,
    OnlyCommonGames = only_common_games
  )

  raw <- pbpstats_api("get-wowy-combination-stats", league, params)
}

#' Fetch WOWY (With Or Without You) combo playing time distribution from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-wowy-combo-playing-time-distribution” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param team_id Integer or character. The pbpstats ID of the team.
#' @param player_id Integer, character, or numeric vector. One or more pbpstats player IDs to
#' include in the distribution.
#' @param all_players_on Logical. If `TRUE`, only returns results for when all specified
#' `player_id`s are on the court; defaults to `FALSE`.
#' @return A tibble of WOWY combo playing time distribution as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.  \url{https://api.pbpstats.com/docs}
#' @export
get_wowy_combo_playing_time_distribution <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    team_id,
    player_id,
    all_players_on
) {

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  params <- list(
    Season = season,
    SeasonType = season_type,
    TeamId = as.character(team_id),
    PlayerIds = as.character(player_id),
    OnlyShowAllPlayersOn = all_players_on
  )

  raw <- pbpstats_api("get-wowy-combo-playing-time-distribution", league, params)
}

#' Fetch WOWY (With Or Without You) statistics from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “get-wowy-stats” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @param season Character or integer. The season to query; e.g., `"2024-25"`.
#' @param season_type Character. One of:
#'   \itemize{
#'     \item `"Regular Season"`
#'     \item `"Playoffs"`
#'     \item `"PlayIn"`
#'     \item `"All"`
#'   }; defaults to `"Regular Season"`.
#' @param team_id Integer or character. The pbpstats ID of the team.
#' @param filter_comparison Character. One of:
#'   \itemize{
#'     \item `"Exactly"`
#'     \item `"GreaterThan"`
#'     \item `"LessThan"`
#'   }.
#' @param filter_event Character. One of:
#'   \itemize{
#'     \item `"OnFloor"`
#'     \item `"OffFloor"`
#'     \item `"PlayedInGame"`
#'     \item `"DidNotPlayInGame"`
#'     \item `"Started"`
#'     \item `"CameOffBench"`
#'   }.
#' @param filter_value Numeric or character. The value to filter with the specified comparison
#' and event.
#' @param type Character. One of:
#'   \itemize{
#'     \item `"Player"`
#'     \item `"Team"`
#'     \item `"Lineup"`
#'     \item `"LineupOpponent"`
#'     \item `"Opponent"`
#'   }.
#' @param starter_state Character. One or more starter configurations; one of:
#'   \itemize{
#'     \item `"5v5"`
#'     \item `"4v5"`
#'     \item `"3v5"`
#'     \item `"2v5"`
#'     \item `"1v5"`
#'     \item `"0v5"`
#'   }; comma‐separated for multiple values; defaults to `NULL`.
#' @param opponent_id Integer or character. Optional pbpstats ID of the opponent team;
#' defaults to `NULL`.
#' @param period Character or integer. One of:
#'   \itemize{
#'     \item `"All"`
#'     \item `"1"`
#'     \item `"2"`
#'     \item `"3"`
#'     \item `"4"`
#'     \item `"SecondHalf"`
#'   }; defaults to `NULL`.
#' @param from_margin Numeric. Optional starting point margin for possessions; defaults to `NULL`.
#' @param to_margin Numeric. Optional ending point margin for possessions; defaults to `NULL`.
#' @param from_time Integer. Optional seconds remaining at the start of a possession;
#' defaults to `NULL`.
#' @param to_time Integer. Optional seconds remaining at the end of a possession;
#' defaults to `NULL`.
#' @param leverage Character. One or more leverage levels; one of:
#'   \itemize{
#'     \item `"VeryLow"`
#'     \item `"Low"`
#'     \item `"Medium"`
#'     \item `"High"`
#'     \item `"VeryHigh"`
#'   }; comma‐separated for multiple values; defaults to `NULL`.
#' @details
#' Constructs the filter parameter name by combining `filter_comparison` and `filter_event`
#' (e.g. `ExactlyOnFloor`) and sets its value to `filter_value`. Supply `starter_state`,
#' `opponent_id`, `period`, margins, times, or `leverage` as needed.
#' @return A tibble of WOWY statistics as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.  \url{https://api.pbpstats.com/docs}
#' @export
get_wowy_stats <- function(
    league = "nba",
    season,
    season_type = "Regular Season",
    team_id,
    filter_comparison,
    filter_event,
    filter_value,
    type,
    starter_state = NULL,
    opponent_id = NULL,
    period = NULL,
    from_margin = NULL,
    to_margin = NULL,
    from_time = NULL,
    to_time = NULL,
    leverage = NULL
) {
  filter_comparison <- match.arg(
    filter_comparison,
    c("Exactly", "GreaterThan", "LessThan")
  )
  filter_event <- match.arg(
    filter_event,
    c(
      "OnFloor", "OffFloor", "PlayedInGame", "DidNotPlayInGame",
      "Started", "CameOffBench"
    )
  )
  filter_name <- paste0(filter_comparison, filter_event)

  season_type <- match.arg(season_type,
                           c("Regular Season","Playoffs","PlayIn","All"))

  type <- match.arg(type,
                    c("Team","Opponent","Lineup","LineupOpponent","Player"))

  leverage <- match.arg(leverage,
                        c("VeryLow","Low","Medium","High","VeryHigh"), several.ok = TRUE)

  params <- list(
    Season = season,
    SeasonType = season_type,
    TeamId = as.character(team_id),
    Type = type
  )

  if (!is.null(opponent_id)) params$Opponent <- as.character(opponent_id)
  if (!is.null(starter_state)) params$StarterState <- starter_state
  if (!is.null(period)) params$Period <- as.character(period)
  if (!is.null(from_margin)) params$FromMargin <- from_margin
  if (!is.null(to_margin)) params$ToMargin <- to_margin
  if (!is.null(from_time)) params$FromTime <- from_time
  if (!is.null(to_time)) params$ToTime <- to_time
  if (!is.null(leverage)) params$Leverage <- leverage

  raw <- pbpstats_api("get-wowy-stats", league, params)
  return(raw)
}

#' Fetch live game data from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “live/game” endpoint.
#'
#' @param game_id Character or integer. The pbpstats ID of the game.
#' @param result_type Character. One of:
#'   \itemize{
#'     \item `"team"`
#'     \item `"player"`
#'   }.
#' @return A tibble of live game data as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.  \url{https://api.pbpstats.com/docs}
#' @export
get_live_game <- function(game_id, result_type) {

  result_type <- match.arg(result_type,
                           c("team", "player"))

  url_addon <- paste0(as.character(game_id), "/", result_type)

  raw <- pbpstats_api("live/game", url_addon)
}

#' Fetch live games from the pbpstats API
#'
#' A thin wrapper around the pbpstats API’s “live/games” endpoint.
#'
#' @param league Character. Which league to query; e.g., `"nba"` or `"wnba"`; defaults to `"nba"`.
#' @return A tibble of live games data as returned by the API.
#' @seealso
#' \url{https://api.pbpstats.com/docs}
#' @references
#' Darryl Blackport (2020). PBPStats API, MIT License.
#' \url{https://api.pbpstats.com/docs}
#' @export
get_live_games <- function(league = "nba") {

  raw <- pbpstats_api("live/games", league)
}
