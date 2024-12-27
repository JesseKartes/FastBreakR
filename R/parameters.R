#' Generate HTTP Headers for cdn.nba.com
#'
#' This function generates HTTP headers for the NBA Schedule API request.
#'
#' @return A named character vector of HTTP headers.
generate_headers_cdn <- function() {
    headers <- c(
        `Sec-Fetch-Site` = "same-site",
        `Accept` = "*/*",
        `Origin` = "https://www.nba.com",
        `Sec-Fetch-Dest` = "empty",
        `Accept-Language` = "en-US,en;q=0.9",
        `Sec-Fetch-Mode` = "cors",
        `Host` = "cdn.nba.com",
        `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15",
        `Referer` = "https://www.nba.com/",
        `Accept-Encoding` = "gzip, deflate, br",
        `Connection` = "keep-alive"
    )
    return(headers)
}

#' Generate HTTP Headers for stats.nba.com
#'
#' This function generates HTTP headers for stats API requests.
#'
#' @return A named character vector of HTTP headers.
generate_headers_stats <- function() {
    headers <- c(
        `Sec-Fetch-Site` = "same-site",
        `Accept` = "*/*",
        `Origin` = "https://www.nba.com",
        `Sec-Fetch-Dest` = "empty",
        `Accept-Language` = "en-US,en;q=0.9",
        `Sec-Fetch-Mode` = "cors",
        `Host` = "stats.nba.com",
        `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15",
        `Referer` = "https://www.nba.com/",
        `Accept-Encoding` = "gzip, deflate, br",
        `Connection` = "keep-alive"
    )
    return(headers)
}

#' Generate Query Parameters for Stats API Requests
#'
#' This function generates query parameters for the API request.
#'
#' @param year The season year for which to generate parameters.
#' @param measure_type The measure type for the API request.
#' @param season_type The season type for the API request.
#' @return A named list of query parameters.
generate_parameters_stats <- function(year, measure_type, season_type) {
    year <- (year - 1)
    season <- sprintf("%d-%02d", year, (year + 1) %% 100)
    params <- list(
        `DateFrom` = "",
        `DateTo` = "",
        `GameSegment` = "",
        `ISTRound` = "",
        `LastNGames` = "0",
        `LeagueID` = "00",
        `Location` = "",
        `MeasureType` = measure_type,
        `Month` = "0",
        `OpponentTeamID` = "0",
        `Outcome` = "",
        `PORound` = "0",
        `PaceAdjust` = "N",
        `PerMode` = "Totals",
        `Period` = "0",
        `PlusMinus` = "N",
        `Rank` = "N",
        `Season` = season,
        `SeasonSegment` = "",
        `SeasonType` = str_to_title(season_type),
        `ShotClockRange` = "",
        `VsConference` = "",
        `VsDivision` = ""
    )
    return(params)
}

#' Generate Query Parameters for Shots API Requests
#'
#' @param year The season year for which to generate parameters.
#' @param season_type The season type for the API request.
#' @return A list of query parameters for the API request.
generate_parameters_shots <- function(year, season_type) {
    year <- (year - 1)
    season <- sprintf("%d-%02d", year, (year + 1) %% 100)

    params = list(
        `AheadBehind` = '',
        # `CFID` = '155',
        # `CFPARAMS` = '2021-22',
        `ClutchTime` = '',
        `Conference` = '',
        `ContextFilter` = '',
        `ContextMeasure` = 'FGA',
        `DateFrom` = '',
        `DateTo` = '',
        `Division` = '',
        `EndPeriod` = '10',
        `EndRange` = '28800',
        `GROUP_ID` = '',
        `GameEventID` = '',
        `GameID` = '',
        `GameSegment` = '',
        `GroupID` = '',
        `GroupMode` = '',
        `GroupQuantity` = '5',
        `LastNGames` = '0',
        `LeagueID` = '00',
        `Location` = '',
        `Month` = '0',
        `OnOff` = '',
        `OppPlayerID` = '',
        `OpponentTeamID` = '0',
        `Outcome` = '',
        `PORound` = '0',
        `Period` = '0',
        `PlayerID` = '0',
        `PlayerID1` = '',
        `PlayerID2` = '',
        `PlayerID3` = '',
        `PlayerID4` = '',
        `PlayerID5` = '',
        `PlayerPosition` = '',
        `PointDiff` = '',
        `Position` = '',
        `RangeType` = '0',
        `RookieYear` = '',
        `Season` = season,
        `SeasonSegment` = '',
        `SeasonType` = season_type,
        `ShotClockRange` = '',
        `StartPeriod` = '1',
        `StartRange` = '0',
        `StarterBench` = '',
        `TeamID` = '0',
        `VsConference` = '',
        `VsDivision` = '',
        `VsPlayerID1` = '',
        `VsPlayerID2` = '',
        `VsPlayerID3` = '',
        `VsPlayerID4` = '',
        `VsPlayerID5` = '',
        `VsTeamID` = ''
    )
    return(params)
}

#' Generate Query Parameters for Standings API Requests
#'
#' This function generates the query parameters required for fetching NBA
#' standings data for a given season.
#'
#' @param year The season year for which to generate parameters.
#' @return A named list of query parameters.
generate_parameters_standings <- function(year) {
    year <- (year - 1)
    season <- sprintf("%d-%02d", year, (year + 1) %% 100)
    params = list(
        `GroupBy` = "conf",
        `LeagueID` = "00",
        `Season` = season,
        `SeasonType` = "Regular Season",
        `Section` = "overall"
    )
    return(params)
}

#' Generate Query Parameters for Box Score API Requests
#'
#' This function generates the query parameters required for fetching NBA box
#' scores data for a given season.
#'
#' @param game_id The game id for which to generate parameters.
#' @return A named list of query parameters.
generate_parameters_box_scores <- function(game_id) {
    params = list(
        `GameID` = as.character(game_id),
        `LeagueID` = "00",
        `endPeriod` = "0",
        `endRange` = "28800",
        `rangeType` = "0",
        `startPeriod` = "0",
        `startRange` = "0"
    )
    return(params)
}

#' Generate Query Parameters for Player Dictionary API Requests
#'
#' This function generates the query parameters required for fetching the
#' NBA player dictionary
#'
#' @return A named list of query parameters.
generate_parameters_player_dict <- function() {
    player_params = list(
        `College` = "",
        `Country` = "",
        `DraftPick` = "",
        `DraftRound` = "",
        `DraftYear` = "",
        `Height` = "",
        `Historical` = "1",
        `LeagueID` = "00",
        `Season` = "2024-25",
        `SeasonType` = "Regular Season",
        `TeamID` = "0",
        `Weight` = ""
    )
    return(player_params)
}

#' Generate Query Parameters for Team Dictionary API Requests
#'
#' This function generates the query parameters required for fetching the
#' NBA team dictionary
#'
#' @return A named list of query parameters.
generate_parameters_team_dict <- function() {
    team_params = list(
        `LeagueID` = "00",
        `Season` = "2024-25"
    )
    return(team_params)
}
