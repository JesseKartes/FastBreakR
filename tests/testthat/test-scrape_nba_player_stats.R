# tests/testthat/test-scrape_nba_player_stats.R

test_that("nba_player_stats works correctly for valid seasons", {
    # Mock fetch_player_stats to return fake data
    mock_fetch_player_stats <- function(season, measure_type, season_type) {
        tibble::tibble(
            player_id = c(1, 2),
            player_name = c("Player A", "Player B"),
            game_date = c("2024-01-01", "2024-01-02"),
            min = c("30", "35"),
            points = c(20, 25)
        )
    }

    # Mock process_player_measures to process data correctly
    mock_process_player_measures <- function(player_data) {
        player_data %>%
            dplyr::mutate(game_date = as.Date(game_date), min = as.numeric(min))
    }

    # Replace dependent functions with mocks
    with_mocked_bindings(
        fetch_player_stats = mock_fetch_player_stats,
        process_player_measures = mock_process_player_measures,
        {
            # Call the function for seasons 2024 and 2023
            result <- nba_player_stats(c(2024, 2023))

            # Assertions
            expect_type(result, "list")
            expect_named(result, c("season_2024", "season_2023"))

            # Check data frame structure for season_2024
            expect_s3_class(result$season_2024$Base, "data.frame")
            expect_equal(nrow(result$season_2024$Base), 2)
            expect_equal(result$season_2024$Base$player_name[1], "Player A")
        }
    )
})

test_that("nba_player_stats handles fetch errors gracefully", {
    # Mock fetch_player_stats to throw an error
    mock_fetch_player_stats <- function(season, measure_type, season_type) {
        stop("Data fetch error")
    }

    # Mock process_player_measures (won't be called)
    mock_process_player_measures <- function(player_data) player_data

    with_mocked_bindings(
        fetch_player_stats = mock_fetch_player_stats,
        process_player_measures = mock_process_player_measures,
        {
            # Call the function with an error during fetch
            expect_message(result <- nba_player_stats(c(2024, 2023)),
                           "Error processing season 2024")

            # Result should have NULL for the failed season
            expect_null(result$season_2024)
            expect_null(result$season_2023)
        }
    )
})

test_that("scrape_nba_player_stats validates input seasons correctly", {
    # Invalid inputs
    expect_error(nba_player_stats(NULL),
                 "The 'seasons' parameter must be a non-empty numeric vector.")
    expect_error(nba_player_stats("2024"),
                 "The 'seasons' parameter must be a non-empty numeric vector.")
    expect_error(nba_player_stats(c()),
                 "The 'seasons' parameter must be a non-empty numeric vector.")
})
