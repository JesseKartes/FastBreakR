# tests/testthat/test-scrape_nba_team_stats.R

test_that("nba_team_stats handles invalid input", {
  # Test for non-numeric input
  expect_error(
    nba_team_stats("2024"),
    "The 'seasons' parameter must be a non-empty numeric vector."
  )

  # Test for empty vector input
  expect_error(
    nba_team_stats(numeric(0)),
    "The 'seasons' parameter must be a non-empty numeric vector."
  )
})

test_that("nba_team_stats returns a named list with valid seasons", {
  # Mock the helper functions
  mock_fetch_team_stats <- function(season, measure_type, season_type) {
    tibble::tibble(
      game_date = as.character(Sys.Date()),
      min = 48,
      points = 100
    )
  }
  mock_process_team_measures <- function(team_data) {
    team_data %>%
      dplyr::mutate(
        game_date = lubridate::as_date(game_date),
        min = as.numeric(min),
        points = as.numeric(points)
      )
  }

  # Use testthat::with_mocked_bindings for mocking
  with_mocked_bindings(
    fetch_team_stats = mock_fetch_team_stats,
    process_team_measures = mock_process_team_measures,
    {
      # Call the function with mock data
      result <- nba_team_stats(c(2024, 2023))

      # Test that the output is a named list
      expect_type(result, "list")
      expect_named(result, c("season_2024", "season_2023"))

      # Test that each list element contains data frames
      expect_s3_class(result$season_2024$Base, "data.frame")
      expect_s3_class(result$season_2023$Base, "data.frame")
    }
  )
})

test_that("nba_team_stats handles fetch errors gracefully", {
  # Mock fetch_team_stats to throw an error
  mock_fetch_team_stats <- function(season, measure_type, season_type) {
    stop("Data fetch error")
  }
  mock_process_team_measures <- function(team_data) team_data

  # Use testthat::with_mocked_bindings for mocking
  with_mocked_bindings(
    fetch_team_stats = mock_fetch_team_stats,
    process_team_measures = mock_process_team_measures,
    {
      # Call the function with a valid season and expect NULL for failed fetch
      expect_message(
        result <- nba_team_stats(2024),
        "Error processing season 2024"
      )
      expect_null(result$season_2024)
    }
  )
})
