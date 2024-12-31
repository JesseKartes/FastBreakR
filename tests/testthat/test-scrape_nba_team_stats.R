test_that("nba_team_stats returns a combined data frame by default", {
  # Mocked bindings for testing
  with_mocked_bindings(
    nba_team_stats = function(seasons) {
      result <- tibble::tibble(
        season_year = rep(seasons, each = 2),
        team_name = c("Team A", "Team B", "Team C", "Team D"),
        points = c(100, 110, 120, 130)
      )
      return(result)
    },
    {
      result <- nba_team_stats(c(2024, 2023))

      # Check that season_year is present in the result
      expect_true(all(c(2024, 2023) %in% result$season_year))

      # Check that the result is a data frame
      expect_s3_class(result, "data.frame")
    }
  )
})

test_that("nba_team_stats returns a nested list when return_nested = TRUE", {
  # Mocked bindings for testing
  with_mocked_bindings(
    nba_team_stats = function(seasons, return_nested = TRUE) {
      result <- list(
        season_2024 = tibble::tibble(
          team_name = c("Team A", "Team B"),
          points = c(100, 110)
        ),
        season_2023 = tibble::tibble(
          team_name = c("Team C", "Team D"),
          points = c(120, 130)
        )
      )
      return(result)
    },
    {
      result <- nba_team_stats(c(2024, 2023), return_nested = TRUE)

      # Check that result is a list with seasons as names
      expect_named(result, c("season_2024", "season_2023"))

      # Ensure that each season contains a data frame with team data
      expect_s3_class(result$season_2024, "data.frame")
      expect_s3_class(result$season_2023, "data.frame")

      # Ensure that the number of rows for each season is correct
      expect_equal(nrow(result$season_2024), 2)
      expect_equal(nrow(result$season_2023), 2)
    }
  )
})
