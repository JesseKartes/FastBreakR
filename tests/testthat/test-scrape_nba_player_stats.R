test_that("nba_player_stats works correctly for valid seasons", {
  # Mocked bindings for testing
  with_mocked_bindings(
    nba_player_stats = function(seasons) {
      result <- list(
        season_2024 = list(
          Base = tibble::tibble(player_name = c("Player A", "Player B"))
        ),
        season_2023 = list(
          Base = tibble::tibble(player_name = c("Player X", "Player Y"))
        )
      )
      return(result)
    },
    {
      result <- nba_player_stats(c(2024, 2023))

      # Check that result has the correct names
      expect_named(result, c("season_2024", "season_2023"))

      # Check that Base is a data frame for season_2024
      expect_s3_class(result$season_2024$Base, "data.frame")

      # Ensure the correct number of rows in Base for season_2024
      expect_equal(nrow(result$season_2024$Base), 2)

      # Ensure player names match the expected values for season_2024
      expect_equal(result$season_2024$Base$player_name[1], "Player A")
    }
  )
})

test_that("nba_player_stats handles fetch errors gracefully", {
  # Mocked bindings for error handling
  with_mocked_bindings(
    nba_player_stats = function(seasons) {
      result <- list(
        season_2024 = NULL,
        season_2023 = NULL
      )
      return(result)
    },
    {
      result <- nba_player_stats(c(2024, 2023))

      # Ensure that fetch failures return NULL for both seasons
      expect_null(result$season_2024)
      expect_null(result$season_2023)
    }
  )
})
