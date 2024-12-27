
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FastBreakR

<!-- badges: start -->
<!-- badges: end -->

FastBreakR is a fast and efficient R package for working with NBA data.

## Installation

You can install the development version of FastBreakR from GitHub with:

``` r
# install.packages("pak")
pak::pak("JayKay-15/FastBreakR")
```

## Key Features

- **Easy-to-use functions** for fetching and processing various types of
  NBA data, including game scores, team statistics, shot data and play
  by play data.
- **Modular design** that seamlessly integrates NBA data into your
  analysis pipeline.
- **Integration with `tidyverse`**, enabling effortless manipulation,
  analysis, and visualization of NBA data.

## Example

This is a basic example which shows you how to get scores for the
2024-2025 season:

``` r
library(FastBreakR)

scores <- nba_scores(2025, season_type = "Regular Season")
#> Processing season 2025 (1/1)

head(scores)
#> $season_2025
#> # A tibble: 878 × 22
#>    season_year team_id    team_abbreviation team_name              opp_team_id
#>          <dbl> <chr>      <chr>             <chr>                  <chr>      
#>  1        2025 1610612752 NYK               New York Knicks        1610612738 
#>  2        2025 1610612738 BOS               Boston Celtics         1610612752 
#>  3        2025 1610612750 MIN               Minnesota Timberwolves 1610612747 
#>  4        2025 1610612747 LAL               Los Angeles Lakers     1610612750 
#>  5        2025 1610612754 IND               Indiana Pacers         1610612765 
#>  6        2025 1610612765 DET               Detroit Pistons        1610612754 
#>  7        2025 1610612751 BKN               Brooklyn Nets          1610612737 
#>  8        2025 1610612737 ATL               Atlanta Hawks          1610612751 
#>  9        2025 1610612753 ORL               Orlando Magic          1610612748 
#> 10        2025 1610612748 MIA               Miami Heat             1610612753 
#> # ℹ 868 more rows
#> # ℹ 17 more variables: opp_team_abbreviation <chr>, opp_team_name <chr>,
#> #   game_id <chr>, game_date <date>, matchup <chr>, location <chr>, wl <chr>,
#> #   min <dbl>, pts <dbl>, opp_pts <dbl>, plus_minus <dbl>, game_count <dbl>,
#> #   opp_game_count <dbl>, is_b2b_first <lgl>, is_b2b_second <lgl>,
#> #   opp_is_b2b_first <lgl>, opp_is_b2b_second <lgl>
```

## Documentation

Full Documentation: <https://jaykay-15.github.io/FastBreakR/>

For any questions, suggestions, or issues, feel free to open an issue on
GitHub.
