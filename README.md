
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
```

## Documentation

Full Documentation: <https://jaykay-15.github.io/FastBreakR/>

For any questions, suggestions, or issues, feel free to open an issue on
GitHub.
