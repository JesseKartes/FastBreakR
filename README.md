
# FastBreakR

<img src="man/figures/fastbreakr_logo.png" width="350" align="right" />

FastBreakR is a fast and efficient R package for working with NBA data.
With a robust set of core tools, the package is fully functional and
continually evolving, with new features regularly added to enhance its
capabilities.

## Newly Added pbpstats API Wrapper

FastBreakR now includes a comprehensive wrapper for the popular pbpstats
API, developed by Daryl Blackport. This wrapper provides easy access to
every endpoint of the pbpstats API — including live game data,
play-by-play, team stats, player stats, and more. FastBreakR is the only
R package offering a complete pbpstats API wrapper covering all
available endpoints.

See <https://api.pbpstats.com/docs> for more info.

## Installation

You can install the development version of FastBreakR from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("JesseKartes/FastBreakR")
#> Using github PAT from envvar GITHUB_TOKEN. Use `gitcreds::gitcreds_set()` and unset GITHUB_TOKEN in .Renviron (or elsewhere) if you want to use the more secure git credential store instead.
#> Downloading GitHub repo JesseKartes/FastBreakR@HEAD
#> Rcpp (1.0.14 -> 1.1.0) [CRAN]
#> curl (6.3.0  -> 6.4.0) [CRAN]
#> Installing 2 packages: Rcpp, curl
#> Installing packages into '/private/var/folders/l2/0bcgcf_d3pq5259wmlvt2rq80000gn/T/RtmpwiELof/temp_libpathb59f3f885bb3'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/l2/0bcgcf_d3pq5259wmlvt2rq80000gn/T//Rtmp4QC7D8/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/l2/0bcgcf_d3pq5259wmlvt2rq80000gn/T/Rtmp4QC7D8/remotesbb396805dc87/JesseKartes-FastBreakR-d0a5a8d/DESCRIPTION’ ... OK
#> * preparing ‘FastBreakR’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> Omitted ‘LazyData’ from DESCRIPTION
#> * building ‘FastBreakR_0.1.5.tar.gz’
#> Installing package into '/private/var/folders/l2/0bcgcf_d3pq5259wmlvt2rq80000gn/T/RtmpwiELof/temp_libpathb59f3f885bb3'
#> (as 'lib' is unspecified)
```

## Core Features

- **Speed and Efficiency**: Optimized to process large data requests
  rapidly, FastBreakR includes built-in mechanisms like rate-limiting
  pauses to ensure smooth operation while adhering to NBA API
  constraints.
- **Data Consolidation**: Seamlessly merges multiple statistical
  categories into a single, unified dataset while automatically
  eliminating duplicate columns, ensuring a cleaner and more organized
  output for analysis.
- **Consistent and Tidy Naming**: Column names are cleaned and
  standardized while staying consistent with NBA naming conventions.
- **User-Friendly Design**: Functions are intuitive, making it easy to
  fetch and manipulate data.

## Example

This is a basic example which shows you how to get scores for the
2024-2025 season:

``` r
library(FastBreakR)

scores <- nba_scores(2025, season_type = "Regular Season")
#> Fetching season 2025 (1/1)
```

## Documentation

Full Documentation:
<https://jessekartes.github.io/FastBreakR/reference/index.html>

For any questions, suggestions, or issues, feel free to open an issue on
GitHub.
