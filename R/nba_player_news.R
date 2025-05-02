#' Get NBA Player News
#'
#' Retrieves and processes player news for a single or multiple players.
#'
#' @param player_id A single or vector of player IDs to fetch player news for.
#' @return A tibble containing the processed player news for the player(s).
#' @export
nba_player_news <- function(player_id) {
  convert_name <- function(name) {
    name %>%
      str_to_lower() %>%
      str_replace_all(" ", "-")
  }

  player_lookup <- nba_player_lookup() %>%
    mutate(across(c(person_id), as.character))

  results <- map_dfr(player_id, function(id) {
    player_list <- player_lookup %>%
      filter(person_id == as.character(id)) %>%
      mutate(player_name = convert_name(player_name))

    if (nrow(player_list) == 0) {
      message("Player ID ", id, " not found in lookup table.")
      return(NULL)
    }

    player_url <- paste0(
      "https://www.nba.com/player/", player_list$person_id,
      "/", player_list$player_name
    )

    fetch_player_news(player_url) %>%
      mutate(
        date = with_tz(mdy_hm(date, tz = "UTC"), "America/New_York"),
        player_id = player_list$person_id,
        player_slug = player_list$player_name
      )
  })

  return(results)
}

#' Fetch Player News from Profile Page
#'
#' This function fetches player news for given player ID's.
#'
#' @param url The NBA player's profile news page URL.
#' @return A tibble containing the raw player news.
fetch_player_news <- function(url) {
  tryCatch(
    {
      webpage <- read_html(url)

      news_items <- webpage %>%
        html_nodes(".PlayerNews_item__10b5O")

      if (length(news_items) == 0) {
        message("No news found for URL: ", url)
        return(tibble())
      }

      tibble(
        date = map_chr(
          news_items, ~ .x %>%
            html_node(".PlayerNews_date___Te0H") %>%
            html_text(trim = TRUE)
        ),
        headline = map_chr(
          news_items, ~ .x %>%
            html_node(".PlayerNews_headline__w4cFW") %>%
            html_text(trim = TRUE)
        ),
        update = map_chr(
          news_items, ~ .x %>%
            html_node(".PlayerNews_update__ntYMq") %>%
            html_text(trim = TRUE)
        )
      )
    },
    error = function(e) {
      message("Error scraping the webpage: ", e$message)
      return(tibble())
    }
  )
}
