#' Get NBA Shots
#'
#' This function gets NBA shot data for the specified seasons and returns a data frame.
#' Function pauses for five seconds after each season to prevent timeout issues.
#'
#' @param seasons A numeric vector of seasons (e.g., 2024) for which to fetch NBA shot data.
#' @param season_type A character string specifying the type of season.
#' Valid options include:
#' \itemize{
#'   \item \strong{"Pre Season"} - Pre Season games.
#'   \item \strong{"Regular Season"} - Regular Season games.
#'   \item \strong{"Playoffs"} - Playoff games.
#'   \item \strong{"All Star"} - All Star games.
#'   \item \strong{"IST"} - NBA Cup games.
#'   \item \strong{"PlayIn"} - Play In games.
#' }
#' @param return_nested A logical value. If FALSE (default), returns a single
#' combined data frame for all seasons. If TRUE, returns a list of data frames, one for each season.
#' @return A named a data frame containing NBA shots data for the specified seasons.
#' @export
nba_shots <- function(seasons,
                      season_type = "Regular Season",
                      return_nested = FALSE) {
  if (!is.numeric(seasons) || length(seasons) == 0) {
    stop("The 'seasons' parameter must be a non-empty numeric vector.")
  }

  results <- map_dfr(seq_along(seasons), function(i) {
    season <- seasons[i]
    message(glue("Fetching season {season} ({i}/{length(seasons)})"))

    # Try to fetch and process data for the season
    shots_data <- tryCatch(
      {
        data <- fetch_shots_data(season, season_type)

        data <- data$shots_data
      },
      error = function(e) {
        message(glue("Error fetching season {season}: {e$message}"))
        return(NULL) # Return NULL if an error occurs
      }
    )

    # Pause after processing each season unless it's the last
    if (i < length(seasons)) {
      message(glue(
        "Pausing for 5 seconds before fetching the next season..."
      ))
      Sys.sleep(5)
    }

    return(shots_data)
  })

  # Return nested results (list of data frames)
  if (return_nested) {
    results_list <- split(results, results$season_year)
    names(results_list) <- glue("season_{seasons}") %>% as.character()

    return(results_list)
  }

  # If return_nested is FALSE (default), return a combined data frame
  return(results)
}

#' Get League Average NBA Shots
#'
#' This function gets league average NBA shot data for the specified seasons and returns a
#' data frame.
#' Function pauses for five seconds after each season to prevent timeout issues.
#'
#' @param seasons A numeric vector of seasons (e.g., 2024) for which to fetch NBA shot data.
#' @param season_type A character string specifying the type of season.
#' Valid options include:
#' \itemize{
#'   \item \strong{"Pre Season"} - Pre Season games.
#'   \item \strong{"Regular Season"} - Regular Season games.
#'   \item \strong{"Playoffs"} - Playoff games.
#'   \item \strong{"All Star"} - All Star games.
#'   \item \strong{"IST"} - NBA Cup games.
#'   \item \strong{"PlayIn"} - Play In games.
#' }
#' @param return_nested A logical value. If FALSE (default), returns a single
#' combined data frame for all seasons. If TRUE, returns a list of data frames, one for each season.
#' @return A named a data frame containing league average NBA shots data for the specified seasons.
#' @export
nba_shots_league_avg <- function(seasons,
                                 season_type = "Regular Season",
                                 return_nested = FALSE) {
  if (!is.numeric(seasons) || length(seasons) == 0) {
    stop("The 'seasons' parameter must be a non-empty numeric vector.")
  }

  results <- map_dfr(seq_along(seasons), function(i) {
    season <- seasons[i]
    message(glue("Fetching season {season} ({i}/{length(seasons)})"))

    # Try to fetch and process data for the season
    shots_data <- tryCatch(
      {
        data <- fetch_shots_data(season, season_type)

        data <- data$league_data
      },
      error = function(e) {
        message(glue("Error fetching season {season}: {e$message}"))
        return(NULL) # Return NULL if an error occurs
      }
    )

    # Pause after processing each season unless it's the last
    if (i < length(seasons)) {
      message(glue(
        "Pausing for 5 seconds before fetching the next season..."
      ))
      Sys.sleep(5)
    }

    return(shots_data)
  })

  # Return nested results (list of data frames)
  if (return_nested) {
    results_list <- split(results, results$season_year)
    names(results_list) <- glue("season_{seasons}") %>% as.character()

    return(results_list)
  }

  # If return_nested is FALSE (default), return a combined data frame
  return(results)
}

#' Fetch NBA Shots from API
#'
#' This function fetches raw NBA shot data and league average data for a given season.
#'
#' @param season A numeric value representing the season (e.g., 2024).
#' @param season_type A character string specifying the type of season (e.g., "Regular Season").
#' @return A list containing raw shots data and league average data.
fetch_shots_data <- function(season, season_type) {
  headers <- generate_headers_stats()

  url <- "https://stats.nba.com/stats/shotchartdetail"

  params <- generate_params_shots(season, season_type)

  data <- get_data(url, headers, params)

  shots_column_names <- data$resultSets$headers[[1]] %>%
    as.character()

  shots_dt <- data$resultSets$rowSet[[1]] %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    set_names(shots_column_names) %>%
    clean_names() %>%
    mutate(
      season_year = season,
      game_date = as_date(game_date, format = "%Y%m%d")
    )

  league_column_names <- data$resultSets$headers[[2]] %>%
    as.character()

  league_dt <- data$resultSets$rowSet[[2]] %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    set_names(league_column_names) %>%
    clean_names() %>%
    mutate(season_year = season)

  list(
    shots_data = shots_dt,
    league_data = league_dt
  )
}

#' Process NBA Shots Data
#'
#' To be used on the output of `nba_shots()` or `nba_shots_league_avg()` within
#' the `nba_shot_chart()` function.
#' Processes shot and league average data within each NBA season.
#' Applies transformations to prepare the data for analysis and visualization.
#'
#' @param data A named list or data frame with shots or league average data.
#' @return A named list or data frame, whichever was provided as input.
process_shots <- function(data) {
  # If data is a single data frame
  if (is.data.frame(data)) {
    if ("game_id" %in% names(data)) {
      # Process shots data
      processed_shots <- data %>%
        mutate(
          loc_x = as.numeric(loc_x) / 10,
          loc_y = as.numeric(loc_y) / 10 + 5.25,
          loc_y = if_else(loc_y >= 47, 47, loc_y),
          shot_distance = as.numeric(shot_distance),
          shot_made_flag = factor(shot_made_flag,
            levels = c("1", "0"),
            labels = c("Make", "Miss")
          ),
          shot_value = if_else(shot_type == "3PT Field Goal", 3, 2),
          game_date = as.Date(game_date, "%Y%m%d")
        )
      return(processed_shots)
    } else {
      # Process league average data
      processed_league_avg <- data %>%
        mutate(
          fga = as.numeric(fga),
          fgm = as.numeric(fgm),
          fg_pct = as.numeric(fg_pct),
          shot_value = if_else(
            str_detect(shot_zone_basic, "3") | shot_zone_basic == "Backcourt",
            3, 2
          )
        )
      return(processed_league_avg)
    }

    # If data is a list
  } else if (is.list(data)) {
    if ("game_id" %in% names(data[[1]])) {
      # Process shots data for each data frame in the list.
      result <- data %>%
        map(function(season_data) {
          if (!is.data.frame(season_data)) {
            return(NULL)
          }
          processed_shots <- season_data %>%
            mutate(
              loc_x = as.numeric(loc_x) / 10,
              loc_y = as.numeric(loc_y) / 10 + 5.25,
              loc_y = if_else(loc_y >= 47, 47, loc_y),
              shot_distance = as.numeric(shot_distance),
              shot_made_flag = factor(shot_made_flag,
                levels = c("1", "0"),
                labels = c("Make", "Miss")
              ),
              shot_value = if_else(shot_type == "3PT Field Goal", 3, 2),
              game_date = as.Date(game_date, "%Y%m%d")
            )
          list(processed_shots = processed_shots)
        }) %>%
        discard(is.null)
      return(result)
    } else {
      # Process league_avg data for each data frame in the list.
      result <- data %>%
        map(function(season_data) {
          if (!is.data.frame(season_data)) {
            return(NULL)
          }
          processed_league_avg <- season_data %>%
            mutate(
              fga = as.numeric(fga),
              fgm = as.numeric(fgm),
              fg_pct = as.numeric(fg_pct),
              shot_value = if_else(
                str_detect(
                  shot_zone_basic, "3"
                ) | shot_zone_basic == "Backcourt", 3, 2
              )
            )
          list(processed_league_avg = processed_league_avg)
        }) %>%
        discard(is.null)
      return(result)
    }
  } else {
    stop("Input data must be a data frame or a list of data frames.")
  }
}

#' NBA Half‑Court Plot
#'
#' Create a \pkg{ggplot2} half‑court plot of an NBA basketball court.
#' Draws the court boundary, paint, free‑throw circle, restricted area, rim, backboard,
#' 3‑point line, and mid‑court hash marks. To be used as a background for shot‑chart functions.
#'
#' @param title A character string to use as the plot title.
#' @return A \code{\link[ggplot2]{ggplot}} object representing the NBA half‑court.
nba_half_court <- function(title = NULL) {
  # Helper function: create circle/arc data
  circle_arc <- function(cx, cy, r, start, end, n = 100) {
    theta <- seq(start, end, length.out = n)
    data.frame(
      x = cx + r * cos(theta),
      y = cy + r * sin(theta)
    )
  }

  # Court boundary (50 ft wide, 47 ft long)
  df_court_boundary <- data.frame(
    xmin = -25, xmax = 25, ymin = 0, ymax = 47
  )

  # The paint (key) with a semi-transparent fill (16 ft wide, from 0 to 19 ft)
  df_paint <- data.frame(
    xmin = -8, xmax = 8, ymin = 0, ymax = 19
  )

  # Free-throw circle: split into top (solid) and bottom (dashed) halves
  # (centered at (0,19))
  df_free_throw_top <- circle_arc(
    cx = 0, cy = 19, r = 6, start = 0, end = pi
  )
  df_free_throw_bottom <- circle_arc(
    cx = 0, cy = 19, r = 6, start = pi, end = 2 * pi
  )

  # Restricted area (semi-circle, oriented upward; center at (0,4))
  df_restricted <- circle_arc(
    cx = 0, cy = 4, r = 4, start = 0, end = pi
  )

  # Rim: shifted so the back of the rim aligns with the backboard (at y = 4)
  df_rim <- circle_arc(
    cx = 0, cy = 4 + 0.75, r = 0.75, start = 0, end = 2 * pi
  )
  # Optional inner rim for additional detail (using a slightly smaller radius)
  df_inner_rim <- circle_arc(
    cx = 0, cy = 4 + 0.75, r = 0.5, start = 0, end = 2 * pi
  )

  # Backboard: horizontal line 6 ft wide at y = 4
  df_backboard <- data.frame(
    x1 = -3, x2 = 3, y1 = 4, y2 = 4
  )

  # 3-pt line: corner lines and arc from (0,4)
  corner_to_arc_angle <- acos(22 / 23.75)
  t_left <- pi - corner_to_arc_angle
  t_right <- corner_to_arc_angle
  df_3pt_arc <- circle_arc(
    cx = 0, cy = 4, r = 23.75, start = t_right, end = t_left
  )
  corner_intersect_y <- 4 + 23.75 * sin(corner_to_arc_angle)

  # Build ggplot object
  p <- ggplot() +
    # Background: wooden floor look
    geom_rect(
      data = df_court_boundary,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "burlywood1", color = NA
    ) +

    # Outer boundary lines
    geom_rect(
      data = df_court_boundary,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = NA, color = "black", linewidth = 1
    ) +

    # Paint area with semi-transparent fill
    geom_rect(
      data = df_paint,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "wheat2", alpha = 0.5, color = "black", linewidth = 1
    ) +

    # Free-throw circle: top half (solid) and bottom half (dashed)
    geom_path(
      data = df_free_throw_top, aes(x, y),
      color = "black", linewidth = 1
    ) +
    geom_path(
      data = df_free_throw_bottom, aes(x, y),
      color = "black", linetype = "dashed", linewidth = 1
    ) +

    # Restricted area
    geom_path(
      data = df_restricted, aes(x, y),
      color = "black", linewidth = 1
    ) +

    # Rim (outer rim in orange, inner rim in white for detail)
    geom_path(
      data = df_rim, aes(x, y),
      color = "orange", linewidth = 1.2
    ) +
    geom_path(
      data = df_inner_rim, aes(x, y),
      color = "white", linewidth = 1
    ) +

    # Backboard
    geom_segment(
      data = df_backboard,
      aes(x = x1, y = y1, xend = x2, yend = y2),
      color = "black", linewidth = 1.2
    ) +

    # 3-pt corner lines
    geom_segment(aes(x = 22, y = 0, xend = 22, yend = corner_intersect_y),
      color = "black", linewidth = 1
    ) +
    geom_segment(aes(x = -22, y = 0, xend = -22, yend = corner_intersect_y),
      color = "black", linewidth = 1
    ) +

    # 3-pt arc
    geom_path(
      data = df_3pt_arc, aes(x, y),
      color = "black", linewidth = 1
    ) +

    # Single hash marks on each sideline at mid-court (y = 23.5)
    geom_segment(aes(x = -25, y = 28, xend = -23, yend = 28),
      color = "black", linewidth = 1
    ) +
    geom_segment(aes(x = 25, y = 28, xend = 23, yend = 28),
      color = "black", linewidth = 1
    ) +
    coord_fixed() +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "burlywood1", color = NA),
      plot.background = element_rect(fill = "burlywood1", color = NA),
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    ggtitle(title)

  return(p)
}

#' NBA Shot Chart
#'
#' Create and return a shot chart for the given shot data, either as an
#' interactive Plotly object or a static ggplot2 plot.
#' Internally calls \code{process_shots()} to prepare the data, then overlays the shots on the
#' half‑court background.
#'
#' @param shot_data A \code{data.frame} containing at least \code{loc_x},
#' \code{loc_y}, and \code{shot_made_flag} columns (plus optional
#' \code{shot_distance}, \code{shot_value}, and \code{game_date} for tooltips).
#' @param interactive \code{logical} Whether to return an interactive Plotly chart (\code{TRUE})
#' or a static ggplot2 plot (\code{FALSE}). Defaults to \code{TRUE}.
#' @param title A \code{character} string to use as the chart title. Defaults to \code{NULL}.
#' @return If \code{process_shots()} returns a data frame, that data frame is returned.
#' Otherwise, returns a \code{ggplot2} object (when
#' \code{interactive = FALSE}) or a Plotly object (when
#' \code{interactive = TRUE}).
#'
#' @examples
#' \dontrun{
#' shots_data <- FastBreakR::nba_shots(2025) %>%
#'   dplyr::filter(player_name == "Stephen Curry")
#'
#' FastBreakR::nba_shot_chart(
#'   shot_data = shots_data,
#'   interactive = FALSE,
#'   title = "Steph Curry Shot Chart"
#' )
#' }
#'
#' @export
nba_shot_chart <- function(shot_data, interactive = TRUE, title = NULL) {
  if (!all(c("loc_x", "loc_y", "shot_made_flag") %in% names(shot_data))) {
    stop("Input must contain loc_x, loc_y, and shot_made_flag columns.")
  }
  shots_df <- process_shots(shot_data)
  if (!is.data.frame(shots_df)) {
    shots_df <- bind_rows(shots_df)
  }
  p <- nba_half_court(title) +
    geom_point(
      data = shots_df,
      aes(
        x = loc_x,
        y = loc_y,
        color = shot_made_flag,
        text = glue(
          "Distance: {round(shot_distance,1)} ft\n",
          "Shot Value: {shot_value} pts\n",
          "Result: {shot_made_flag}\n",
          "Date: {format(game_date, '%Y-%m-%d')}"
        )
      ),
      size = 3,
      alpha = 0.8
    ) +
    scale_color_manual(
      name = NULL,
      values = c(Make = "forestgreen", Miss = "mediumpurple"),
      labels = c(Make = "Make", Miss = "Miss")
    ) +
    theme(legend.position = "bottom")
  if (interactive) {
    p <- ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 0
        ),
        hoverlabel = list(
          font = list(color = "white")
        ),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      )
  }
  return(p)
}
