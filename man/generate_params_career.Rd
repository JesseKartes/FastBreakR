% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameters.R
\name{generate_params_career}
\alias{generate_params_career}
\title{Generate Query Parameters for Player Career API Requests}
\usage{
generate_params_career(player_id, per_mode = "PerGame")
}
\arguments{
\item{player_id}{The player id for which to generate parameters.}

\item{per_mode}{A character string representing the aggregation method}
}
\value{
A named list of query parameters.
}
\description{
This function generates the query parameters required for fetching the
NBA player's career page
}
