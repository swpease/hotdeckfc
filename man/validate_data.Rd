% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hotdeckfc.R
\name{validate_data}
\alias{validate_data}
\title{Validators for hot deck forecast input.}
\usage{
validate_data(data, .datetime, .observation)
}
\arguments{
\item{data}{The input \code{.data} to \code{hot_deck_forecast}.}

\item{.datetime}{The datetime column of .data. Passed via pipe.}

\item{.observation}{The observation column of .data. Passed via pipe.}
}
\description{
Checks that:
\enumerate{
\item The data is a \code{tsibble}.
\item The tsibble is not multi-keyed (i.e. multiple time serieses).
\item The data has no gaps (i.e. missing rows; NA observations okay).
\item The final observation is not NA.
}
}
