% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid_search.R
\name{build_window_args}
\alias{build_window_args}
\title{Build grid search window argument.}
\usage{
build_window_args(window_back, window_fwd = window_back)
}
\arguments{
\item{window_back}{The window_back. length == 1 or h.}

\item{window_fwd}{The window_fwd. length == 1 or h.}
}
\value{
list(window_back = window_back, window_fwd = window_fwd)
}
\description{
Use this function to build the \code{window_args} arg of \code{build_grid}.
}
