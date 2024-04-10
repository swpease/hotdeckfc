#' Add a column of leads to data.
#'
#' Adds a new column, named "next_obs", of the next (i.e. lead) observations.
#'
#' The corresponding selector, `sample_lead`, defaults to this
#' name for the lead column, so it makes things slightly easier in that regard.
#'
#' This appender should be paired with `sample_lead` for use in CV.
#' It is applied to the training data after the train-test split,
#' to avoid data leakage.
#'
#' @param .data The data. A tsibble.
#' @param .observation The observation column. Passed via pipe.
#' @returns .data, augmented with a column, named `next_obs`, of leads.
#'
#' @export
append_lead <- function(.data, .observation) {
  .data %>% mutate(next_obs = dplyr::lead({{ .observation }}))
}


#' Add a column of covariate and target leads to data.
#'
#' Adds two new columns, named "next_cov_obs" and "next_target_obs",
#' of the next (i.e. lead) covariate and target observations.
#'
#' The corresponding, covariate-using selectors, `sample_covariate_lead`
#' and `sample_forecasted_covariate` default to these name(s)
#' for the lead column(s) (the latter doesn't actually use the covariate's lead),
#' so it makes things slightly easier in that regard.
#'
#' This appender should be paired with samplers that rely on
#' covariates, for use in CV. If your target observation column is not named
#' "observation", you can use `purrr::partial` to pass the right name
#' before passing that output to CV.
#'
#' It is applied to the training data after the train-test split,
#' to avoid data leakage.
#'
#' @param .data_ts The data. A tsibble.
#' @param .cov_observation The covariate observation column. Passed via pipe.
#' @param target_obs_col_name The target observation column name.
#' @returns .data_ts, augmented with two columns:
#'   `next_cov_obs`, of covariate leads.
#'   `next_target_obs`, of target leads.
#'
#' @export
append_lead_cov_lead <- function(.data_ts,
                             .cov_observation,
                             target_obs_col_name = "observation") {
  .data_ts %>% mutate(
    next_cov_obs = dplyr::lead({{ .cov_observation }}),
    next_target_obs = dplyr::lead(.data[[target_obs_col_name]]))
}


#' Add a column of differences to the next observation.
#'
#' Adds a new column, named "diff_to_next_obs", of the differences to the
#' next (i.e. lead) observations.
#'
#' The corresponding selector, `sample_diff`, defaults to this name
#' for the diff column, so it makes things slightly easier in that regard.
#'
#' This appender should be paired with `sample_diff` for use in CV.
#' It is applied to the training data after the train-test split,
#' to avoid data leakage.
#'
#' @param .data The data. A tsibble.
#' @param .observation The observation column. Passed via pipe.
#' @returns .data, augmented with a column, named `diff_to_next_obs`,
#' of leads of differences.
#'
#' @export
append_diff <- function(.data, .observation) {
  .data %>%
    mutate(diff_to_next_obs = dplyr::lead(tsibble::difference({{ .observation }})))
}


#' Return data, unmodified.
#'
#' This appender should be paired with any sampler functions that do not have
#' data leakage concerns (as with those that rely on, say, `lead` or `diff`),
#' for use in CV.
#' It is applied to the training data after the train-test split.
#'
#' @param .data The data. A tsibble.
#' @param .observation The observation column. Passed via pipe.
#' @returns .data
#'
#' @export
append_nothing <- function(.data, .observation) {
  .data
}
