#' Append a column of leads
#'
#' Appends a new column, named "next_obs", of the next (i.e. lead) observations.
#'
#' The corresponding selector, [sample_lead()], defaults to this
#' name for the lead column, so it makes things slightly easier in that regard.
#'
#' This appender should be paired with [sample_lead()] for use in CV.
#' It is applied to the training data after the train-test split,
#' to avoid data leakage.
#'
#' @param .data tsibble. The data.
#' @param .observation symbol. The observation column.
#' @returns .data, augmented with a column, named `next_obs`, of leads.
#'
#' @examples
#' data = tsibble::tsibble(date = as.Date("2022-02-02") + 0:9,
#'                         obs = 1:10,
#'                         index = date)
#' append_lead(data, obs)
#'
#' @export
append_lead <- function(.data, .observation) {
  .data %>% mutate(next_obs = dplyr::lead({{ .observation }}))
}


#' Append columns of covariate and target leads
#'
#' Adds two new columns, named "next_cov_obs" and "next_target_obs",
#' of the next (i.e. lead) covariate and target observations.
#'
#' The corresponding, covariate-using selectors, [sample_covariate_lead()]
#' and [sample_forecasted_covariate()] default to these name(s)
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
#' @param .data_ts tsibble. The data.
#' @param .cov_observation symbol. The covariate observation column.
#' @param target_obs_col_name string. The target observation column name.
#' @returns .data_ts, augmented with two columns:
#'   * `next_cov_obs`, of covariate leads.
#'   * `next_target_obs`, of target leads.
#'
#' @examples
#' # regular use
#' data = tsibble::tsibble(date = as.Date("2022-02-02") + 0:9,
#'                         obs = 1:10,
#'                         cov_obs = 11:20,
#'                         index = date)
#' append_lead_cov_lead(data, cov_obs, "obs")
#'
#' # if you need to partialize it for CV
#' data = tsibble::tsibble(date = as.Date("2022-02-02") + 0:9,
#'                         obs = 1:10,
#'                         cov_obs = 11:20,
#'                         index = date)
#' append_cov_partial = purrr::partial(append_lead_cov_lead, ... =, "obs")
#' append_cov_partial(data, cov_obs)
#'
#' @export
append_lead_cov_lead <- function(.data_ts,
                             .cov_observation,
                             target_obs_col_name = "observation") {
  .data_ts %>% mutate(
    next_cov_obs = dplyr::lead({{ .cov_observation }}),
    next_target_obs = dplyr::lead(.data[[target_obs_col_name]]))
}


#' Append a column of differences-to-next-observation
#'
#' Adds a new column, named "diff_to_next_obs", of the differences to the
#' next (i.e. lead) observations.
#'
#' The corresponding selector, [sample_diff()], defaults to this name
#' for the diff column, so it makes things slightly easier in that regard.
#'
#' This appender should be paired with [sample_diff()] for use in CV.
#' It is applied to the training data after the train-test split,
#' to avoid data leakage.
#'
#' @param .data tsibble. The data.
#' @param .observation symbol. The observation column.
#' @returns .data, augmented with a column, named `diff_to_next_obs`,
#' of leads of differences.
#'
#' @examples
#' data = tsibble::tsibble(date = as.Date("2022-02-02") + 0:9,
#'                         obs = 1:10,
#'                         index = date)
#' append_diff(data, obs)
#'
#' @export
append_diff <- function(.data, .observation) {
  .data %>%
    mutate(diff_to_next_obs = dplyr::lead(tsibble::difference({{ .observation }})))
}


#' Return data, unmodified
#'
#' Do nothing to your data.
#'
#' This appender should be paired with any sampler functions that do not have
#' data leakage concerns for use in CV.
#' It is applied to the training data after the train-test split.
#'
#' @param .data tsibble. The data.
#' @param .observation symbol. The observation column.
#' @returns .data
#'
#' @examples
#' data = tsibble::tsibble(date = as.Date("2022-02-02") + 0:9,
#'                         obs = 1:10,
#'                         index = date)
#' append_nothing(data, obs)
#'
#' @export
append_nothing <- function(.data, .observation) {
  .data
}


#' Append a column of lags
#'
#' Appends a new column, named "next_obs", of the prior (i.e. lag) observations.
#'
#' The corresponding selector is [sample_lead()].
#'
#' This appender is for [impute()] usage, applied internally during
#' backcasting. Note that the column name is still "next_obs", because
#' of the kludgy implementation of backcasting (co-opting the
#' forecasting infrastructure).
#'
#' @param .data tsibble. The data.
#' @param .observation symbol. The observation column.
#' @returns .data, augmented with a column, named `next_obs`, of lags.
#'
#' @examples
#' data = tsibble::tsibble(date = as.Date("2022-02-02") + 0:9,
#'                         obs = 1:10,
#'                         index = date)
#' append_lag(data, obs)
#'
#' @noRd
append_lag <- function(.data, .observation) {
  .data %>% mutate(next_obs = dplyr::lag({{ .observation }}))
}




#' Append a column of differences-to-prior-observation
#'
#' Adds a new column, named "diff_to_next_obs", of the differences to the
#' prior (i.e. lag) observations.
#'
#' The corresponding selector is [sample_diff()].
#'
#' This appender is for [impute()] usage, applied internally during
#' backcasting. Note that the column name is still "diff_to_next_obs", because
#' of the kludgy implementation of backcasting (co-opting the
#' forecasting infrastructure).
#'
#' @param .data tsibble. The data.
#' @param .observation symbol. The observation column.
#' @returns .data, augmented with a column, named `diff_to_next_obs`,
#' of differences to the prior observations.
#'
#' @examples
#' data = tsibble::tsibble(date = as.Date("2022-02-02") + 0:4,
#'                         obs = c(1,3,5,1,10),
#'                         index = date)
#' append_lag_diff(data, obs)
#'
#' @noRd
append_lag_diff <- function(.data, .observation) {
  .data %>%
    mutate(diff_to_next_obs = -1 * tsibble::difference({{ .observation }}))
}
