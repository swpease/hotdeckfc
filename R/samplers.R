#' Generate a hot deck forecast value using `lead`s.
#'
#' Given a `local_rows` and `current_obs`, it:
#'   - Finds those rows in `local_rows` with observations closest to `current_obs`.
#'   - Takes the `n_closest` of those.
#'   - Randomly samples one of those.
#'   - Returns that sample's "tomorrow" observation as both
#'     the next `current_obs` and the `forecast`.
#'
#' For `n_closest`, `dplyr::slice_min` is used, and tie values are included.
#'
#' @param next_obs_col_name Name of the column in your data containing the next
#' observation (from, e.g. `dplyr::lead`).
#' @returns list(new_current_obs, forecast), where
#' new_current_obs = The value to use for `current_obs` in the next iteration.
#' forecast = The forecasted value.
#'
#' @export
hot_deck_lead_sampler <- function(next_obs_col_name = "next_obs") {
  purrr::partial(internal_hot_deck_lead_sampler, ... =, next_obs_col_name)
}


#' Wrapped method for `hot_deck_lead_sampler`.
#'
#' @param local_rows tibble (NOT tsibble) The `local_rows` in `simulate_sample_path`.
#' @param .observation The observation column.
#' @param current_obs The `current_obs` in `simulate_sample_path`.
#' @param n_closest Scalar.
#' @param next_obs_col_name Name of the column in your data containing the next
#' observation (from, e.g. `dplyr::lead`).
#' @returns list(new_current_obs, forecast), where
#' new_current_obs = The value to use for `current_obs` in the next iteration,
#'                   i.e. the next forecast.
#' forecast = The forecasted value.
internal_hot_deck_lead_sampler <- function(local_rows,
                                            .observation,
                                            current_obs,
                                            n_closest,
                                            next_obs_col_name) {
  rand_row = local_rows %>% sample_local_rows({{ .observation }},
                                              current_obs = current_obs,
                                              n_closest = n_closest,
                                              filter_na_col_names = next_obs_col_name)
  # What was tomorrow's obs for our randomly selected row that had
  # a similar obs to our current obs?
  # That's our new current obs, as well as our forecast.
  new_current_obs = rand_row %>% dplyr::pull(.data[[next_obs_col_name]])

  list(
    new_current_obs = new_current_obs,
    forecast = new_current_obs
  )
}


#' Generate a hot deck forecast value using a covariate's `lead`s.
#'
#' Given a `local_rows` and `current_obs`, it:
#'   - Finds those rows in `local_rows` with a covariate's
#'   observations closest to `current_obs`.
#'   - Takes the `n_closest` of those.
#'   - Randomly samples one of those.
#'   - Returns:
#'     - That sample's "tomorrow"s covariate's observation as the next `current_obs`
#'     - That sample's tomorrow's target variable's observation and the `forecast`.
#'
#' For `n_closest`, `dplyr::slice_min` is used, and tie values are included.
#'
#' For `filter_na_col_names`, you must include the `next_cov_obs_col_name`,
#' or the sampling will break.
#'
#' @param next_cov_obs_col_name Name of the column in your data
#' containing the next covariate observation (from, e.g. `lead_cov_mutator`).
#' @param next_target_obs_col_name The corresponding target observation
#' (from, e.g. `lead_cov_mutator`).
#' @param filter_na_col_names char vec. The names of any columns that you want to
#' filter any NAs from.
#' @returns list(new_current_obs, forecast), where
#' new_current_obs = The value to use for `current_obs` in the next iteration.
#' forecast = The forecasted value.
#'
#' @export
hot_deck_covariate_lead_sampler <- function(next_cov_obs_col_name = "next_cov_obs",
                                            next_target_obs_col_name = "next_target_obs",
                                            filter_na_col_names = next_cov_obs_col_name) {
  purrr::partial(internal_hot_deck_covariate_lead_sampler,
                 ... =,
                 next_cov_obs_col_name,
                 next_target_obs_col_name,
                 filter_na_col_names)
}


#' Wrapped method for `hot_deck_covariate_lead_sampler`.
#'
#' @param local_rows tibble (NOT tsibble) The `local_rows` in `simulate_sample_path`.
#' @param .observation The covariate's observation column.
#' @param current_obs The `current_obs` in `simulate_sample_path`.
#' @param n_closest Scalar.
#' @param next_cov_obs_col_name Name of the column in your data
#' containing the next covariate observation (from, e.g. `lead_cov_mutator`).
#' @param next_target_obs_col_name The corresponding target observation
#' (from, e.g. `lead_cov_mutator`).
#' @param filter_na_col_names char vec. The names of any columns that you want to
#' filter any NAs from.
#' @returns list(new_current_obs, forecast), where
#' new_current_obs = The value to use for `current_obs` in the next iteration,
#'                   i.e. the next forecast.
#' forecast = The forecasted value.
internal_hot_deck_covariate_lead_sampler <- function(local_rows,
                                                     .observation,
                                                     current_obs,
                                                     n_closest,
                                                     next_cov_obs_col_name,
                                                     next_target_obs_col_name,
                                                     filter_na_col_names) {
  rand_row = local_rows %>% sample_local_rows({{ .observation }},
                                              current_obs = current_obs,
                                              n_closest = n_closest,
                                              filter_na_col_names = filter_na_col_names)
  # What was tomorrow's covariate's obs for our randomly selected row that had
  # a similar obs to our current covariate obs?
  # That's our new current obs.
  new_current_obs = rand_row %>% dplyr::pull(.data[[next_cov_obs_col_name]])
  # And our forecast is its observation of the target variable
  forecast = rand_row %>% dplyr::pull(.data[[next_target_obs_col_name]])

  list(
    new_current_obs = new_current_obs,
    forecast = forecast
  )
}


#' Generate a hot deck forecast value using `lead`s of `difference`s.
#'
#' Given a `local_rows` and `current_obs`, it:
#'   - Finds those rows in `local_rows` with observations closest to `current_obs`.
#'   - Takes the `n_closest` of those.
#'   - Randomly samples one of those.
#'   - Adds that sample's difference-to-its-tomorrow to the current observation.
#'   - Returns this value as both the next `current_obs` and the `forecast`.
#'
#' For `n_closest`, `dplyr::slice_min` is used, and tie values are included.
#'
#' @param diff_to_next_obs_col_name Name of the column in your data containing the next
#' observation (from, e.g. `diff_mutator`).
#' @returns list(new_current_obs, forecast), where
#' new_current_obs = The value to use for `current_obs` in the next iteration.
#' forecast = The forecasted value.
#'
#' @export
hot_deck_diff_sampler <- function(diff_to_next_obs_col_name = "diff_to_next_obs") {
  purrr::partial(internal_hot_deck_diff_sampler, ... =, diff_to_next_obs_col_name)
}


#' Wrapped method for `hot_deck_diff_sampler`.
#'
#' @param local_rows tibble (NOT tsibble) The `local_rows` in `simulate_sample_path`.
#' @param .observation The observation column.
#' @param current_obs The `current_obs` in `simulate_sample_path`.
#' @param n_closest Scalar.
#' @param diff_to_next_obs_col_name Name of the column in your data containing
#' the diff to the next observation (from, e.g. `diff_mutator`).
#' @returns list(new_current_obs, forecast), where
#' new_current_obs = The value to use for `current_obs` in the next iteration.
#' forecast = The forecasted value.
internal_hot_deck_diff_sampler <- function(local_rows,
                                           .observation,
                                           current_obs,
                                           n_closest,
                                           diff_to_next_obs_col_name) {
  rand_row = local_rows %>% sample_local_rows({{ .observation }},
                                              current_obs = current_obs,
                                              n_closest = n_closest,
                                              filter_na_col_names = diff_to_next_obs_col_name)
  # What was the difference to tomorrow's obs for our randomly selected
  # row that had a similar obs to our current obs?
  # We add that to our current obs to get our new current obs,
  # which is also our forecast for tomorrow.
  diff_to_tomorrow = rand_row %>% dplyr::pull(.data[[diff_to_next_obs_col_name]])
  new_current_obs = current_obs + diff_to_tomorrow

  list(
    new_current_obs = new_current_obs,
    forecast = new_current_obs
  )
}


#' Sample a random row from local rows.
#'
#' Procedure outline:
#'   - Filter rows with NAs in derived_col
#'   - Take the `n_closest` closest rows
#'   - Randomly sample one of these.
#'
#' The `filter_na_col_names` may be something like the column of leads or
#' differences, or it could be the observation column itself, or it could be
#' nothing. In some cases a value is required. For instance, NAs are removed
#' from the leads column for the `hot_deck_lead_sampler` because these values
#' are used for subsequent horizons as the "current value".
#'
#' If you leave NAs in your observation column, then they will only be
#' possibly sampled if your `n_closest` exceeds the number of non-NA observations.
#' That is, they are a last resort.
#'
#' If you leave NAs in the column that yields your forecasts (and doing so is a
#' permissible option), then you will wind up with NAs in your forecasts, which
#' may be desirable from the perspective of yielding weaker confidence in those
#' regions with less data to draw from.
#'
#' The returned row has its `offset` column removed. This is just easier for
#' this function; it would not be difficult to include, but I don't see a reason
#' to include it; its purpose should have been fulfilled within this function.
#'
#' @param local_rows tibble (NOT tsibble) The `local_rows` in `simulate_sample_path`.
#' @param .observation The observation column.
#' @param current_obs The `current_obs` in `simulate_sample_path`.
#' @param n_closest Scalar.
#' @param filter_na_col_names char vec. The names of any columns that you want to
#' filter any NAs from.
#' @returns A row from `local_rows`, without the `offset` column anymore.
sample_local_rows <- function(local_rows,
                              .observation,
                              current_obs,
                              n_closest,
                              filter_na_col_names = vector(mode = "character")) {
  # Currently, I do not filter out cases where obs is missing but
  # derived_col is not. dplyr's sorting puts NA's at the bottom, so if they
  # are selected, they're the last resorts, which seems reasonable to me.
  local_rows = local_rows %>%
    dplyr::mutate(obs_distance = abs(current_obs - {{ .observation }}))

  # removing NAs
  filtered_local_rows = local_rows
  for (col_name in filter_na_col_names) {
    filtered_local_rows = filtered_local_rows %>%
      dplyr::filter(!is.na(.data[[col_name]]))
  }

  # Do we have something to work with?
  if (nrow(filtered_local_rows) == 0) {
    stop("No local values to draw from for:\n", call. = FALSE)
  }

  # Getting a random row
  # TODO: if using prop, add check here.
  if (nrow(filtered_local_rows) <= n_closest) {
    warning(paste("Local rows does not have more values than",
                  "the closeness cut-off specifies.",
                  "\n(i.e. nrow(local_rows) <= n_closest)\n",
                  "Sampling from entire contents of local rows."),
            call. = FALSE)
  }
  closest_rows = filtered_local_rows %>%
    dplyr::slice_min(order_by = obs_distance, n = n_closest)
  rand_row = closest_rows %>%
    dplyr::slice_sample(n = 1) %>%
    dplyr::select(-dplyr::any_of(c("offset", "obs_distance")))
    # ^ removing internal cols, plus (TB settled on) "offset"

  rand_row
}
