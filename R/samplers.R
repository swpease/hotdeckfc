#' Generate a basic hot deck forecast value.
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
#' new_current_obs = The value to use for `current_obs` in the next iteration,
#'                   i.e. the next forecast.
#' forecast = The forecasted value.
#'
#' @export
basic_hot_deck_sampler <- function(next_obs_col_name) {
  purrr::partial(internal_basic_hot_deck_sampler, ... =, next_obs_col_name)
}


#' Wrapped method for `basic_hot_deck_sampler`.
#'
#' @param local_rows The `local_rows` in `simulate_sample_path`.
#' @param .observation The observation column.
#' @param current_obs The `current_obs` in `simulate_sample_path`.
#' @param n_closest Scalar.
#' @param next_obs_col_name Name of the column in your data containing the next
#' observation (from, e.g. `dplyr::lead`).
#' @returns list(new_current_obs, forecast), where
#' new_current_obs = The value to use for `current_obs` in the next iteration,
#'                   i.e. the next forecast.
#' forecast = The forecasted value.
internal_basic_hot_deck_sampler <- function(local_rows,
                                            .observation,
                                            current_obs,
                                            n_closest,
                                            next_obs_col_name) {
    local_rows = local_rows %>%
      dplyr::mutate(distance = abs(current_obs - {{ .observation }}))
    # remove cases where next obs is missing
    local_rows = local_rows %>%
      dplyr::filter(!is.na(.data[[next_obs_col_name]]))
    if (nrow(local_rows) == 0) {
      stop("No local values to draw from for:\n", call. = FALSE)
    }
    closest_rows = local_rows %>%
      dplyr::slice_min(order_by = distance, n = n_closest)
    rand_row = closest_rows %>%
      dplyr::slice_sample(n = 1)
    # What was tomorrow's obs for our randomly selected row that had
    # a similar obs to our current obs?
    # That's our new current obs, as well as our forecast.
    new_current_obs = rand_row %>% dplyr::pull(.data[[next_obs_col_name]])

    list(
      new_current_obs = new_current_obs,
      forecast = new_current_obs
    )
}
