#' Generate a hot deck forecast.
#'
#' Produces a single possible realization of an h-step forecast.
#'
#' This method is iterative. At the most recent observation, it:
#'   - Takes all historical observations from a window around
#'   the same portion of the season.
#'     - e.g., Jun 30 +- 5 days across all years.
#'   - Takes the `n` closest observations from these.
#'   - Randomly selects one of these `n` observations.
#'   - Uses the observation of the day after that one as the forecasted value.
#'   - Repeats, using this new forecasted value and its respective date as
#'   the new "most recent observation", up to h forecasts.
#'
#'
#' @param .data tsibble. The data. Passed via pipe.
#' @param .datetime The datetime column of .data. Passed via pipe.
#' @param .observation The observation column of .data. Passed via pipe.
#' @param h How many days to forecast.
#' @param window_back How many days back to include in the window for
#' a given season.
#' @param window_fwd How many days forward to include in the window for
#' a given season.
#' @param n The number of closest observations to pick from
#' per hot deck random sampling.
#' @returns tibble of forecasts:
#'   - nrow = h,
#'   - columns:
#'     - datetime: the date for the forecast
#'     - forecast: the forecasted value
#'
#' @export
hot_deck_forecast <- function(.data,
                              .datetime,
                              .observation,
                              h,
                              window_back,
                              window_fwd,
                              n) {
  .data = .data %>%
    dplyr::mutate(next_obs = dplyr::lead({{ .observation }}))
  T_date = .data %>%
    dplyr::slice_max(order_by = {{ .datetime }}) %>%
    dplyr::pull({{ .datetime }})
  current_obs = .data %>%
    dplyr::slice_max(order_by = {{ .datetime }}) %>%
    dplyr::pull({{ .observation }})
  if (is.na(current_obs)) {
    stop(paste("Latest observation in data is NA.\n",
               "Trim your data to the latest non-NA observation.\n",
               "Observation date: ", T_date),
         call. = FALSE)
  }

  forecast = vector(mode = "numeric", length = h)  # inits to 0's
  h_curr = 1
  while (h_curr <= h) {
    # a tibble
    local_rows = .data %>%
      get_local_rows({{ .datetime }}, h_curr, window_back, window_fwd)
    local_rows = local_rows %>%
      dplyr::mutate(distance = abs(current_obs - {{ .observation }}))
    # remove cases where next obs is missing
    local_rows = local_rows %>%
      dplyr::filter(!is.na(next_obs))
    closest_rows = local_rows %>%
      dplyr::slice_min(order_by = distance, n = n)
    rand_row = closest_rows %>%
      dplyr::slice_sample(n = 1)
    # What was tomorrow's obs for our randomly selected row that had
    # a similar obs to our current obs?
    # That's our new current obs.
    current_obs = rand_row %>% dplyr::pull(next_obs)
    forecast[[h_curr]] = current_obs
    h_curr = h_curr + 1
  }

  tibble(
    datetime = T_date + 1:h,
    forecast = forecast
  )
}


#' Get rows near (in a seasonal sense) a reference date.
#'
#' The reference date is the max date of the data, offset by `h_curr`.
#' So, if your latest data is for 2023-03-03, and ignoring the offset for
#' simplicity, it collects the rows within a window of March 3 across
#' all years of your data.
#'
#' @param .data tsibble. The data. Passed via pipe.
#' @param .datetime The datetime column of .data. Passed via pipe.
#' @param h_curr Current forecast horizon position.
#' @param window_back How many days back to include in the window for
#' a given season.
#' @param window_fwd How many days forward to include in the window for
#' a given season.
get_local_rows <- function(.data,
                           .datetime,
                           h_curr,
                           window_back,
                           window_fwd) {
  # hedge against neg nums
  window_back = abs(window_back)
  window_fwd = abs(window_fwd)
  # min and max dates in data.
  T_date = .data %>%
    dplyr::slice_max(order_by = {{ .datetime }}) %>%
    dplyr::pull({{ .datetime }})
  t0_date = .data %>%
    dplyr::slice_min(order_by = {{ .datetime }}) %>%
    dplyr::pull({{ .datetime }})
  # `- 1` b/c we want to center around "now", which is the day before
  # whichever day we're forecasting (that is, the day before `h_curr`'s
  # date).
  ref_date = T_date + (h_curr - 1)
  local_rows = NULL
  while (TRUE) {
    # dodge Feb 29th
    if ((lubridate::month(ref_date) == 2) & (lubridate::day(ref_date) == 29)) {
      lubridate::day(ref_date) = 28
    }
    window_start = as.character(ref_date - window_back)
    window_end = as.character(ref_date + window_fwd)
    local_rows_part = .data %>%
      tsibble::filter_index(window_start ~ window_end) %>%
      tibble::as_tibble() %>%
      dplyr::ungroup()
    # If we're before our earliest obs and have nothing in our slice.
    if ((ref_date < t0_date) & (nrow(local_rows_part) == 0)) {
      break
    }
    local_rows = dplyr::bind_rows(local_rows, local_rows_part)
    lubridate::year(ref_date) = lubridate::year(ref_date) - 1
  }

  local_rows
}
