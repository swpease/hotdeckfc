#' Produce a hot deck forecast.
#'
#' Produces multiple h-step forecast simulated sample paths.
#'
#' For `times` times, it produces a simulated sample paths. For each
#' simulation, the process proceeds as follows:
#' This method is iterative. At the most recent observation, it:
#'   - Takes all historical observations from a window around
#'   the same portion of the season.
#'     - e.g., Jun 30 +- 5 days across all years.
#'   - Takes the `n_closest` closest observations from these.
#'   - Randomly selects one of these `n_closest` observations.
#'   - Uses the observation of the day after that one as the forecasted value.
#'   - Repeats, using this new forecasted value and its respective date as
#'   the new "most recent observation", up to h forecasts.
#'
#'
#' @param .data tsibble. The data. Passed via pipe.
#' @param .datetime The datetime column of .data. Passed via pipe.
#' @param .observation The observation column of .data. Passed via pipe.
#' @param times The number of simulated sample paths to produce.
#' @param h How many days to forecast.
#' @param window_back How many days back to include in the window for
#' a given season.
#' @param window_fwd How many days forward to include in the window for
#' a given season.
#' @param n_closest The number of closest observations to pick from
#' per hot deck random sampling.
#' @returns tibble of forecasts:
#'   - nrow = h * times,
#'   - columns:
#'     - datetime: the date for the forecast
#'     - h: the forecast horizon
#'     - forecast: the forecasted value
#'     - simulation_num: the simulated sample path number
#'
#' @export
hot_deck_forecast <- function(.data,
                              .datetime,
                              .observation,
                              times,
                              h,
                              window_back,
                              window_fwd,
                              n_closest) {
  partially_validate_hotdeckfc_input(.data)  # Put here or in simulate_s_p?

  forecasts = NULL
  n_time = 1
  while (n_time <= times) {
    fc = .data %>%
      simulate_sample_path({{ .datetime }},
                           {{ .observation }},
                           h = h,
                           window_back = window_back,
                           window_fwd = window_fwd,
                           n_closest = n_closest)
    fc = fc %>%
      dplyr::mutate(simulation_num = n_time)
    forecasts = dplyr::bind_rows(forecasts, fc)
    n_time = n_time + 1
  }

  forecasts
}

#' Simulate a sample path for a hot deck forecast.
#'
#' Produces a single possible sample path of an h-step forecast.
#'
#' This method is iterative. At the most recent observation, it:
#'   - Takes all historical observations from a window around
#'   the same portion of the season.
#'     - e.g., Jun 30 +- 5 days across all years.
#'   - Takes the `n_closest` closest observations from these.
#'   - Randomly selects one of these `n_closest` observations.
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
#' @param n_closest The number of closest observations to pick from
#' per hot deck random sampling.
#' @returns tibble of forecasts:
#'   - nrow = h,
#'   - columns:
#'     - datetime: the date for the forecast
#'     - h:        the forecast horizon
#'     - forecast: the forecasted value
simulate_sample_path <- function(.data,
                                 .datetime,
                                 .observation,
                                 h,
                                 window_back,
                                 window_fwd,
                                 n_closest) {
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
      dplyr::slice_min(order_by = distance, n = n_closest)
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
    h = 1:h,
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

  # working by indexes
  idx_max = nrow(.data)
  # Need to add rows for leap year shenanigans at bottom of while loop.
  # Otherwise, you'll index to beyond your data.
  .data = .data %>% tsibble::append_row(n = (h_curr - 1))
  .data = .data %>%
    tibble::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::arrange({{ .datetime }}) %>%  # Probably unnecessary.
    dplyr::mutate(idx = dplyr::row_number())
  # `- 1` b/c we want to center around "now", which is the day before
  # whichever day we're forecasting (that is, the day before `h_curr`'s
  # date).
  ref_idx = idx_max + (h_curr - 1)

  local_rows = NULL
  while (TRUE) {
    window_start = ref_idx - window_back
    window_end = ref_idx + window_fwd
    local_rows_part = .data %>%
      dplyr::filter(
        idx >= window_start,
        idx <= window_end
      )

    # If we're before our earliest obs and have nothing in our slice.
    if ((ref_idx < 1) && (nrow(local_rows_part) == 0)) {
      break
    }
    local_rows = dplyr::bind_rows(local_rows, local_rows_part)

    # Shift ref_idx back.

    # Need to account for leap years, so I need a date, but ref_idx could be < 1...
    # ...so if it is, then I can just subtract 365 w/o worrying:
    if (ref_idx < 1) {
      ref_idx = ref_idx - 365
    } else {
      # Otherwise, we need to get a date and handle a possible leap year:
      # What's our ref_idx's respective date?
      ref_date = .data %>%
        dplyr::filter(idx == ref_idx) %>%
        dplyr::pull({{ .datetime }})
      ref_idx = if (lubridate::leap_year(ref_date)) (ref_idx - 366) else (ref_idx - 365)
    }
  }

  local_rows %>% dplyr::select(-idx)
}

#' Remove leading rows with NA observations.
#'
#' Use this function before passing your data to `hot_deck_forecast`,
#' which requires that the most recent observation exists (is not NA).
#'
#' @param .data The data. Passed via pipe.
#' @param .observation The observation column of .data. Passed via pipe.
#'
#' @export
trim_leading_nas <- function(.data, .observation) {
  .data %>%
    dplyr::slice(
      1:purrr::detect_index(
        {{ .observation }},
        \(x) !is.na(x),
        .dir = "backward"
      )
    )
}


#' Some validators for hot deck forecast input.
#'
#' Does not check if final obs is NA; put that in the code itself
#' b/c it logically goes w/ `current_obs`. Subject to change.
#'
#' @param data The input `.data` to `hot_deck_forecast`.
partially_validate_hotdeckfc_input <- function(data) {
  if (isFALSE(tsibble::is_tsibble(data))) {
    stop("Your data needs to be a `tsibble`.", call. = FALSE)
  }
  # keyless tsibbles have n_keys == 1
  if (tsibble::n_keys(data) > 1) {
    stop(paste("`hot_deck_forecast` cannot handle multi-key tsibbles.\n",
               "Multiple keys means multiple time series.",
               "Examine your keys with `tsibble::key`,",
               "and filter to one key's data."))
  }
  if (isTRUE(tsibble::has_gaps(data) %>% pull())) {
    stop(paste("Your tsibble contains gaps.\n",
               "Try using `tsibble::fill_gaps`."),
         call. = FALSE)
  }
}
