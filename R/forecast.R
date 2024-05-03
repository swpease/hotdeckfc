#' Produce a hot deck forecast.
#'
#' Produces multiple h-step forecast simulated sample paths.
#'
#' For `times` times, it produces a simulated sample path. For each
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
#' `window_back`, `window_fwd`, and `n_closest` can either each be a scalar
#' (i.e. length 1), or a vector of length h. If a scalar, the same value is
#' used for every horizon. If a vector, then for horizon h_i, the ith value
#' is used. This may be useful as the borders of periodically missing data,
#' where something like `window_fwd = c(1:10, rep(10,20))`, and maybe the same
#' for `window_back` could be used to ensure symmetric windows near the border.
#'
#' The `sampler` function is what produces the actual forecasted values. It
#' takes the local rows and current value (plus some other params), and returns
#' a new current value and a forecasted value, which may or may not be the same.
#'
#' This design allows for custom sampler functions to be supplied.
#'
#' `covariate_forecasts`, if provided, must adhere to the following:
#'   - It must be a `tsibble`.
#'   - The simulation number (sample path) must go from 1 to (# of simulations),
#'   and must be made the "key" of the tsibble.
#' So, your tsibble's "key" must be the covariate forecast's simulation number,
#' its "index" must be the temporal column, and its "measures" (everything
#' else) should be the covariates.
#' All of the `covariate_forecasts` "measure"s (filtered by each simulation number)
#' will be passed to the `sampler`, and that returned value (a partial application)
#' will be used to simulate a sample path.
#' The number of simulated sample paths generated per covariate simulation is
#' the ceiling of (`times` / (# of covariate simulations))
#'
#' @param .data tsibble. The data.
#' @param .observation symbol. The observation column of `.data`.
#' @param times integer. The number of simulated sample paths to produce.
#' @param h integer. How many days to forecast.
#' @param window_back integer (scalar OR vector).
#'   How many days back to include in the window for a given season.
#'   Either scalar (length == 1) or vector of length == h.
#' @param window_fwd integer (scalar OR vector).
#'   How many days forward to include in the window for a given season.
#'   Either scalar (length == 1) or vector of length == h.
#' @param n_closest integer (scalar OR vector).
#'   The number of closest observations to pick from per hot deck random sampling.
#'   Either scalar (length == 1) or vector of length == h.
#' @param sampler Sampler function to generate forecasted values.
#' @param covariate_forecasts optional tsibble. Simulated sample paths of covariates.
#' @returns tibble of forecasts:
#'   - nrow = h * times,
#'   - columns:
#'     - datetime: the date for the forecast
#'     - h: the forecast horizon
#'     - forecast: the forecasted value
#'     - simulation_num: the simulated sample path number
#'
#' @examples
#' data = append_diff(hotdeckts::SUGG_temp, observation)
#' data = trim_leading_nas(data, observation)
#' out = hot_deck_forecast(data,
#'                         .observation = observation,
#'                         times = 3,
#'                         h = 20,
#'                         window_back = 20,
#'                         window_fwd = 20,
#'                         n_closest = 5,
#'                         sampler = sample_diff())  # don't forget to call!
#'
#' @export
hot_deck_forecast <- function(.data,
                              .observation,
                              times,
                              h,
                              window_back = 20,
                              window_fwd = 20,
                              n_closest = 5,
                              sampler = sample_lead("next_obs"),
                              covariate_forecasts = NULL) {
  # Validate
  .data %>% validate_data({{ .observation }})
  window_back = ensure_vector(window_back, h)
  window_fwd = ensure_vector(window_fwd, h)
  n_closest = ensure_vector(n_closest, h)
  if (!is.null(covariate_forecasts)) {
    validate_cov_fcs(covariate_forecasts)
  }

  # Forecast
  .datetime = tsibble::index(.data)

  forecasts = NULL
  if (is.null(covariate_forecasts)) {
    forecasts = .data %>%
      internal_hot_deck_forecast({{ .datetime }},
                                 {{ .observation }},
                                 times = times,
                                 h = h,
                                 window_back = window_back,
                                 window_fwd = window_fwd,
                                 n_closest = n_closest,
                                 sampler = sampler)
  } else {
    n_sims = tsibble::n_keys(covariate_forecasts)
    k = tsibble::key(covariate_forecasts)[[1]]
    times_per_key = ceiling(times / n_sims)
    max_sim_num = 0
    for (sim_num in 1:n_sims) {
      cov_fc = covariate_forecasts %>%
        dplyr::filter({{ k }} == .env$sim_num)
      sampler_w_covs = sampler(cov_fc)
      fc = .data %>%
        internal_hot_deck_forecast({{ .datetime }},
                                   {{ .observation }},
                                   times = times_per_key,
                                   h = h,
                                   window_back = window_back,
                                   window_fwd = window_fwd,
                                   n_closest = n_closest,
                                   sampler = sampler_w_covs)
      fc = fc %>%
        dplyr::mutate(simulation_num = simulation_num + .env$max_sim_num)
      forecasts = dplyr::bind_rows(forecasts, fc)
      max_sim_num = max_sim_num + times_per_key
    }
  }

  forecasts
}


#' Internal method for hot deck forecasting.
#'
#' @param .data tsibble. The data. Passed via pipe.
#' @param .datetime The datetime column of .data.
#' @param .observation The observation column of .data.
#' @param times The number of simulated sample paths to produce.
#' @param h How many days to forecast.
#' @param window_back How many days back to include in the window for
#' a given season. Either scalar (length == 1) or vector
#' of length == h.
#' @param window_fwd How many days forward to include in the window for
#' a given season. Either scalar (length == 1) or vector
#' of length == h.
#' @param n_closest The number of closest observations to pick from
#' per hot deck random sampling. Either scalar (length == 1) or vector
#' of length == h.
#' @param sampler Sampler function to generate forecasted values.
#'
#' @noRd
internal_hot_deck_forecast <- function(.data,
                                       .datetime,
                                       .observation,
                                       times,
                                       h,
                                       window_back,
                                       window_fwd,
                                       n_closest,
                                       sampler) {
  forecasts = NULL
  n_time = 1
  while (n_time <= times) {
    fc = .data %>%
      simulate_sample_path({{ .datetime }},
                           {{ .observation }},
                           h = h,
                           window_back = window_back,
                           window_fwd = window_fwd,
                           n_closest = n_closest,
                           sampler = sampler)
    fc = fc %>%
      dplyr::mutate(simulation_num = .env$n_time)
    forecasts = dplyr::bind_rows(forecasts, fc)
    n_time = n_time + 1
  }

  forecasts
}

#' Simulate a sample path for a hot deck forecast.
#'
#' Produces a single possible sample path of an h-step forecast.
#'
#' This method is iterative. For iteration i, at the most recent observation, it:
#'   - Takes all historical observations from a window,
#'   [`window_back[[i]]`, `window_fwd[[i]]`] (inclusive) around
#'   the same portion of the season.
#'     - e.g., Jun 30 +- 5 days across all years.
#'   - Takes the `n_closest[[i]]` closest observations from these.
#'   - Randomly selects one of these closest observations.
#'   - Uses the observation of the day after that one as the forecasted value.
#'   - Repeats, using this new forecasted value and its respective date as
#'   the new "most recent observation", up to h forecasts.
#'
#' @param .data tsibble. The data. Passed via pipe.
#' @param .datetime The datetime column of .data. Passed via pipe.
#' @param .observation The observation column of .data. Passed via pipe.
#' @param h How many days to forecast.
#' @param window_back How many days back to include in the window for
#' a given season. Vector of length == h.
#' @param window_fwd How many days forward to include in the window for
#' a given season. Vector of length == h.
#' @param n_closest The number of closest observations to pick from
#' per hot deck random sampling. Vector of length == h.
#' @param sampler Sampler function to generate forecasted values.
#' @returns tibble of forecasts:
#'   - nrow = h,
#'   - columns:
#'     - datetime: the date for the forecast
#'     - h:        the forecast horizon
#'     - forecast: the forecasted value
#'
#' @noRd
simulate_sample_path <- function(.data,
                                 .datetime,
                                 .observation,
                                 h,
                                 window_back,
                                 window_fwd,
                                 n_closest,
                                 sampler) {
  # .data = .data %>%
    # dplyr::mutate(next_obs = dplyr::lead({{ .observation }}))
  T_date = .data %>%
    dplyr::slice_max(order_by = {{ .datetime }}) %>%
    dplyr::pull({{ .datetime }})
  current_obs = .data %>%
    dplyr::slice_max(order_by = {{ .datetime }}) %>%
    dplyr::pull({{ .observation }})

  forecast = vector(mode = "numeric", length = h)  # inits to 0's
  h_curr = 1
  while (h_curr <= h) {
    # a tibble
    local_rows = .data %>%
      get_local_rows({{ .datetime }}, h_curr, window_back[[h_curr]], window_fwd[[h_curr]])
    sampler_out = tryCatch(
      expr = local_rows %>% sampler({{ .observation }},
                                    current_obs = current_obs,
                                    n_closest = n_closest[[h_curr]]),
      error = function(e) {
        # TODO: handle "unused arguments" sampler error.
        stop(paste("\n`sampler` Error Msg:\n", conditionMessage(e),
                   "\nHot deck values that generated this error:\n",
                   "h =", h_curr, "\n",
                   "window_back =", window_back[[h_curr]], "\n",
                   "window_fwd =", window_fwd[[h_curr]], "\n",
                   "current_obs =", current_obs,
                   "n_closest =", n_closest[[h_curr]]),
             call. = FALSE)
      }
    )

    current_obs = sampler_out[["new_current_obs"]]
    forecast[[h_curr]] = sampler_out[["forecast"]]
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
#' Augments the data with a new column, `offset`, which gives the position
#' of the observation row relative to the "now". e.g. if "now" is 2020-02-02,
#' 2020-02-01 has an offset == -1, 2020-02-02 == 0, 2020-02-03 == 1.
#'
#' @param .data tsibble. The data. Passed via pipe.
#' @param .datetime The datetime column of .data. Passed via pipe.
#' @param h_curr Current forecast horizon position.
#' @param window_back How many days back to include in the window for
#' a given season.
#' @param window_fwd How many days forward to include in the window for
#' a given season.
#' @returns tibble (NO "s") of local rows. Same columns as .data, plus one new:
#' "offset".
#'
#' @noRd
get_local_rows <- function(.data,
                           .datetime,
                           h_curr,
                           window_back,
                           window_fwd) {
  # hedge against neg nums
  window_back = abs(window_back)
  window_fwd = abs(window_fwd)

  # working by indexes
  .data = .data %>%
    tibble::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::arrange({{ .datetime }}) %>%  # Probably unnecessary.
    dplyr::mutate(idx = dplyr::row_number())

  idx_max = nrow(.data)
  # `- 1` b/c we want to center around "now", which is the day before
  # whichever day we're forecasting (that is, the day before `h_curr`'s
  # date).
  ref_idx = idx_max + (h_curr - 1)

  local_rows = NULL
  while (TRUE) {
    # Need this `max` for case where window extends partially prior
    # to our earliest obs; we still want to capture that part that
    # *does* overlap, though, and `slice` doesn't handle neg nums.
    # window:    ______
    # obs:          1 2 3 4
    window_start = max((ref_idx - window_back), 1)
    window_end = ref_idx + window_fwd
    # If the window is entirely before our earliest data, we're done.
    if (window_end < 1) {
      break
    }
    local_rows_part = .data %>%
      dplyr::slice(.env$window_start:.env$window_end)

    # nrow could == 0 if h is larger than window_back, such as for the
    # tail (recent-est) end of a long forecast horizon
    if (nrow(local_rows_part) > 0) {
      # For binning in samplers.
      # edge case example:
      # window:  _________________
      # idx:           1 2 3 4 5
      # ref_idx:         *
      # offset:       -1 0 1 2 3
      offset_start = window_start - ref_idx
      # (continuing example)
      # Since window_end > max(idx) [while w_s is limited to 1, so
      # don't need to worry about; window_end I left unlimited b/c
      # slicing doesn't care if the end is past the last index],
      # need to keep the offset_start:offset_end sequence correct length.
      local_idx_max = local_rows_part %>% pull(idx) %>% max()
      offset_end = min(window_end, local_idx_max) - ref_idx
      local_rows_part = local_rows_part %>%
        mutate(offset = offset_start:offset_end)
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
      # ref_idx could be beyond your data's max index...
      if (ref_idx > idx_max) {
        ref_date = .data %>%
          dplyr::slice_tail(n = 1) %>%
          dplyr::pull({{ .datetime }})
        # So we add the difference.
        # This way can handle multi-year-ahead forecasts.
        ref_date = ref_date + (ref_idx - idx_max)
      } else {
      ref_date = .data %>%
        dplyr::filter(idx == .env$ref_idx) %>%
        dplyr::pull({{ .datetime }})
      }
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
#' @param .data tsibble. The data.
#' @param .observation symbol. The observation column of `.data`.
#'
#' @examples
#' data = tsibble::tsibble(date = as.Date("2022-02-02") + 0:9,
#'                         obs = c(1:8, NA, NA),
#'                         index = date)
#' trim_leading_nas(data, obs)
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


#' Validators for hot deck forecast input.
#'
#' Checks that:
#'   1. The data is a `tsibble`.
#'   2. The index is a Date.
#'   3. The tsibble is not multi-keyed (i.e. multiple time serieses).
#'   4. The data has no gaps (i.e. missing rows; NA observations okay).
#'   5. The final observation is not NA.
#'
#' @param data The input `.data` to `hot_deck_forecast`.
#' @param .observation The observation column of .data. Passed via pipe.
#'
#' @noRd
validate_data <- function(data,
                          .observation) {
  if (isFALSE(tsibble::is_tsibble(data))) {
    stop("Your data needs to be a `tsibble`.", call. = FALSE)
  }

  is_date = data %>%
    dplyr::pull(tsibble::index(.)) %>%
    is.Date()
  if (isFALSE(is_date)) {
    stop("Your tsibble's index needs to be a `Date`.",
         call. = FALSE)
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

  .datetime = tsibble::index(data)
  latest_row = data %>% dplyr::slice_max(order_by = {{ .datetime }})
  T_date = latest_row %>% dplyr::pull({{ .datetime }})
  current_obs = latest_row %>% dplyr::pull({{ .observation }})
  if (is.na(current_obs)) {
    stop(paste("Latest observation in data is NA.\n",
               "Trim your data to the latest non-NA observation.\n",
               "Observation date: ", T_date),
         call. = FALSE)
  }
}


#' Converts scalar to vector of length h, and raises an error if
#' vector is not of length h.
#'
#' @param hotdeck_arg Argument passed to `hot_deck_forecast` required to end as
#' a vector of length h.
#' @param h Forecast horizon, per `hot_deck_forecast`.
#' @returns hotdeck_arg vector of length h.
#'
#' @noRd
ensure_vector <- function(hotdeck_arg, h) {
  if (length(hotdeck_arg) == 1) {
    hotdeck_arg = rep(hotdeck_arg, h)
  }
  if (length(hotdeck_arg) != h) {
    stop(paste(deparse(substitute(hotdeck_arg)),
               "is length", length(hotdeck_arg), ".\n",
               "It needs to be either length 1 (e.g. `x = 3L`)",
               "or length h,", h),
         call. = FALSE)
  }

  hotdeck_arg
}


#' Validators for covariate forecasts.
#'
#' Checks that:
#'   1. The cov_fcs is a tsibble.
#'   2. The "key" is one column.
#'   3. The "key" is from 1:n, where n = # of simulations.
#'
#' @param covariate_forecasts tsibble. Simulated sample paths of covariates.
#'
#' @noRd
validate_cov_fcs <- function(covariate_forecasts) {
  if (isFALSE(tsibble::is_tsibble(covariate_forecasts))) {
    stop("Your covariate forecasts needs to be a `tsibble`.", call. = FALSE)
  }

  # should have one key col, which contains values over 1:n_keys
  k = tsibble::key(covariate_forecasts)  # list
  if (length(k) > 1) {
    stop("You `covariate_forecasts` has more than one key column.",
         call. = FALSE)
  }
  k = k[[1]]
  n = tsibble::n_keys(covariate_forecasts)
  k_vals = covariate_forecasts %>%
    dplyr::distinct({{ k }}) %>%
    dplyr::arrange() %>%
    dplyr::pull()
  if (!identical(as.integer(k_vals), 1:n)) {
    stop(paste("Your `covariate_forecasts`'s \"key\" contains",
               "the following keys:\n", k_vals, "\n",
               "It should only contain values from 1 to", n
               ),
         call. = FALSE)
  }
}
