#' Generate a num-seasons-fold(-ish) hot deck forecast.
#'
#' Produces a set of hot deck forecasts, one per season if possible.
#'
#' If the `offset = 0`, then the CV will use the latest datetime as a
#' starting point, and go back one year at a time through the data.
#' If the observation is missing, then that year's hot deck forecast is
#' skipped. The latest datetime is also skipped, because there's no test
#' data to use for its forecasts.
#'
#' If `offset` is non-zero, then the starting point will be the latest
#' datetime + the offset.
#'
#' The parameters after `offset`, i.e. `times` through `n_closest`,
#' are passed to `hot_deck_forecast`.
#'
#' The current implementation splits the data into training and testing sets
#' as follows: The testing set is any data in the forecast horizon (`h`) for
#' the current season in question. The training data is any data before
#' that or after the `window_fwd` from the largest `h`. This is
#' a conservative approach, aimed at eliminating data leakage. While the data
#' after the largest `h` but within the `window_fwd` should be minimally leaky,
#' it seemed off for larger `h`'s to have increasing pools of data to draw from.
#'
#' I think that it might be worthwhile to offer an alternative splitting method,
#' in which all of the data is in the training set. While this would leak data,
#' it might be a better option for cases of only a few seasons worth of data.
#' In that case, the current, conservative implementation would have markedly
#' reduced data to draw from, which seems like it could significantly effect
#' the conclusion on which hyperparameter values to select.
#'
#' @param .data tsibble. The data. Passed via pipe.
#' @param .datetime The datetime column of .data. Passed via pipe.
#' @param .observation The observation column of .data. Passed via pipe.
#' @param offset integer. Offset (in +- days) from the most recent row of .data
#' to use as the starting point.
#' @param times The number of simulated sample paths to produce per hot deck forecast.
#' @param h How many days to forecast.
#' @param window_back How many days back to include in the window for
#' a given season.
#' @param window_fwd How many days forward to include in the window for
#' a given season.
#' @param n_closest The number of closest observations to pick from
#' per hot deck random sampling.
#' @returns tibble of hot deck forecasts:
#'   - nrow = (h * times) \* k (k is # of viable seasons),
#'   - columns:
#'     - datetime: the date for the forecast
#'     - forecast: the forecasted value
#'     - simulation_num: the simulated sample path number
#'     - k: the CV number, i.e. the hot deck forecast number
#' @export
cv_hot_deck_forecast <- function(.data,
                                 .datetime,
                                 .observation,
                                 offset,  # the non-hdfc arg
                                 times,
                                 h,
                                 window_back,
                                 window_fwd,
                                 n_closest) {
  max_date = .data %>%
    dplyr::pull({{ .datetime }}) %>%
    max()
  min_date = .data %>%
    dplyr::pull({{ .datetime }}) %>%
    min()
  ref_date = max_date + offset
  # Nothing to put in test set if we're at or past our max obs. Go back a year.
  if (ref_date >= max_date) {
    ref_date = subtract_year(ref_date)
  }

  k = 1
  forecasts = NULL
  while (TRUE) {
    ref_row = .data %>% dplyr::filter({{ .datetime }} == ref_date)
    ref_row_obs = ref_row %>% dplyr::pull({{ .observation }})
    # If missing entire row or just obs
    if ((nrow(ref_row) == 0) || (is.na(ref_row_obs))) {
      # No more data
      if (ref_date < min_date) {
        break
      }
      # Maybe more data; go back a year and try again.
      ref_date = subtract_year(ref_date)
      next
    }

    # A working case

    # Split and arrange data
    # Need to move the data that is beyond the test_data back in time to
    # before the start of the data, then join it.
    #
    # training data = mobile + ante
    # start:    ante_split < test_data < mobile_data
    # want:     mobile_data < ante_split < test_data
    # unused:   data in `window_fwd` rows after test_data
    # TODO: h + window_fwd - 1?
    ante_split = .data %>% dplyr::filter({{ .datetime }} <= ref_date)
    post_split = .data %>% dplyr::filter({{ .datetime }} > ref_date)
    post_split = post_split %>% dplyr::mutate(idx = dplyr::row_number())
    test_data = post_split %>%
      dplyr::filter(idx <= h) %>%
      dplyr::select(-idx)
    mobile_data = post_split %>%
      dplyr::filter(idx > (h + window_fwd)) %>%
      dplyr::select(-idx)
    # Shift mobile_data back to before the start of your original data
    while (TRUE) {
      max_mobile_date = mobile_data %>%
        dplyr::pull({{ .datetime }}) %>%
        max()
      if (max_mobile_date < min_date) {
        break
      }
      mobile_data = mobile_data %>%
        dplyr::mutate(
          {{ .datetime }} := case_when(
            lubridate::leap_year(max_mobile_date) ~ {{ .datetime }} - 366,
            .default = {{ .datetime }} - 365))
    }
    train_data = dplyr::bind_rows(mobile_data, ante_split)

    # Forecasting
    fc = train_data %>%
      hot_deck_forecast({{ .datetime }},
                        {{ .observation }},
                        times = times,
                        h = h,
                        window_back = window_back,
                        window_fwd = window_fwd,
                        n_closest = n_closest)
    fc = fc %>% dplyr::mutate(k = k)
    forecasts = dplyr::bind_rows(forecasts, fc)

    # Update vals
    k = k + 1
    ref_date = subtract_year(ref_date)
  }

  # make this a tsibble? If so, need to change tests
  # key = simulation_num, k
  forecasts
}

#' Subtract a year(ish) from a date.
#'
#' If the date is in a leap year, subtract 366 days, else 365 days,
#' so it might yield a different day number.
#'
#' @param date A datetime.
subtract_year <- function(date) {
  as.Date(ifelse(lubridate::leap_year(date),
                 date - 366,
                 date - 365))
}