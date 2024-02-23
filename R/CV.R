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
#' The parameters `times` through `n_closest`, are passed to `hot_deck_forecast`.
#'
#' The current implementation has two train-test data splitting methods.
#' The default is "conservative", which goes as follows:
#' The testing set is any data in the forecast horizon (`h`) for
#' the current season in question. The training data is any data before
#' that or after the `window_fwd` from the largest `h`. This is aimed at
#' eliminating data leakage. While the data after the largest `h` but within
#' the `window_fwd` should be minimally leaky, it seemed off for larger `h`'s
#' to have increasing pools of data to draw from.
#'
#' The alternative splitting method, "leaky", uses all of the data
#' in the training set. While this leaks data, it might be a better option
#' (TBD) for cases of only a few seasons worth of data.
#' In that case, the default conservative implementation would have markedly
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
#' @param train_test_split_type default = "conservative". See help for details.
#' @returns list containing:
#'   forecasts: a `tibble` of hot deck forecasts:
#'     - nrow = (h * times) \* k (k is # of viable seasons),
#'     - columns:
#'       - datetime: the date for the forecast
#'       - forecast: the forecasted value
#'       - simulation_num: the simulated sample path number
#'       - k: the CV number, i.e. the hot deck forecast number
#'   test_data_sets: a `tsibble` of test data sets from your `.data`
#'     - new columns:
#'       - h: the horizon number
#'       - k: the CV number, i.e. the hot deck forecast number
#' @export
cv_hot_deck_forecast <- function(.data,
                                 .datetime,
                                 .observation,
                                 offset,  # the non-hdfc arg
                                 times,
                                 h,
                                 window_back,
                                 window_fwd,
                                 n_closest,
                                 train_test_split_type = c("conservative", "leaky")) {
  train_test_split_type = match.arg(train_test_split_type)

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
  test_data_sets = NULL
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
    train_test_data = .data %>%
      train_test_split({{ .datetime }},
                                    ref_date = ref_date,
                                    h = h,
                                    window_fwd = window_fwd,
                                    min_date = min_date,
                                    split_type = train_test_split_type)
    train_data = train_test_data$train_data
    # TODO: test_data is a tsibble right now. Do I want it and forecasts
    # to be both tibbles or both tsibbles?
    test_data = train_test_data$test_data %>%
      mutate(k = k)
    test_data_sets = dplyr::bind_rows(test_data_sets, test_data)

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

  # make forecasts a tsibble? If so, need to change tests
  # key = simulation_num, k
  list(forecasts = forecasts, test_data_sets = test_data_sets)
}

#' Subtract a year(ish) from a date.
#'
#' If the date is in a leap year, subtract 366 days, else 365 days,
#' so it might yield a different day number.
#'
#' @param date A datetime.
subtract_year <- function(date) {
  if (lubridate::leap_year(date)) (date - 366) else (date - 365)
}

#' Train test split for hot deck CV.
#'
#' re `split_type`:
#' "conservataive" excludes the test data, plus the data in the window_fwd
#' region beyond the final date to forecast, from the training data.
#'
#' "leaky" uses all the data as training data.
#'
#' @param .data tsibble. The data. Passed via pipe.
#' @param .datetime The datetime column of .data. Passed via pipe.
#' @param ref_date The current "now" per the CV.
#' @param h How many days to forecast.
#' @param window_fwd How many days forward to include in the window for
#' a given season.
#' @param min_date The earliest date in `.data`.
#' @param split_type c("conservative", "leaky"). See `cv_hot_deck_forecast`
#' help for details.
#' @returns list(
#' train_data = train_data fraction of .data,
#' test_data  = test_data fraction of .data, augmented with:
#'                h = forecast horizon
#' )
train_test_split <- function(.data,
                             .datetime,
                             ref_date,
                             h,
                             window_fwd,
                             min_date,
                             split_type) {
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
    dplyr::select(-idx) %>%
    dplyr::mutate(h = row_number())
  if (split_type == "conservative") {
    mobile_data = post_split %>%
      dplyr::filter(idx > (h + window_fwd)) %>%
      dplyr::select(-idx)
  } else {
    mobile_data = post_split %>% dplyr::select(-idx)
  }

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

  list(train_data = train_data, test_data = test_data)
}


#' Calculate the CRPS from hot deck CV.
#'
#' Augments the CV's test data with a "score" column of CRPSes, calculated
#' by `scoringRules::crps_sample`.
#'
#' The score is listed as NA for missing test data observations.
#'
#' The CRPS uses the `times` (arg passed to `hot_deck_forecast`
#' via `cv_hot_deck_forecast`) number of simulated values, e.g. if
#' `times = 30` then the 30 forecasted values, for each row in the test data.
#'
#' @param cv_out Output of `cv_hot_deck_forecast`.
#' @param obs_col_name string. the observation column name.
#' @returns `cv_out$test_data_sets`, augmented with the column `score`,
#' which is the CRPS of the simulated values against the given observation.
#'
#' @export
cv_crps <- function(cv_out, obs_col_name) {
  # Joining
  forecasts = cv_out$forecasts %>% select(-simulation_num, -datetime)
  # Have this here for now b/c unsure if tsibble will affect `rowwise`,
  # and still not sure if I want tds as a tsibble or not.
  test_data_sets = cv_out$test_data_sets %>% as_tibble() %>% ungroup()
  # Joining by h and k groups all of the replicates at a particular horizon h,
  # for a particular CV season k, together. `datetime` is redundant to h.
  test_data_sets = test_data_sets %>%
    nest_join(forecasts,
              by = join_by(h, k),
              name = "forecasts")

  # Scoring
  # crps doesn't handle NAs so I have to check in that hideous looking if else
  test_data_sets = test_data_sets %>%
    rowwise() %>%
    mutate(score = if (is.na(.data[[obs_col_name]])) NA else {
      scoringRules::crps_sample(.data[[obs_col_name]],
                                forecasts %>% select(forecast) %>% pull())
    }) %>%
    ungroup()

  test_data_sets %>% select(-forecasts)
}
