#' Hot-deck Impute
#'
#' Impute using hot deck forecasting.
#'
#' Restrictions on the input data:
#'   1. The data is a `tsibble`.
#'   2. The tsibble is not multi-keyed (i.e. multiple time serieses).
#'   3. The data has no gaps (i.e. missing rows; NA observations okay).
#'   4. The index is a Date.
#'
#' `max_gap` and your windows are related. If your window contains no
#' observations, then the imputation will error. So, you need to make sure
#' that the `max_gap` is small enough -- or that your window is wide enough --
#' so that there are always observations to sample from.
#'
#' `window_back`, `window_fwd`, and `n_closest` are restricted to being
#' scalars for imputation (they can be vectors when calling
#' [hot_deck_forecast()]).
#'
#' For `sampler`, `"diff"` can be tried if `"lead"` isn't producing
#' sensible forecasts. It uses differences between observations -- rather
#' than taking those adjacent observations -- to generate the imputed
#' values. As such, it can inject some diversity into sparse regions.
#'
#' @param .data tsibble. The data
#' @param .observation symbol. The observations column.
#' @param max_gap integer. Largest gap size to impute. Larger gaps return NAs.
#' @param n_imputations integer. Number of imputations to perform.
#' @param window_back integer.
#'   How many days back to include in the window for a given season.
#' @param window_fwd integer.
#'   How many days forward to include in the window for a given season.
#' @param n_closest integer.
#'   The number of closest observations to pick from per hot deck random sampling.
#' @inheritParams hot_deck_forecast
#' @returns .data, augmented with `n_imputations` new columns of imputed
#'   observations, named "imputation_{n}".
#'
#' @export
impute <- function(.data,
                   .observation,
                   max_gap = 1000,
                   n_imputations = 5,
                   window_back = 20,
                   window_fwd = 20,
                   n_closest = 5,
                   sampler = c("lead", "diff")) {
  validate_impute_input_data(.data, {{ .observation }})

  # I'm restricting the user's sampler options b/c of backcasting-related
  # restrictions.
  sampler = match.arg(sampler)
  sampler = if (sampler == "lead") sample_lead() else sample_diff()

  date_col = tsibble::index(.data)

  na_tibble = build_na_tibble(.data, {{ .observation }})
  casts = purrr::pmap(na_tibble,
                      \(na_len, forward_start_date, backward_start_date) {
      tryCatch(
        expr = cast(.data = .data,
                    .datetime = {{ date_col }},
                    .observation = {{ .observation }},
                    na_len,
                    forward_start_date,
                    backward_start_date,
                    max_gap = max_gap,
                    n_imputations = n_imputations,
                    window_back = window_back,
                    window_fwd = window_fwd,
                    n_closest = n_closest,
                    sampler = sampler),
        error = function(e) {
          # grep returns integer(0) if no matches; index of first match if match
          added_msg = if (length(grep("No local values", conditionMessage(e), fixed = TRUE)) > 0) {
            paste("\n\nTry setting a smaller `max_gap`\n",
                  "and/or larger `window_back` and/or larger `window_fwd`.\n\n")
          } else {
            ""
          }
          stop(paste("\nHot Deck Error Msg:\n", conditionMessage(e),
                     added_msg),
               call. = FALSE)
        }
      )

    }) %>%
    purrr::reduce(\(acc, nxt) dplyr::bind_rows(acc, nxt))  # , .init = NULL

  for (i in 1:n_imputations) {
    imp = casts %>%
      dplyr::filter(simulation_num == .env$i) %>%
      dplyr::select(-simulation_num)
    .data = dplyr::left_join(.data,
                             imp,
                             by = dplyr::join_by({{ date_col }} == datetime))
    .data = .data %>%
      dplyr::mutate("imputation_{i}" := dplyr::if_else(is.na({{ .observation }}),
                                                       forecast,
                                                       {{ .observation }})) %>%
      dplyr::select(-forecast)
  }

  .data
}


#' Cast an NA contig
#'
#' Cast (fore + back melded) a contiguous sequence of NAs.
#'
#' See [build_na_tibble()] for details on `na_len`, `forward_start_date`,
#' and `backward_start_date`.
#'
#' @param .datetime symbol. The dates column.
#' @param na_len integer. The length of the NA contig.
#' @param forward_start_date Date. The date from which to being forecasting.
#' @param backward_start_date Date. The date from which to being backcasting.
#' @inheritParams impute
#' @inheritParams hot_deck_forecast
#' @returns tibble of casts:
#'   - nrow = na_len * n_imputations,
#'   - columns:
#'     - simulation_num: the simulated sample path number
#'     - datetime: the date for the cast
#'     - forecast: the casted value
#' @noRd
cast <- function(.data,
                 .datetime,
                 .observation,
                 na_len,
                 forward_start_date,
                 backward_start_date,
                 max_gap,
                 n_imputations,
                 window_back,
                 window_fwd,
                 n_closest,
                 sampler) {
  if (na_len > max_gap) {
    sim_nums = purrr::map(1:n_imputations, \(x) rep(x, na_len)) %>% purrr::list_c()
    tibble::tibble(
      datetime = rep(forward_start_date + 1:na_len, n_imputations),
      simulation_num = sim_nums,
      forecast = NA
    )
  } else {
    forecasts = forecast_for_imputation(.data = .data,
                                        .datetime = {{ .datetime }},
                                        .observation = {{ .observation }},
                                        forward_start_date = forward_start_date,
                                        n_imputations = n_imputations,
                                        na_len = na_len,
                                        window_back = window_back,
                                        window_fwd = window_fwd,
                                        n_closest = n_closest,
                                        sampler = sampler)
    backcasts = backcast_for_imputation(.data = .data,
                                        .datetime = {{ .datetime }},
                                        .observation = {{ .observation }},
                                        backward_start_date = backward_start_date,
                                        n_imputations = n_imputations,
                                        na_len = na_len,
                                        window_back = window_back,
                                        window_fwd = window_fwd,
                                        n_closest = n_closest,
                                        sampler = sampler)

    meld(forecasts, backcasts)
  }
}


#' Meld forecasts and backcasts
#'
#' Meld forecasts and backcasts, yeilding combined casts.
#'
#' Finds the closest point between the forecasts and backcasts for each
#' `simulation_num`, then creates a melded cast where points prior to
#' said closest point are from the forecast, points after are from the
#' backcast, and the point in question is taken as the average between the two.
#'
#' @param forecasts tibble. Output of [forecast_for_imputation()]
#' @param backcasts tibble. Output of [backcast_for_imputation()]
#' @returns tibble.
#' @noRd
meld <- function(forecasts, backcasts) {
  casts = dplyr::inner_join(forecasts,
                            backcasts,
                            by = dplyr::join_by(datetime, simulation_num),
                            suffix = c(".fc", ".bc"),
                            unmatched = "error")
  casts = casts %>% dplyr::mutate(diff = abs(forecast.fc - forecast.bc))
  casts = casts %>%
    dplyr::group_by(simulation_num) %>%
    dplyr::group_modify(.f = group_meld) %>%
    dplyr::ungroup()

  casts
}


#' Helper function for [meld()]
#'
#' This function does the actual melding.
#'
#' @param grp The group data (without the grouping cols)
#' @param .by The corresponding grouping values
#' @noRd
group_meld <- function(grp, .by) {
  diff = grp %>% dplyr::pull(diff)
  min_diff = min(diff)
  min_diff_idx = purrr::detect_index(diff, \(x) x == min_diff)
  avg_at_min = grp %>%
    dplyr::slice(min_diff_idx) %>%
    dplyr::mutate(avg = (forecast.fc + forecast.bc) / 2) %>%
    dplyr::pull(avg)

  grp = grp %>% dplyr::mutate(idx = dplyr::row_number())

  grp = grp %>% dplyr::mutate(
    forecast = dplyr::case_when(
      idx < min_diff_idx ~ forecast.fc,
      idx == min_diff_idx ~ avg_at_min,
      idx > min_diff_idx ~ forecast.bc
    )
  )

  grp %>% dplyr::select(datetime, forecast)
}


#' @inheritParams hot_deck_forecast
#' @noRd
backcast <- function(.data,
                     .datetime,
                     .observation,
                     times,
                     h,
                     window_back,
                     window_fwd,
                     n_closest,
                     sampler) {
  backcasts = NULL
  for (sim_num_curr in 1:times) {
    data_curr = .data
    h_curr = h
    while (h_curr > 0) {
      one_step_backcast = hot_deck_forecast(data_curr,
                                            .datetime = {{ .datetime }},
                                            .observation = {{ .observation }},
                                            times = 1,
                                            h = 1,
                                            window_back = window_back,
                                            window_fwd = window_fwd,
                                            n_closest = n_closest,
                                            sampler = sampler)
      one_step_backcast = one_step_backcast %>%
        # hdfc puts date as next; we want prior. c.f. forecast.R#269
        dplyr::mutate(datetime = datetime - 2,
                      h = .env$h_curr,
                      simulation_num = .env$sim_num_curr)
      backcasts = dplyr::bind_rows(backcasts, one_step_backcast)

      # Updating dataset
      new_obs = one_step_backcast %>% dplyr::pull(forecast)
      data_curr = data_curr %>% dplyr::slice(1:(dplyr::n() - 1))
      data_curr = data_curr %>% dplyr::mutate(
        {{ .observation }} := dplyr::if_else(dplyr::row_number() == dplyr::n(),
                                             .env$new_obs,
                                             {{ .observation }})
      )
      h_curr = h_curr - 1
    }
  }

  backcasts
}


#' Backcast an NA contig
#'
#' Backcast a contiguous sequence of NAs.
#'
#' This method handles the details that make the backcasting aspect
#' of the imputation work. It adjusts the leading date, calls the `append`ers
#' -- which I don't want to bother the user with -- and removes the `h` column
#' from the backcast.
#'
#' @inheritParams hot_deck_forecast
#' @inheritParams cast
#' @returns tibble of backcasts:
#'   - nrow = na_len * n_imputations,
#'   - columns:
#'     - datetime: the date for the backcast
#'     - forecast: the backcasted value
#'     - simulation_num: the simulated sample path number
#' @noRd
backcast_for_imputation <- function(.data,
                                    .datetime,
                                    .observation,
                                    backward_start_date,
                                    n_imputations,  # = times
                                    na_len,  # = h
                                    window_back,
                                    window_fwd,
                                    n_closest,
                                    sampler) {
  .data = set_head(.data, backward_start_date)
  .data = append_lag(.data, {{ .observation }})
  .data = append_lag_diff(.data, {{ .observation }})

  bcs = backcast(.data,
                 {{ .datetime }},
                 {{ .observation }},
                 times = n_imputations,
                 h = na_len,
                 window_back = window_back,
                 window_fwd = window_fwd,
                 n_closest = n_closest,
                 sampler = sampler)

  bcs %>% dplyr::select(-h)  # Useless col for imputation.
}


#' Forecast an NA contig
#'
#' Forecast a contiguous sequence of NAs.
#'
#' This method handles the details that make the forecasting aspect
#' of the imputation work. It adjusts the leading date, calls the `append`ers
#' -- which I don't want to bother the user with -- and removes the `h` column
#' from the forecast.
#'
#' @inheritParams hot_deck_forecast
#' @inheritParams cast
#' @returns tibble of forecasts:
#'   - nrow = na_len * n_imputations,
#'   - columns:
#'     - datetime: the date for the forecast
#'     - forecast: the forecasted value
#'     - simulation_num: the simulated sample path number
#' @noRd
forecast_for_imputation <- function(.data,
                                    .datetime,
                                    .observation,
                                    forward_start_date,
                                    n_imputations,  # = times
                                    na_len,  # = h
                                    window_back,
                                    window_fwd,
                                    n_closest,
                                    sampler) {
  .data = set_head(.data, forward_start_date)
  .data = append_lead(.data, {{ .observation }})
  .data = append_diff(.data, {{ .observation }})

  fcs = hot_deck_forecast(.data,
                          {{ .datetime }},
                          {{ .observation }},
                          times = n_imputations,
                          h = na_len,
                          window_back = window_back,
                          window_fwd = window_fwd,
                          n_closest = n_closest,
                          sampler = sampler,
                          covariate_forecasts = NULL)

  fcs %>% dplyr::select(-h)  # Useless col for imputation.
}


#' Set head of data by date
#'
#' Moves all data beyond `date` to before earliest date in `.data`.
#'
#' This method co-opts [train_test_split()].
#'
#' @param .data tsibble. Your data
#' @param date Date.
#' @returns .data, reordered
#'
#' @noRd
set_head <- function(.data, date) {
  datetime = tsibble::index(.data)
  min_date = .data %>%
    dplyr::pull({{ datetime }}) %>%
    dplyr::first()
  # Edge case: two obses before NAs at head will yield a
  # missing data point. Oh well.
  tt_split = train_test_split(.data,
                              {{ datetime }},
                              ref_date = date,
                              h = 1,
                              window_fwd = 1,
                              min_date = min_date,
                              split_type = "leaky")
  tt_split$train_data
}


#' Build NA tibble
#'
#' Build tibble of lengths, start dates (-1), and end dates (+1) for missing data.
#'
#' For each row of the output, you can think of the `forward_start_date` (and
#' `backward_start_date`) as equivalent to the observation you would use for
#' the start of your forecast(/backcast), with the `na_len` equivalent to `h`.
#' So if you had NAs from 2022/2/23-2022/2/25, you'd have:
#'   - `na_len` == 3,
#'   - `forward_start_date` == 2022/2/22
#'   - `backward_start_date` == 2022/2/26
#'
#' @param .data tsibble. The data
#' @param .observation symbol. The observations column.
#'
#' @returns tibble:
#'   - na_len: integer. Length of NA sequence
#'   - forward_start_date: Date. The latest date that bounds the respective
#'     na_len from below (exclusive, so the latest actual observation's date)
#'   - backward_start_date: Date. The earliest date that bounds the respective
#'     na_len from above (exclusive, so the earliest actual observation's date)
#'
#' @noRd
build_na_tibble <- function(.data, .observation) {
  date_col = tsibble::index(.data)

  augmented_data = .data %>%
    dplyr::mutate(
      lead_obs = dplyr::lead({{ .observation }}),
      lag_obs = dplyr::lag({{ .observation }}))
  fwd_start_dates = augmented_data %>%
    dplyr::filter(!is.na({{ .observation }}),
                  is.na(lead_obs)) %>%
    dplyr::slice(1:(dplyr::n() - 1)) %>%  # Final obs always has NA for lead
    dplyr::pull(.env$date_col)
  back_start_dates = augmented_data %>%
    dplyr::filter(!is.na({{ .observation }}),
                  is.na(lag_obs)) %>%
    dplyr::slice(2:dplyr::n()) %>%  # First obs always has NA for lag
    dplyr::pull(.env$date_col)
  # ... also re slicing, don't want to include any leading/trailing NAs
  # anyway. c.f. `trim_nas`, below.

  obses = .data %>% dplyr::pull({{ .observation }})
  trimmed_obses = trim_nas(obses)  # Only deal w/ NAs bounded by obses
  na_rle = rle(is.na(trimmed_obses))
  na_lens = na_rle$length[na_rle$value]

  tibble::tibble(
    na_len = na_lens,
    forward_start_date = fwd_start_dates,
    backward_start_date = back_start_dates
  )
}


#' Trim NAs
#'
#' Trim NAs (leading and trailing) from a vector.
#'
#' @param vec vector
#' @returns vector without leading/trailing NAs
#'
#' @noRd
trim_nas <- function(vec) {
  first_val_idx = purrr::detect_index(vec, \(x) !is.na(x))
  last_val_idx = purrr::detect_index(vec, \(x) !is.na(x), .dir = "backward")
  vec[first_val_idx:last_val_idx]
}


#' [impute()] validation
#'
#' Validation for [impute()].
#'
#' Checks that:
#'   1. The data is a `tsibble`.
#'   2. The tsibble is not multi-keyed (i.e. multiple time serieses).
#'   3. The data has no gaps (i.e. missing rows; NA observations okay).
#'   4. The index is a Date.
#'
#' @inheritParams impute
#'
#' @noRd
validate_impute_input_data <- function(data, .observation) {
  if (isFALSE(tsibble::is_tsibble(data))) {
    stop("Your data needs to be a `tsibble`.", call. = FALSE)
  }

  # keyless tsibbles have n_keys == 1
  if (tsibble::n_keys(data) > 1) {
    stop(paste("`impute` cannot handle multi-key tsibbles.\n",
               "Multiple keys means multiple time series.",
               "Examine your keys with `tsibble::key`,",
               "and filter to one key's data."))
  }

  if (isTRUE(tsibble::has_gaps(data) %>% pull())) {
    stop(paste("Your tsibble contains gaps.\n",
               "Try using `tsibble::fill_gaps`."),
         call. = FALSE)
  }

  is_date = data %>%
    dplyr::pull(tsibble::index(.)) %>%
    is.Date()
  if (isFALSE(is_date)) {
    stop("Your tsibble's index needs to be a `Date`.",
         call. = FALSE)
  }
}
