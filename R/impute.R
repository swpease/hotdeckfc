#' Hot-deck Impute
#'
#' Impute using hot deck forecasting.
#'
#' @param .data tsibble. The data
#' @param .observation symbol. The observations column.
#' @param n_imputations integer. Number of imputations to perform.
#' @inheritParams hot_deck_forecast
#' @returns .data, augmented with `n_imputations` new columns of imputed
#'   observations, named "imputation_{n}".
#'
#' @export
impute <- function(.data,
                   .observation,
                   n_imputations = 5,
                   window_back = 20,
                   window_fwd = 20,
                   n_closest = 5,
                   sampler = sample_lead("next_obs")) {
  # TODO: validation
  date_col = tsibble::index(.data)

  na_tibble = build_na_tibble(.data, {{ .observation }})
  casts = purrr::pmap(na_tibble,
                      \(na_len, forward_start_date, backward_start_date) {
      cast(.data = .data,
           .datetime = {{ date_col }},
           .observation = {{ .observation }},
           na_len,
           forward_start_date,
           backward_start_date,
           n_imputations = n_imputations,
           window_back = window_back,
           window_fwd = window_fwd,
           n_closest = n_closest,
           sampler = sampler
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


#' @noRd
cast <- function(.data,
                 .datetime,
                 .observation,
                 na_len,
                 forward_start_date,
                 backward_start_date,
                 n_imputations,
                 window_back,
                 window_fwd,
                 n_closest,
                 sampler) {
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
        dplyr::mutate(h = .env$h_curr,
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
#' Build tibble of lengths, start dates, and end dates for missing data.
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
    dplyr::slice(1:(dplyr::n() - 1)) %>%
    dplyr::pull(.env$date_col)
  back_start_dates = augmented_data %>%
    dplyr::filter(!is.na({{ .observation }}),
                  is.na(lag_obs)) %>%
    dplyr::slice(2:dplyr::n()) %>%
    dplyr::pull(.env$date_col)

  obses = .data %>% dplyr::pull({{ .observation }})
  trimmed_obses = trim_nas(obses)
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
validate_imputation <- function(.data, .observation) {
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

  is_date = .data %>%
    dplyr::pull(tsibble::index(.)) %>%
    is.Date()
  if (isFALSE(is_date)) {
    stop("Your tsibble's index need to be a `Date`.",
         call. = FALSE)
  }
}


plot_imputation <- function(.imputation, .observation) {
  x_axis = tsibble::index(.imputation)
  # b/c tsibble, keeps index col
  imputations = .imputation %>%
    dplyr::select(dplyr::starts_with("imputation_")) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(!{{ x_axis }},
                        names_to = "imputation_num",
                        values_to = "imputed_obs")

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = imputations,
      mapping = ggplot2::aes(
        y = imputed_obs,
        x = {{ x_axis }},
        color = imputation_num
      ),
      show.legend = FALSE,
      alpha = 0.6
    ) +
    ggplot2::geom_line(
      data = .imputation,
      mapping = ggplot2::aes(
        y = {{ .observation }},
        x = {{ x_axis }}
      )
    )
}
