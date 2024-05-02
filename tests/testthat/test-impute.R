test_that("trim_nas", {
  inner = c(1,NA,2,NA,3)
  full = c(NA, NA, inner, NA, NA)
  out1 = trim_nas(full)
  out2 = trim_nas(inner)
  expect_equal(out1, inner)
  expect_equal(out2, inner)
})

test_that("build_na_tibble", {
  data = tsibble::tsibble(
    date = as.Date("2022-02-01") + 0:11,
    obs = c(NA,2,3,NA,NA,6,7,NA,NA,NA,11,NA),
    index = date
  )

  expected = tibble::tibble(
    na_len = c(2, 3),
    forward_start_date = c(as.Date("2022-02-03"), as.Date("2022-02-07")),
    backward_start_date = c(as.Date("2022-02-06"), as.Date("2022-02-11"))
  )
  out = build_na_tibble(data, obs)
  expect_equal(out, expected)
})


test_that("impute", {
  data = tsibble::tsibble(
    date = c(
      as.Date("2021-02-10") + 0:3,
      as.Date("2022-02-11") + 0:4,
      as.Date("2023-02-12") + 0:3
    ),
    obs = c(
      1,2,3,4,
      1,NA,NA,NA,1,
      20,5,10,1
    ),
    index = date
  ) %>% tsibble::fill_gaps()

  expected = tsibble::tsibble(
    date = c(
      as.Date("2021-02-10") + 0:3,
      as.Date("2022-02-11") + 0:4,
      as.Date("2023-02-12") + 0:3
    ),
    obs = c(
      1,2,3,4,
      1,NA,NA,NA,1,
      20,5,10,1
    ),
    imputation_1 = c(
      1,2,3,4,
      1,2,4,10,1,
      20,5,10,1
    ),
    index = date
  ) %>% tsibble::fill_gaps()

  out = impute(data,
               .observation = obs,
               max_gap = 3,
               n_imputations = 1,
               window_back = 1,
               window_fwd = 1,
               n_closest = 1,
               sampler = "lead")

  expect_equal(out, expected)
})


test_that("impute using diff", {
  # Huh. Apparently it yields the same answer w/ deterministic setup.
  data = tsibble::tsibble(
    date = c(
      as.Date("2021-02-10") + 0:3,
      as.Date("2022-02-11") + 0:4,
      as.Date("2023-02-12") + 0:3
    ),
    obs = c(
      1,2,3,4,
      1,NA,NA,NA,1,
      20,5,10,1
    ),
    index = date
  ) %>% tsibble::fill_gaps()

  expected = tsibble::tsibble(
    date = c(
      as.Date("2021-02-10") + 0:3,
      as.Date("2022-02-11") + 0:4,
      as.Date("2023-02-12") + 0:3
    ),
    obs = c(
      1,2,3,4,
      1,NA,NA,NA,1,
      20,5,10,1
    ),
    imputation_1 = c(
      1,2,3,4,
      1,2,4,10,1,
      20,5,10,1
    ),
    index = date
  ) %>% tsibble::fill_gaps()

  out = impute(data,
               .observation = obs,
               max_gap = 3,
               n_imputations = 1,
               window_back = 1,
               window_fwd = 1,
               n_closest = 1,
               sampler = "diff")

  expect_equal(out, expected)
})


test_that("impute two gaps", {
  data = tsibble::tsibble(
    date = c(
      as.Date("2021-02-10") + 0:3,
      as.Date("2022-02-11") + 0:4,
      as.Date("2023-02-12") + 0:3
    ),
    obs = c(
      1,2,4,1,
      1,NA,1,NA,1,
      20,5,9,1
    ),
    index = date
  ) %>% tsibble::fill_gaps()

  expected = tsibble::tsibble(
    date = c(
      as.Date("2021-02-10") + 0:3,
      as.Date("2022-02-11") + 0:4,
      as.Date("2023-02-12") + 0:3
    ),
    obs = c(
      1,2,4,1,
      1,NA,1,NA,1,
      20,5,9,1
    ),
    imputation_1 = c(
      1,2,4,1,
      1,3,1,5,1,
      20,5,9,1
    ),
    index = date
  ) %>% tsibble::fill_gaps()

  out = impute(data,
               .observation = obs,
               max_gap = 3,
               n_imputations = 1,
               window_back = 1,
               window_fwd = 1,
               n_closest = 1,
               sampler = "lead")

  expect_equal(out, expected)
})


test_that("imputation errors", {
  # NA sequence too big
  data = readr::read_csv(test_path("CRAM_target_temp_data.csv")) %>%
    dplyr::select(-site_id, -field_site_subtype) %>%
    tsibble::as_tsibble(index = datetime) %>%
    tsibble::fill_gaps()
  expect_error(suppressWarnings(impute(data, observation)),
               regexp = "Try setting a smaller `max_gap`")

  # Not a tsibble
  data = readr::read_csv(test_path("CRAM_target_temp_data.csv"))
  expect_error(impute(data, observation),
               regexp = "needs to be a `tsibble`")

  # tsibble has gaps
  data_w_gaps = tibble(
    datetime = c(
      as.Date("2021-01-01") + 0:3,
      as.Date("2022-04-01") + 0:3,
      as.Date("2023-01-01") + 0:2
    ),
    obs = c(
      1, 3, 1, 2,
      1, 1, 1, 1,
      1, 2, 3
    )
  ) %>%
    as_tsibble(index = datetime)
  expect_error(impute(data_w_gaps, obs),
               regexp = "gap")


  # multi-key tsibble
  data_w_keys = tibble(
    datetime = c(
      as.Date("2021-01-01") + 0:3,
      as.Date("2022-04-01") + 0:3,
      as.Date("2023-01-01") + 0:2
    ),
    obs = c(
      1, 3, 1, 2,
      1, 1, 1, 1,
      1, 2, 3
    ),
    k = c(rep("A", 4), rep("B", 7))
  ) %>%
    as_tsibble(index = datetime, key = k)
  expect_error(impute(data_w_keys, obs),
               regexp = "key")


  # tsibble index not Date
  data_w_posix = tibble(
    datetime = c(as.POSIXct("2021-01-01") + 0:1),
    obs = c(1, 3)
  ) %>%
    as_tsibble(index = datetime)
  expect_error(impute(data_w_posix, obs),
               regexp = "to be a `Date`")
})


test_that("impute mocked cast", {
  # NB: This only works for n_imputations <= 2.
  local_mocked_bindings(
    cast = function(.data,
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
      a = tibble::tibble(
        datetime = forward_start_date + 1:na_len,
        forecast = 1,
        simulation_num = 1
      )
      b = tibble::tibble(
        datetime = forward_start_date + 1:na_len,
        forecast = 2,
        simulation_num = 2
      )
      bind_rows(a, b)
    }
  )

  data = tsibble::tsibble(
    date = as.Date("2022-02-01") + 0:11,
    obs = c(NA,2,3,NA,NA,6,7,NA,NA,NA,11,NA),
    index = date
  )
  expected = tsibble::tsibble(
    date = as.Date("2022-02-01") + 0:11,
    obs = c(NA,2,3,NA,NA,6,7,NA,NA,NA,11,NA),
    imputation_1 = c(NA,2,3,1,1,6,7,1,1,1,11,NA),
    imputation_2 = c(NA,2,3,2,2,6,7,2,2,2,11,NA),
    index = date
  )

  out = impute(data, obs, max_gap = 30, n_imputations = 2, sampler = "lead")
  expect_equal(out, expected)
})


test_that("set_head", {
  data = tsibble::tsibble(
    date = as.Date("2022-02-01") + 0:11,
    obs = c(NA,2,3,NA,NA,6,7,NA,NA,NA,11,NA),
    index = date
  )

  expected = tibble::tibble(
    date = c(as.Date("2021-02-05") + 0:7,
             as.Date("2022-02-01") + 0:3),
    obs = c(NA,6,7,NA,NA,NA,11,NA,NA,2,3,NA)
  ) %>%
    as_tsibble(index = date) %>%
    tsibble::fill_gaps()

  out = set_head(data, "2022-02-04")
  expect_equal(out, expected)
})


test_that("backcast", {
  data = tibble(
    datetime = c(
      as.Date("2021-01-01") + 0:1,
      as.Date("2022-01-01") + 0:2,
      as.Date("2023-01-01")
    ),
    obs = c(
      2, 7,
      7, 8, 10,
      10
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()
  data = append_lag(data, obs)

  expected = tibble(
    datetime = c(as.Date("2022-12-31") - 0:2,
                 as.Date("2022-12-31") - 0:2),
    h = c(3, 2, 1, 3, 2, 1),
    forecast = c(8, 7, 2, 8, 7, 2),
    simulation_num = c(1, 1, 1, 2, 2, 2)
  )

  out = backcast(data,
                 .datetime = datetime,
                 .observation = obs,
                 times = 2,
                 h = 3,
                 window_back = 20,
                 window_fwd = 20,
                 n_closest = 1,
                 sampler = sample_lead())

  expect_equal(out, expected)
})
