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


test_that("impute mocked cast", {
  # NB: This only works for n_imputations <= 2.
  local_mocked_bindings(
    cast = function(.data,
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

  out = impute(data, obs, n_imputations = 2)
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
