test_that("cv works", {
  data = tibble(
    datetime = c(
      as.Date("2021-04-02"),
      as.Date("2022-03-31") + 0:7,
      as.Date("2023-04-01") + 0:6,
      as.Date("2024-04-03")
    ),
    obs = c(
      10,
      1, 3, 1, 2, 5, 5, 5, 10,
      1, 2, 3, NA, 8, 8, 10,
      10
    )
  ) %>%
    as_tsibble(index = datetime)

  expected_2023 = tibble(
    datetime = c(as.Date("2023-04-04") + 0:1,
                 as.Date("2023-04-04") + 0:1),
    forecast = c(1, 2, 1, 2),
    simulation_num = c(1, 1, 2, 2),
    k = 1
  )
  expected_2022 = tibble(
    datetime = c(as.Date("2022-04-04") + 0:1,
                 as.Date("2022-04-04") + 0:1),
    forecast = 3,
    simulation_num = c(1, 1, 2, 2),
    k = 2
  )
  # if change to tsibble, reverse these args
  expected_fcs = bind_rows(expected_2023, expected_2022)

  expected_test_data_sets = tibble(
    datetime = c(as.Date("2022-04-04") + 0:1,
                 as.Date("2023-04-04") + 0:1),
    obs = c(5, 5, NA, 8),
    h = c(1,2,1,2),
    k = c(2,2,1,1)
  )

  out = data %>%
    cv_hot_deck_forecast(
      datetime,
      obs,
      offset = 0,
      times = 2,
      h = 2,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1
    )

  expect_equal(out$forecasts, expected_fcs)
  expect_equal(out$test_data_sets %>% as_tibble() %>% ungroup(), expected_test_data_sets)
})


test_that("cv train-test split wrong arg err", {
  data = tibble(
    datetime = as.Date("2021-04-02"),
    obs = 10
  ) %>%
    as_tsibble(index = datetime)

  expect_error(data %>%
    cv_hot_deck_forecast(
      datetime,
      obs,
      offset = 0,
      times = 2,
      h = 2,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1,
      train_test_split_type = "asdfasdf"
    ))
})


test_that("conservative train-test split", {
  data = tibble(
    datetime = c(
      as.Date("2021-04-02"),
      as.Date("2022-03-31") + 0:7,
      as.Date("2023-04-01") + 0:6,
      as.Date("2024-04-03")
    ),
    obs = c(
      10,
      1, 3, 1, 2, 5, 5, 5, 10,
      1, 2, 3, NA, 8, 8, 10,
      10
    )
  ) %>%
    as_tsibble(index = datetime)
  min_date = data %>%
    dplyr::pull(datetime) %>%
    min()
  ref_date = as.Date("2022-04-03")

  output = data %>%
    train_test_split(datetime,
                     ref_date = ref_date,
                     h = 2,
                     window_fwd = 2,
                     min_date = min_date,
                     split_type = "conservative")

  expected_test = tibble(
    datetime = as.Date("2022-04-04") + 0:1,
    obs = c(5,5),
    h = c(1,2)
  )
  # If I use the `- 1` adjustment, then I'd need to tack on a 10 in 2018.
  expected_train = tibble(
    datetime = c(
      as.Date("2019-04-01") + 0:6,
      as.Date("2020-04-03"),
      as.Date("2021-04-02"),
      as.Date("2022-03-31") + 0:3
    ),
    obs = c(
      1, 2, 3, NA, 8, 8, 10,
      10,
      10,
      1, 3, 1, 2
    )
  )

  expect_equal(output$train_data %>% as_tibble() %>% ungroup(), expected_train)
  expect_equal(output$test_data %>% as_tibble() %>% ungroup(), expected_test)
})


test_that("leaky train-test split", {
  data = tibble(
    datetime = c(
      as.Date("2021-04-02"),
      as.Date("2022-03-31") + 0:7,
      as.Date("2023-04-01") + 0:6,
      as.Date("2024-04-03")
    ),
    obs = c(
      10,
      1, 3, 1, 2, 5, 5, 5, 10,
      1, 2, 3, NA, 8, 8, 10,
      10
    )
  ) %>%
    as_tsibble(index = datetime)
  min_date = data %>%
    dplyr::pull(datetime) %>%
    min()
  ref_date = as.Date("2022-04-03")

  output = data %>%
    train_test_split(datetime,
                     ref_date = ref_date,
                     h = 2,
                     window_fwd = 2,
                     min_date = min_date,
                     split_type = "leaky")

  expected_test = tibble(
    datetime = as.Date("2022-04-04") + 0:1,
    obs = c(5,5),
    h = c(1,2)
  )

  expected_train = tibble(
    datetime = c(
      as.Date("2018-04-04") + 0:3,
      as.Date("2019-04-01") + 0:6,
      as.Date("2020-04-03"),
      as.Date("2021-04-02"),
      as.Date("2022-03-31") + 0:3
    ),
    obs = c(
      5, 5, 5, 10,
      1, 2, 3, NA, 8, 8, 10,
      10,
      10,
      1, 3, 1, 2
    )
  )

  expect_equal(output$train_data %>% as_tibble() %>% ungroup(), expected_train)
  expect_equal(output$test_data %>% as_tibble() %>% ungroup(), expected_test)
})


test_that("subtract a year", {
  d1 = as.Date("2024-03-01")
  d2 = as.Date("2024-02-28")
  d3 = as.Date("2023-03-01")
  expect_equal(subtract_year(d1), as.Date("2023-03-01"))
  expect_equal(subtract_year(d2), as.Date("2023-02-27"))
  expect_equal(subtract_year(d3), as.Date("2022-03-01"))
})
