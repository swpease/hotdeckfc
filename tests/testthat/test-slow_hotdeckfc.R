test_that("slowhot deck fc basic test", {
  data = tibble(
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
    as_tsibble(index = datetime) %>%
    fill_gaps()

  output = data %>%
    slow_hot_deck_forecast(
      .datetime = datetime,
      .observation = obs,
      times = 2,
      h = 2,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1  # only get closest obs; tsibble designed to yield unique closest obs
    )
  expected = tibble(
    datetime = c(as.Date("2023-01-04") + 0:1,
                 as.Date("2023-01-04") + 0:1),
    h = c(1, 2, 1, 2),
    forecast = c(1, 2, 1, 2),
    simulation_num = c(1, 1, 2, 2)
  )

  expect_equal(output, expected)
})


test_that("slowhot deck fc partial validators", {
  data_not_tsib = tibble(
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
  )

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

  expect_error(data_not_tsib %>%
                 slow_hot_deck_forecast(
                   .datetime = datetime,
                   .observation = obs,
                   times = 2,
                   h = 2,
                   window_back = 2,
                   window_fwd = 2,
                   n_closest = 1
                 ),
               regexp = "needs to be a")
  expect_error(data_w_gaps %>%
                 slow_hot_deck_forecast(
                   .datetime = datetime,
                   .observation = obs,
                   times = 2,
                   h = 2,
                   window_back = 2,
                   window_fwd = 2,
                   n_closest = 1
                 ),
               regexp = "gap")
  expect_error(data_w_keys %>%
                 slow_hot_deck_forecast(
                   .datetime = datetime,
                   .observation = obs,
                   times = 2,
                   h = 2,
                   window_back = 2,
                   window_fwd = 2,
                   n_closest = 1
                 ),
               regexp = "key")
})


# window = +- 2
# h = 1
# "now":                                 x
# near_rows: _______                 _____
# obs:       1 3 1 2     1 1 1 1     1 2 3
# next_obs:  3 1 2 /     1 1 1 /     2 3 /
#
# h = 2
# "now":                                   x
# near_rows:   _______                 _____
#            1 3 1 2     1 1 1 1     1 2 3
test_that("slowsimulate sample path basic test", {
  data = tibble(
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
    as_tsibble(index = datetime) %>%
    fill_gaps()

  output = data %>%
    slow_simulate_sample_path(
      .datetime = datetime,
      .observation = obs,
      h = 2,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1  # only get closest obs; tsibble designed to yield unique closest obs
    )
  expected = tibble(
    datetime = as.Date("2023-01-04") + 0:1,
    h = c(1, 2),
    forecast = c(1, 2)
  )
  expect_equal(output, expected)
})

test_that("slowsimulate sample path error if latest obs is NA", {
  data = tibble(
    datetime = c(
      as.Date("2022-03-01") + 0:1
    ),
    obs = c(1, NA)
  ) %>%
    as_tsibble(index = datetime)

  expect_error(data %>% slow_simulate_sample_path(datetime, obs, 1, 1, 1, 1))
})


test_that("slowget_local_rows", {
  # so we start on Feb 29, 2024
  # we get it plus the prior day (per window back)
  # prior year has nothing in Feb 27 - Mar 1 (leap yr shift; window)
  # two yrs ago has Mar 1 in window, even though Feb 28th is missing.
  data = tibble(
    datetime = c(
      as.Date("2022-03-01"),
      as.Date("2023-04-01") + 0:4,
      as.Date("2024-02-28") + 0:1  # leap year
    ),
    obs = 1:8
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  # goes backwards in time by year
  expected = tibble(
    datetime = c(
      as.Date("2024-02-28") + 0:1,
      as.Date("2023-02-27") + 0:2,  # +- a day around 2/28
      as.Date("2022-03-01")   # +- a day around 2/28, but 2/27 dne
    ),
    obs = c(
      7, 8,
      NA, NA, NA,
      1)
  )
  output = data %>% slow_get_local_rows(datetime,
                                   h_curr = 1,
                                   window_back = 1,
                                   window_fwd = 1)
  expect_equal(output, expected)
})
