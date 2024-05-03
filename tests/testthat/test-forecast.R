test_that("hot deck fc real data", {
  data = readr::read_csv(test_path("SUGG_target_temp_data.csv")) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  data = data %>%
    mutate(next_observation = dplyr::lead(observation))

  set.seed(3)
  output = data %>%
    hot_deck_forecast(
      .observation = observation,
      times = 3,
      h = 400,
      window_back = 20,
      window_fwd = 20,
      n_closest = 10,
      sampler = sample_lead("next_observation")
    )

  set.seed(3)
  expected = data %>%
    slow_hot_deck_forecast(
      .datetime = datetime,
      .observation = observation,
      times = 3,
      h = 400,
      window_back = 20,
      window_fwd = 20,
      n_closest = 10
    )

  expect_equal(output, expected)
})


test_that("hot deck fc basic test", {
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

  data = data %>%
    mutate(next_obs = dplyr::lead(obs))

  output = data %>%
    hot_deck_forecast(
      .observation = obs,
      times = 2,
      h = 2,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1,  # only get closest obs; tsibble designed to yield unique closest obs
      sampler = sample_lead("next_obs")
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


# h = 1
# window = +- 2
# n_closest = 1
# current val = 3
# "now":         *                               x
# near_rows: _________                       _____
# obs:       1 3 1 2 / / 1 4     1 1 1 1     1 2 3
# next_obs:  3 1 2 / / 1 4 /     1 1 1 /     2 3 /
# yields: 1
#
# h = 2
# window = (back = 2, fwd = 3)
# n_closest = 2
# current val = 1
# "now":           *                             x
# near_rows:   ___________                   _____
#            1 3 1 2 / / 1 4   1 1 1 1     1 2 3
# yields: 2 or 4
test_that("hot deck fc vector test", {
  data = tibble(
    datetime = c(
      as.Date("2021-01-01") + 0:7,
      as.Date("2022-04-01") + 0:3,
      as.Date("2023-01-01") + 0:2
    ),
    obs = c(
      1, 3, 1, 2, NA, NA, 1, 4,
      1, 1, 1, 1,
      1, 2, 3
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  data = data %>%
    mutate(next_obs = dplyr::lead(obs))

  set.seed(3)
  output = data %>%
    hot_deck_forecast(
      .observation = obs,
      times = 2,
      h = 2,
      window_back = 2,
      window_fwd = c(2,3),
      n_closest = c(1,2),
      sampler = sample_lead("next_obs")
    )
  expected = tibble(
    datetime = c(as.Date("2023-01-04") + 0:1,
                 as.Date("2023-01-04") + 0:1),
    h = c(1, 2, 1, 2),
    forecast = c(1, 4, 1, 2),
    simulation_num = c(1, 1, 2, 2)
  )

  expect_equal(output, expected)
})


test_that("hot deck fc covs", {
  # Setup
  cov_tsib = tibble(
    datetime = as.Date("2022-02-04"),  # dt is irrelevant actually...
    sim_num = c(1,2),
    cov_pred = c(2,3)
  ) %>%
    as_tsibble(index = datetime, key = sim_num)
  data = tibble(
    datetime = as.Date("2022-02-01") + 0:2,
    observation = c(1,8,20),
    cov_obs = c(2, 3, 4)
  ) %>%
    as_tsibble(index = datetime)
  data = data %>% append_lead_cov_lead(cov_obs,
                                   target_obs_col_name = "observation")
  sampler = sample_forecasted_covariate()

  out = data %>% hot_deck_forecast(cov_obs,
                                   times = 1,
                                   h = 2,
                                   window_back = 5,
                                   window_fwd = 5,
                                   n_closest = 1,
                                   sampler = sampler,
                                   covariate_forecasts = cov_tsib)

  # h = 1 uses latest cov obs, and target obs lead = NA,
  # h = 2 uses cov fc
  expected = tibble(
    datetime = c(as.Date("2022-02-04") + 0:1,
                 as.Date("2022-02-04") + 0:1),
    h = c(1, 2, 1, 2),
    forecast = c(NA, 8, NA, 20),  # No NA filters used
    simulation_num = c(1, 1, 2, 2)
  )

  expect_equal(out, expected)
})


test_that("hot deck fc data validators", {
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

  data_w_posix = tibble(
    datetime = c(as.POSIXct("2021-01-01") + 0:1),
    obs = c(1, 3)
  ) %>%
    as_tsibble(index = datetime)

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

  data_missing_last_obs = tibble(
    datetime = c(
      as.Date("2022-03-01") + 0:1
    ),
    obs = c(1, NA)
  ) %>%
    as_tsibble(index = datetime)


  expect_error(data_not_tsib %>%
    hot_deck_forecast(
      .observation = obs,
      times = 2,
      h = 2,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1
    ),
    regexp = "needs to be a `tsibble`")
  expect_error(data_w_posix %>%
                 hot_deck_forecast(
                   .observation = obs,
                   times = 2,
                   h = 2,
                   window_back = 2,
                   window_fwd = 2,
                   n_closest = 1
                 ),
               regexp = "needs to be a `Date`")
  expect_error(data_w_gaps %>%
                 hot_deck_forecast(
                   .observation = obs,
                   times = 2,
                   h = 2,
                   window_back = 2,
                   window_fwd = 2,
                   n_closest = 1
                 ),
               regexp = "gap")
  expect_error(data_w_keys %>%
                 hot_deck_forecast(
                   .observation = obs,
                   times = 2,
                   h = 2,
                   window_back = 2,
                   window_fwd = 2,
                   n_closest = 1
                 ),
               regexp = "key")
  expect_error(data_missing_last_obs %>%
                 hot_deck_forecast(
                   .observation = obs,
                   times = 1,
                   h = 1,
                   window_back = 1,
                   window_fwd = 1,
                   n_closest = 1
                 ),
               regexp = "Latest obs")
})


test_that("hot deck fc vector errors", {
  data = tibble(
    datetime = c(as.Date("2023-01-01") + 0:2),
    obs = c(1, 3, 1)
  ) %>%
    as_tsibble(index = datetime)

  expect_error(data %>%
                 hot_deck_forecast(
                   .observation = obs,
                   times = 2,
                   h = 5,
                   window_back = c(2,3),
                   window_fwd = 2,
                   n_closest = 1
              ),
              regexp = "window_back.*is length 2.*h, 5")
  expect_error(data %>%
                 hot_deck_forecast(
                   .observation = obs,
                   times = 2,
                   h = 5,
                   window_back = 2,
                   window_fwd = c(2,3),
                   n_closest = 1
                 ),
               regexp = "window_fwd.*is length 2.*h, 5")
  expect_error(data %>%
                 hot_deck_forecast(
                   .observation = obs,
                   times = 2,
                   h = 5,
                   window_back = 2,
                   window_fwd = 2,
                   n_closest = c(2,3)
                 ),
               regexp = "n_closest.*is length 2.*h, 5")
})


test_that("hot deck fc covariate validators", {
  data = tibble(
    datetime = c(
      as.Date("2021-01-01") + 0:3,
      as.Date("2023-01-01") + 0:2
    ),
    obs = c(
      1, 3, 1, 2,
      1, 2, 3
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  cov_data_not_tsib = tibble(
    datetime = c(as.Date("2023-01-04") + 0:3),
    cov_obs = 1:4,
    sim_num = c(1,1,2,2)
  )

  cov_data_multicol_key = tibble(
    datetime = c(as.Date("2023-01-04") + 0:3),
    cov_obs = 1:4,
    sim_num = c(1,1,2,2),
    key_b = c(1,1,2,2)
  ) %>%
    as_tsibble(key = c(sim_num, key_b), index = datetime)

  cov_data_wrong_key_vals = tibble(
    datetime = c(as.Date("2023-01-04") + 0:3),
    cov_obs = 1:4,
    sim_num = c(0,0,1,1)
  ) %>%
    as_tsibble(key = sim_num, index = datetime)

  valid_cov_data = tibble(
    datetime = c(as.Date("2023-01-04") + 0:3),
    cov_obs = 1:4,
    sim_num = c(1,1,2,2)
  ) %>%
    as_tsibble(key = sim_num, index = datetime)


  expect_error(data %>%
                 hot_deck_forecast(
                   .observation = obs,
                   times = 2,
                   h = 2,
                   window_back = 2,
                   window_fwd = 2,
                   n_closest = 1,
                   covariate_forecasts = cov_data_not_tsib
                 ),
               regexp = "needs to be a")

  expect_error(data %>%
                 hot_deck_forecast(
                   .observation = obs,
                   times = 2,
                   h = 2,
                   window_back = 2,
                   window_fwd = 2,
                   n_closest = 1,
                   covariate_forecasts = cov_data_multicol_key
                 ),
               regexp = "more than one key column")

  expect_error(data %>%
                 hot_deck_forecast(
                   .observation = obs,
                   times = 2,
                   h = 2,
                   window_back = 2,
                   window_fwd = 2,
                   n_closest = 1,
                   covariate_forecasts = cov_data_wrong_key_vals
                 ),
               regexp = "values from 1 to")

  expect_no_error(validate_cov_fcs(valid_cov_data))
})


test_that("hot deck fc warning then error in h incrementing up to empty window", {
  data = tibble(
    datetime = c(
      as.Date("2021-01-01") + 0:3,
      as.Date("2023-01-01") + 0:2
    ),
    obs = c(
      1, 3, 1, 2,
      1, 2, 3
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  data = data %>%
    mutate(next_obs = dplyr::lead(obs))

  # first, there's a warning as h = 3 yields a single
  # non-NA next_obs to draw from.
  # then, at h = 4, there's no non-NA next_obs'es, so error.
  expect_error(
    expect_warning(data %>%
      hot_deck_forecast(
        .observation = obs,
        times = 2,
        h = 20,
        window_back = 2,
        window_fwd = 2,
        n_closest = 1
      ),
      regexp = "Sampling from entire contents"),
    regexp = "No local values")
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
test_that("simulate sample path basic test", {
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

  data = data %>%
    mutate(next_obs = dplyr::lead(obs))

  output = data %>%
    simulate_sample_path(
      .datetime = datetime,
      .observation = obs,
      h = 2,
      window_back = rep(2,2),
      window_fwd = rep(2,2),
      n_closest = rep(1,2),  # only get closest obs; tsibble designed to yield unique closest obs
      sampler = sample_lead("next_obs")
    )
  expected = tibble(
    datetime = as.Date("2023-01-04") + 0:1,
    h = c(1, 2),
    forecast = c(1, 2)
  )
  expect_equal(output, expected)
})


test_that("get_local_rows", {
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
    obs = 1:8,
    ref_idx = "HI",  # These four cols are junk cols to test that .env is used.
    sim_num = ref_idx,
    window_start = ref_idx,
    window_end = ref_idx
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  # goes backwards in time by year
  expected = tibble(
    datetime = c(
      as.Date("2024-02-28") + 0:1,  # +- a day around 2/29, but 3/1 dne
      as.Date("2023-02-27") + 0:2,  # +- a day around 2/28
      as.Date("2022-03-01")   # +- a day around 2/28, but 2/27 and 2/28 dne
    ),
    obs = c(
      7:8,
      NA, NA, NA,
      1),
    ref_idx = c(
      "HI", "HI",
      NA, NA, NA,
      "HI"
    ),
    sim_num = ref_idx,
    window_start = ref_idx,
    window_end = ref_idx,
    offset = c(
      -1, 0,
      -1, 0, 1,
       1
    )
  )
  output = data %>% get_local_rows(datetime,
                                   h_curr = 1,
                                   window_back = 1,
                                   window_fwd = 1)
  expect_equal(output, expected)
})


test_that("trim leading NAs", {
  data = tibble(
    datetime = c(
      as.Date("2022-03-01"),
      as.Date("2023-04-01") + 0:4,
      as.Date("2024-02-28") + 0:1  # leap year
    ),
    obs = c(NA, 2, NA, NA, 5, NA, NA, NA)
  ) %>%
    as_tsibble(index = datetime)

  output = data %>% trim_leading_nas(obs)
  expected = tibble(
    datetime = c(
      as.Date("2022-03-01"),
      as.Date("2023-04-01") + 0:3
    ),
    obs = c(NA, 2, NA, NA, 5)
  ) %>%
    as_tsibble(index = datetime)

  expect_equal(output, expected)
})
