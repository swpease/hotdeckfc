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
    ),
    ref_date = "HI",  # These five cols are junk cols to test that .env is used.
    window_fwd = ref_date,
    max_mobile_date = ref_date,
    k = ref_date,
    h = ref_date
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expected_2023 = tibble(
    datetime = c(as.Date("2023-04-04") + 0:1,
                 as.Date("2023-04-04") + 0:1),
    h = c(1, 2, 1, 2),
    forecast = c(1, 2, 1, 2),
    simulation_num = c(1, 1, 2, 2),
    k = 1
  )
  expected_2022 = tibble(
    datetime = c(as.Date("2022-04-04") + 0:1,
                 as.Date("2022-04-04") + 0:1),
    h = c(1, 2, 1, 2),
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
    ref_date = "HI",
    window_fwd = ref_date,
    max_mobile_date = ref_date,
    k = c(2,2,1,1),  # Note col reorder. TODO: hedge against these user col names.
    h = c(1,2,1,2)
  )

  out = data %>%
    cv_hot_deck_forecast(
      obs,
      offset = 0,
      times = 2,
      h = 2,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1,
      sampler = sample_lead("next_obs")
    )

  expect_equal(out$forecasts, expected_fcs)
  expect_equal(out$test_data_sets %>% as_tibble() %>% ungroup(), expected_test_data_sets)
})


test_that("cv w/ cov fc getter", {
  cov_fc_dummy_getter <- function(training_max_date) {
    tibble(
      datetime = as.Date("2023-04-04"),  # doesn't matter
      sim_num = 1,
      cov_pred = 2
    ) %>%
      as_tsibble(index = datetime, key = sim_num)
  }

  data = tibble(
    datetime = c(
      as.Date("2023-04-01") + 0:4,
      as.Date("2024-04-03")
    ),
    observation = c(
      1,8,20,99,98,
      10
    ),
    cov_obs = c(
      2, 3, 4, 11, 12,
      10
    ),
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expected_fcs = tibble(
    datetime = c(as.Date("2023-04-04") + 0:1,
                 as.Date("2023-04-04") + 0:1),
    h = c(1, 2, 1, 2),
    forecast = c(20, 8, 20, 8),
    simulation_num = c(1, 1, 2, 2),
    k = 1
  )

  expected_test_data_sets = tibble(
    datetime = as.Date("2023-04-04") + 0:1,
    observation = c(99, 98),
    cov_obs = c(11, 12),
    h = c(1,2),
    k = 1
  )

  out = data %>%
    cv_hot_deck_forecast(
      cov_obs,
      offset = 0,
      times = 2,
      h = 2,
      window_back = 5,
      window_fwd = 2,
      n_closest = 1,
      sampler = sample_forecasted_covariate(next_target_obs_col_name = "next_target_obs",
                                                      filter_na_col_names = "next_target_obs"),
      appender = append_lead_cov_lead,
      cov_fc_getter = cov_fc_dummy_getter
    )

  expect_equal(out$forecasts, expected_fcs)
  expect_equal(out$test_data_sets %>% as_tibble() %>% ungroup(), expected_test_data_sets)
})


test_that("cv empty window err", {
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
    as_tsibble(index = datetime) %>%
    fill_gaps()

  # c.f. "test-hotdeckts.R: hot deck fc warning then error"
  expect_error(
    expect_warning(data %>%
      cv_hot_deck_forecast(
        obs,
        offset = 0,
        times = 2,
        h = 20,
        window_back = 2,
        window_fwd = 2,
        n_closest = 1
     ),
     regexp = "Sampling from entire contents"),
   regexp = "k =")
})


test_that("cv train-test split wrong arg err", {
  data = tibble(
    datetime = as.Date("2021-04-02"),
    obs = 10
  ) %>%
    as_tsibble(index = datetime)

  expect_error(data %>%
    cv_hot_deck_forecast(
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
  ) %>%
    as_tsibble(index = datetime)
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
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expect_equal(output$train_data, expected_train)
  expect_equal(output$test_data, expected_test)
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
  ) %>%
    as_tsibble(index = datetime)

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
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expect_equal(output$train_data, expected_train)
  expect_equal(output$test_data, expected_test)
})


test_that("train-test split no mobile part", {
  # Testing empty mobile part.
  data = tibble(
    datetime = c(
      as.Date("2021-04-02"),
      as.Date("2022-03-31") + 0:7,
      as.Date("2023-04-01") + 0:6,
      as.Date("2024-04-03") + 0:4  # adding four els s.t. `mobile` is empty
    ),
    obs = c(
      10,
      1, 3, 1, 2, 5, 5, 5, 10,
      1, 2, 3, NA, 8, 8, 10,
      10, 4, 4, 4, 4
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()
  min_date = data %>%
    dplyr::pull(datetime) %>%
    min()
  ref_date = as.Date("2024-04-03")  # NB Date: four els after = 2 h + 2 w_fwd

  output = data %>%
    train_test_split(datetime,
                     ref_date = ref_date,
                     h = 2,
                     window_fwd = 2,
                     min_date = min_date,
                     split_type = "conservative")

  expected_test = tibble(
    datetime = as.Date("2024-04-04") + 0:1,
    obs = c(4,4),
    h = c(1,2)
  ) %>%
    as_tsibble(index = datetime)

  expected_train = tibble(
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
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expect_equal(output$train_data, expected_train)
  expect_equal(output$test_data, expected_test)

  # Testing len 1 mobile part
  next_row = tsibble(
    datetime = as.Date("2024-04-08"),
    obs = 44)
  data = bind_rows(data, next_row)

  expect_equal(output$train_data, expected_train)
  expect_equal(output$test_data, expected_test)
})


test_that("train-test split mobile part day before first obs in ante", {
  data = tibble(
    datetime = c(
      as.Date("2022-04-10"),
      as.Date("2023-04-03") + 0:6  # adding 6 els s.t. `mobile` has 2 els
    ),
    obs = c(
      0,
      10, 4, 4, 4, 4, 6, 6
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()
  min_date = data %>%
    dplyr::pull(datetime) %>%
    min()
  ref_date = as.Date("2023-04-03")  # NB Date: four els after = 2 h + 2 w_fwd

  output = data %>%
    train_test_split(datetime,
                     ref_date = ref_date,
                     h = 2,
                     window_fwd = 2,
                     min_date = min_date,
                     split_type = "conservative")

  expected_test = tibble(
    datetime = as.Date("2023-04-04") + 0:1,
    obs = c(4,4),
    h = c(1,2)
  ) %>%
    as_tsibble(index = datetime)

  expected_train = tibble(
    datetime = c(
      as.Date("2021-04-08") + 0:1,
      as.Date("2022-04-10"),
      as.Date("2023-04-03")
    ),
    obs = c(
      6, 6,
      0,
      10
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expect_equal(output$train_data, expected_train)
  expect_equal(output$test_data, expected_test)
})


test_that("train-test split vector window", {
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
                     window_fwd = c(1,2),
                     min_date = min_date,
                     split_type = "conservative")

  expected_test = tibble(
    datetime = as.Date("2022-04-04") + 0:1,
    obs = c(5,5),
    h = c(1,2)
  ) %>%
    as_tsibble(index = datetime)
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
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expect_equal(output$train_data, expected_train)
  expect_equal(output$test_data, expected_test)
})


test_that("calc_cv_crps", {
  data = tibble(
    datetime = c(
      as.Date("2021-04-02"),
      as.Date("2022-03-31") + 0:7,
      as.Date("2023-04-01") + 0:6,
      as.Date("2024-04-03")
    ),
    obs = c(
      10,
      1, 3, 1, 2, 5, 6, 5, 10,  # NB: change 5,5,5 -> 5,6,5 from "cv works" test
      1, 2, 3, NA, 8, 8, 10,
      10
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expected_crps_out = tibble(
    datetime = c(as.Date("2022-04-04") + 0:1,
                 as.Date("2023-04-04") + 0:1),
    obs = c(5, 6, NA, 8),
    h = c(1,2,1,2),
    k = c(2,2,1,1),
    score = c(2,3,NA,6)
  )

  out = data %>%
    cv_hot_deck_forecast(
      obs,
      offset = 0,
      times = 2,
      h = 2,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1,
      sampler = sample_lead("next_obs"),
      appender = append_lead
    )
  crps_out = calc_cv_crps(out, "obs")

  expect_equal(crps_out, expected_crps_out)
})




test_that("calc_cv_crps NAs", {
  # basic
  data = tibble(
    datetime = c(
      as.Date("2022-04-01") + 0:3,
      as.Date("2023-04-01") + 0:2
    ),
    cov = c(
      0, 0, 1, 0,
      1, 1, 0
    ),
    observation = c(
      0, 0, 0, 1,
      1, 2, 0
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expected_crps_out = tibble(
    datetime = as.Date("2022-04-04"),
    cov = 0,
    observation = 1,
    h = 1,
    k = 1,
    score = 0.5
  )

  set.seed(3)  # gets 04-01 then 04-02
  out = data %>%
    cv_hot_deck_forecast(
      cov,
      offset = 0,
      times = 2,
      h = 1,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1,
      sampler = sample_covariate_lead(),
      appender = append_lead_cov_lead
    )
  crps_out = calc_cv_crps(out, "observation")

  expect_equal(crps_out, expected_crps_out)


  # test obs NA should yield CRPS NA
  data = tibble(
    datetime = c(
      as.Date("2022-04-01") + 0:3,
      as.Date("2023-04-01") + 0:2
    ),
    cov = c(
      0, 0, 1, 0,
      1, 1, 0
    ),
    observation = c(
      0, 0, 0, NA,
      1, 2, 0
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expected_crps_out = expected_crps_out %>%
    dplyr::mutate(score = NA,
                  observation = as.double(NA))

  set.seed(3)  # gets 04-01 then 04-02
  out = data %>%
    cv_hot_deck_forecast(
      cov,
      offset = 0,
      times = 2,
      h = 1,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1,
      sampler = sample_covariate_lead(),
      appender = append_lead_cov_lead
    )
  crps_out = calc_cv_crps(out, "observation")

  expect_equal(crps_out, expected_crps_out)


  # fc all NA should yield CRPS NA
  data = tibble(
    datetime = c(
      as.Date("2022-04-01") + 0:3,
      as.Date("2023-04-01") + 0:2
    ),
    cov = c(
      0, 0, 1, 0,
      1, 1, 0
    ),
    observation = c(
      0, 0, 0, 1,
      1, NA, NA
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expected_crps_out = expected_crps_out %>%
    dplyr::mutate(score = NA,
                  observation = 1)

  set.seed(3)  # gets 04-01 then 04-02
  out = data %>%
    cv_hot_deck_forecast(
      cov,
      offset = 0,
      times = 2,
      h = 1,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1,
      sampler = sample_covariate_lead(),
      appender = append_lead_cov_lead
    )
  crps_out = calc_cv_crps(out, "observation")

  expect_equal(crps_out, expected_crps_out)


  # fc some NA: does it filter forecasted NAs (scoring breaks w/ NAs)?
  # should yield the CRPS of all non-NA forecasted values.
  data = tibble(
    datetime = c(
      as.Date("2022-04-01") + 0:3,
      as.Date("2023-04-01") + 0:2
    ),
    cov = c(
      0, 0, 1, 0,
      1, 1, 0
    ),
    observation = c(
      0, 0, 0, 1,
      1, 1, NA
    )
  ) %>%
    as_tsibble(index = datetime) %>%
    fill_gaps()

  expected_crps_out = expected_crps_out %>%
    dplyr::mutate(score = 0,
                  observation = 1)

  set.seed(3)  # gets 04-01 then 04-02
  out = data %>%
    cv_hot_deck_forecast(
      cov,
      offset = 0,
      times = 2,
      h = 1,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1,
      sampler = sample_covariate_lead(),
      appender = append_lead_cov_lead
    )
  crps_out = calc_cv_crps(out, "observation")

  expect_equal(crps_out, expected_crps_out)
})


# TODO: fix
test_that("subtract a year", {
  d1 = as.Date("2024-03-01")
  d2 = as.Date("2024-02-28")
  d3 = as.Date("2023-03-01")
  expect_equal(subtract_year(d1), as.Date("2023-03-01"))
  expect_equal(subtract_year(d2), as.Date("2023-02-27"))
  expect_equal(subtract_year(d3), as.Date("2022-03-01"))
  expect_equal(lubridate::is.Date(d1), TRUE)
  expect_equal(lubridate::is.Date(d2), TRUE)
  expect_equal(lubridate::is.Date(d3), TRUE)
})
