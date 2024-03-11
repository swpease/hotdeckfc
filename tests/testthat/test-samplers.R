test_that("basic_hot_deck_sampler basic test", {
  local_rows = tibble(
    obs = 1:10,
    next_obs = dplyr::lead(obs),
    offset = 1:10
  )
  current_obs = 4
  n_closest = 2

  wrapped = basic_hot_deck_sampler("next_obs")
  expected_1 = list(
    new_current_obs = 5,
    forecast = 5
  )
  expected_2 = list(
    new_current_obs = 6,
    forecast = 6
  )
  expected_3 = list(
    new_current_obs = 4,
    forecast = 4
  )

  set.seed(3)
  out1 = local_rows %>% wrapped(obs, current_obs, n_closest)
  expect_equal(out1, expected_1)
  set.seed(4)
  out2 = local_rows %>% wrapped(obs, current_obs, n_closest)
  expect_equal(out2, expected_2)
  set.seed(5)
  out3 = local_rows %>% wrapped(obs, current_obs, n_closest)
  expect_equal(out3, expected_3)
})

test_that("basic_hot_deck_sampler no local values", {
  local_rows = tibble(
    obs = 1:10,
    next_obs = rep(NA, 10),
    offset = 1:10
  )
  current_obs = 4
  n_closest = 2

  wrapped = basic_hot_deck_sampler("next_obs")

  expect_error(local_rows %>% wrapped(obs, current_obs, n_closest),
               regexp = "No local values.*for")
})


test_that("sample_local_rows errors", {
  local_rows = tibble(
    obs = 1:10,
    der = rep(NA, 10),
    offset = 1:10
  )

  expect_error(local_rows %>%
                 sample_local_rows(obs,
                                   current_obs = 2,
                                   n_closest = 2,
                                   derived_col_name = "der",
                                   n_bins = 0),
               regexp = "No local values.*for")

  local_rows = tibble(
    obs = 1:3,
    der = c(1, NA, NA),
    offset = c(-1, 0, 1)
  )

  set.seed(3)  # will get non-negative portion
  expect_error(local_rows %>%
                 sample_local_rows(obs,
                                   current_obs = 2,
                                   n_closest = 2,
                                   derived_col_name = "der",
                                   n_bins = 1),
               regexp = "No local values.*non-neg.*offset portion")
})


test_that("sample_local_rows warning entire bin", {
  local_rows = tibble(
    obs = 1:10,
    der = rep(1, 10),
    offset = 1:10
  )

  expect_warning(local_rows %>%
                 sample_local_rows(obs,
                                   current_obs = 2,
                                   n_closest = 20,
                                   derived_col_name = "der",
                                   n_bins = 0),
               regexp = "Sampling from entire bin")
})


test_that("sample_local_rows n_bins > nrows works", {
  local_rows = tibble(
    obs = 1:10,
    der = rep(1, 10),
    offset = -5:4
  )

  expect_no_error(
    suppressWarnings(local_rows %>%
      sample_local_rows(obs,
                        current_obs = 2,
                        n_closest = 1,
                        derived_col_name = "der",
                        n_bins = 20))
  )
})


test_that("sample_local_rows binning", {
  local_rows = tibble(
    obs = c(1,2,3,8,8),
    der = c(9,4,5,NA,NA),
    offset = -2:2
  )

  set.seed(4)
  expect_warning(local_rows %>%
    sample_local_rows(obs,
                      current_obs = 1,
                      n_closest = 1,
                      derived_col_name = "der",
                      n_bins = 2),
                  regexp = "Sampling from entire bin")
  set.seed(4)
  out = suppressWarnings(local_rows %>%
    sample_local_rows(obs,
                      current_obs = 1,
                      n_closest = 1,
                      derived_col_name = "der",
                      n_bins = 2))
  expected = local_rows %>% slice(1) %>% select(-offset)
  expect_equal(out, expected)

  set.seed(8)
  expect_no_warning(local_rows %>%
    sample_local_rows(obs,
                      current_obs = 1,
                      n_closest = 1,
                      derived_col_name = "der",
                      n_bins = 2))
  set.seed(8)
  out = suppressWarnings(local_rows %>%
    sample_local_rows(obs,
                      current_obs = 1,
                      n_closest = 1,
                      derived_col_name = "der",
                      n_bins = 2))
  expected = local_rows %>% slice(2) %>% select(-offset)
  expect_equal(out, expected)
})
