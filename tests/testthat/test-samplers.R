test_that("hot_deck_covariate_lead_sampler basic test", {
  local_rows = tibble(
    observation = c(1,8,20),
    cov_obs = c(2, 3, 4)
  )
  n_closest = 1
  current_obs = 4

  local_rows = local_rows %>% lead_cov_mutator(cov_obs)

  wrapped = hot_deck_covariate_lead_sampler(next_cov_obs_col_name = "next_cov_obs",
                                            next_target_obs_col_name = "next_target_obs",
                                            n_bins = 0)
  expected = list(
    new_current_obs = 4,
    forecast = 20
  )


  out = local_rows %>% wrapped(cov_obs, current_obs, n_closest)
  expect_equal(out, expected)
})


test_that("hot_deck_diff_sampler basic test", {
  local_rows = tibble(
    obs = c(1,8,20),
    diff_to_next_obs = dplyr::lead(tsibble::difference(obs)),
    offset = 1:3
  )
  n_closest = 1
  current_obs_1 = 4
  current_obs_2 = 20

  wrapped = hot_deck_diff_sampler("diff_to_next_obs", n_bins = 0)
  expected_1 = list(
    new_current_obs = 11,
    forecast = 11
  )
  expected_2 = list(
    new_current_obs = 32,
    forecast = 32
  )


  out1 = local_rows %>% wrapped(obs, current_obs_1, n_closest)
  expect_equal(out1, expected_1)

  out2 = local_rows %>% wrapped(obs, current_obs_2, n_closest)
  expect_equal(out2, expected_2)
})


test_that("hot_deck_lead_sampler basic test", {
  local_rows = tibble(
    obs = 1:10,
    next_obs = dplyr::lead(obs),
    offset = 1:10
  )
  current_obs = 4
  n_closest = 2

  wrapped = hot_deck_lead_sampler("next_obs", n_bins = 0)
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


test_that("hot_deck_lead_sampler no local values", {
  local_rows = tibble(
    obs = 1:10,
    next_obs = rep(NA, 10),
    offset = 1:10
  )
  current_obs = 4
  n_closest = 2

  wrapped = hot_deck_lead_sampler("next_obs", n_bins = 0)

  expect_error(local_rows %>% wrapped(obs, current_obs, n_closest),
               regexp = "No local values.*for")
})


test_that("sample_local_rows no local der vals error", {
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
})


test_that("sample_local_rows warning entire contents", {
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
               regexp = "Sampling from entire contents")
})
