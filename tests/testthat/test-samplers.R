test_that("basic_hot_deck_sampler basic test", {
  local_rows = tibble(
    obs = 1:10,
    next_obs = dplyr::lead(obs)
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
    next_obs = rep(NA, 10)
  )
  current_obs = 4
  n_closest = 2

  wrapped = basic_hot_deck_sampler("next_obs")

  expect_error(local_rows %>% wrapped(obs, current_obs, n_closest),
               regexp = "No local values")
})
