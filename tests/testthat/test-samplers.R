test_that("hot_deck_forecasted_covariate_sampler basic test", {
  # Setup
  cov_tsib = tibble(
    datetime = as.Date("2022-02-02"),
    sim_num = 1,
    cov_pred = 2
  ) %>%
    as_tsibble(index = datetime, key = sim_num)

  local_rows = tibble(
    observation = c(1,8,20),
    cov_obs = c(2, 3, 4),  # 4 gets filtered out b/c its lead is NA
    offset = c(-2, -1, 0)
  )
  n_closest = 1
  current_obs = 4

  local_rows = local_rows %>% lead_cov_mutator(cov_obs)

  wrapped = hot_deck_forecasted_covariate_sampler(next_target_obs_col_name = "next_target_obs",
                                                  filter_na_col_names = "next_target_obs")
  wrapped_w_covs = wrapped(cov_tsib)

  # Do three iterations: (1) the latest obs, (2) the fc, (3) beyond the fc
  # (1)
  expected1 = list(
    new_current_obs = list(2),
    forecast = 20
  )

  out = local_rows %>% wrapped_w_covs(cov_obs, current_obs, n_closest)
  expect_equal(out, expected1)

  # (2)
  expected2 = list(
    new_current_obs = list(),
    forecast = 8
  )

  out = local_rows %>% wrapped_w_covs(cov_obs, out$new_current_obs, n_closest)
  expect_equal(out, expected2)

  # (3)
  expected3 = list(
    new_current_obs = list(),
    forecast = NA
  )

  out = local_rows %>% wrapped_w_covs(cov_obs, out$new_current_obs, n_closest)
  expect_equal(out, expected3)
})


test_that("hot_deck_forecasted_covariate_sampler multi-covariates err", {
  cov_tsib = tibble(
    datetime = as.Date("2022-02-02"),
    sim_num = 1,
    cov_pred = 2,
    cov2_pred = 89
  ) %>%
    as_tsibble(index = datetime, key = sim_num)

  local_rows = tibble(
    observation = c(1,8,20),
    cov_obs = c(2, 3, 4),
    offset = c(-2, -1, 0)
  )
  n_closest = 1
  current_obs = 4

  local_rows = local_rows %>% lead_cov_mutator(cov_obs)

  wrapped = hot_deck_forecasted_covariate_sampler(next_target_obs_col_name = "next_target_obs",
                                                  filter_na_col_names = "next_target_obs")

  expect_error(wrapped(cov_tsib),
               regexp = "Multiple forecasted covariates")
})


test_that("hot_deck_covariate_lead_sampler basic test", {
  local_rows = tibble(
    observation = c(1,8,20),
    cov_obs = c(2, 3, 4)  # 4 gets filtered out b/c its lead is NA
  )
  n_closest = 1
  current_obs = 4

  local_rows = local_rows %>% lead_cov_mutator(cov_obs)

  wrapped = hot_deck_covariate_lead_sampler(next_cov_obs_col_name = "next_cov_obs",
                                            next_target_obs_col_name = "next_target_obs")
  expected = list(
    new_current_obs = 4,
    forecast = 20
  )


  out = local_rows %>% wrapped(cov_obs, current_obs, n_closest)
  expect_equal(out, expected)
})


test_that("hot_deck_covariate_lead_sampler multiple na filters test", {
  local_rows = tibble(
    observation = c(0,1,8,NA),  # b/c NA, cov_obs == 3 gets filtered out
    cov_obs = c(0, 2, 3, 4)
  )
  n_closest = 1
  current_obs = 4

  local_rows = local_rows %>% lead_cov_mutator(cov_obs)

  wrapped = hot_deck_covariate_lead_sampler(next_cov_obs_col_name = "next_cov_obs",
                                            next_target_obs_col_name = "next_target_obs",
                                            filter_na_col_names = c("next_cov_obs",
                                                                    "next_target_obs"))
  expected = list(
    new_current_obs = 3,
    forecast = 8
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

  wrapped = hot_deck_diff_sampler("diff_to_next_obs")
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
    offset = 1:10,
    current_obs = 99  # Junk col to test that .env is used.
  )
  current_obs = 4
  n_closest = 2  # Ties are included, so could take any of [3,4,5]

  wrapped = hot_deck_lead_sampler("next_obs")
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

  wrapped = hot_deck_lead_sampler("next_obs")

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
                                   filter_na_col_names = "der"),
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
                                   filter_na_col_names = "der"),
               regexp = "Sampling from entire contents")
})


test_that("sample_local_rows filters", {
  local_rows = tibble(
    obs = 1:10,
    a = rep(1, 10),
    b = c(1, NA, 3:10),
    c = NA,
    d = c(NA, 2:10),
    offset = 1:10
  )

  # No filters
  output1 = local_rows %>%
    sample_local_rows(obs,
                      current_obs = 1,
                      n_closest = 1)
  expected1 = local_rows %>%
    dplyr::slice(1) %>%
    dplyr::select(-offset)
  expect_equal(output1, expected1)

  # One filter
  output2 = local_rows %>%
    sample_local_rows(obs,
                      current_obs = 1,
                      n_closest = 1,
                      filter_na_col_names = c("d"))
  expected2 = local_rows %>%
    dplyr::slice(2) %>%
    dplyr::select(-offset)
  expect_equal(output2, expected2)

  # Two filters
  output3 = local_rows %>%
    sample_local_rows(obs,
                      current_obs = 1,
                      n_closest = 1,
                      filter_na_col_names = c("b", "d"))
  expected3 = local_rows %>%
    dplyr::slice(3) %>%
    dplyr::select(-offset)
  expect_equal(output3, expected3)

  # All, redundant filters
  expect_error(local_rows %>%
                 sample_local_rows(obs,
                                   current_obs = 2,
                                   n_closest = 2,
                                   filter_na_col_names = c("a", "b", "c", "d")),
               regexp = "No local values.*for")

})
