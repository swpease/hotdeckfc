test_that("lead mutator", {
  data = tibble(
    datetime = as.Date("2022-03-31") + 0:9,
    obs = 1:10
  ) %>%
    as_tsibble(index = datetime)

  expected = tibble(
    datetime = as.Date("2022-03-31") + 0:9,
    obs = 1:10,
    next_obs = c(2:10, NA)
  ) %>%
    as_tsibble(index = datetime)

  expect_equal(data %>% lead_mutator(obs), expected)
})


test_that("cov mutator", {
  data = tibble(
    datetime = as.Date("2022-03-31") + 0:9,
    obs = 1:10,
    cov = 11:20
  ) %>%
    as_tsibble(index = datetime)

  expected = tibble(
    datetime = as.Date("2022-03-31") + 0:9,
    obs = 1:10,
    cov = 11:20,
    next_cov_obs = c(12:20, NA),
    next_target_obs = c(2:10, NA)
  ) %>%
    as_tsibble(index = datetime)

  expect_equal(data %>% lead_cov_mutator(cov, target_obs_col_name = "obs"), expected)
})


test_that("diff mutator", {
  data = tibble(
    datetime = as.Date("2022-03-31") + 0:9,
    obs = c(0,1,3,5,8,8,5,3,1,0)
  ) %>%
    as_tsibble(index = datetime)

  expected = tibble(
    datetime = as.Date("2022-03-31") + 0:9,
    obs = c(0,1,3,5,8,8,5,3,1,0),
    diff_to_next_obs = c(1,2,2,3,0,-3,-2,-2,-1,NA)
  ) %>%
    as_tsibble(index = datetime)

  expect_equal(data %>% diff_mutator(obs), expected)
})


test_that("non mutator", {
  data = tibble(
    datetime = as.Date("2022-03-31") + 0:9,
    obs = c(0,1,3,5,8,8,5,3,1,0)
  ) %>%
    as_tsibble(index = datetime)

  expect_equal(data %>% non_mutator(obs), data)
})
