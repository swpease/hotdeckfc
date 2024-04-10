test_that("grid search gridding", {
  # This just returns the args passed to the fn.
  local_mocked_bindings(
    cv_hot_deck_forecast = function(...) {rlang::list2(...)}
  )

  data = tibble(
    datetime = as.Date("2021-04-02"),
    obs = 10
  ) %>%
    as_tsibble(index = datetime)

  ## basic test
  grid = build_grid(
    h = 5,
    window_args = list(
      build_window_args(window_back = 1:5),
      build_window_args(window_back = 1, window_fwd = 2)
    ),
    sampler_args = list(build_sampler_args(
      sa_name = "name",
      sampler = sample_lead("next_obs"),
      appender = append_lead,
      cov_fc_getter = NULL
    ))
  )

  out = data %>%
    grid_search_hot_deck_cv(
      datetime,
      obs,
      grid = grid,
      echo = FALSE
    )

  expected_args_list_1 = list(
    times = 30,
    h = 5,
    n_closest = 20,
    window_back = 1:5,
    window_fwd = 1:5,
    sa_name = "name",
    sampler = sample_lead("next_obs"),
    appender = append_lead,
    cov_fc_getter = NULL
  )
  expected_passed_args_list_1 = list(
    times = 30,
    h = 5,
    n_closest = 20,
    window_back = 1:5,
    window_fwd = 1:5,
    sampler = sample_lead("next_obs"),
    appender = append_lead,
    cov_fc_getter = NULL
  )

  expected_args_list_2 = list(
    times = 30,
    h = 5,
    n_closest = 20,
    window_back = 1,
    window_fwd = 2,
    sa_name = "name",
    sampler = sample_lead("next_obs"),
    appender = append_lead,
    cov_fc_getter = NULL
  )
  expected_passed_args_list_2 = list(
    times = 30,
    h = 5,
    n_closest = 20,
    window_back = 1,
    window_fwd = 2,
    sampler = sample_lead("next_obs"),
    appender = append_lead,
    cov_fc_getter = NULL
  )

  expect_equal(out[[1]]$arg_list, expected_args_list_1)
  expect_equal(tail(out[[1]]$cv_out, -3), expected_passed_args_list_1)
  expect_equal(out[[1]]$cv_out[[1]], data)
  expect_equal(out[[2]]$arg_list, expected_args_list_2)
  expect_equal(tail(out[[2]]$cv_out, -3), expected_passed_args_list_2)
  expect_equal(out[[2]]$cv_out[[1]], data)
  # expect_equal(out[[1]]$cv_out[[1]], data)
  # expect_equal(out[[2]]$arg_list, expected_out_2)
  # expect_equal(out[[2]]$cv_out[4:5], expected_out_2)
  # expect_equal(out[[2]]$cv_out[[1]], data)
})


# test_that("grid search sampler_arg fn", {
#   expected = list(
#     list(
#       sa_name = "a",
#       sampler = sample_lead("hi"),
#       appender = append_lead
#     )
#   )
#   expect_equal(
#     build_sampler_args("a",
#                        sample_lead("hi"),
#                        append_lead),
#     expected
#   )
# })
#
#
# test_that("grid search build_window_args fn", {
#   expected_symm = list(
#     window_back = 3,
#     window_fwd = 3
#   )
#   expected_diff = list(
#     window_back = 3,
#     window_fwd = 4
#   )
#   expect_equal(build_window_args(window_back = 3), expected_symm)
#   expect_equal(build_window_args(window_back = 3,
#                                  window_fwd = 4), expected_diff)
# })


test_that("build grid", {
  dummy_fn <- function(mdt) 3

  expected = tibble(
    times = c(30, 30),
    h = c(4, 4),
    n_closest = c(20, 20),
    window_args = list(
      list(window_back = 20, window_fwd = 20),
      list(window_back = 1:4, window_fwd = 11)
    ),
    sampler_args = list(
      list(
        sa_name = "sa_name",
        sampler = sample_lead("as"),
        appender = append_lead,
        cov_fc_getter = dummy_fn
      ),
      list(
        sa_name = "sa_name",
        sampler = sample_lead("as"),
        appender = append_lead,
        cov_fc_getter = dummy_fn
      )
    )
  )

  out = build_grid(
    times = 30,
    h = 4,
    n_closest = 20,
    window_args = list(build_window_args(20), build_window_args(1:4, 11)),
    sampler_args = list(build_sampler_args(
      sa_name = "sa_name",
      sampler = sample_lead("as"),
      appender = append_lead,
      cov_fc_getter = dummy_fn
    ))
  )

  expect_equal(out, expected)
})


test_that("build grid len error", {
  expect_error(build_grid(times = 30,
                          h = 4,
                          n_closest = list(c(1:3))),
               regexp = "n_closest has elements that are the wrong length")
})
