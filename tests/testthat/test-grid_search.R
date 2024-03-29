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
      sm_name = "name",
      sampler = hot_deck_lead_sampler("next_obs"),
      mutator = lead_mutator
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
    sm_name = "name",
    sampler = hot_deck_lead_sampler("next_obs"),
    mutator = lead_mutator
  )
  expected_passed_args_list_1 = list(
    times = 30,
    h = 5,
    n_closest = 20,
    window_back = 1:5,
    window_fwd = 1:5,
    sampler = hot_deck_lead_sampler("next_obs"),
    mutator = lead_mutator
  )

  expected_args_list_2 = list(
    times = 30,
    h = 5,
    n_closest = 20,
    window_back = 1,
    window_fwd = 2,
    sm_name = "name",
    sampler = hot_deck_lead_sampler("next_obs"),
    mutator = lead_mutator
  )
  expected_passed_args_list_2 = list(
    times = 30,
    h = 5,
    n_closest = 20,
    window_back = 1,
    window_fwd = 2,
    sampler = hot_deck_lead_sampler("next_obs"),
    mutator = lead_mutator
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
#       sm_name = "a",
#       sampler = hot_deck_lead_sampler("hi"),
#       mutator = lead_mutator
#     )
#   )
#   expect_equal(
#     build_sampler_args("a",
#                        hot_deck_lead_sampler("hi"),
#                        lead_mutator),
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
        sm_name = "sm_name",
        sampler = hot_deck_lead_sampler("as"),
        mutator = lead_mutator
      ),
      list(
        sm_name = "sm_name",
        sampler = hot_deck_lead_sampler("as"),
        mutator = lead_mutator
      )
    )
  )

  out = build_grid(
    times = 30,
    h = 4,
    n_closest = 20,
    window_args = list(build_window_args(20), build_window_args(1:4, 11)),
    sampler_args = list(build_sampler_args(
      sm_name = "sm_name",
      sampler = hot_deck_lead_sampler("as"),
      mutator = lead_mutator
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
