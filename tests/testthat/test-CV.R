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
    )
  ) %>%
    as_tsibble(index = datetime)

  expected_2023 = tibble(
    datetime = c(as.Date("2023-04-04") + 0:1,
                 as.Date("2023-04-04") + 0:1),
    forecast = c(1, 2, 1, 2),
    simulation_num = c(1, 1, 2, 2),
    k = 1
  )
  expected_2022 = tibble(
    datetime = c(as.Date("2022-04-04") + 0:1,
                 as.Date("2022-04-04") + 0:1),
    forecast = 3,
    simulation_num = c(1, 1, 2, 2),
    k = 2
  )
  # if change to tsibble, reverse these args
  expected = bind_rows(expected_2023, expected_2022)

  out = data %>%
    cv_hot_deck_forecast(
      datetime,
      obs,
      offset = 0,
      times = 2,
      h = 2,
      window_back = 2,
      window_fwd = 2,
      n_closest = 1
    )

  expect_equal(out, expected)
})

test_that("subtract a year", {
  d1 = as.Date("2024-03-01")
  d2 = as.Date("2024-02-28")
  d3 = as.Date("2023-03-01")
  expect_equal(subtract_year(d1), as.Date("2023-03-01"))
  expect_equal(subtract_year(d2), as.Date("2023-02-27"))
  expect_equal(subtract_year(d3), as.Date("2022-03-01"))
})
