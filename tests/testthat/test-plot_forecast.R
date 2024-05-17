suppressWarnings(shiny::testServer(
  app = shiny_visualize_forecast(append_lead(hotdeckts::SUGG_temp %>% as_tsibble(),
                                             observation),
                                 date,
                                 observation),
  expr = {
    session$setInputs(sampler = "lead",
                      times = 5,
                      h = 5,
                      window_back = 10,
                      window_fwd = 10,
                      n_closest = 5,
                      fc_geom_type = "line")
    # TEST 1: changing plot geom
    # initial fc
    fc1a1 = suppressWarnings(forecast())
    # changing plot graphics shouldn't change fc
    session$setInputs(fc_geom_type = "point")
    fc1a2 = forecast()
    expect_equal(fc1a1, fc1a2)

    # TEST 2:
    #  - click "recalculate" (now have 2 fc's w/ these params)
    #  - change a param and "calculate" (now have a new fc w/ new params)
    #  - change back to orig params
    #    - should fc same as initial fc
    #  - recalculate
    #    - should equal fc from first item in this list
    session$setInputs(recalculate = 1)
    fc1b = forecast()
    session$setInputs(times = 3, calculate = 1)
    fc2a = forecast()
    session$setInputs(times = 5, calculate = 2)
    fc1d = forecast()
    expect_equal(fc1a1, fc1d)
    session$setInputs(recalculate = 1)
    fc1e = forecast()
    expect_equal(fc1b, fc1e)
}))


test_that("plot_forecast snapshot", {
  data = append_lead(hotdeckts::SUGG_temp %>% as_tsibble(), observation)
  set.seed(3)
  fc = hot_deck_forecast(data,
                         observation,
                         times = 3,
                         h = 10,
                         window_back = 20,
                         window_fwd = 20,
                         n_closest = 5)
  suppressWarnings(vdiffr::expect_doppelganger("plot_forecast", plot_forecast(fc, data)))
})


# My testing strategy here is kinda annoying.
# vdiffr only snapshots the last plot shown.
# So, I'm extracting plot 1 on its own,
# then running the whole shebang to get plot 2.
# This way, I snapshot both, and make sure that the mapping works.
test_that("plot_grid_search_crps snapshot", {
  grid = build_grid(
    h = 2,
    n_closest = c(5, 10),
    # even if you only use a single `build_window_args()`, wrap in `list()`
    window_args = list(build_window_args(20)),
    # also need to wrap in list
    sampler_args = list(
      build_sampler_args(sa_name = "nxt",
                         sampler = sample_lead(),  # remember to call!
                         appender = append_lead)
    )
  )
  set.seed(3)
  out = grid_search_hot_deck_cv(hotdeckts::SUGG_temp,
                                .observation = observation,
                                grid = grid)
  fist_el = list(out[[1]])
  vdiffr::expect_doppelganger("plot_grid_search_crps_1",
                              suppressWarnings(plot_grid_search_crps(fist_el, "observation", 5)))
  vdiffr::expect_doppelganger("plot_grid_search_crps_2",
                              suppressWarnings(plot_grid_search_crps(out, "observation", 5)))
})


# Same strategy as above, except this is the last plot for...
# the last two plots, b/c I'm slicing the output list prior to
# all that purrr rearrangement shenanigans
test_that("plot_grid_search_forecasts snapshot", {
  grid = build_grid(
    h = 2,
    n_closest = c(5, 10),
    # even if you only use a single `build_window_args()`, wrap in `list()`
    window_args = list(build_window_args(20)),
    # also need to wrap in list
    sampler_args = list(
      build_sampler_args(sa_name = "nxt",
                         sampler = sample_lead(),  # remember to call!
                         appender = append_lead)
    )
  )
  set.seed(3)
  out = grid_search_hot_deck_cv(hotdeckts::SUGG_temp,
                                .observation = observation,
                                grid = grid)
  fist_el = list(out[[1]])
  suppressWarnings(vdiffr::expect_doppelganger(
    "plot_grid_search_forecasts_1",
    suppressWarnings(plot_grid_search_forecasts(fist_el, hotdeckts::SUGG_temp))
  ))
  suppressWarnings(vdiffr::expect_doppelganger(
    "plot_grid_search_forecasts_2",
    suppressWarnings(plot_grid_search_forecasts(out, hotdeckts::SUGG_temp))
  ))
})
