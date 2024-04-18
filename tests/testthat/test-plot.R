suppressWarnings(shiny::testServer(
  app = shiny_visualize_forecast(append_lead(SUGG_temp, observation),
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
    #  - change a param (now have a new fc w/ new params)
    #  - change back to orig params
    #    - should fc same as initial fc
    #  - recalculate
    #    - should equal fc from first item in this list
    session$setInputs(recalculate = 1)
    fc1b = forecast()
    session$setInputs(times = 3)
    fc2a = forecast()
    session$setInputs(times = 5)
    fc1d = forecast()
    expect_equal(fc1a1, fc1d)
    session$setInputs(recalculate = 1)
    fc1e = forecast()
    expect_equal(fc1b, fc1e)
}))

test_that("plot_forecast snapshot", {
  data = append_lead(hotdeckfc::SUGG_temp, observation)
  set.seed(3)
  fc = hot_deck_forecast(data,
                         date,
                         observation,
                         times = 3,
                         h = 10,
                         window_back = 20,
                         window_fwd = 20,
                         n_closest = 5)
  suppressWarnings(vdiffr::expect_doppelganger("plot_forecast", plot_forecast(fc, data)))
})
