test_that("imputation plots snapshots", {
  set.seed(3)
  imputed = hot_deck_impute(SUGG_temp %>% as_tsibble(), observation)

  vdiffr::expect_doppelganger(
    "plot_imputation",
    plot_imputation(imputed, observation)
  )
  vdiffr::expect_doppelganger(
    "plot_imputation_gg_season",
    plot_imputation_gg_season(imputed, observation)
  )
  vdiffr::expect_doppelganger(
    "plot_imputation_seasonal_facet",
    plot_imputation_seasonal_facet(imputed, observation)
  )
  vdiffr::expect_doppelganger(
    "plot_imputation_seasonal_separate",
    plot_imputation_seasonal_separate(imputed, observation)
  )
})
