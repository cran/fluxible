test_that("extracts the fit correclty", {
  slopes30qua <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "quadratic",
    end_cut = 30
  ))

  expect_equal(
    flux_fit_type(slopes30qua),
    "quadratic"
  )
})



test_that("fit transfer from flux_fitting", {
  slopes_test <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exponential"
  )
  )
  expect_equal(
    flux_fit_type(slopes_test),
    "exp_zhao18"
  )
})
