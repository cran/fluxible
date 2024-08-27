test_that("quadratic fit works", {
  expect_snapshot(
    flux_fitting(co2_conc, fit_type = "quadratic", t_zero = 10, end_cut = 30)
  )
})
