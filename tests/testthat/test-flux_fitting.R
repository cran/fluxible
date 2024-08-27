test_that("works for exponential fitting", {
  expect_snapshot(
    flux_fitting(co2_conc, fit_type = "expo")
  )
})

test_that("works for linear fitting", {
  expect_snapshot(
    flux_fitting(co2_conc, fit_type = "lin")
  )
})

test_that("works for exponential fitting with cut", {
  expect_snapshot(
    flux_fitting(co2_conc, fit_type = "expo", start_cut = 20)
  )
})

test_that("works for linear fitting with cut", {
  expect_snapshot(
    flux_fitting(co2_conc, fit_type = "line", start_cut = 20)
  )
})
