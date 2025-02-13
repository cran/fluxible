test_that("extracts the fit correclty", {
  expect_equal(
    flux_fit_type(slopes30qua),
    "quadratic"
  )
})


test_that("fit can be set by user", {
  expect_equal(
    flux_fit_type(slopes0, fit_type = "expo"),
    "exponential"
  )
})

test_that("fit transfer from flux_fitting", {
  slopes_test <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp"
  )
  )
  expect_equal(
    flux_fit_type(slopes_test),
    "exponential"
  )
})

test_that("fit_type can be NULL", {
  expect_equal(
    flux_fit_type(slopes60),
    NULL
  )
})
