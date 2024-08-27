test_that("works for exponential fitting", {
  expect_snapshot(
    flux_quality(slopes0,
      fit_type = "expo",
      slope_col = "f_slope"
    )
  )
})

test_that("works for linear fitting", {
  expect_snapshot(
    flux_quality(slopes0lin,
      fit_type = "li"
    )
  )
})

test_that("works for quadratic fitting", {
  expect_snapshot(
    flux_quality(slopes30qua,
      fit_type = "quadratic"
    )
  )
})
