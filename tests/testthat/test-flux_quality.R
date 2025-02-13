test_that("works for exponential fitting", {
  expect_snapshot(
    flux_quality(slopes0,
      conc,
      fit_type = "expo"
    ) |>
      dplyr::select(f_fluxid, f_quality_flag, f_RMSE, f_cor_coef, f_ratio) |>
      dplyr::distinct()
  )
})

test_that("works for linear fitting", {
  expect_snapshot(
    flux_quality(slopes30lin,
      conc,
      fit_type = "li"
    ) |>
      dplyr::select(f_fluxid, f_quality_flag, f_pvalue, f_rsquared) |>
      dplyr::distinct()
  )
})

test_that("works for quadratic fitting", {
  expect_snapshot(
    flux_quality(slopes30qua,
      conc,
      fit_type = "quadratic"
    ) |>
      dplyr::select(f_fluxid, f_quality_flag, f_pvalue, f_rsquared) |>
      dplyr::distinct()
  )
})
