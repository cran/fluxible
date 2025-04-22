test_that("quality flags count works", {
  slopes30lin_flag <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "linear",
    end_cut = 30
  )) |>
    flux_quality(
      conc
    )
  expect_snapshot(
    flux_flag_count(slopes30lin_flag)
  )
})
