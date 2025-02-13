test_that("works for exponential fitting", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "expo"
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for linear fitting", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "lin"
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for quadratic fitting", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "qua"
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for exponential fitting with cut", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "expo",
      start_cut = 20
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("works for linear fitting with cut", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "line",
      start_cut = 20
    ) |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("removing duplicated datetime", {
  rep <- co2_conc[rep(1, 200), ] |>
    mutate(
      conc = runif(n = 200, min = 420, max = 460)
    )

  rep_data <- rbind(co2_conc, rep) |>
    arrange(datetime)

  expect_snapshot(
    flux_fitting(
      rep_data,
      conc,
      datetime,
      fit_type = "exp"
    )
  )
})

test_that("correct flux with duplicated datetime", {
  rep <- co2_conc[rep(1, 200), ] |>
    mutate(
      conc = runif(n = 200, min = 420, max = 460)
    )

  rep_data <- rbind(co2_conc, rep) |>
    arrange(datetime)

  qflux_fitting <- purrr::quietly(flux_fitting)
  expect_equal(
    qflux_fitting(
      rep_data,
      conc,
      datetime,
      fit_type = "exp"
    ),
    qflux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "exp"
    )
  )
})
