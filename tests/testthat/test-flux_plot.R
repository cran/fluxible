test_that("plot for exponential fit", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  ))
  slopes0_flag <- suppressMessages(flux_quality(
    slopes0,
    conc
  ))

  expect_snapshot(
    suppressMessages( # because the progress bar is messing with check()
      flux_plot(slopes0_flag,
        conc,
        datetime,
        f_plotname = "test_exp_plot",
        print_plot = FALSE,
        output = "pdfpages"
      )
    )
  )
  # the plots are quite heavy so we do not keep them
  unlink("f_quality_plots/", recursive = TRUE, force = TRUE)
})

test_that("plot for linear fit", {
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
    vdiffr::expect_doppelganger(
      "plot for linear fit",
      flux_plot(slopes30lin_flag, conc, datetime)
    )
  )
})

test_that("plot for linear fit with jpg extension works", {
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
    suppressMessages( # because the progress bar is messing with check()
      flux_plot(slopes30lin_flag,
        conc,
        datetime,
        f_plotname = "test_lin_plot",
        print_plot = FALSE,
        output = "ggsave",
        ggsave_args = list(device = "jpg")
      )
    )
  )
  # the plots are quite heavy so we do not keep them
  unlink("f_quality_plots/", recursive = TRUE, force = TRUE)
})

test_that("plot can be exported as an object", {
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

  plot_object <- flux_plot(slopes30lin_flag, conc, datetime)
  vdiffr::expect_doppelganger("plot as an object", plot_object)
})

test_that("plot for exp_tz fit", {
  expect_snapshot(
    vdiffr::expect_doppelganger(
      "plot for exp_tz fit",
      flux_fitting(
        co2_conc,
        conc,
        datetime,
        fit_type = "exp_tz",
        end_cut = 60,
        t_zero = 20
      ) |>
        flux_quality(conc) |>
        flux_plot(conc, datetime)
    )
  )
})


test_that("plot for exp_tz fit with mid missing data", {
  expect_snapshot(
    vdiffr::expect_doppelganger(
      "plot for exp_tz fit with mid missing data",
      flux_fitting(
        co2_conc_mid_missing,
        conc,
        datetime,
        fit_type = "exp_tz",
        end_cut = 60,
        t_zero = 20
      ) |>
        flux_quality(conc) |>
        flux_plot(conc, datetime)
    )
  )
})

test_that("plot for exp_zhao18 fit with mid missing data", {
  expect_snapshot(
    vdiffr::expect_doppelganger(
      "plot for exp_zhao18 fit with mid missing data",
      flux_fitting(
        co2_conc_mid_missing,
        conc,
        datetime,
        fit_type = "exp_zhao18",
        end_cut = 60,
        t_zero = 20
      ) |>
        flux_quality(conc) |>
        flux_plot(conc, datetime)
    )
  )
})

test_that("plot for quadratic fit with mid missing data", {
  expect_snapshot(
    vdiffr::expect_doppelganger(
      "plot for quadratic fit with mid missing data",
      flux_fitting(
        co2_conc_mid_missing,
        conc,
        datetime,
        fit_type = "quadratic",
        end_cut = 60,
        t_zero = 20
      ) |>
        flux_quality(conc) |>
        flux_plot(conc, datetime)
    )
  )
})

test_that("plot for exp_hm fit", {
  expect_snapshot(
    vdiffr::expect_doppelganger(
      "plot for exp_hm fit",
      flux_fitting(
        co2_conc,
        conc,
        datetime,
        fit_type = "exp_hm",
        end_cut = 60,
        t_zero = 20
      ) |>
        flux_quality(conc) |>
        flux_plot(conc, datetime)
    )
  )
})

test_that("plot for kappamax fit", {
  expect_snapshot(
    vdiffr::expect_doppelganger(
      "plot for kappamax fit",
      flux_fitting(
        co2_conc,
        conc,
        datetime,
        fit_type = "exp_hm",
        end_cut = 30,
        t_zero = 10
      ) |>
        flux_quality(
          conc,
          f_pvalue = f_pvalue_lm,
          f_rsquared = f_rsquared_lm,
          kappamax = TRUE
        ) |>
        flux_plot(conc, datetime)
    )
  )
})
