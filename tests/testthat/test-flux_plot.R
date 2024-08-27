test_that("plot for exponential fit", {
  expect_snapshot(
    suppressMessages( # because the progress bar is messing with check()
      flux_plot(slopes0_flag,
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
  expect_snapshot(
    vdiffr::expect_doppelganger(
      "plot for linear fit",
      flux_plot(slopes30lin_flag)
    )
  )
})

test_that("plot for linear fit with jpg extension works", {
  expect_snapshot(
    suppressMessages( # because the progress bar is messing with check()
      flux_plot(slopes30lin_flag,
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
  plot_object <- flux_plot(slopes30lin_flag)
  vdiffr::expect_doppelganger("plot as an object", plot_object)
})
