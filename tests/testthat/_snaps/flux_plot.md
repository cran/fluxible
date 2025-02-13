# plot for exponential fit

    Code
      suppressMessages(flux_plot(slopes0_flag, conc, datetime, f_plotname = "test_exp_plot",
        print_plot = FALSE, output = "pdfpages"))

# plot for linear fit

    Code
      vdiffr::expect_doppelganger("plot for linear fit", flux_plot(slopes30lin_flag,
        conc, datetime))
    Message
      Plotting in progress

# plot for linear fit with jpg extension works

    Code
      suppressMessages(flux_plot(slopes30lin_flag, conc, datetime, f_plotname = "test_lin_plot",
        print_plot = FALSE, output = "ggsave", ggsave_args = list(device = "jpg")))

