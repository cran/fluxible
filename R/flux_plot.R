#' plotting fluxes for visual evaluation
#' @description plots the fluxes, fit and slope in facets
#' with color code indicating quality flags
#' This function takes time to run and is optional in the workflow,
#' but it is still highly recommended to use it to visually check
#' the measurements.
#' Note that 'flux_plot' is specific to the 'fluxible' package and
#' will work best with datasets produced following a fluxible workflow.
#' @param slopes_df dataset containing slopes,
#' with flags produced by flux_quality
#' @param conc_col column with gas concentration
#' @param datetime_col column with datetime of each data point
#' @param color_discard color for fits with a discard quality flag
#' @param color_cut color for the part of the flux that is cut
#' @param color_ok color for fits with an ok quality flag
#' @param color_zero color for fits with a zero quality flag
#' @param scale_x_datetime_args list of arguments for
#' \link[ggplot2:scale_x_datetime]{scale_x_datetime}
#' @param f_ylim_upper y axis upper limit
#' @param f_ylim_lower y axis lower limit
#' @param f_plotname filename for the extracted pdf file;
#' if empty, the name of `slopes_df` will be used
#' @param facet_wrap_args list of arguments for
#' \link[ggforce:facet_wrap_paginate]{facet_wrap_paginate}
#' @param y_text_position position of the text box
#' @param print_plot logical, if TRUE it prints the plot as a ggplot object
#' but will take time depending on the size of the dataset
#' @param output `pdfpages`, the plots are saved as A4 landscape pdf pages;
#' `ggsave`, the plots can be saved with the ggsave function;
#' `print_only` (default) prints the plot without creating a file
#' (independently from 'print_plot' being TRUE or FALSE)
#' @param ggsave_args list of arguments for \link[ggplot2:ggsave]{ggsave}
#' (in case `output = "ggsave"`)
#' @return plots of fluxes, with raw concentration data points, fit, slope,
#' and color code indicating quality flags and cuts. The plots are organized
#' in facets according to flux ID, and a text box display the quality flag and
#' diagnostics of each measurement.
#' The plots are returned as a ggplot object if `print_plot = TRUE`;
#' if `print_plot = FALSE` it will not return anything but will produce a file
#' according to the `output` argument.
#' @importFrom dplyr select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_manual
#' scale_x_datetime ylim facet_wrap labs geom_text theme_bw ggsave
#' scale_linetype_manual guides guide_legend
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom progress progress_bar
#' @examples
#' data(slopes0_flag)
#' flux_plot(slopes0_flag, conc, datetime)
#' data(slopes30lin_flag)
#' flux_plot(slopes30lin_flag, conc, datetime)
#' data(slopes30qua_flag)
#' flux_plot(slopes30qua_flag, conc, datetime)
#' @export

flux_plot <- function(slopes_df,
                      conc_col,
                      datetime_col,
                      color_discard = "#D55E00",
                      color_cut = "#D55E00",
                      color_ok = "#009E73",
                      color_zero = "#CC79A7",
                      scale_x_datetime_args = list(
                        date_breaks = "1 min",
                        minor_breaks = "10 sec",
                        date_labels = "%e/%m \n %H:%M"
                      ),
                      f_ylim_upper = 800,
                      f_ylim_lower = 400,
                      f_plotname = "",
                      facet_wrap_args = list(
                        ncol = 4,
                        nrow = 3,
                        scales = "free"
                      ),
                      y_text_position = 500,
                      print_plot = "FALSE",
                      output = "print_only",
                      ggsave_args = list()) {
  args_ok <- flux_fun_check(list(
    f_ylim_upper = f_ylim_upper,
    f_ylim_lower = f_ylim_lower,
    y_text_position = y_text_position
  ),
  fn = list(is.numeric, is.numeric, is.numeric),
  msg = rep("has to be numeric", 3))


  if (any(!args_ok))
    stop("Please correct the arguments", call. = FALSE)

  output <- match.arg(output, c("pdfpages", "ggsave", "print_only"))

  if (output == "print_only") {
    print_plot <- "TRUE"
  }

  fit_type <- flux_fit_type(
    slopes_df
  )

  if (f_plotname == "") {
    f_plotname <- deparse(substitute(slopes_df))
  }

  if (output %in% c("pdfpages", "ggsave")) {
    f_plotname <- paste("f_quality_plots/", f_plotname, sep = "")

    folder <- "./f_quality_plots"
    if (!file.exists(folder)) {
      dir.create(folder)
    }
  }

  if (
    max(slopes_df[[deparse(substitute(conc_col))]], na.rm = TRUE) > f_ylim_upper
  ) {
    message("Some concentration data points will not be displayed
    because f_ylim_upper is too low.")
  }

  if (max(slopes_df$f_fit, na.rm = TRUE) > f_ylim_upper) {
    message("Part of the fit will not be displayed
    because f_ylim_upper is too low.")
  }

  if (
    min(slopes_df[[deparse(substitute(conc_col))]], na.rm = TRUE) < f_ylim_lower
  ) {
    message("Some concentration data points will not be displayed
    because f_ylim_lower is too high.")
  }

  if (min(slopes_df$f_fit, na.rm = TRUE) < f_ylim_lower) {
    message("Part of the fit will not be displayed
    because f_ylim_lower is too high.")
  }

  flags <- slopes_df |>
    select("f_fluxid", "f_quality_flag") |>
    distinct() |>
    filter(.data$f_quality_flag == "no data") |>
    mutate(
      f_warnings = paste(
        "\n", "fluxID", .data$f_fluxid, "dropped because there is no data"
      ),
      f_warnings = as.character(.data$f_warnings)
    ) |>
    pull(.data$f_warnings)

  f_warnings <- stringr::str_c(flags)


  if (any(!is.na(f_warnings))) message(f_warnings)

  slopes_df <- slopes_df |>
    filter(
      .data$f_quality_flag != "no data"
    )




  if (fit_type == "exponential") {
    f_plot <- flux_plot_exp(
      slopes_df,
      {{conc_col}},
      {{datetime_col}},
      y_text_position = y_text_position
    )
  }


  if (fit_type == "linear") {
    f_plot <- flux_plot_lin(
      slopes_df,
      {{conc_col}},
      {{datetime_col}},
      y_text_position = y_text_position
    )
  }

  if (fit_type == "quadratic") {
    f_plot <- flux_plot_quadratic(
      slopes_df,
      {{conc_col}},
      {{datetime_col}},
      y_text_position = y_text_position
    )
  }

  message("Plotting in progress")

  f_plot <- f_plot +
    geom_line(
      aes(y = .data$f_fit, linetype = .data$linetype),
      linewidth = 0.3,
      na.rm = TRUE,
      show.legend = TRUE
    ) +
    scale_color_manual(values = c(
      "cut" = color_cut,
      "ok" = color_ok,
      "discard" = color_discard,
      "zero" = color_zero,
      "start_error" = color_discard,
      "force_discard" = color_discard,
      "force_ok" = color_ok
    )) +
    scale_linetype_manual(values = c(
      "f_fit" = "longdash",
      "f_fit_slope" = "dashed"
    )) +
    do.call(scale_x_datetime, args = scale_x_datetime_args) +
    ylim(f_ylim_lower, f_ylim_upper) +
    do.call(facet_wrap_paginate, # do.call is here to pass arguments as a list
      args = c(facets = ~f_fluxid, facet_wrap_args)
    ) +
    labs(
      title = "Fluxes quality assessment",
      subtitle = paste(fit_type, "model"),
      x = "Datetime",
      y = "Concentration",
      colour = "Quality flags",
      linetype = "Fits"
    ) +
    guides(color = guide_legend(override.aes = list(linetype = 0)))

  if (output == "print_only") {
    return(f_plot)
  }

  if (output == "pdfpages") {
    f_plotname <- paste(f_plotname, ".pdf", sep = "")
    pdf(f_plotname, paper = "a4r", width = 11.7,
        height = 8.3, title = f_plotname)
    pb <- progress_bar$new(
      format =
        "Printing plots in pdf document [:bar] :current/:total (:percent)",
      total = n_pages(f_plot)
    )
    pb$tick(0)
    Sys.sleep(3)
    for (i in 1:n_pages(f_plot)) {
      pb$tick()
      Sys.sleep(0.1)
      print(f_plot +
        do.call(facet_wrap_paginate,
          args = c(
            facets = ~f_fluxid,
            page = i,
            facet_wrap_args
          )
        ))
    }
    quietly(dev.off())
    message("Plots saved in f_quality_plots folder.")
    if (print_plot == TRUE) {
      return(f_plot)
    }
  }

  if (output == "ggsave") {
    message("Saving plots with ggsave.")
    do.call(
      ggsave,
      args = c(filename = f_plotname, ggsave_args)
    )

    message("Plots saved in f_quality_plots folder.")
    if (print_plot == TRUE) {
      return(f_plot)
    }
  }
}
