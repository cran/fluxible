#' plotting fluxes for visual evaluation
#' @description plots the fluxes, fit and slope in facets
#' with color code indicating quality flags
#' This function takes time to run and is optional in the workflow,
#' but it is still highly recommended to use it to visually check
#' the measurements.
#' @param slopes_df dataset containing slopes,
#' with flags produced by flux_quality
#' @param color_discard color for fits with a discard quality flag
#' @param color_cut color for the part of the flux that is cut
#' @param color_ok color for fits with an ok quality flag
#' @param color_zero color for fits with a zero quality flag
#' @param f_date_breaks date_breaks argument for scale_x_datetime
#' @param f_minor_breaks minor breaks argument for scale_x_datetime
#' @param f_date_labels date_labels argument for scale_x_datetime
#' @param f_ylim_upper y axis upper limit
#' @param f_ylim_lower y axis lower limit
#' @param f_plotname filename for the extracted pdf file
#' @param facet_wrap_args list of arguments for
#' \link[ggforce:facet_wrap_paginate]{facet_wrap_paginate}
#' @param y_text_position position of the text box
#' @param print_plot FALSE or TRUE, if TRUE it prints the plot in R
#' but will take time depending on the size of the dataset
#' @param output "pdfpages", the plots are saved as A4 landscape pdf pages;
#' "ggsave", the plots can be saved with the ggsave function;
#' "print_only" (default) prints the plot without creating a file
#' (independently from 'print_plot' being TRUE or FALSE)
#' @param ggsave_args list of arguments for \link[ggplot2:ggsave]{ggsave}
#' (in case `output = "ggsave"`)
#' @param no_data_flag flag marking fluxID without data in f_quality_flag
#' @param cut_arg argument pointing rows to be cut from the measurements
#' @return a ggplot object if `print_plot = TRUE`,
#' if `print_plot = FALSE` it will not return anything but will produce a file
#' depending on `output`
#' @importFrom dplyr rename select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_manual
#' scale_x_datetime ylim facet_wrap labs geom_text theme_bw ggsave
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom progress progress_bar
#' @examples
#' data(slopes0_flag)
#' flux_plot(slopes0_flag, output = "print_only")
#' data(slopes30lin_flag)
#' flux_plot(slopes30lin_flag, output = "print_only")
#' flux_plot(slopes30qua_flag, output = "print_only")
#' @export

flux_plot <- function(slopes_df,
                      color_discard = "#D55E00",
                      color_cut = "#D55E00",
                      color_ok = "#009E73",
                      color_zero = "#CC79A7",
                      f_date_breaks = "1 min",
                      f_minor_breaks = "10 sec",
                      f_date_labels = "%e/%m \n %H:%M",
                      f_ylim_upper = 800,
                      f_ylim_lower = 400,
                      f_plotname = "plot_quality",
                      facet_wrap_args = list(
                        ncol = 4,
                        nrow = 3,
                        scales = "free"
                      ),
                      y_text_position = 500,
                      print_plot = "FALSE",
                      output = "print_only",
                      ggsave_args = list(),
                      cut_arg = "cut",
                      no_data_flag = "no_data") {
  output <- match.arg(((output)), c("pdfpages", "ggsave", "print_only"))

  fit_type <- flux_fit_type(
    slopes_df
  )


  if (((output)) %in% c("pdfpages", "ggsave")) {
    f_plotname <- paste("f_quality_plots/", f_plotname, sep = "")

    folder <- "./f_quality_plots"
    if (!file.exists(folder)) {
      dir.create(folder)
    }
  }

  if (max(slopes_df$f_conc, na.rm = TRUE) > ((f_ylim_upper))) {
    message("Some concentration data points will not be displayed
    because f_ylim_upper is too low.")
  }

  if (max(slopes_df$f_fit, na.rm = TRUE) > ((f_ylim_upper))) {
    message("Part of the fit will not be displayed
    because f_ylim_upper is too low.")
  }

  if (min(slopes_df$f_conc, na.rm = TRUE) < ((f_ylim_lower))) {
    message("Some concentration data points will not be displayed
    because f_ylim_lower is too high.")
  }

  if (min(slopes_df$f_fit, na.rm = TRUE) < ((f_ylim_lower))) {
    message("Part of the fit will not be displayed
    because f_ylim_lower is too high.")
  }

  flags <- slopes_df |>
    select("f_fluxID", "f_quality_flag") |>
    filter(.data$f_quality_flag == ((no_data_flag))) |>
    mutate(
      f_warnings = paste(
        "\n", "fluxID", .data$f_fluxID, "dropped because there is no data"
      ),
      f_warnings = as.character(.data$f_warnings)
    ) |>
    pull(.data$f_warnings)

  f_warnings <- stringr::str_c(flags)


  if (any(!is.na(f_warnings))) message(f_warnings)

  slopes_df <- slopes_df |>
    filter(
      .data$f_quality_flag != ((no_data_flag))
    )




  if (((fit_type)) == "exponential") {
    f_plot <- flux_plot_exp(
      ((slopes_df)),
      cut_arg = ((cut_arg)),
      y_text_position = ((y_text_position))
    )
  }


  if (((fit_type)) == "linear") {
    f_plot <- flux_plot_lin(
      ((slopes_df)),
      cut_arg = ((cut_arg)),
      y_text_position = ((y_text_position))
    )
  }

  if (((fit_type)) == "quadratic") {
    f_plot <- flux_plot_quadratic(
      ((slopes_df)),
      cut_arg = ((cut_arg)),
      y_text_position = ((y_text_position))
    )
  }

  message("Plotting in progress")

  f_plot <- f_plot +
    scale_color_manual(values = c(
      "cut" = ((color_cut)),
      "ok" = ((color_ok)),
      "discard" = ((color_discard)),
      "zero" = ((color_zero)),
      "start_error" = ((color_discard)),
      "weird_flux" = ((color_discard)),
      "force_ok" = ((color_ok))
    )) +
    scale_x_datetime(
      date_breaks = ((f_date_breaks)), minor_breaks = ((f_minor_breaks)),
      date_labels = ((f_date_labels))
    ) +
    ylim(((f_ylim_lower)), ((f_ylim_upper))) +
    do.call(facet_wrap_paginate,
      args = c(facets = ~f_fluxID, ((facet_wrap_args)))
    ) +
    labs(
      title = "Fluxes quality assessment",
      x = "Datetime",
      y = "Concentration",
      colour = "Quality flags"
    )

  if (((output)) == "print_only") {
    return(f_plot)
  }

  if (((output)) == "pdfpages") {
    f_plotname <- paste(f_plotname, ".pdf", sep = "")
    pdf(((f_plotname)), paper = "a4r", width = 11.7, height = 8.3)
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
            facets = ~f_fluxID,
            page = i,
            ((facet_wrap_args))
          )
        ))
    }
    quietly(dev.off())
    message("Plots saved in f_quality_plots folder.")
    if (((print_plot)) == TRUE) {
      return(f_plot)
    }
  }

  if (((output)) == "ggsave") {
    message("Saving plots with ggsave.")
    do.call(
      ggsave,
      args = c(filename = ((f_plotname)), ((ggsave_args)))
    )

    message("Plots saved in f_quality_plots folder.")
    if (((print_plot)) == TRUE) {
      return(f_plot)
    }
  }
}
