#' plotting fluxes with a quadratic fit
#' @description specific part of flux_plot for
#' quadratic fit
#' @param slopes_df dataset containing slopes
#' @param y_text_position position of the text box
#' @param cut_arg argument pointing rows to be cut from the measurements
#' @importFrom dplyr rename select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off



flux_plot_quadratic <- function(slopes_df,
                                y_text_position = 500,
                                cut_arg = "cut") {
  plot_quadratic <- slopes_df |>
    flux_plot_lin(
      y_text_position = ((y_text_position)),
      cut_arg = ((cut_arg))
    )

  plot_quadratic <- plot_quadratic +
    geom_line(
      aes(y = .data$f_fit_slope, color = .data$f_quality_flag),
      linetype = "dashed",
      na.rm = TRUE
    )

  plot_quadratic
}
