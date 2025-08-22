#' plotting fluxes with a quadratic fit
#' @description specific part of flux_plot for
#' quadratic fit
#' @param slopes_df dataset containing slopes
#' @param f_conc column with gas concentration
#' @param f_datetime column with datetime of each data point
#' @param y_text_position position of the text box
#' @importFrom dplyr select distinct
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text geom_vline
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off
#' @importFrom tidyr pivot_longer
#' @keywords internal



flux_plot_quadratic <- function(slopes_df,
                                f_conc,
                                f_datetime,
                                y_text_position) {
  param_df <- flux_param_qua(slopes_df)

  slopes_df <- flux_plot_flag(slopes_df, param_df)

  slopes_df <- slopes_df |>
    pivot_longer(
      cols = c("f_fit", "f_fit_slope", "f_fit_lm"),
      names_to = "linetype",
      values_to = "f_fit"
    )

  plot_quadratic <- slopes_df |>
    ggplot(aes({{f_datetime}})) +
    theme_bw() +
    geom_vline(xintercept = slopes_df$f_start_z,
               color = "grey", linewidth = 0.5) +
    geom_point(aes(y = {{f_conc}}, color = .data$f_quality_flag),
      size = 0.4,
      na.rm = TRUE
    ) +
    geom_text(
      data = param_df,
      aes(
        x = .data$f_start, y = y_text_position,
        label = .data$print_col
      ),
      vjust = 0, hjust = "inward",
      na.rm = TRUE
    )

  plot_quadratic
}
