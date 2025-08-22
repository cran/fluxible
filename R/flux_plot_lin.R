#' plotting fluxes with linear fit
#' @description plots the fluxes that were fitted
#' with a linear model
#' @param slopes_df dataset containing slopes
#' @param f_conc column with gas concentration
#' @param f_datetime column with datetime of each data point
#' @param y_text_position position of the text box
#' @importFrom dplyr select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off
#' @keywords internal



flux_plot_lin <- function(slopes_df,
                          f_conc,
                          f_datetime,
                          y_text_position) {
  param_df <- flux_param_lm(slopes_df)

  slopes_df <- flux_plot_flag(slopes_df, param_df)

  slopes_df <- slopes_df |>
    mutate(
      linetype = "f_fit_slope"
    )

  plot_lin <- slopes_df |>
    ggplot(aes({{f_datetime}})) +
    theme_bw() +
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

  plot_lin
}
