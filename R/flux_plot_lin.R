#' plotting fluxes with linear fit
#' @description plots the fluxes that were fitted
#' with a linear model
#' @param slopes_df dataset containing slopes
#' @param conc_col column with gas concentration
#' @param datetime_col column with datetime of each data point
#' @param y_text_position position of the text box
#' @importFrom dplyr select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off



flux_plot_lin <- function(slopes_df,
                          conc_col,
                          datetime_col,
                          y_text_position) {
  param_df <- flux_param_lm(slopes_df, {{conc_col}})

  slopes_df <- flux_plot_flag(slopes_df, param_df)

  slopes_df <- slopes_df |>
    mutate(
      linetype = "f_fit"
    )

  plot_lin <- slopes_df |>
    ggplot(aes({{datetime_col}})) +
    theme_bw() +
    geom_point(aes(y = {{conc_col}}, color = .data$f_quality_flag),
      size = 0.2,
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
