#' plotting fluxes with exponential fit
#' @description plots the fluxes that were fitted with
#' an exponential model
#' @param slopes_df dataset containing slopes
#' @param conc_col column with gas concentration
#' @param datetime_col column with datetime of each data point
#' @param y_text_position position of the text box
#' @importFrom dplyr select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw geom_vline
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off
#' @importFrom tidyr pivot_longer



flux_plot_exp <- function(slopes_df,
                          conc_col,
                          datetime_col,
                          y_text_position) {

  kappamax <- attributes(slopes_df)$kappamax

  if (is.null(kappamax)) {
    kappamax <- FALSE
  }

  if (kappamax == TRUE) {
    param_df <- flux_param_kappamax(slopes_df, {{conc_col}})
  }

  if (kappamax == FALSE) {
    param_df <- flux_param_exp(slopes_df, {{conc_col}})
  }

  slopes_df <- flux_plot_flag(slopes_df, param_df)

  slopes_df <- slopes_df |>
    pivot_longer(
      cols = c("f_fit", "f_fit_slope", "f_fit_lm"),
      names_to = "linetype",
      values_to = "f_fit"
    )



  plot_exp <- slopes_df |>
    ggplot(aes({{datetime_col}})) +
    theme_bw() +
    geom_vline(xintercept = slopes_df$f_start_z,
               color = "grey", linewidth = 0.5) +
    geom_point(
      aes(y = {{conc_col}}, color = .data$f_quality_flag),
      size = 0.2,
      na.rm = TRUE
    ) +
    geom_text(
      data = param_df,
      aes(x = .data$f_start, y = y_text_position, label = .data$print_col),
      vjust = 0, hjust = "inward",
      na.rm = TRUE
    )

  plot_exp
}
