#' prepares text to print for flux_plot function
#' @description creates a df with quality flags and quality diagnostics
#' to print on the plots produced by flux_plot.
#' flux_param_lm is for fit in the lm family (linear and quadratic)
#' flux_param_exp is for the exponential fit
#' @param slopes_df the slopes_df that is being provided to flux_plot
#' @param f_conc column with gas concentration
#' @importFrom dplyr select group_by mutate ungroup distinct filter

flux_param_exp <- function(slopes_df,
                           f_conc) {
  param_df <- slopes_df |>
    select(
      {{f_conc}}, "f_start", "f_fluxid", "f_RMSE", "f_cor_coef", "f_b",
      "f_gfactor", "f_cut", "f_quality_flag"
    ) |>
    filter(.data$f_cut != "cut") |>
    select(!{{f_conc}}) |>
    distinct() |>
    mutate(
      f_RMSE = signif(.data$f_RMSE, digits = 2),
      f_cor_coef = signif(.data$f_cor_coef, digits = 2),
      f_gfactor = signif(.data$f_gfactor, digits = 2),
      f_b = signif(.data$f_b, digits = 5),
      print_col = paste(
        .data$f_quality_flag, "\n",
        "RMSE = ", .data$f_RMSE, "\n",
        "Corr coef = ", .data$f_cor_coef, "\n",
        "g-factor = ", .data$f_gfactor, "\n",
        "b = ", .data$f_b,
        sep = ""
      )
    ) |>
    select("f_start", "f_fluxid", "print_col", "f_quality_flag")

  param_df
}
