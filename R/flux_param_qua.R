#' prepares text to print in flux_plot
#' @description creates a df with quality flags and quality diagnostics
#' to print on the plots produced by flux_plot.
#' flux_param_lm is for fit in the lm family (linear and quadratic)
#' flux_param_exp is for the exponential fit
#' @param slopes_df the slopes_df that is being provided to flux_plot
#' @param f_conc column with gas concentration
#' @importFrom dplyr select group_by mutate ungroup distinct filter

flux_param_qua <- function(slopes_df,
                           f_conc) {
  param_df <- slopes_df |>
    select(
      {{f_conc}}, "f_start", "f_fluxid", "f_rsquared", "f_pvalue",
      "f_gfactor", "f_quality_flag", "f_cut"
    ) |>
    filter(.data$f_cut != "cut") |>
    select(!{{f_conc}}) |>
    distinct() |>
    mutate(
      f_rsquared = round(.data$f_rsquared, digits = 2),
      f_pvalue = round(.data$f_pvalue, digits = 6),
      f_gfactor = signif(.data$f_gfactor, digits = 2),
      print_col = paste(
        .data$f_quality_flag, "\n",
        "R2 = ", .data$f_rsquared, "\n",
        "p-value = ", .data$f_pvalue, "\n",
        "g-factor = ", .data$f_gfactor,
        sep = ""
      )
    ) |>
    select("f_start", "f_fluxid", "print_col", "f_quality_flag")

  param_df
}
