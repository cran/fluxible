#' prepares text to print in flux_plot
#' @description creates a df with quality flags and quality diagnostics
#' to print on the plots produced by flux_plot.
#' flux_param_lm is for fit in the lm family (linear and quadratic)
#' flux_param_exp is for the exponential fit
#' @param slopes_df the slopes_df that is being provided to flux_plot
#' @param f_datetime column with datetime of each data point
#' @importFrom dplyr select group_by mutate ungroup distinct filter
#' @keywords internal

flux_param_qua <- function(slopes_df, f_datetime) {
  param_df <- slopes_df |>
    select(
      {{f_datetime}}, "f_facetid", "f_rsquared", "f_pvalue",
      "f_gfactor", "f_quality_flag", "f_cut"
    ) |>
    mutate(
      .by = "f_facetid",
      .keep = "unused",
      f_start_og = min({{f_datetime}})
    ) |>
    filter(.data$f_cut != "cut") |>
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
    select("f_start_og", "f_facetid", "print_col", "f_quality_flag")

  param_df
}
