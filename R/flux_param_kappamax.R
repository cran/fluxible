#' prepares text to print for flux_plot function
#' @description creates a df with quality flags and quality diagnostics
#' to print on the plots produced by flux_plot.
#' flux_param_lm is for fit in the lm family (linear and quadratic)
#' flux_param_exp is for the exponential fit
#' @param slopes_df the slopes_df that is being provided to flux_plot
#' @param f_datetime column with datetime of each data point
#' @importFrom dplyr select group_by mutate ungroup distinct filter
#' @keywords internal

flux_param_kappamax <- function(slopes_df, f_datetime) {
  param_df <- slopes_df |>
    select(
      {{f_datetime}}, "f_facetid", "f_RMSE", "f_cor_coef", "f_b",
      "f_gfactor", "f_cut", "f_quality_flag", "f_model",
      "f_rsquared_lm", "f_pvalue_lm"
    ) |>
    mutate(
      .by = "f_facetid",
      .keep = "unused",
      f_start_og = min({{f_datetime}})
    ) |>
    filter(.data$f_cut != "cut") |>
    distinct() |>
    mutate(
      f_rsquared_lm = round(.data$f_rsquared_lm, digits = 2),
      f_pvalue_lm = round(.data$f_pvalue_lm, digits = 6),
      f_RMSE = signif(.data$f_RMSE, digits = 2),
      f_cor_coef = signif(.data$f_cor_coef, digits = 2),
      f_gfactor = signif(.data$f_gfactor, digits = 2),
      f_b = signif(.data$f_b, digits = 5),
      print_col_exp = paste(
        .data$f_quality_flag, "\n",
        "RMSE = ", .data$f_RMSE, "\n",
        "Corr coef = ", .data$f_cor_coef, "\n",
        "g-factor = ", .data$f_gfactor, "\n",
        "b = ", .data$f_b, "\n",
        .data$f_model,
        sep = ""
      ),
      print_col_lm = paste(
        .data$f_quality_flag, "\n",
        "R2 = ", .data$f_rsquared_lm, "\n",
        "p-value = ", .data$f_pvalue_lm, "\n",
        .data$f_model,
        sep = ""
      ),
      print_col = case_when(
        .data$f_model == "linear" ~ print_col_lm,
        str_detect(.data$f_model, "exp") ~ print_col_exp
      )
    ) |>
    select("f_start_og", "f_facetid", "print_col", "f_quality_flag")

  param_df
}
