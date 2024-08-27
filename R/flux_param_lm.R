#' prepares text to print in flux_plot
#' @description creates a df with quality flags and quality diagnostics
#' to print on the plots produced by flux_plot.
#' flux_param_lm is for fit in the lm family (linear and quadratic)
#' flux_param_exp is for the exponential fit
#' @param slopes_df the slopes_df that is being provided to flux_plot
#' @param cut_arg argument pointing rows to be cut from the measurements
#' @importFrom dplyr select group_by mutate ungroup distinct filter

flux_param_lm <- function(slopes_df,
                          cut_arg = "cut") {
  param_df <- slopes_df |>
    select(
      "f_conc", "f_start", "f_fluxID", "f_rsquared", "f_pvalue",
      "f_quality_flag", "f_cut"
    ) |>
    filter(.data$f_cut != ((cut_arg))) |>
    group_by(.data$f_fluxID) |>
    mutate(
      conc_start = .data$f_conc[1]
    ) |>
    ungroup() |>
    select(!"f_conc") |>
    distinct() |>
    mutate(
      f_rsquared = round(.data$f_rsquared, digits = 2),
      f_pvalue = round(.data$f_pvalue, digits = 4),
      print_col = paste(
        .data$f_quality_flag, "\n",
        "R2 = ", .data$f_rsquared, "\n", "p-value = ", .data$f_pvalue,
        sep = ""
      )
    ) |>
    select("f_fluxID", "conc_start", "print_col", "f_quality_flag")

  param_df
}
