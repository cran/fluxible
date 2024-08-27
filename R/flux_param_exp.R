#' prepares text to print for flux_plot function
#' @description creates a df with quality flags and quality diagnostics
#' to print on the plots produced by flux_plot.
#' flux_param_lm is for fit in the lm family (linear and quadratic)
#' flux_param_exp is for the exponential fit
#' @param slopes_df the slopes_df that is being provided to flux_plot
#' @param cut_arg argument pointing rows to be cut from the measurements
#' @importFrom dplyr select group_by mutate ungroup distinct filter

flux_param_exp <- function(slopes_df,
                           cut_arg = "cut") {
  param_df <- slopes_df |>
    select(
      "f_conc", "f_start", "f_fluxID", "f_RMSE", "f_cor_coef", "f_b", "f_cut",
      "f_quality_flag"
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
      f_RMSE = round(.data$f_RMSE, digits = 1),
      f_cor_coef = round(.data$f_cor_coef, digits = 2),
      f_b = round(.data$f_b, digits = 5),
      print_col = paste(
        .data$f_quality_flag, "\n",
        "RMSE = ", .data$f_RMSE, "\n", "Corr coef = ",
        .data$f_cor_coef, "\n", "b = ", .data$f_b,
        sep = ""
      )
    ) |>
    select("f_fluxID", "conc_start", "print_col", "f_quality_flag")

  param_df
}
