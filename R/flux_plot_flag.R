#' creates the flag column to be used by flux_plot
#' @description creates a column with quality flags (from flux_quality)
#' for the part of the rows to be kept, and cut flag for rows to be discarded
#' @param slopes_df as provided in flux_plot
#' @param param_df as provided by flux_param
#' @param cut_arg argument pointing rows to be cut from the measurements
#' @importFrom dplyr select left_join mutate case_when

flux_plot_flag <- function(slopes_df,
                           param_df,
                           cut_arg = "cut") {
  slopes_df <- slopes_df |>
    select(!c("f_quality_flag")) |>
    left_join(param_df, by = "f_fluxID") |>
    mutate(
      f_quality_flag = case_when(
        f_cut == ((cut_arg)) ~ f_cut,
        f_cut != ((cut_arg)) ~ f_quality_flag
      )
    )

  slopes_df
}
