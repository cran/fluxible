#' creates the flag column to be used by flux_plot
#' @description creates a column with quality flags (from flux_quality)
#' for the part of the rows to be kept, and cut flag for rows to be discarded
#' @param slopes_df as provided in flux_plot
#' @param param_df as provided by flux_param
#' @importFrom dplyr select left_join mutate case_when join_by
#' @keywords internal

flux_plot_flag <- function(slopes_df,
                           param_df) {
  slopes_df <- slopes_df |>
    select(!c("f_quality_flag")) |>
    left_join(param_df, by = join_by("f_facetid", "f_start")) |>
    mutate(
      f_quality_flag = case_when(
        f_cut == "cut" ~ f_cut,
        f_cut != "cut" ~ f_quality_flag
      )
    )
  # print_col needs to have only 1 row/fluxID

  slopes_df
}
