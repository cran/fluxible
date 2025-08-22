#' filter cut data before calculating fluxes
#' @param slopes_df dataset containing slopes and cut column
#' @param cut_col column containing cutting information
#' @param keep_arg name in cut_col of data to keep
#' @keywords internal

flux_cut <- function(slopes_df,
                     cut_col,
                     keep_arg) {
  slopes_df <- slopes_df |>
    filter({{cut_col}} == keep_arg)
}
