#' Using a fixed measurement length to slice the measurements
#' @description Provides the `f_end` column for `flux_match`
#' @param field_record dataframe recording which measurement happened when.
#' Has to contain at least a column containing the start of each measurement,
#' and any other column identifying the measurements.
#' @param start_col start column in field_record (`ymd_hms` format)
#' @param measurement_length length of the measurement (in seconds)
#' from the start specified in the `field_record`
#' @importFrom dplyr mutate
#' @keywords internal

flux_match_fixed <- function(
  field_record,
  start_col,
  measurement_length
) {
  args_ok2 <- flux_fun_check(list(
    measurement_length = measurement_length
  ),
  fn = list(is.numeric),
  msg = "has to be numeric")

  if (!args_ok2)
    stop("Please correct the arguments", call. = FALSE)

  field_record <- field_record |>
    mutate(
      f_end = {{start_col}} + measurement_length
    )

  field_record
}
