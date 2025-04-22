#' Using an already existing end column to slice measurements
#' @description Provides the `f_end` column for `flux_match`
#' @param field_record dataframe recording which measurement happened when.
#' Has to contain at least a column containing the start of each measurement,
#' and any other column identifying the measurements.
#' @param start_col start column in field_record (`ymd_hms` format)
#' @param end_col end columne in field_record (`ymd_hms` format)
#' @param name_field_record name of the df (for error message)
#' @importFrom dplyr mutate

flux_match_col <- function(
  field_record,
  start_col,
  end_col,
  name_field_record
) {
  field_record_check2 <- field_record |>
    select({{end_col}})

  field_record_ok2 <- flux_fun_check(field_record_check2,
                                     fn = list(is.POSIXct),
                                     msg = "has to be POSIXct",
                                     name_df = name_field_record)

  if (!field_record_ok2)
    stop("Please correct the arguments", call. = FALSE)

  field_record <- field_record |>
    mutate(
      f_end = {{end_col}}
    )

  field_record
}
