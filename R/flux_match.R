#' Matching continuously measured fluxes with measurement IDs and meta data
#' @description Matching a dataframe of continuously measured
#' gas concentration data with measurement metadata from another dataframe.
#' Measurements are paired with their metadata based on datetime.
#' Extra variables in both dataframes are kept in the output.
#' @param raw_conc dataframe of CO2 concentration measured continuously.
#' Has to contain at least a datetime column in ymd_hms format and
#' a gas concentration column as double.
#' @param field_record dataframe recording which measurement happened when.
#' Has to contain at least a column containing the start of each measurement,
#' and any other column identifying the measurements.
#' @param startcrop how many seconds should be discarded at the beginning of
#' the measurement
#' @param measurement_length length of the measurement (in seconds)
#' from the start specified in the `field_record`
#' @param ratio_threshold ratio (number of concentration measurement compared to
#' length of measurement in seconds) below which the data should be flagged as
#' too little
#' @param time_diff time difference (in seconds) between the two datasets.
#' Will be added to the datetime column of the `raw_conc` dataset.
#' For situations where the time was not synchronized correctly.
#' @param f_datetime datetime column in raw_conc (`ymd_hms` format)
#' @param f_conc concentration column in `raw_conc`
#' @param start_col start column in field_record (`ymd_hms` format)
#' @param end_col end columne in field_record (`ymd_hms` format)
#' @param fixed_length if `TRUE` (default), the `measurement_length` is used to
#' create the end column. If `FALSE`, `end_col` has to be provided.
#' @return a dataframe with concentration measurements, corresponding datetime,
#' flux ID (`f_fluxid`), measurements start (`f_start`) and end (`f_end`),
#' flags in case of no data or low number of data (`f_flag_match`),
#' the number of datapoints per measurement (`f_n_conc`),
#' the ratio of number of datapoints over the length of each measurement
#' in seconds (`f_ratio`), and any variables present in one of the inputs.
#' @importFrom dplyr arrange mutate row_number full_join case_when
#' group_by filter ungroup select distinct pull join_by coalesce
#' @importFrom tidyr fill drop_na
#' @importFrom lubridate is.POSIXct
#' @examples
#' data(co2_df_short, record_short)
#' flux_match(co2_df_short, record_short, datetime, start, conc, startcrop = 10,
#' measurement_length = 180)
#' @export


flux_match <- function(raw_conc,
                       field_record,
                       f_datetime,
                       start_col,
                       f_conc,
                       end_col,
                       startcrop,
                       measurement_length,
                       fixed_length = TRUE,
                       ratio_threshold = 0.5,
                       time_diff = 0) {

  name_raw_conc <- deparse(substitute(raw_conc))
  name_field_record <- deparse(substitute(field_record))

  args_ok <- flux_fun_check(list(
    startcrop = startcrop,
    ratio_threshold = ratio_threshold,
    time_diff = time_diff
  ),
  fn = list(is.numeric, is.numeric, is.numeric),
  msg = rep("has to be numeric", 3))

  raw_conc_check <- raw_conc |>
    select({{f_datetime}}, {{f_conc}})

  field_record_check <- field_record |>
    select({{start_col}})

  raw_conc_ok <- flux_fun_check(raw_conc_check,
                                fn = list(is.POSIXct, is.numeric),
                                msg = c(
                                  "has to be POSIXct",
                                  "has to be numeric"
                                ),
                                name_df = name_raw_conc)

  field_record_ok <- flux_fun_check(field_record_check,
                                    fn = list(is.POSIXct),
                                    msg = "has to be POSIXct",
                                    name_df = name_field_record)

  if (any(!c(args_ok, raw_conc_ok, field_record_ok)))
    stop("Please correct the arguments", call. = FALSE)




  if (ratio_threshold < 0 || ratio_threshold > 1) {
    stop("ratio_threshold has to be a number between 0 and 1")
  }


  field_record <- field_record |>
    arrange({{start_col}}) |>
    mutate(
      f_start = {{start_col}} + startcrop,
      f_fluxid = row_number()
    )

  if (fixed_length) {

    field_record <- flux_match_fixed(
      field_record,
      {{start_col}},
      measurement_length = measurement_length
    )
  }


  if (!fixed_length) {

    field_record <- flux_match_col(
      field_record,
      {{start_col}},
      {{end_col}},
      name_field_record = name_field_record
    )
  }


  raw_conc <- raw_conc |>
    mutate(
      {{f_datetime}} := {{f_datetime}} + time_diff
    )


  conc_df <- full_join(
    raw_conc, field_record,
    by = join_by({{f_datetime}} == "f_start"), keep = TRUE
  ) |>
    mutate(
      {{f_datetime}} := coalesce({{f_datetime}}, .data$f_start)
    ) |>
    arrange({{f_datetime}}) |>
    fill("f_fluxid") |>
    drop_na("f_fluxid")

  conc_df <- conc_df |>
    group_by(.data$f_fluxid) |>
    fill(names(field_record)) |>
    filter(
      ({{f_datetime}} < .data$f_end &
         {{f_datetime}} >= .data$f_start)
    ) |>
    mutate(
      f_n_conc = sum(!is.na({{f_conc}})),
      f_length = difftime(.data$f_end, .data$f_start, units = "secs"),
      f_length = as.numeric(.data$f_length),
      f_ratio = .data$f_n_conc / .data$f_length,
      f_flag_match = case_when(
        .data$f_ratio == 0 ~ "no data",
        .data$f_ratio <= ratio_threshold ~ "nb of data too low"
      )
    ) |>
    ungroup()

  conc_df <- conc_df |>
    mutate(
      f_fluxid = as.factor(.data$f_fluxid),
      f_flag_match = as.character(.data$f_flag_match)
    ) |>
    arrange(.data$f_fluxid)


  flags <- conc_df |>
    select("f_fluxid", "f_flag_match") |>
    drop_na("f_flag_match") |>
    distinct() |>
    mutate(
      f_warnings = paste(
        "\n", "fluxID", .data$f_fluxid, ":",
        .data$f_flag_match
      ),
      f_warnings = as.character(.data$f_warnings)
    ) |>
    pull(.data$f_warnings)

  f_warnings <- str_c(flags)


  if (any(!is.na(conc_df$f_flag_match))) warning(f_warnings)


  conc_df
}
