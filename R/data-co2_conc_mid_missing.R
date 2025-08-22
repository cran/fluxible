#' CO2 concentration with missing data
#'
#' CO2 concentration with measurements meta data and missing data in the middle
#' of the measurements
#'
#' @format A tibble with 1251 rows and 13 variables
#' \describe{
#' \item{datetime}{Datetime at which CO2 concentration was recorded.}
#' \item{temp_air}{Air temperature inside the flux chamber in Celsius.}
#' \item{temp_soil}{Ground temperature inside the flux chamber in Celsius.}
#' \item{conc}{CO2 concentration in ppm.}
#' \item{PAR}{Photosynthetically active radiation inside the chamber
#' in micromol/s/sqm.}
#' \item{turfID}{Unique ID of the turf in which the measurement took place.}
#' \item{type}{Type of measurement: ecosystems respiration (ER)
#' or net ecosystem exchange (NEE).}
#' \item{f_start}{Datetime at which the measurement was started.}
#' \item{f_end}{Datetime at which the measurement ended.}
#' \item{f_fluxid}{Unique ID for each flux.}
#' \item{f_n_conc}{Number of data point per flux.}
#' \item{f_ratio}{Ratio of n_conc over length of the measurement (in seconds).}
#' \item{f_flag_match}{Data quality flags.}
#' }
#' @examples
#' co2_conc_mid_missing
#' @keywords internal
"co2_conc_mid_missing"
