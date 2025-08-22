#' CO2 concentration with missing data
#'
#' Continuous CO2 concentration as measured on the field, with missing data.
#'
#' @format A tibble with 1148 rows and 5 variables
#' \describe{
#' \item{datetime}{Datetime at which CO2 concentration was recorded.}
#' \item{temp_air}{Air temperature inside the flux chamber in Celsius.}
#' \item{temp_soil}{Ground temperature inside the flux chamber in Celsius.}
#' \item{conc}{CO2 concentration in ppm.}
#' \item{PAR}{Photosynthetically active radiation inside the chamber
#' in micromol/s/sqm.}
#' }
#' @examples
#' co2_df_missing
#' @keywords internal
"co2_df_missing"
