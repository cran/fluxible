#' CO2 fluxes
#'
#' Calculated CO2 fluxes
#'
#' @format A tibble with 6 rows and 11 variables
#' \describe{
#' \item{fluxID}{Unique ID for each flux.}
#' \item{slope_tz}{Slope of C(t) at t zero.}
#' \item{temp_air_ave}{Air temperature inside the flux chamber in Celsius
#' averaged over the flux measurement.}
#' \item{flux}{CO2 flux in mmol/sqm/hour.}
#' \item{PAR}{Photosynthetically active radiation inside the chamber in
#' micromol/s/sqm averaged over the flux measurement.}
#' \item{temp_soil}{Ground temperature inside the flux chamber in Celsius
#' averaged over the flux measurement.}
#' \item{turfID}{Unique ID of the turf in which the measurement took place.}
#' \item{type}{Type of measurement: ecosystems respiration (ER)
#' or net ecosystem exchange (NEE).}
#' \item{start}{Datetime at which the measurement started.}
#' \item{temp_fahr}{Air temperature inside the flux chamber in Fahrenheit
#' averaged over the flux measurement.}
#' \item{temp_kelvin}{Air temperature inside the flux chamber in Kelvin
#' averaged over the flux measurement.}
#' }
#' @examples
#' co2_fluxes
"co2_fluxes"
