#' CO2 fluxes with PAR values
#'
#' CO2 fluxes with photosynthetically active radiation (PAR) for testing and
#' examples of light response curves (LRC)
#'
#' @format A tibble with 257 rows and 5 variables
#' \describe{
#' \item{f_flux}{CO2 flux in mmol/sqm/hour.}
#' \item{datetime}{Time and date of the measurement.}
#' \item{PAR_ave}{Photosynthetically active radiation inside the chamber in
#' micromol/s/sqm averaged over the flux measurement.}
#' \item{type}{Type of measurement: ecosystems respiration (ER), net ecosystem
#' exchange (NEE), or light response curve (LRC).}
#' \item{warming}{Treatment: control or warming.}
#' }
#' @examples
#' co2_fluxes_lrc
"co2_fluxes_lrc"
