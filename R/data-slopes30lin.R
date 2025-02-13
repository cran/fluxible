#' Slopes for each flux
#'
#' Slopes of linear fit for each flux with a 30 seconds cut at
#' the end of each flux.
#'
#' @format A tibble with 1251 rows and 19 variables
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
#' \item{f_flag_match}{Flags from flux_match.}
#' \item{f_time}{Time variable of the flux in seconds.}
#' \item{f_cut}{Indicating if the measurement should be kept (keep)
#' or discarded (cut).}
#' \item{f_pvalue}{P-value of the linear model of CO2 concentration over time.}
#' \item{f_rsquared}{R squared of the linear model of CO2 concentration
#' over time.}
#' \item{f_adj_rsquared}{Adjusted R squared of the linear model of
#' CO2 concentration over time.}
#' \item{f_intercept}{Intercept of the linear model of CO2 concentration
#' over time.}
#' \item{f_slope}{Slope of the linear model of CO2 concentration over time.}
#' \item{f_fit}{Output of the linear model of CO2 concentration over time.}
#' }
#' @examples
#' slopes30lin
"slopes30lin"
