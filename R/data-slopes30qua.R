#' Slopes for each flux
#'
#' Slopes of quadratic fit for each flux with 30 seconds end cut and
#' t_zero of 10 seconds, without quality flags.
#' C(t) = a + b*t + c*t^2
#'
#' @format A tibble with 1251 rows and 27 variables
#' \describe{
#' \item{f_datetime}{Datetime at which CO2 concentration was recorded.}
#' \item{temp_air}{Air temperature inside the flux chamber in Celsius.}
#' \item{temp_soil}{Ground temperature inside the flux chamber in Celsius.}
#' \item{f_conc}{CO2 concentration in ppm.}
#' \item{PAR}{Photosynthetically active radiation inside the chamber in
#' micromol/s/sqm.}
#' \item{turfID}{Unique ID of the turf in which the measurement took place.}
#' \item{type}{Type of measurement: ecosystems respiration (ER)
#' or net ecosystem exchange (NEE).}
#' \item{f_start}{Datetime at which the measurement was started.}
#' \item{f_end}{Datetime at which the measurement ended.}
#' \item{f_fluxID}{Unique ID for each flux.}
#' \item{n_conc}{Number of data point per flux.}
#' \item{ratio}{Ratio of n_conc over length of the measurement (in seconds).}
#' \item{flag}{Data quality flags.}
#' \item{f_time}{Time variable of the flux in seconds.}
#' \item{f_cut}{Indicating if the measurement should be kept (keep)
#' or discarded (cut).}
#' \item{f_pvalue}{P-value of the quadratic model of gas concentration
#' over time.}
#' \item{f_rsquared}{R squared of the quadratic model of gas concentration
#' over time.}
#' \item{f_adj_rsquared}{Adjusted R squared of the quadratic model of
#' gas concentration over time.}
#' \item{f_intercept}{Intercept of the quadratic model of gas concentration
#' over time.}
#' \item{f_param1}{b parameter of C(t)}
#' \item{f_param2}{c parameter of C(t)}
#' \item{f_slope}{Slope of the quadratic model of gas concentration
#' over time at t_zero.}
#' \item{f_fit}{Output of the quadratic model of gas concentration over time.}
#' \item{f_fit_slope}{output of linear expression describing
#' the slope at t_zero}
#' }
#' @examples
#' slopes30qua
"slopes30qua"
