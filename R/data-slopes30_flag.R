#' Slopes for each flux
#'
#' Slopes of C(t) for each flux with 30 seconds end cut, with quality flags.
#'
#' @format A tibble with 1251 rows and 27 variables
#' \describe{
#' \item{datetime}{Datetime at which CO2 concentration was recorded.}
#' \item{temp_air}{Air temperature inside the flux chamber in Celsius.}
#' \item{temp_soil}{Ground temperature inside the flux chamber in Celsius.}
#' \item{conc}{CO2 concentration in ppm.}
#' \item{PAR}{Photosynthetically active radiation inside the chamber in
#' micromol/s/sqm.}
#' \item{turfID}{Unique ID of the turf in which the measurement took place.}
#' \item{type}{Type of measurement: ecosystems respiration (ER)
#' or net ecosystem exchange (NEE).}
#' \item{f_start}{Datetime at which the measurement was started.}
#' \item{f_end}{Datetime at which the measurement ended.}
#' \item{f_fluxid}{Unique ID for each flux.}
#' \item{f_flag_match}{Flags from flux_match.}
#' \item{f_time}{Time variable of the flux in seconds.}
#' \item{f_cut}{Indicating if the measurement should be kept (keep) or
#' discarded (cut).}
#' \item{f_Cz}{Cz parameter of the C(t) function.}
#' \item{f_Cm}{Cm parameter of the C(t) function, calculated by optim() with
#' Cm_est as starting point.}
#' \item{f_a}{a parameter of the C(t) function, calculated by optim() with
#' a_est as starting point.}
#' \item{f_b}{b parameter of the C(t) function, calculated by optim() with
#' b_est as starting point.}
#' \item{f_tz}{tz parameter of the C(t) function, calculated by optim() with
#' tz_est as starting point.}
#' \item{f_slope}{Slope of C(t) at tz}
#' \item{f_fit}{C(t), modeled CO2 concentration as a function of time.}
#' \item{f_fit_slope}{Output of linear model of CO2 concentration passing by
#' C(tz) and a slope of slope_tz.}
#' \item{f_start_z}{Datetime format of tz}
#' \item{f_ratio}{Ratio of number of data points compared
#' to length of measurement in seconds.}
#' \item{f_cor_coef}{coefficient of correlation between gas concentration
#' and time}
#' \item{f_RMSE}{RMSE of the exponential fit and the measured data}
#' \item{f_quality_flag}{quality flag advising if the slope has to be
#' replaced by 0 or NA}
#' \item{f_slope_corr}{slope corrected according to quality flag}
#' }
#' @examples
#' slopes30_flag
"slopes30_flag"
