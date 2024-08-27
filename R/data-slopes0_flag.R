#' Slopes for each flux
#'
#' Slopes of C(t) for each flux with 0 second cut, with quality flags.
#'
#' @format A tibble with 1251 rows and 36 variables
#' \describe{
#' \item{datetime}{Datetime at which CO2 concentration was recorded.}
#' \item{temp_air}{Air temperature inside the flux chamber in Celsius.}
#' \item{temp_soil}{Ground temperature inside the flux chamber in Celsius.}
#' \item{f_conc}{CO2 concentration in ppm.}
#' \item{PAR}{Photosynthetically active radiation inside the chamber in
#' micromol/s/sqm.}
#' \item{turfID}{Unique ID of the turf in which the measurement took place.}
#' \item{type}{Type of measurement: ecosystems respiration (ER)
#' or net ecosystem exchange (NEE).}
#' \item{start}{Datetime at which the measurement was started.}
#' \item{end}{Datetime at which the measurement ended.}
#' \item{f_fluxID}{Unique ID for each flux.}
#' \item{n_conc}{Number of data point per flux.}
#' \item{ratio}{Ratio of n_conc over length of the measurement (in seconds).}
#' \item{flag}{Data quality flags.}
#' \item{f_time}{Time variable of the flux in seconds.}
#' \item{f_cut}{Indicating if the measurement should be kept (keep) or
#' discarded (cut).}
#' \item{Cm_est}{Estimation of the Cm parameter.}
#' \item{a_est}{Estimation of the a parameter.}
#' \item{b_est}{Estimation of the b parameter.}
#' \item{tz_est}{Estimation of the tz parameter.}
#' \item{Cz}{Cz parameter of the C(t) function.}
#' \item{Cm}{Cm parameter of the C(t) function, calculated by optim() with
#' Cm_est as starting point.}
#' \item{a}{a parameter of the C(t) function, calculated by optim() with
#' a_est as starting point.}
#' \item{f_b}{b parameter of the C(t) function, calculated by optim() with
#' b_est as starting point.}
#' \item{tz}{tz parameter of the C(t) function, calculated by optim() with
#' tz_est as starting point.}
#' \item{f_slope}{Slope of C(t) at tz}
#' \item{f_fit}{C(t), modeled CO2 concentration as a function of time.}
#' \item{fit_slope}{Output of linear model of CO2 concentration passing by
#' C(tz) and a slope of slope_tz.}
#' \item{start_z}{Datetime format of tz}
#' \item{f_cor_coef}{coefficient of correlation between gas concentration
#' and time}
#' \item{f_RMSE}{RMSE of the exponential fit and the measured data}
#' \item{f_start_error}{flagging if measurement started outside of the possible
#' ambient concentration}
#' \item{f_fit_quality}{flagging bad fit}
#' \item{f_correlation}{flagging if there is a correlation between
#' gas concentration and time}
#' \item{f_quality_flag}{quality flag advising if the slope has to be
#' replaced by 0 or NA}
#' \item{f_slope_corr}{slope corrected according to quality flag}
#' }
#' @examples
#' slopes0_flag
"slopes0_flag"
