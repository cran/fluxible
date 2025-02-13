#' CO2 concentration
#'
#' CO2 concentration used to make the plot in the poster
#'
#' @format A tibble with 530 rows and 11 variables
#' \describe{
#' \item{datetime}{Datetime at which CO2 concentration was recorded.}
#' \item{conc}{CO2 concentration in ppm.}
#' \item{f_fluxid}{Unique ID for each flux.}
#' \item{f_quality_flag}{quality flag advising if the slope has to be
#' replaced by 0 or NA}
#' \item{f_start}{Datetime at which the measurement was started.}
#' \item{f_RMSE}{RMSE of the exponential fit and the measured data}
#' \item{f_cor_coef}{coefficient of correlation between gas concentration
#' and time}
#' \item{f_b}{b parameter of the C(t) function, calculated by optim() with
#' b_est as starting point.}
#' \item{f_cut}{Indicating if the measurement should be kept (keep) or
#' discarded (cut).}
#' \item{f_fit}{C(t), modeled CO2 concentration as a function of time.}
#' \item{fit_slope}{Output of linear model of CO2 concentration passing by
#' C(tz) and a slope of slope_tz.}
#' }
#' @examples
#' conc_poster
"conc_poster"
