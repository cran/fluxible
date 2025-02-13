#' Fitting a model to concentration data and estimating the slope
#' @description fits gas concentration over time data with a model
#' (exponential, quadratic or linear) and provides the slope later used
#' to calculate gas fluxes with flux_calc
#' @param fit_type `exponential`, `quadratic` or `linear.`
#' Exponential is using the exponential model from Zhao et al (2018)
#' @references Zhao, P., Hammerle, A., Zeeman, M., Wohlfahrt, G., 2018.
#' On the calculation of daytime CO2 fluxes measured by automated closed
#' transparent chambers. Agricultural and Forest Meteorology 263, 267–275.
#' https://doi.org/10.1016/j.agrformet.2018.08.022
#' @param conc_df dataframe of gas concentration over time
#' @param conc_col column with gas concentration
#' @param t_window enlarge focus window before and after tmin and tmax
#' (exponential fit)
#' @param cz_window window used to calculate Cz, at the beginning of cut window
#' (exponential fit)
#' @param b_window window to estimate b. It is an interval after tz where
#' it is assumed that the model fits the data perfectly (exponential fit)
#' @param a_window window at the end of the flux to estimate a (exponential fit)
#' @param roll_width width of the rolling mean for CO2 when looking for tz,
#' ideally same as cz_window (exponential fit)
#' @param start_cut time to discard at the start of the measurements
#' (in seconds)
#' @param end_cut time to discard at the end of the measurements (in seconds)
#' @param f_start column with datetime when the measurement started
#' @param f_end column with datetime when the measurement ended
#' @param datetime_col column with datetime of each concentration measurement
#' Note that if there are duplicated datetime in the same f_fluxid only
#' the first row will be kept
#' @param conc_col column with gas concentration data
#' @param f_fluxid column with ID of each flux
#' @param t_zero time at which the slope should be calculated
#' (for quadratic fit)
#' @return a dataframe with the slope at t zero (`f_slope`),
#' a datetime column of t zero (`f_start_z`), a factor column indicating the
#' cuts (`f_cut`), the time in seconds since the start of the measurement
#' (`f_time`), the modeled fit (`f_fit`), the modeled slope (`f_fit_slope`),
#' the parameters of the fit depending on the model used,
#' and any columns present in the input.
#' The type of fit is added as an attribute for use by the other functions.
#' @importFrom lubridate int_length interval
#' @examples
#' data(co2_conc)
#' flux_fitting(co2_conc, conc, datetime, fit_type = "exp")
#' flux_fitting(co2_conc, conc, datetime,  fit_type = "quadratic",
#' t_zero = 10, end_cut = 30)
#' @export

flux_fitting <- function(conc_df,
                         conc_col,
                         datetime_col,
                         f_start = f_start,
                         f_end = f_end,
                         f_fluxid = f_fluxid,
                         start_cut = 0,
                         end_cut = 0,
                         t_window = 20,
                         cz_window = 15,
                         b_window = 10,
                         a_window = 10,
                         roll_width = 15,
                         t_zero = 0,
                         fit_type) {

  name_df <- deparse(substitute(conc_df))

  args_ok <- flux_fun_check(list(
    start_cut = start_cut,
    end_cut = end_cut
  ),
  fn = list(is.numeric, is.numeric),
  msg = rep("has to be numeric", 2))

  conc_df_check <- conc_df |>
    select(
      {{conc_col}},
      {{f_start}},
      {{f_end}},
      {{datetime_col}}
    )

  conc_df_ok <- flux_fun_check(conc_df_check,
                               fn = list(
                                 is.numeric,
                                 is.POSIXct,
                                 is.POSIXct,
                                 is.POSIXct
                               ),
                               msg = rep(c(
                                 "has to be numeric",
                                 "has to be POSIXct"
                               ),
                               c(1, 3)
                               ),
                               name_df = name_df)


  if (any(!c(args_ok, conc_df_ok)))
    stop("Please correct the arguments", call. = FALSE)

  length_flux_max <- conc_df |>
    mutate(
      length_flux = int_length(interval({{f_start}}, {{f_end}}))
    ) |>
    select("length_flux") |>
    max()

  if ((start_cut + end_cut) >= length_flux_max) {
    stop(
      "You cannot cut more than the length of the measurements!"
    )
  }

  conc_df <- conc_df |>
    group_by({{f_fluxid}}) |>
    distinct({{datetime_col}}, .keep_all = TRUE) |>
    ungroup()

  fit_type <- flux_fit_type(
    conc_df,
    fit_type = fit_type
  )

  if (fit_type == "exponential") {
    conc_fitting <- flux_fitting_exp(
      conc_df,
      {{conc_col}},
      {{datetime_col}},
      {{f_start}},
      {{f_end}},
      {{f_fluxid}},
      start_cut = start_cut,
      end_cut = end_cut,
      t_window = t_window,
      cz_window = cz_window,
      b_window = b_window,
      a_window = a_window,
      roll_width = roll_width
    )
  }


  if (fit_type == "linear") {
    conc_fitting <- flux_fitting_lin(
      conc_df,
      {{conc_col}},
      {{datetime_col}},
      {{f_start}},
      {{f_end}},
      {{f_fluxid}},
      start_cut = start_cut,
      end_cut = end_cut
    )
  }

  if (fit_type == "quadratic") {
    conc_fitting <- flux_fitting_quadratic(
      conc_df,
      {{conc_col}},
      {{datetime_col}},
      {{f_start}},
      {{f_end}},
      {{f_fluxid}},
      start_cut = start_cut,
      end_cut = end_cut,
      t_zero = t_zero
    )
  }

  conc_fitting <- conc_fitting |>
    select(!"f_n_conc")

  attr(conc_fitting, "fit_type") <- fit_type

  conc_fitting
}
