#' selecting linear slope with kappamax method
#' @description selecting linear slope with kappamax method
#' @param slopes_df dataset containing slopes
#' @param f_slope column containing the slope of each flux
#' (as calculated by the \link[fluxible:flux_fitting]{flux_fitting} function)
#' @param f_fit column containing the modeled data (exponential fits)
#' @param f_slope_lm column containing the linear slope of each flux
#' @param f_fit_lm column with the fit of the linear model.
#' @param f_b column containing the b parameter of the exponential expression
#' @param force_exp vector of fluxIDs for which the exponential slope should be
#' used by the user's decision (kappamax method)
#' @param fit_type model fitted to the data, linear, quadratic or exponential.
#' Will be automatically filled if `slopes_df` was produced using
#' \link[fluxible:flux_fitting]{flux_fitting}
#' @param instr_error error of the instrument, in the same unit as the
#' gas concentration
#' @param name_df name of `slopes_df`
#' @param f_fluxid column of ID for each measurement
#' @keywords internal

flux_quality_kappamax <- function(slopes_df,
                                  f_slope,
                                  f_fit,
                                  f_fluxid,
                                  f_slope_lm,
                                  f_fit_lm,
                                  f_b,
                                  force_exp,
                                  fit_type,
                                  instr_error,
                                  name_df) {

  args_ok <- flux_fun_check(list(
    instr_error = instr_error
  ),
  fn = list(is.numeric),
  msg = "has to be numeric"
  )

  slopes_df_check <- slopes_df |>
    select(
      {{f_slope_lm}},
      {{f_fit_lm}}
    )

  df_ok <- flux_fun_check(slopes_df_check,
                          fn = list(
                            is.numeric,
                            is.numeric
                          ),
                          msg = rep(
                            "has to be numeric",
                            2
                          ),
                          name_df = name_df)


  if (any(!c(args_ok, df_ok)))
    stop("Please correct the arguments", call. = FALSE)

  slopes_df <- slopes_df |>
    mutate(
      f_kappamax = abs({{f_slope_lm}} / instr_error),
      {{f_slope}} := case_when(
        {{f_fluxid}} %in% force_exp ~ {{f_slope}},
        is.na({{f_b}}) ~ {{f_slope_lm}},
        abs({{f_b}}) <= f_kappamax ~ {{f_slope}},
        abs({{f_b}}) > f_kappamax ~ {{f_slope_lm}}
      ),
      f_model = case_when(
        {{f_fluxid}} %in% force_exp ~ fit_type,
        is.na({{f_b}}) ~ "linear",
        abs({{f_b}}) <= f_kappamax ~ fit_type,
        abs({{f_b}}) > f_kappamax ~ "linear"
      )
    )

  attr(slopes_df, "kappamax") <- TRUE


  total_lm <- slopes_df |>
    select({{f_fluxid}}, "f_model") |>
    distinct() |>
    filter(.data$f_model == "linear") |>
    nrow()

  message <- paste("\n", "Number of measurements with linear fit:", total_lm)

  if (any(!is.na(message))) message(message)


  slopes_df

}
