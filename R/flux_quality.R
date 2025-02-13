#' assessing quality of slopes calculated with flux_fitting
#' @description indicates if slopes should be discarded or replaced
#' by 0 according to quality thresholds set by user
#' @param slopes_df dataset containing slopes
#' @param fit_type model fitted to the data, linear, quadratic or exponential.
#' Will be automatically filled if slopes_df was produced using flux_fitting()
#' @param ambient_conc ambient gas concentration in ppm at the site of
#' measurement (used to detect measurement that started with a polluted setup)
#' @param error error of the setup, defines a window outside of which
#' the starting values indicate a polluted setup
#' @param f_fluxid column containing unique IDs for each flux
#' @param f_slope column containing the slope of each flux
#' (as calculated by the flux_fitting function)
#' @param force_discard vector of fluxIDs that should be discarded
#' by the user's decision
#' @param force_ok vector of fluxIDs for which the user wants to keep
#' the calculated slope despite a bad quality flag
#' @param ratio_threshold ratio of gas concentration data points over length of
#' measurement (in seconds) below which the measurement will be considered as
#' not having enough data points to be considered for calculations
#' @param f_pvalue column containing the p-value of each flux
#' (linear and quadratic fit)
#' @param f_rsquared column containing the r squared of each flux
#' (linear and quadratic fit)
#' @param pvalue_threshold threshold of p-value below which the change of
#' gas concentration over time is considered not significant
#' (linear and quadratic fit)
#' @param rsquared_threshold threshold of r squared value below which
#' the linear model is considered an unsatisfactory fit
#' (linear and quadratic fit)
#' @param conc_col column containing the measured gas concentration
#' (exponential fit)
#' @param f_b column containing the b parameter of the exponential expression
#' (exponential fit)
#' @param f_time column containing the time of each measurement in seconds
#' (exponential fit)
#' @param f_start column with datetime of the start of the measurement
#' (after cuts)
#' @param f_end column with datetime of the end of the measurement
#' (after cuts)
#' @param f_fit column containing the modeled data (exponential fit)
#' @param rmse_threshold threshold for the RMSE of each flux above which
#' the fit is considered unsatisfactory (exponential fit)
#' @param cor_threshold threshold for the correlation coefficient of
#' gas concentration with time below which the correlation
#' is considered not significant (exponential fit)
#' @param f_cut column containing the cutting information
#' @param cut_arg argument defining that the data point should be cut out
#' @param b_threshold threshold for the b parameter.
#' Defines a window with its opposite inside which the fit is
#' considered good enough (exponential fit)
#' @return a dataframe with added columns of quality flags (`f_quality_flag`),
#' the slope corrected according to the quality flags (`f_slope_corr`),
#' some diagnostics depending on the fit, and any columns present in the input.
#' @importFrom dplyr mutate case_when group_by rowwise summarise ungroup
#' @importFrom tidyr nest unnest
#' @importFrom stats cor
#' @importFrom lubridate int_length interval
#' @examples
#' data(slopes0lin)
#' flux_quality(slopes0lin, conc, fit_type = "li")
#' data(slopes30)
#' flux_quality(slopes30, conc, fit_type = "expo")
#' @export

flux_quality <- function(slopes_df,
                         conc_col,
                         f_fluxid = f_fluxid,
                         f_slope = f_slope,
                         f_time = f_time,
                         f_start = f_start,
                         f_end = f_end,
                         f_fit = f_fit,
                         f_cut = f_cut,
                         f_pvalue = f_pvalue,
                         f_rsquared = f_rsquared,
                         f_b = f_b,
                         force_discard = c(),
                         force_ok = c(),
                         ratio_threshold = 0,
                         fit_type = c(),
                         ambient_conc = 421,
                         error = 100,
                         pvalue_threshold = 0.3,
                         rsquared_threshold = 0.7,
                         rmse_threshold = 25,
                         cor_threshold = 0.5,
                         b_threshold = 1,
                         cut_arg = "cut") {

  name_df <- deparse(substitute(slopes_df))

  args_ok <- flux_fun_check(list(
    ambient_conc = ambient_conc,
    error = error,
    ratio_threshold = ratio_threshold
  ),
  fn = list(is.numeric, is.numeric, is.numeric),
  msg = rep("has to be numeric", 3))

  slopes_df_check <- slopes_df |>
    select(
      {{f_slope}},
      {{conc_col}},
      {{f_fit}},
      {{f_time}}
    )

  df_ok <- flux_fun_check(slopes_df_check,
                          fn = list(
                            is.numeric,
                            is.numeric,
                            is.numeric,
                            is.numeric
                          ),
                          msg = rep(
                            "has to be numeric",
                            4
                          ),
                          name_df = name_df)


  if (any(!c(args_ok, df_ok)))
    stop("Please correct the arguments", call. = FALSE)


  fit_type <- flux_fit_type(
    slopes_df,
    fit_type = fit_type
  )

  name_conc <- names(select(slopes_df, {{conc_col}}))


  slopes_df <- slopes_df |>
    mutate(
      f_n_conc = sum(!is.na(.data[[name_conc]])),
      f_ratio = .data$f_n_conc / int_length(interval({{f_start}}, {{f_end}})),
      f_flag_ratio = case_when(
        .data$f_ratio == 0 ~ "no_data",
        .data$f_ratio <= ratio_threshold ~ "too_low",
        TRUE ~ "ok"
      ),
      .by = c({{f_fluxid}}, {{f_cut}})
    )

  quality_par_start <- slopes_df |>
    # for the start error we take the entire flux into account
    group_by({{f_fluxid}}) |>
    nest() |>
    rowwise() |>
    summarise(
      f_start_error = case_when(
        data[[name_conc]][1] < (ambient_conc - error) ~ "error",
        data[[name_conc]][1] > (ambient_conc + error) ~ "error",
        TRUE ~ "ok"
      ),
      .groups = "drop"
    ) |>
    unnest({{f_fluxid}})

  slopes_df <- slopes_df |>
    left_join(quality_par_start, by = dplyr::join_by({{f_fluxid}}))

  if (fit_type == "exponential") {
    quality_flag <- flux_quality_exp(
      slopes_df,
      {{conc_col}},
      {{f_fluxid}},
      {{f_slope}},
      {{f_time}},
      {{f_fit}},
      {{f_cut}},
      {{f_b}},
      force_discard = force_discard,
      force_ok = force_ok,
      rmse_threshold = rmse_threshold,
      cor_threshold = cor_threshold,
      b_threshold = b_threshold
    )
  }


  if (fit_type %in% c("linear", "quadratic")) {
    quality_flag <- flux_quality_lm(slopes_df,
      {{conc_col}},
      {{f_fluxid}},
      {{f_slope}},
      {{f_cut}},
      {{f_pvalue}},
      {{f_rsquared}},
      force_discard = force_discard,
      force_ok = force_ok,
      pvalue_threshold = pvalue_threshold,
      rsquared_threshold = rsquared_threshold,
      name_df = name_df
    )
  }


  flag_count <- flux_flag_count(
    quality_flag,
    cut_arg = cut_arg
  )

  flag_msg <- flag_count |>
    mutate(
      message = paste(
        "\n", .data$f_quality_flag, "\t", .data$n,
        "\t", round(.data$ratio, 2) * 100, "%"
      )
    ) |>
    pull(message)

  message(paste("\n", "Total number of measurements:", sum(flag_count$n)))
  message(flag_msg)

  attr(quality_flag, "fit_type") <- {{fit_type}}

  quality_flag
}
