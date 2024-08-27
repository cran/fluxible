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
#' @param fluxid_col column containing unique IDs for each flux
#' @param slope_col column containing the slope of each flux
#' (as calculated by the flux_fitting function)
#' @param weird_fluxes_id vector of fluxIDs that should be discarded
#' by the user's decision
#' @param force_ok_id vector of fluxIDs for which the user wants to keep
#' the calculated slope despite a bad quality flag
#' @param ratio_threshold ratio of gas concentration data points over length of
#' measurement (in seconds) below which the measurement will be considered as
#' not having enough data points to be considered for calculations
#' @param pvalue_col column containing the p-value of each flux
#' (linear and quadratic fit)
#' @param rsquared_col column containing the r squared of each flux
#' (linear and quadratic fit)
#' @param pvalue_threshold threshold of p-value below which the change of
#' gas concentration over time is considered not significant
#' (linear and quadratic fit)
#' @param rsquared_threshold threshold of r squared value below which
#' the linear model is considered an unsatisfactory fit
#' (linear and quadratic fit)
#' @param conc_col column containing the measured gas concentration
#' (exponential fit)
#' @param b_col column containing the b parameter of the exponential expression
#' (exponential fit)
#' @param time_col column containing the time of each measurement in seconds
#' (exponential fit)
#' @param fit_col column containing the modeled data (exponential fit)
#' @param rmse_threshold threshold for the RMSE of each flux above which
#' the fit is considered unsatisfactory (exponential fit)
#' @param cor_threshold threshold for the correlation coefficient of
#' gas concentration with time below which the correlation
#' is considered not significant (exponential fit)
#' @param cut_col column containing the cutting information
#' @param cut_arg argument defining that the data point should be cut out
#' @param b_threshold threshold for the b parameter.
#' Defines a window with its opposite inside which the fit is
#' considered good enough (exponential fit)
#' @return same dataframe with added quality flags and corrected slope column
#' @importFrom dplyr mutate case_when rename group_by rowwise summarise ungroup
#' @importFrom tidyr nest unnest
#' @importFrom stats cor
#' @examples
#' data(slopes0lin)
#' flux_quality(slopes0lin, fit_type = "li")
#' data(slopes30)
#' flux_quality(slopes30, fit_type = "expo", slope_col = "f_slope")
#' @export

flux_quality <- function(slopes_df,
                         fit_type = c(),
                         ambient_conc = 421,
                         error = 100,
                         fluxid_col = "f_fluxID",
                         slope_col = "f_slope",
                         weird_fluxes_id = c(),
                         force_ok_id = c(),
                         ratio_threshold = 0,
                         pvalue_col = "f_pvalue",
                         rsquared_col = "f_rsquared",
                         pvalue_threshold = 0.3,
                         rsquared_threshold = 0.7,
                         conc_col = "f_conc",
                         b_col = "f_b",
                         time_col = "f_time",
                         fit_col = "f_fit",
                         cut_col = "f_cut",
                         rmse_threshold = 25,
                         cor_threshold = 0.5,
                         b_threshold = 1,
                         cut_arg = "cut") {
  slopes_df <- slopes_df |>
    rename(
      f_fluxID = all_of(((fluxid_col))),
      f_slope = all_of(((slope_col))),
      f_conc = all_of(((conc_col))),
      f_time = all_of(((time_col))),
      f_fit = all_of(((fit_col))),
      f_cut = all_of(((cut_col)))
    )

  fit_type <- flux_fit_type(
    slopes_df,
    fit_type = ((fit_type))
  )

  slopes_df <- slopes_df |>
    group_by(.data$f_fluxID, .data$f_cut) |>
    mutate(
      f_n_conc = sum(!is.na(.data$f_conc)),
      f_ratio = .data$f_n_conc / as.double((difftime(
        .data$f_end, .data$f_start,
        units = "secs"
      ))),
      f_flag_ratio = case_when(
        .data$f_ratio == 0 ~ "no_data",
        .data$f_ratio <= ((ratio_threshold)) ~ "too_low",
        TRUE ~ "ok"
      )
    ) |>
    ungroup()

  quality_par_start <- slopes_df |>
    # for the start error we take the entire flux into account
    group_by(.data$f_fluxID) |>
    nest() |>
    rowwise() |>
    summarise(
      f_start_error = case_when(
        data$f_conc[1] < (((ambient_conc)) - ((error))) ~ "error",
        data$f_conc[1] > (((ambient_conc)) + ((error))) ~ "error",
        TRUE ~ "ok"
      ),
      .groups = "drop"
    ) |>
    unnest("f_fluxID")

  slopes_df <- slopes_df |>
    left_join(quality_par_start, by = "f_fluxID")

  if (((fit_type)) == "exponential") {
    quality_flag <- flux_quality_exp(
      ((slopes_df)),
      weird_fluxes_id = ((weird_fluxes_id)),
      force_ok_id = ((force_ok_id)),
      b_col = ((b_col)),
      rmse_threshold = ((rmse_threshold)),
      cor_threshold = ((cor_threshold)),
      b_threshold = ((b_threshold))
    )
  }


  if (((fit_type)) %in% c("linear", "quadratic")) {
    quality_flag <- flux_quality_lm(
      ((slopes_df)),
      weird_fluxes_id = ((weird_fluxes_id)),
      force_ok_id = ((force_ok_id)),
      pvalue_col = ((pvalue_col)),
      rsquared_col = ((rsquared_col)),
      pvalue_threshold = ((pvalue_threshold)),
      rsquared_threshold = ((rsquared_threshold))
    )
  }


  flag_count <- flux_flag_count(
    ((quality_flag)),
    cut_arg = ((cut_arg))
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

  attr(quality_flag, "fit_type") <- ((fit_type))

  quality_flag
}
