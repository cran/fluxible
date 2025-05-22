#' quality assessment for the slopes estimated by flux_fitting
#' @description indicates if fluxes should be discarded or replaced by 0
#' according to parameters set by user.
#' flux_quality_lm is for the model of the lm family.
#' flux_quality_exp is for the exponential model.
#' @param slopes_df dataset containing slopes, fluxID, p.value and r.squared
#' @param f_fluxid column of ID for each measurement
#' @param f_slope column containing the slope of each flux
#' (as calculated by the \link[fluxible:flux_fitting]{flux_fitting} function)
#' @param f_slope_lm column containing the linear slope of each flux
#' (as calculated by the \link[fluxible:flux_fitting]{flux_fitting} function)
#' @param pvalue_threshold threshold of p-value below which the change
#' of gas concentration over time is considered not significant (user decided)
#' @param rsquared_threshold threshold of r squared value below which
#' the linear model is considered an unsatisfactory fit
#' @param force_discard vector of fluxIDs that should be discarded
#' by the user's decision
#' @param force_ok vector of fluxIDs for which the user wants to keep
#' the calculated slope despite a bad quality flag
#' @param force_zero vector of fluxIDs that should be replaced by zero by
#' the user's decision
#' @param force_lm vector of fluxIDs for which the linear slope should be used
#' by the user's decision
#' @param f_pvalue column containing the p-value of each flux
#' @param f_rsquared column containing the r squared to be used for
#' the quality assessment
#' @param name_df name of slopes_df (used for error message)
#' @param gfactor_threshold threshold for the g-factor. Defines a window
#' with its opposite outside which the flux will be flagged `discard`.
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when left_join



flux_quality_qua <- function(slopes_df,
                             f_fluxid,
                             f_slope,
                             f_pvalue,
                             f_rsquared,
                             f_slope_lm,
                             force_discard,
                             force_ok,
                             force_zero,
                             force_lm,
                             gfactor_threshold,
                             pvalue_threshold,
                             rsquared_threshold,
                             name_df) {



  args_ok <- flux_fun_check(list(
    pvalue_threshold = pvalue_threshold,
    rsquared_threshold = rsquared_threshold
  ),
  fn = list(is.numeric, is.numeric),
  msg = rep("has to be numeric", 2))

  slopes_df_check <- slopes_df |>
    select(
      {{f_pvalue}},
      {{f_rsquared}}
    )

  slopes_df_ok <- flux_fun_check(slopes_df_check,
    fn = list(
      is.numeric,
      is.numeric
    ),
    msg = rep("has to be numeric", 2),
    name_df = name_df
  )


  if (any(!c(args_ok, slopes_df_ok)))
    stop("Please correct the arguments", call. = FALSE)



  slopes_df <- slopes_df |>
    mutate(
      f_gfactor = {{f_slope}} / {{f_slope_lm}},
      f_quality_flag = case_when(
        {{f_fluxid}} %in% force_discard ~ "force_discard",
        {{f_fluxid}} %in% force_ok ~ "force_ok",
        {{f_fluxid}} %in% force_zero ~ "force_zero",
        {{f_fluxid}} %in% force_lm ~ "force_lm",
        .data$f_flag_ratio == "no_data" ~ "no_data",
        .data$f_flag_ratio == "too_low" ~ "discard",
        .data$f_start_error == "error" ~ "start_error",
        abs(.data$f_gfactor) > gfactor_threshold &
          abs({{f_slope_lm}}) > abs(.data$f_min_slope) ~ "discard",
        abs(.data$f_gfactor) > gfactor_threshold &
          abs({{f_slope_lm}}) <= abs(.data$f_min_slope) ~ "zero",
        {{f_rsquared}} >= rsquared_threshold ~ "ok",
        {{f_rsquared}} < rsquared_threshold &
          {{f_pvalue}} >= pvalue_threshold ~ "zero",
        {{f_rsquared}} < rsquared_threshold &
          {{f_pvalue}} < pvalue_threshold &
          abs({{f_slope_lm}}) > abs(.data$f_min_slope) ~ "discard",
        {{f_rsquared}} < rsquared_threshold &
          {{f_pvalue}} < pvalue_threshold &
          abs({{f_slope_lm}}) <= abs(.data$f_min_slope) ~ "zero"
      ),
      f_slope_corr = case_when(
        .data$f_quality_flag == "no_data" ~ NA,
        .data$f_quality_flag == "force_discard" ~ NA,
        .data$f_quality_flag == "force_ok" ~ {{f_slope}},
        .data$f_quality_flag == "force_zero" ~ 0,
        .data$f_quality_flag == "force_lm" ~ {{f_slope_lm}},
        .data$f_quality_flag == "ok" ~ {{f_slope}},
        .data$f_quality_flag == "discard" ~ NA,
        .data$f_quality_flag == "zero" ~ 0
      ),
      .by = c({{f_fluxid}})
    ) |>
    select(!c("f_n_conc", "f_flag_ratio", "f_start_error"))

  slopes_df
}
