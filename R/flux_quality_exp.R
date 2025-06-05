#' quality assessment for the slopes estimated by flux_fitting
#' @description indicates if fluxes should be discarded or replaced by 0
#' according to parameters set by user.
#' flux_quality_lm is for the model of the lm family.
#' flux_quality_exp is for the exponential model.
#' @param slopes_df dataset containing slopes, fluxID,
#' and parameters of the exponential expression
#' @param f_conc column with gas concentration
#' @param f_fluxid column of ID for each measurement
#' @param f_slope column containing the slope of each flux
#' (as calculated by the \link[fluxible:flux_fitting]{flux_fitting} function)
#' @param f_slope_lm column containing the linear slope of each flux
#' (as calculated by the \link[fluxible:flux_fitting]{flux_fitting} function)
#' @param f_time column containing the time of each measurement in seconds
#' @param f_fit column containing the modeled data
#' @param f_b column containing the b parameter of the exponential expression
#' @param force_discard vector of fluxIDs that should be discarded
#' by the user's decision
#' @param force_ok vector of fluxIDs for which the user wants to keep
#' the calculated slope despite a bad quality flag
#' @param force_zero vector of fluxIDs that should be replaced by zero by
#' the user's decision
#' @param force_lm vector of fluxIDs for which the linear slope should be used
#' by the user's decision
#' @param rmse_threshold threshold for the RMSE of each flux above
#' which the fit is considered unsatisfactory
#' @param cor_threshold threshold for the correlation coefficient
#' of gas concentration with time below which the correlation
#' is considered non significant
#' @param b_threshold threshold for the b parameter. Defines a window
#' with its opposite inside which the fit is considered good enough.
#' @param gfactor_threshold threshold for the g-factor. Defines a window
#' with its opposite outside which the flux will be flagged `discard`.
#' @param name_df name of `slopes_df`
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when group_by rowwise summarise ungroup
#' @importFrom tidyr nest unnest
#' @importFrom stats cor



flux_quality_exp <- function(slopes_df,
                             f_conc,
                             f_fluxid,
                             f_slope,
                             f_time,
                             f_fit,
                             f_slope_lm,
                             f_b,
                             force_discard,
                             force_ok,
                             force_zero,
                             force_lm,
                             gfactor_threshold,
                             rmse_threshold,
                             cor_threshold,
                             b_threshold,
                             name_df) {


  args_ok <- flux_fun_check(list(
    gfactor_threshold = gfactor_threshold,
    rmse_threshold = rmse_threshold,
    cor_threshold = cor_threshold,
    b_threshold = b_threshold
  ),
  fn = list(is.numeric, is.numeric, is.numeric, is.numeric),
  msg = rep("has to be numeric", 4))

  slopes_df_check <- slopes_df |>
    select({{f_b}}, {{f_slope_lm}})

  slopes_df_ok <- flux_fun_check(slopes_df_check,
                                 fn = list(is.numeric, is.numeric),
                                 msg = rep("has to be numeric", 2),
                                 name_df = name_df)


  if (any(!c(args_ok, slopes_df_ok)))
    stop("Please correct the arguments", call. = FALSE)



  quality_par <- slopes_df |>
    drop_na({{f_conc}}) |>
    group_by({{f_fluxid}}) |>
    summarise(
      f_cor_coef = cor({{f_conc}}, {{f_time}}),
      f_RMSE =
        sqrt((1 / length({{f_time}})) * sum(({{f_fit}} - {{f_conc}})^2)),
      .groups = "drop"
    )



  quality_flag <- slopes_df |>
    left_join(quality_par, by = join_by(
      {{f_fluxid}} == {{f_fluxid}}
    )
    ) |>
    mutate(
      f_gfactor = {{f_slope}} / {{f_slope_lm}},
      f_fit_quality = case_when(
        abs({{f_b}}) >= b_threshold ~ "bad",
        .data$f_RMSE > rmse_threshold ~ "bad"
      ),
      f_correlation = case_when(
        abs(.data$f_cor_coef) < cor_threshold ~ "no",
        TRUE ~ "yes"
      ),
      f_quality_flag = case_when(
        {{f_fluxid}} %in% force_discard ~ "force_discard",
        {{f_fluxid}} %in% force_ok ~ "force_ok",
        {{f_fluxid}} %in% force_zero ~ "force_zero",
        {{f_fluxid}} %in% force_lm ~ "force_lm",
        .data$f_flag_ratio == "no_data" ~ "no_data",
        .data$f_flag_ratio == "too_low" ~ "discard",
        .data$f_start_error == "error" ~ "start_error",
        is.na({{f_slope}}) ~ "no_slope",
        abs(.data$f_gfactor) > gfactor_threshold  &
          abs({{f_slope_lm}}) > abs(.data$f_min_slope) ~ "discard",
        abs(.data$f_gfactor) > gfactor_threshold  &
          abs({{f_slope_lm}}) <= abs(.data$f_min_slope) ~ "zero",
        .data$f_fit_quality == "bad" &
          .data$f_correlation == "yes" &
          abs({{f_slope_lm}}) > abs(.data$f_min_slope) ~ "discard",
        .data$f_fit_quality == "bad" &
          .data$f_correlation == "yes" &
          abs({{f_slope_lm}}) <= abs(.data$f_min_slope) ~ "zero",
        .data$f_fit_quality == "bad" &
          .data$f_correlation == "no" ~ "zero",
        .data$f_RMSE <= rmse_threshold ~ "ok"
      ),
      f_slope_corr = case_when(
        .data$f_quality_flag == "no_data" ~ NA,
        .data$f_quality_flag == "no_slope" ~ NA,
        .data$f_quality_flag == "force_discard" ~ NA,
        .data$f_quality_flag == "force_ok" ~ {{f_slope}},
        .data$f_quality_flag == "force_zero" ~ 0,
        .data$f_quality_flag == "force_lm" ~ {{f_slope_lm}},
        .data$f_quality_flag == "start_error" ~ NA,
        .data$f_quality_flag == "discard" ~ NA,
        .data$f_quality_flag == "zero" ~ 0,
        .data$f_quality_flag == "ok" ~ {{f_slope}}
      )
    ) |>
    select(!c("f_n_conc", "f_flag_ratio", "f_start_error",
              "f_fit_quality", "f_correlation"))

  quality_flag
}
