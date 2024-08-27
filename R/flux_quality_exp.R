#' quality assessment for the slopes estimated by flux_fitting
#' @description indicates if fluxes should be discarded or replaced by 0
#' according to parameters set by user.
#' flux_quality_lm is for the model of the lm family.
#' flux_quality_exp is for the exponential model.
#' @param slopes_df dataset containing slopes, fluxID,
#' and parameters of the exponential expression
#' @param b_col column containing the b parameter of the exponential expression
#' @param weird_fluxes_id vector of fluxIDs that should be discarded
#' by the user's decision
#' @param force_ok_id vector of fluxIDs for which the user wants to keep
#' the calculated slope despite a bad quality flag
#' @param rmse_threshold threshold for the RMSE of each flux above
#' which the fit is considered unsatisfactory
#' @param cor_threshold threshold for the correlation coefficient
#' of gas concentration with time below which the correlation
#' is considered non significant
#' @param b_threshold threshold for the b parameter. Defines a window
#' with its opposite inside which the fit is considered good enough.
#' @return same dataframe with added flag and corrected slopes columns
#' @importFrom dplyr mutate case_when rename group_by rowwise summarise ungroup
#' @importFrom tidyr nest unnest
#' @importFrom stats cor



flux_quality_exp <- function(slopes_df,
                             weird_fluxes_id = c(),
                             force_ok_id = c(),
                             b_col = "f_b",
                             rmse_threshold = 25,
                             cor_threshold = 0.5,
                             b_threshold = 1) {
  slopes_df <- slopes_df |>
    rename(
      f_b = all_of(((b_col)))
    )


  quality_par <- slopes_df |>
    group_by(.data$f_fluxID, .data$f_cut) |>
    nest() |>
    rowwise() |>
    summarise(
      f_cor_coef = cor(data$f_conc, data$f_time),
      f_RMSE = sqrt((1 / length(data$f_time))
                    * sum((data$f_fit - data$f_conc)^2)),
      .groups = "drop"
    ) |>
    unnest(c("f_fluxID", "f_cut"))



  quality_flag <- slopes_df |>
    left_join(quality_par, by = c("f_fluxID", "f_cut")) |>
    mutate(
      f_fit_quality = case_when(
        .data$f_b >= ((b_threshold)) ~ "bad_b",
        .data$f_RMSE > ((rmse_threshold)) ~ "bad_RMSE"
      ),
      f_correlation = case_when(
        abs(.data$f_cor_coef) < ((cor_threshold)) ~ "no",
        TRUE ~ "yes"
      ),
      f_quality_flag = case_when(
        .data$f_flag_ratio == "no_data" ~ "no_data",
        .data$f_flag_ratio == "too_low" ~ "discard",
        .data$f_fluxID %in% ((weird_fluxes_id)) ~ "weird_flux",
        .data$f_fluxID %in% ((force_ok_id)) ~ "force_ok",
        .data$f_start_error == "error" ~ "start_error",
        .data$f_fit_quality == "bad_b" &
          .data$f_correlation == "yes" ~ "discard",
        .data$f_fit_quality == "bad_b" &
          .data$f_correlation == "no" ~ "zero",
        .data$f_fit_quality == "bad_RMSE" &
          .data$f_correlation == "yes" ~ "discard",
        .data$f_fit_quality == "bad_RMSE" &
          .data$f_correlation == "no" ~ "zero",
        TRUE ~ "ok"
      ),
      f_slope_corr = case_when(
        .data$f_quality_flag == "no_data" ~ NA_real_,
        .data$f_quality_flag == "weird_flux" ~ NA_real_,
        .data$f_quality_flag == "force_ok" ~ .data$f_slope,
        .data$f_quality_flag == "start_error" ~ NA_real_,
        .data$f_quality_flag == "discard" ~ NA_real_,
        .data$f_quality_flag == "zero" ~ 0,
        .data$f_quality_flag == "ok" ~ .data$f_slope
      )
    )

  quality_flag
}
