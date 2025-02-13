#' Fitting a model to the gas concentration curve and estimating the slope
#' over time, using the exponential model from Zhao et al (2018)
#' @references Zhao, P., Hammerle, A., Zeeman, M., Wohlfahrt, G., 2018.
#' On the calculation of daytime CO2 fluxes measured by automated closed
#' transparent chambers. Agricultural and Forest Meteorology 263, 267–275.
#' https://doi.org/10.1016/j.agrformet.2018.08.022
#' @description Fits an exponential expression to the concentration evolution
#' @param conc_df dataframe of gas concentration over time
#' @param conc_col column with gas concentration
#' @param datetime_col column with datetime of each concentration measurement
#' Note that if there are duplicated datetime in the same f_fluxid only
#' the first row will be kept
#' @param f_start column with datetime when the measurement started
#' @param f_end column with datetime when the measurement ended
#' @param f_fluxid column with ID of each flux
#' @param t_window enlarge focus window before and after tmin and tmax
#' @param cz_window window used to calculate Cz, at the beginning of cut window
#' @param b_window window to estimate b. It is an interval after tz
#' where it is assumed that C fits the data perfectly
#' @param a_window window at the end of the flux to estimate a
#' @param roll_width width of the rolling mean for CO2 when looking for tz,
#' ideally same as cz_window
#' @param start_cut time to discard at the start of the measurements
#' (in seconds)
#' @param end_cut time to discard at the end of the measurements (in seconds)
#' @return a dataframe with the slope at t zero,
#' modeled concentration over time and exponential expression parameters
#' @importFrom rlang .data
#' @importFrom dplyr rename mutate select group_by case_when
#' ungroup filter distinct left_join rowwise summarize pull slice
#' @importFrom tidyr pivot_wider drop_na nest unnest
#' @importFrom haven as_factor
#' @importFrom stringr str_c
#' @importFrom stats lm optim
#' @importFrom purrr map
#' @importFrom utils data
#' @importFrom broom tidy



flux_fitting_exp <- function(conc_df,
                             conc_col,
                             datetime_col,
                             f_start,
                             f_end,
                             f_fluxid,
                             t_window,
                             cz_window,
                             b_window,
                             a_window,
                             roll_width,
                             start_cut,
                             end_cut) {

  args_ok <- flux_fun_check(list(
    t_window = t_window,
    cz_window = cz_window,
    b_window = b_window,
    a_window = a_window,
    roll_width = roll_width
  ),
  fn = list(
    is.numeric,
    is.numeric,
    is.numeric,
    is.numeric,
    is.numeric
  ),
  msg = rep("has to be numeric", 5))

  if (any(!args_ok))
    stop("Please correct the arguments", call. = FALSE)


  message("Cutting measurements...")

  name_conc <- names(select(conc_df, {{conc_col}}))

  conc_df <- conc_df |>
    mutate(
      f_time = difftime({{datetime_col}}[seq_along({{datetime_col}})],
        {{datetime_col}}[1],
        units = "secs"
      ),
      f_time = as.double(.data$f_time),
      {{f_start}} := {{f_start}} + start_cut,
      {{f_end}} := {{f_end}} - end_cut,
      f_cut = case_when(
        {{datetime_col}} < {{f_start}} | {{datetime_col}} >= {{f_end}}
        ~ "cut",
        TRUE ~ "keep"
      ),
      f_cut = as_factor(.data$f_cut),
      f_n_conc = sum(!is.na(.data[[name_conc]])),
      .by = {{f_fluxid}}
    )

  conc_df_cut <- conc_df |>
    filter(
      .data$f_cut == "keep"
    ) |>
    drop_na({{conc_col}}) |>
    mutate(
      f_time_cut = difftime({{datetime_col}}[seq_along({{datetime_col}})],
        {{datetime_col}}[1],
        units = "secs"
      ),
      f_time_cut = as.double(.data$f_time_cut),
      f_length_window = max(.data$f_time_cut),
      f_length_flux = difftime({{f_end}}, {{f_start}}, units = "sec"),
      f_time_diff = .data$f_time - .data$f_time_cut,
      f_n_conc_cut = sum(!is.na(.data[[name_conc]])),
      .by = {{f_fluxid}}
    )

  message("Estimating starting parameters for optimization...")



  cm_temp_min <- conc_df_cut |>
    group_by({{f_fluxid}}) |>
    select({{f_fluxid}}, {{conc_col}}, "f_time_cut") |>
    distinct(.data[[name_conc]], .keep_all = TRUE) |>
    dplyr::slice(which.min(.data[[name_conc]])) |>
    rename(
      Cmin = {{conc_col}},
      tmin = "f_time_cut"
    ) |>
    ungroup()

  cm_temp_max <- conc_df_cut |>
    group_by({{f_fluxid}}) |>
    select({{f_fluxid}}, {{conc_col}}, "f_time_cut") |>
    distinct(.data[[name_conc]], .keep_all = TRUE) |>
    dplyr::slice(which.max(.data[[name_conc]])) |>
    rename(
      Cmax = {{conc_col}},
      tmax = "f_time_cut"
    ) |>
    ungroup()

  cm_temp <- left_join(cm_temp_max, cm_temp_min,
    by = dplyr::join_by({{f_fluxid}})
  )



  cm_slope <- conc_df_cut |>
    group_by({{f_fluxid}}) |>
    nest() |>
    mutate(
      model_Cm =
        map(.x = data, \(.x) lm(.x[[name_conc]] ~ f_time_cut, data = .x)),
      tidy = map(.data$model_Cm, broom::tidy)
    ) |>
    unnest("tidy") |>
    filter(.data$term == "f_time_cut") |>
    rename(slope_Cm = "estimate") |>
    unnest({{f_fluxid}}) |>
    select({{f_fluxid}}, "slope_Cm")



  cm_df <- left_join(cm_temp, cm_slope, by = dplyr::join_by({{f_fluxid}})) |>
    mutate(
      f_Cm_est = case_when(
        .data$slope_Cm < 0 ~ .data$Cmin,
        .data$slope_Cm > 0 ~ .data$Cmax
      ),
      tm = case_when(
        .data$slope_Cm < 0 ~ .data$tmin,
        .data$slope_Cm > 0 ~ .data$tmax
      ),
      .by = {{f_fluxid}}
    ) |>
    select({{f_fluxid}}, "f_Cm_est", "tm", "slope_Cm")


  cz_df <- conc_df_cut |>
    filter(
      .data$f_time_cut <= cz_window
    ) |>
    group_by({{f_fluxid}}) |>
    nest() |>
    mutate(
      model_Cz =
        map(.x = data, \(.x) lm(.x[[name_conc]] ~ f_time_cut, data = .x)),
      tidy = map(.data$model_Cz, broom::tidy)
    ) |>
    unnest("tidy") |>
    filter(.data$term == "(Intercept)") |>
    rename(f_Cz = "estimate") |>
    unnest({{f_fluxid}}) |>
    select({{f_fluxid}}, "f_Cz") |>
    ungroup()



  tz_df <- conc_df_cut |>
    left_join(cz_df, by = dplyr::join_by({{f_fluxid}})) |>
    group_by({{f_fluxid}}) |>
    filter(
      .data$f_time_cut < .data$f_length_window / 2
    ) |>
    mutate(
      conc_roll = zoo::rollmean(.data[[name_conc]],
        k = roll_width,
        fill = NA, align = "right"
      ),
      Cd = abs(.data$conc_roll - .data$f_Cz),
      minCd = min(.data$Cd, na.rm = TRUE),
      f_tz_est = min(.data$f_time_cut[.data$Cd == .data$minCd], na.rm = TRUE)
    ) |>
    ungroup() |>
    select({{f_fluxid}}, "f_tz_est") |>
    distinct()



  cb_df <- conc_df_cut |>
    left_join(tz_df, by = dplyr::join_by({{f_fluxid}})) |>
    group_by({{f_fluxid}}) |>
    mutate(
      diff = .data$f_time_cut - .data$f_tz_est + b_window
    ) |>
    distinct(.data$diff, .keep_all = TRUE) |>
    dplyr::slice(which.min(abs(.data$diff))) |>
    rename(f_Cb = {{conc_col}}) |>
    select({{f_fluxid}}, "f_Cb") |>
    ungroup()

  a_df <- conc_df_cut |>
    group_by({{f_fluxid}}) |>
    mutate(
      ta = .data$f_length_window - a_window,
      ta_diff = .data$f_time_cut - .data$ta
    ) |>
    distinct(.data$ta_diff, .keep_all = TRUE) |>
    dplyr::slice(which.min(abs(.data$ta_diff))) |>
    rename(Ca = {{conc_col}}) |>
    select({{f_fluxid}}, "ta", "Ca") |>
    ungroup()

  estimates_df <- left_join(cm_df, cz_df,
    by = dplyr::join_by({{f_fluxid}})
  ) |>
    left_join(tz_df, by = dplyr::join_by({{f_fluxid}})) |>
    left_join(a_df, by = dplyr::join_by({{f_fluxid}})) |>
    left_join(cb_df, by = dplyr::join_by({{f_fluxid}})) |>
    mutate(
      f_b_est = case_when(
        .data$f_Cb == .data$f_Cm_est ~ 0, # special case or flat flux
        .data$f_Cz == .data$f_Cm_est ~ 0, # special case or flat flux
        TRUE ~ log(
          abs((.data$f_Cb - .data$f_Cm_est) / (.data$f_Cz - .data$f_Cm_est))
        )
        * (1 / b_window),
      ),
      f_a_est = case_when(
        # f_tz_est = ta is a special case that is undefined
        .data$ta == .data$f_tz_est ~ 0,
        TRUE ~
          (.data$Ca - .data$f_Cm_est - (.data$f_Cz - .data$f_Cm_est)
           * exp(-.data$f_b_est * (.data$ta - .data$f_tz_est)))
          / (.data$ta - .data$f_tz_est)
      )
    )




  fc_myfn <- function(fc_time, fc_conc, par, fc_cz) {
    sqrt(
      (1 / length(fc_time))
      * sum((par[1] + par[2] * (fc_time - exp(par[4]))
             + (fc_cz - par[1])
             * exp(-par[3] * (fc_time - exp(par[4])))
             - fc_conc)^2)
    )
  }


  message("Optimizing fitting parameters...")

  fitting_par <- conc_df_cut |>
    left_join(estimates_df, by = dplyr::join_by({{f_fluxid}})) |>
    select(
      {{f_fluxid}}, "f_Cm_est", "f_a_est", "f_b_est", "f_tz_est",
      "f_Cz", "f_time_cut", {{conc_col}}, "f_time_diff"
    ) |>
    group_by(
      {{f_fluxid}}, .data$f_Cm_est, .data$f_a_est, .data$f_b_est,
      .data$f_tz_est, .data$f_Cz, .data$f_time_diff
    ) |>
    nest() |>
    rowwise() |>
    summarize(
      results = list(optim(
        par = c(
          .data$f_Cm_est, .data$f_a_est, .data$f_b_est,
          log(.data$f_tz_est)
        ),
        fn = fc_myfn, fc_conc = data[name_conc],
        fc_time = data$f_time_cut, fc_cz = .data$f_Cz
      )),
      f_Cm = .data$results$par[1],
      f_a = .data$results$par[2],
      f_b = .data$results$par[3],
      f_tz = exp(.data$results$par[4]), # we force tz to be positive
      f_slope = .data$f_a + .data$f_b * (.data$f_Cm - .data$f_Cz),
      .groups = "drop"
    ) |>
    select(!c("results", "f_Cm_est", "f_a_est",
              "f_b_est", "f_tz_est"))

  message("Calculating fits and slopes...")

  conc_fitting <- conc_df |>
    left_join(fitting_par, by = dplyr::join_by({{f_fluxid}})) |>
    mutate(
      f_fit = .data$f_Cm + .data$f_a *
        (.data$f_time - .data$f_tz - .data$f_time_diff)
      + (.data$f_Cz - .data$f_Cm)
      * exp(-.data$f_b * (.data$f_time - .data$f_tz - .data$f_time_diff)),
      f_fit_slope = .data$f_slope * (.data$f_time) + .data$f_Cz - .data$f_slope
      * (.data$f_tz + .data$f_time_diff),
      f_start_z = {{f_start}} + .data$f_tz,
      .by = {{f_fluxid}}
    ) |>
    select(!"f_time_diff")


  message("Done.")


  warning_msg <- conc_df |>
    left_join(conc_df_cut,
      by = dplyr::join_by(
        {{f_fluxid}} == {{f_fluxid}},
        "f_n_conc" == "f_n_conc",
        {{datetime_col}} == {{datetime_col}}
      )
    ) |> # we want f_n_conc after cut
    select({{f_fluxid}}, "f_n_conc", "f_n_conc_cut", "f_length_flux") |>
    distinct() |>
    mutate(
      low_data = paste(
        "\n", "fluxID", {{f_fluxid}}, ": slope was estimated on",
        .data$f_n_conc_cut, "points out of", .data$f_length_flux,
        "seconds"
      ),
      no_data = paste(
        "\n", "fluxID", {{f_fluxid}},
        "dropped (no data in the conc column)"
      ),
      warnings = case_when(
        .data$f_n_conc == 0 ~ .data$no_data,
        .data$f_n_conc_cut != .data$f_length_flux ~ .data$low_data
      ),
      warnings = as.character(.data$warnings)
    ) |>
    drop_na(warnings) |>
    pull(.data$warnings)

  warnings <- str_c(warning_msg)

  if (any(!is.na(warnings))) warning(warnings)


  conc_fitting
}
