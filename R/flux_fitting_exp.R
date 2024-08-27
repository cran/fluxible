#' Fitting a model to the gas concentration curve and estimating the slope
#' over time, using the exponential model from Zhao et al (2018)
#' @references Zhao, P., Hammerle, A., Zeeman, M., Wohlfahrt, G., 2018.
#' On the calculation of daytime CO2 fluxes measured by automated closed
#' transparent chambers. Agricultural and Forest Meteorology 263, 267–275.
#' https://doi.org/10.1016/j.agrformet.2018.08.022
#' @description Fits an exponential expression to the concentration evolution
#' @param conc_df dataframe of gas concentration over time
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
#' @param start_col column with datetime when the measurement started
#' @param end_col column with datetime when the measurement ended
#' @param datetime_col column with datetime of each concentration measurement
#' @param conc_col column with gas concentration data
#' @param fluxid_col column with ID of each flux
#' @return a dataframe with the slope at t zero,
#' modeled concentration over time and exponential expression parameters
#' @importFrom rlang .data
#' @importFrom dplyr rename all_of mutate select group_by case_when
#' ungroup filter distinct left_join rowwise summarize pull
#' @importFrom tidyr pivot_wider drop_na nest unnest
#' @importFrom haven as_factor
#' @importFrom stringr str_c
#' @importFrom stats lm optim
#' @importFrom purrr map
#' @importFrom utils data



flux_fitting_exp <- function(conc_df,
                             t_window = 20,
                             cz_window = 15,
                             b_window = 10,
                             a_window = 10,
                             roll_width = 15,
                             start_cut = 0,
                             end_cut = 0,
                             start_col = "f_start",
                             end_col = "f_end",
                             datetime_col = "f_datetime",
                             conc_col = "f_conc",
                             fluxid_col = "f_fluxID") {
  conc_df <- conc_df |>
    rename(
      f_start = all_of((start_col)),
      f_end = all_of((end_col)),
      f_datetime = all_of((datetime_col)),
      f_conc = all_of((conc_col)),
      f_fluxID = all_of((fluxid_col))
    )



  if (!is.double(t_window)) stop("t_window has to be a double")
  if (!is.double(cz_window)) stop("cz_window has to be a double")
  if (!is.double(b_window)) stop("b_window has to be a double")
  if (!is.double(a_window)) stop("a_window has to be a double")
  if (!is.double(roll_width)) stop("roll_width has to be a double")
  if (!is.double(start_cut)) stop("start_cut has to be a double")
  if (!is.double(end_cut)) stop("end_cut has to be a double")




  length_flux_max <- conc_df |>
    mutate(
      length_flux = difftime(.data$f_end, .data$f_start, units = "sec"),
      length_flux = as.double(.data$length_flux)
    ) |>
    select("length_flux") |>
    max()

  if ((start_cut + end_cut) >= length_flux_max) {
    stop(
      "You cannot cut more than the length of the measurements!"
    )
  }

  message("Cutting measurements...")

  conc_df <- conc_df |>
    group_by(.data$f_fluxID) |>
    mutate(
      f_time = difftime(.data$f_datetime[seq_along(.data$f_datetime)],
        .data$f_datetime[1],
        units = "secs"
      ),
      f_time = as.double(.data$f_time),
      f_start = .data$f_start + ((start_cut)),
      f_end = .data$f_end - ((end_cut)),
      f_cut = case_when(
        .data$f_datetime < .data$f_start | .data$f_datetime >= .data$f_end
        ~ "cut",
        TRUE ~ "keep"
      ),
      f_cut = as_factor(.data$f_cut),
      n_conc = sum(!is.na(.data$f_conc))
    ) |>
    ungroup()

  conc_df_cut <- conc_df |>
    filter(
      .data$f_cut == "keep"
    ) |>
    drop_na("f_conc") |>
    group_by(.data$f_fluxID) |>
    mutate(
      f_time_cut = difftime(.data$f_datetime[seq_along(.data$f_datetime)],
        .data$f_datetime[1],
        units = "secs"
      ),
      f_time_cut = as.double(.data$f_time_cut),
      length_window = max(.data$f_time_cut),
      length_flux = difftime(.data$f_end, .data$f_start, units = "sec"),
      time_diff = .data$f_time - .data$f_time_cut,
      n_conc_cut = sum(!is.na(.data$f_conc))
    ) |>
    ungroup()

  message("Estimating starting parameters for optimization...")

  cm_temp <- conc_df_cut |>
    group_by(.data$f_fluxID) |>
    distinct(.data$f_conc, .keep_all = TRUE) |>
    mutate(
      Cmax = max(.data$f_conc),
      Cmin = min(.data$f_conc),
      tmax = .data$f_time_cut[.data$f_conc == .data$Cmax],
      tmin = .data$f_time_cut[.data$f_conc == .data$Cmin]
    ) |>
    select("f_fluxID", "Cmax", "Cmin", "tmax", "tmin") |>
    ungroup() |>
    distinct(.data$Cmax, .data$Cmin, .keep_all = TRUE)

  cm_slope <- conc_df_cut |>
    group_by(.data$f_fluxID) |>
    nest() |>
    mutate(
      model_Cm = map(.data$data, \(d) {
        lm(f_conc ~ f_time_cut, data = d) |>
          broom::tidy() |>
          select("term", "estimate") |>
          pivot_wider(names_from = "term", values_from = "estimate")
      })
    ) |>
    unnest("model_Cm") |>
    rename(slope_Cm = "f_time_cut") |>
    select("f_fluxID", "slope_Cm") |>
    ungroup()

  cm_df <- left_join(cm_temp, cm_slope, by = "f_fluxID") |>
    mutate(
      Cm_est = case_when(
        # Cm is the max mixing point,
        # which is the lim C(t) when t tends to infinite.
        .data$slope_Cm < 0 ~ .data$Cmin,
        .data$slope_Cm > 0 ~ .data$Cmax
      ),
      tm = case_when(
        .data$slope_Cm < 0 ~ .data$tmin,
        .data$slope_Cm > 0 ~ .data$tmax
      )
    ) |>
    select("f_fluxID", "Cm_est", "tm", "slope_Cm") |>
    ungroup()

  cz_df <- conc_df_cut |>
    group_by(.data$f_fluxID) |>
    filter(
      .data$f_time_cut <= ((cz_window))
    ) |>
    nest() |>
    mutate(
      model_Cz = map(.data$data, \(d) {
        lm(f_conc ~ f_time_cut, data = d) |>
          broom::tidy() |>
          select("term", "estimate") |>
          pivot_wider(names_from = "term", values_from = "estimate")
      })
    ) |>
    unnest("model_Cz") |>
    rename(
      slope_Cz = "f_time_cut",
      f_Cz = "(Intercept)"
    ) |>
    select("f_fluxID", "slope_Cz", "f_Cz") |>
    ungroup()



  tz_df <- conc_df_cut |>
    left_join(cz_df, by = "f_fluxID") |>
    group_by(.data$f_fluxID) |>
    filter(
      # tz should be in the first half of the flux
      .data$f_time_cut < .data$length_window / 2
    ) |>
    mutate(
      conc_roll = zoo::rollmean(.data$f_conc,
        k = ((roll_width)),
        fill = NA, align = "right"
      ),
      Cd = abs(.data$conc_roll - .data$f_Cz),
      minCd = min(.data$Cd, na.rm = TRUE),
      tz_est = min(.data$f_time_cut[.data$Cd == .data$minCd], na.rm = TRUE)
    ) |>
    ungroup() |>
    select("f_fluxID", "tz_est") |>
    distinct()



  cb_df <- conc_df_cut |>
    left_join(tz_df, by = "f_fluxID") |>
    group_by(.data$f_fluxID) |>
    mutate(
      f_Cb = .data$f_conc[.data$f_time_cut == .data$tz_est - ((b_window))]
    ) |>
    ungroup() |>
    select("f_fluxID", "f_Cb") |>
    distinct()

  a_df <- conc_df_cut |>
    group_by(.data$f_fluxID) |>
    mutate(
      ta = .data$length_window - ((a_window)),
      Ca = .data$f_conc[.data$f_time_cut == .data$ta]
    ) |>
    ungroup() |>
    select("f_fluxID", "ta", "Ca") |>
    distinct()

  estimates_df <- left_join(cm_df, cz_df, by = "f_fluxID") |>
    left_join(tz_df, by = "f_fluxID") |>
    left_join(a_df, by = "f_fluxID") |>
    left_join(cb_df, by = "f_fluxID") |>
    mutate(
      b_est = case_when(
        .data$f_Cb == .data$Cm_est ~ 0, # special case or flat flux
        .data$f_Cz == .data$Cm_est ~ 0, # special case or flat flux
        TRUE ~ log(
          abs((.data$f_Cb - .data$Cm_est) / (.data$f_Cz - .data$Cm_est))
        )
        * (1 / ((b_window))),
      ),
      a_est = case_when(
        # tz_est = ta is a special case that is undefined
        .data$ta == .data$tz_est ~ 0,
        TRUE ~
          (.data$Ca - .data$Cm_est - (.data$f_Cz - .data$Cm_est)
           * exp(-.data$b_est * (.data$ta - .data$tz_est)))
          / (.data$ta - .data$tz_est)
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
    left_join(estimates_df, by = "f_fluxID") |>
    select(
      "f_fluxID", "Cm_est", "a_est", "b_est", "tz_est",
      "f_Cz", "f_time_cut", "f_conc", "time_diff"
    ) |>
    group_by(
      .data$f_fluxID, .data$Cm_est, .data$a_est, .data$b_est,
      .data$tz_est, .data$f_Cz, .data$time_diff
    ) |>
    nest() |>
    rowwise() |>
    summarize(
      results = list(optim(
        par = c(
          .data$Cm_est, .data$a_est, .data$b_est,
          log(.data$tz_est)
        ),
        fn = fc_myfn, fc_conc = data$f_conc,
        fc_time = data$f_time_cut, fc_cz = .data$f_Cz
      )),
      f_Cm = .data$results$par[1],
      f_a = .data$results$par[2],
      f_b = .data$results$par[3],
      f_tz = exp(.data$results$par[4]), # we force tz to be positive
      f_slope = .data$f_a + .data$f_b * (.data$f_Cm - .data$f_Cz),
      .groups = "drop"
    ) |>
    select(!"results")

  message("Calculating fits and slopes...")

  conc_fitting <- conc_df |>
    left_join(fitting_par, by = "f_fluxID") |>
    group_by(.data$f_fluxID) |>
    mutate(
      f_fit = .data$f_Cm + .data$f_a *
        (.data$f_time - .data$f_tz - .data$time_diff)
      + (.data$f_Cz - .data$f_Cm)
      * exp(-.data$f_b * (.data$f_time - .data$f_tz - .data$time_diff)),
      f_fit_slope = .data$f_slope * (.data$f_time) + .data$f_Cz - .data$f_slope
      * (.data$f_tz + .data$time_diff),
      f_start_z = .data$f_start + .data$f_tz
    ) |>
    ungroup()


  message("Done.")


  warning_msg <- conc_df |>
    left_join(conc_df_cut,
      by = c("f_fluxID", "n_conc", "f_datetime")
    ) |> # we want n_conc after cut
    select("f_fluxID", "n_conc", "n_conc_cut", "length_flux") |>
    distinct() |>
    mutate(
      low_data = paste(
        "\n", "fluxID", .data$f_fluxID, ": slope was estimated on",
        .data$n_conc_cut, "points out of", .data$length_flux,
        "seconds"
      ),
      no_data = paste(
        "\n", "fluxID", .data$f_fluxID,
        "dropped (no data in the conc column)"
      ),
      warnings = case_when(
        .data$n_conc == 0 ~ .data$no_data,
        .data$n_conc_cut != .data$length_flux ~ .data$low_data
      ),
      warnings = as.character(.data$warnings)
    ) |>
    drop_na(warnings) |>
    pull(.data$warnings)

  warnings <- str_c(warning_msg)

  if (any(!is.na(warnings))) warning(warnings)


  conc_fitting
}
