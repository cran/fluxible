#' From raw gas concentration over time to clean fluxes
#' @description Wrapper function for the Fluxible workflow. We recommand using
#' the step-by-step workflow for more control over the process.
#' @param raw_conc dataframe of CO2 concentration measured continuously.
#' Has to contain at least a datetime column in ymd_hms format and
#' a gas concentration column as double.
#' @param field_record dataframe recording which measurement happened when.
#' Has to contain at least a column containing the start of each measurement,
#' and any other column identifying the measurements.
#' @param startcrop how many seconds should be discarded at the beginning of
#' the measurement
#' @param measurement_length length of the measurement (in seconds)
#' from the start specified in the field_record
#' @param ratio_threshold ratio (number of concentration measurement compared to
#' length of measurement in seconds) below which the data should be flagged as
#' too little
#' @param time_diff time difference (in seconds) between the two datasets.
#' Will be added to the datetime column of the raw_conc dataset.
#' For situations where the time was not synchronized correctly.
#' @param datetime_col datetime column in raw_conc (dmy_hms format)
#' @param conc_col concentration column in raw_conc
#' @param start_col start column in field_record (dmy_hms format)
#' @param end_col end columne in field_record (`ymd_hms` format)
#' @param fixed_length if `TRUE` (default), the `measurement_length` is used to
#' create the end column. If `FALSE`, `end_col` has to be provided.
#' @param fit_type `exp_zhao18`, `exp_tz`, `quadratic` or `linear.`
#' `exp_zhao18` is using the exponential model
#' `C(t) = C_m + a (t - t_z) + (C_z - C_m) exp(-b (t - t_z))`
#' from Zhao et al (2018).
#' `expt_tz` is a modified version which allows the user to fix `t_zero`:
#' `C(t) = C_m + a * t + (C_z - C_m) exp(-b * t)`.
#' @references Zhao, P., Hammerle, A., Zeeman, M., Wohlfahrt, G., 2018.
#' On the calculation of daytime CO2 fluxes measured by automated closed
#' transparent chambers. Agricultural and Forest Meteorology 263, 267–275.
#' https://doi.org/10.1016/j.agrformet.2018.08.022
#' `exponential` is equal to `exp_zhao18`, for backwards compatibility
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
#' @param t_zero time at which the slope should be calculated
#' (for `quadratic` and `exp_tz` fits)
#' @param ambient_conc ambient gas concentration in ppm at the site of
#' measurement (used to detect measurement that started with a polluted setup)
#' @param error error of the setup, defines a window outside of which
#' the starting values indicate a polluted setup
#' @param force_discard vector of fluxIDs that should be discarded
#' by the user's decision
#' @param force_ok vector of fluxIDs for which the user wants to keep
#' the calculated slope despite a bad quality flag
#' @param force_zero vector of fluxIDs that should be replaced by zero by
#' the user's decision
#' @param ratio_threshold ratio of gas concentration data points over length of
#' measurement (in seconds) below which the measurement will be considered as
#' not having enough data points to be considered for calculations
#' @param pvalue_threshold threshold of p-value below which the change of
#' gas concentration over time is considered not significant
#' (linear and quadratic fit)
#' @param rsquared_threshold threshold of r squared value below which
#' the linear model is considered an unsatisfactory fit
#' (linear and quadratic fit)
#' @param rmse_threshold threshold for the RMSE of each flux above which
#' the fit is considered unsatisfactory (exponential fit)
#' @param cor_threshold threshold for the correlation coefficient of
#' gas concentration with time below which the correlation
#' is considered not significant (exponential fit)
#' @param b_threshold threshold for the b parameter.
#' Defines a window with its opposite inside which the fit is
#' considered good enough (exponential fit)
#' @param slope_correction logical. If `TRUE`, the flux will be calculated with
#' the slope corrected according to the recommandations of the quality flags.
#' @param conc_unit unit in which the concentration of gas was measured
#' `ppm` or `ppb`
#' @param flux_unit unit in which the calculated flux will be
#' `mmol` outputs fluxes in mmol * m^(-2)*h^(-1);
#' `micromol` outputs fluxes in micromol * m^(-2)*h^(-1)
#' @param chamber_volume volume of the flux chamber in L,
#' can also be a column in case it is a variable
#' @param tube_volume volume of the tubing in L,
#' can also be a column in case it is a variable
#' @param atm_pressure atmospheric pressure,
#' can be a constant (numerical) or a variable (column name)
#' @param plot_area area of the plot in m^2,
#' can also be a column in case it is a variable
#' @param cols_keep columns to keep from the input to the output.
#' Those columns need to have unique values for each flux,
#' as distinct() is applied.
#' @param cols_ave columns with values that should be averaged
#' for each flux in the output. Note that NA are removed in mean calculation.
#' @param cols_sum columns with values for which is sum is provided
#' for each flux in the output. Note that NA are removed in sum calculation.
#' @param cols_med columns with values for which is median is provided
#' for each flux in the output. Note that NA are removed in median calculation.
#' @param temp_air_col column containing the air temperature used
#' to calculate fluxes. Will be averaged with NA removed.
#' @param temp_air_unit units in which air temperature was measured.
#' Has to be either `celsius` (default), `fahrenheit` or `kelvin.`
#' @param cut if 'TRUE' (default), the measurements will be cut according to
#' 'f_cut' before calculating fluxes. This has no influence on the flux itself
#' since the slope is provided from \link[fluxible:flux_fitting]{flux_fitting},
#' but it will influence the values of the columns in `cols_ave`.
#' @return a dataframe containing flux IDs, datetime of measurements' starts,
#' fluxes in mmol*m^(-2)*h^(-1) or micromol*m^(-2)*h^(-1) (`f_flux`) according
#' to `flux_unit`, temperature average for each flux in Kelvin (`f_temp_ave`),
#' the total volume of the setup for each measurement (`f_volume_setup`),
#' the model used in \link[fluxible:flux_fitting]{flux_fitting},
#' any column specified in `cols_keep`, any column specified in `cols_ave` with
#' their value averaged over the measurement after cuts and discarding NA.
#' @examples
#' data(co2_df_short)
#' data(record_short)
#' stupeflux(
#' raw_conc = co2_df_short,
#' field_record = record_short,
#' datetime_col = datetime,
#' start_col = start,
#' conc_col = conc,
#' startcrop = 10,
#' measurement_length = 180,
#' fit_type = "exp_zhao18",
#' temp_air_col = temp_air,
#' conc_unit = "ppm",
#' flux_unit = "mmol",
#' chamber_volume = 24.5,
#' tube_volume = 0.075,
#' atm_pressure = 1,
#' plot_area = 0.0625
#' )
#' @export

stupeflux <- function(raw_conc,
                      field_record,
                      datetime_col,
                      start_col,
                      end_col,
                      conc_col,
                      startcrop,
                      measurement_length,
                      fit_type,
                      temp_air_col,
                      chamber_volume,
                      atm_pressure,
                      plot_area,
                      conc_unit,
                      flux_unit,
                      fixed_length = TRUE,
                      cols_keep = c(),
                      cols_ave = c(),
                      cols_sum = c(),
                      cols_med = c(),
                      tube_volume,
                      ratio_threshold = 0.5,
                      time_diff = 0,
                      start_cut = 0,
                      end_cut = 0,
                      cz_window = 15,
                      b_window = 10,
                      a_window = 10,
                      roll_width = 15,
                      t_zero = 0,
                      force_discard = c(),
                      force_ok = c(),
                      force_zero = c(),
                      ambient_conc = 421,
                      error = 100,
                      pvalue_threshold = 0.3,
                      rsquared_threshold = 0.7,
                      rmse_threshold = 25,
                      cor_threshold = 0.5,
                      b_threshold = 1,
                      temp_air_unit = "celsius",
                      cut = TRUE,
                      slope_correction = TRUE) {
  conc_df <- flux_match(
    raw_conc,
    field_record,
    datetime_col = {{datetime_col}},
    start_col = {{start_col}},
    conc_col = {{conc_col}},
    end_col = {{end_col}},
    fixed_length = fixed_length,
    startcrop = startcrop,
    measurement_length = measurement_length,
    ratio_threshold = ratio_threshold,
    time_diff = time_diff
  )

  conc_fitting <- flux_fitting(
    conc_df,
    {{conc_col}},
    {{datetime_col}},
    start_cut = start_cut,
    end_cut = end_cut,
    cz_window = cz_window,
    b_window = b_window,
    a_window = a_window,
    roll_width = roll_width,
    t_zero = t_zero,
    fit_type = fit_type
  )

  quality_flag <- flux_quality(
    conc_fitting,
    {{conc_col}},
    force_discard = force_discard,
    force_ok = force_ok,
    force_zero = force_zero,
    ratio_threshold = ratio_threshold,
    ambient_conc = ambient_conc,
    error = error,
    pvalue_threshold = pvalue_threshold,
    rsquared_threshold = rsquared_threshold,
    rmse_threshold = rmse_threshold,
    cor_threshold = cor_threshold,
    b_threshold = b_threshold
  )

  if (slope_correction == TRUE) {
    fluxes <- with(quality_flag, flux_calc(
      quality_flag,
      slope_col = f_slope_corr,
      {{datetime_col}},
      {{temp_air_col}},
      chamber_volume = chamber_volume,
      atm_pressure = atm_pressure,
      plot_area = plot_area,
      conc_unit = conc_unit,
      flux_unit = flux_unit,
      cols_keep = cols_keep,
      cols_ave = cols_ave,
      cols_sum = cols_sum,
      cols_med = cols_med,
      tube_volume = tube_volume,
      temp_air_unit = temp_air_unit,
      cut = cut
    ))
  }

  if (slope_correction == FALSE) {
    fluxes <- with(quality_flag, flux_calc(
      quality_flag,
      slope_col = f_slope,
      {{datetime_col}},
      {{temp_air_col}},
      chamber_volume = chamber_volume,
      atm_pressure = atm_pressure,
      plot_area = plot_area,
      conc_unit = conc_unit,
      flux_unit = flux_unit,
      cols_keep = cols_keep,
      cols_ave = cols_ave,
      tube_volume = tube_volume,
      temp_air_unit = temp_air_unit,
      cut = cut
    ))
  }

  fluxes

}
