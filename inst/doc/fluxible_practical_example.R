## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----match--------------------------------------------------------------------
library(fluxible)

str(record_liahovden)
str(co2_liahovden)

conc_liahovden <- flux_match(
  raw_conc = co2_liahovden,
  field_record = record_liahovden,
  startcrop = 0,
  measurement_length = 220,
  ratio_threshold = 0.5,
  time_diff = 0,
  datetime_col = "datetime",
  conc_col = "conc",
  start_col = "start"
)
str(conc_liahovden)

## ----fitting_exp_qua_lin------------------------------------------------------
slopes_exp_liahovden <- flux_fitting(
  conc_df = conc_liahovden,
  start_cut = 0,
  end_cut = 0,
  start_col = "f_start",
  end_col = "f_end",
  datetime_col = "f_datetime",
  conc_col = "f_conc",
  fluxid_col = "f_fluxID",
  t_window = 20,
  cz_window = 15,
  b_window = 10,
  a_window = 10,
  roll_width = 15,
  t_zero = 0,
  fit_type = "exponential"
)
str(slopes_exp_liahovden)

slopes_qua_liahovden <- flux_fitting(
  conc_df = conc_liahovden,
  start_cut = 0,
  end_cut = 0,
  start_col = "f_start",
  end_col = "f_end",
  datetime_col = "f_datetime",
  conc_col = "f_conc",
  fluxid_col = "f_fluxID",
  t_window = 20,
  cz_window = 15,
  b_window = 10,
  a_window = 10,
  roll_width = 15,
  t_zero = 5,
  fit_type = "quadratic"
)
str(slopes_qua_liahovden)

slopes_lin_liahovden <- flux_fitting(
  conc_df = conc_liahovden,
  start_cut = 0,
  end_cut = 0,
  start_col = "f_start",
  end_col = "f_end",
  datetime_col = "f_datetime",
  conc_col = "f_conc",
  fluxid_col = "f_fluxID",
  t_window = 20,
  cz_window = 15,
  b_window = 10,
  a_window = 10,
  roll_width = 15,
  t_zero = 5,
  fit_type = "linear"
)
str(slopes_lin_liahovden)

## ----quality_exp_qua_lin------------------------------------------------------
slopes_exp_liahovden_flag <- flux_quality(
  slopes_df = slopes_exp_liahovden,
  # fit_type is automatically provided as an attribute because
  # slopes_exp_liahovden was produced with flux_fitting
  ambient_conc = 421,
  error = 100,
  fluxid_col = "f_fluxID",
  slope_col = "f_slope",
  weird_fluxes_id = c(),
  force_ok_id = c(),
  ratio_threshold = 0,
  conc_col = "f_conc",
  b_col = "f_b",
  time_col = "f_time",
  fit_col = "f_fit",
  cut_col = "f_cut",
  rmse_threshold = 25,
  cor_threshold = 0.5,
  b_threshold = 1,
  cut_arg = "cut"
)
str(slopes_exp_liahovden_flag)

slopes_qua_liahovden_flag <- flux_quality(
  slopes_df = slopes_qua_liahovden,
  # fit_type is automatically provided as an attribute because
  # slopes_exp_liahovden was produced with flux_fitting
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
  time_col = "f_time",
  fit_col = "f_fit",
  cut_col = "f_cut",
  cut_arg = "cut"
)
str(slopes_qua_liahovden_flag)

slopes_lin_liahovden_flag <- flux_quality(
  slopes_df = slopes_lin_liahovden,
  # fit_type is automatically provided as an attribute because
  # slopes_exp_liahovden was produced with flux_fitting
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
  time_col = "f_time",
  fit_col = "f_fit",
  cut_col = "f_cut",
  cut_arg = "cut"
)
str(slopes_lin_liahovden_flag)

## ----plot_exp_qua_lin, fig.width = 8, fig.height = 9--------------------------
slopes_exp_liahovden_flag |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxID %in% c(54, 95, 100, 101)) |>
  flux_plot(
    color_discard = "#D55E00",
    color_cut = "#D55E00",
    color_ok = "#009E73",
    color_zero = "#CC79A7",
    f_date_breaks = "1 min",
    f_minor_breaks = "10 sec",
    f_date_labels = "%e/%m \n %H:%M",
    f_ylim_upper = 600,
    f_ylim_lower = 300,
    f_plotname = "plot_quality",
    facet_wrap_args = list(
      ncol = 2,
      nrow = 2,
      scales = "free"
    ),
    y_text_position = 400,
    print_plot = "FALSE",
    output = "print_only",
    cut_arg = "cut",
    no_data_flag = "no_data"
  )

slopes_qua_liahovden_flag |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxID %in% c(54, 95, 100, 101)) |>
  flux_plot(
    color_discard = "#D55E00",
    color_cut = "#D55E00",
    color_ok = "#009E73",
    color_zero = "#CC79A7",
    f_date_breaks = "1 min",
    f_minor_breaks = "10 sec",
    f_date_labels = "%e/%m \n %H:%M",
    f_ylim_upper = 600,
    f_ylim_lower = 300,
    f_plotname = "plot_quality",
    facet_wrap_args = list(
      ncol = 2,
      nrow = 2,
      scales = "free"
    ),
    y_text_position = 400,
    print_plot = "FALSE",
    output = "print_only",
    cut_arg = "cut",
    no_data_flag = "no_data"
  )

slopes_lin_liahovden_flag |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxID %in% c(54, 95, 100, 101)) |>
  flux_plot(
    color_discard = "#D55E00",
    color_cut = "#D55E00",
    color_ok = "#009E73",
    color_zero = "#CC79A7",
    f_date_breaks = "1 min",
    f_minor_breaks = "10 sec",
    f_date_labels = "%e/%m \n %H:%M",
    f_ylim_upper = 600,
    f_ylim_lower = 300,
    f_plotname = "plot_quality",
    facet_wrap_args = list(
      ncol = 2,
      nrow = 2,
      scales = "free"
    ),
    y_text_position = 400,
    print_plot = "FALSE",
    output = "print_only",
    cut_arg = "cut",
    no_data_flag = "no_data"
  )

## ----plot_exp_cut, fig.width = 8, fig.height = 9------------------------------
slopes_exp_liahovden_flag_60 <- conc_liahovden |>
  flux_fitting(fit_type = "exp", end_cut = 60) |>
  flux_quality(
    slope_col = "f_slope",
    weird_fluxes_id = c(
      51, # slope is much steeper than the flux because t zero was estimated
      # at the very start of the measurement
      101, # plot starts with a high peak: accumulation in the canopy?
      106 # peak at the beginning of the flux that is messing up the fit
    )
  )

slopes_exp_liahovden_flag_60 |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxID %in% c(54, 95, 100, 101)) |>
  flux_plot(
    f_ylim_lower = 300,
    f_ylim_upper = 600,
    facet_wrap_args = list(
      ncol = 2,
      nrow = 2,
      scales = "free"
    ),
    y_text_position = 400,
    output = "print_only"
  )

## ----plot_qua_cut, fig.width = 8, fig.height = 9------------------------------
slopes_qua_liahovden_flag_60 <- conc_liahovden |>
  flux_fitting(fit_type = "qua", end_cut = 60, t_zero = 5) |>
  flux_quality(
    slope_col = "f_slope"
  )

slopes_qua_liahovden_flag_60 |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxID %in% c(54, 95, 100, 101)) |>
  flux_plot(
    f_ylim_lower = 300,
    f_ylim_upper = 600,
    facet_wrap_args = list(
      ncol = 2,
      nrow = 2,
      scales = "free"
    ),
    y_text_position = 400,
    output = "print_only"
  )

## ----plot_lin_cut, fig.width = 8, fig.height = 9------------------------------
slopes_lin_liahovden_flag_120 <- conc_liahovden |>
  flux_fitting(fit_type = "lin", end_cut = 120, t_zero = 5) |>
  flux_quality(
    slope_col = "f_slope"
  )

slopes_lin_liahovden_flag_120 |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxID %in% c(54, 95, 100, 101)) |>
  flux_plot(
    f_ylim_lower = 300,
    f_ylim_upper = 600,
    facet_wrap_args = list(
      ncol = 2,
      nrow = 2,
      scales = "free"
    ),
    y_text_position = 400,
    output = "print_only"
  )

## ----calc---------------------------------------------------------------------
fluxes_exp_liahovden_60 <- slopes_exp_liahovden_flag_60 |>
  flux_calc(
    slope_col = "f_slope_corr", # we use the slopes provided by flux_quality
    datetime_col = "f_datetime",
    cut_col = "f_cut",
    keep_arg = "keep",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cols_keep = c("f_start", "type"),
    cols_ave = c(),
    fluxid_col = "f_fluxID",
    temp_air_col = "temp_air",
    temp_air_unit = "celsius"
  )
str(fluxes_exp_liahovden_60)

fluxes_qua_liahovden_60 <- slopes_qua_liahovden_flag_60 |>
  flux_calc(
    slope_col = "f_slope_corr", # we use the slopes provided by flux_quality
    datetime_col = "f_datetime",
    cut_col = "f_cut",
    keep_arg = "keep",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cols_keep = c("f_start", "type"),
    cols_ave = c(),
    fluxid_col = "f_fluxID",
    temp_air_col = "temp_air",
    temp_air_unit = "celsius"
  )
str(fluxes_qua_liahovden_60)

fluxes_lin_liahovden_120 <- slopes_lin_liahovden_flag_120 |>
  flux_calc(
    slope_col = "f_slope_corr", # we use the slopes provided by flux_quality
    datetime_col = "f_datetime",
    cut_col = "f_cut",
    keep_arg = "keep",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cols_keep = c("f_start", "type"),
    cols_ave = c(),
    fluxid_col = "f_fluxID",
    temp_air_col = "temp_air",
    temp_air_unit = "celsius"
  )
str(fluxes_lin_liahovden_120)

## ----24h_fluxes, fig.width = 8, fig.height = 9, warning = FALSE, message = FALSE----
library(dplyr)
library(ggplot2)
bind_rows(
  fluxes_exp_liahovden_60,
  fluxes_qua_liahovden_60,
  fluxes_lin_liahovden_120
) |>
  ggplot(aes(x = f_start, y = flux, color = model)) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Net Ecosystem Exchange at Upper Site (Liahovden) during 24 hour",
    x = "Datetime",
    y = bquote(~ CO[2] ~ "flux [mmol/" * m^2 * "/h]"),
    color = "Model used in flux_fitting"
  ) +
  theme(legend.position = "bottom") +
  facet_grid(type ~ ., scales = "free")

