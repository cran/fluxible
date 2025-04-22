## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----match, message = FALSE---------------------------------------------------

library(fluxible)
library(dplyr)

str(record_liahovden)
str(co2_liahovden)

conc_liahovden <- flux_match(
  co2_liahovden,
  record_liahovden,
  datetime,
  start,
  conc,
  startcrop = 0,
  measurement_length = 220,
  ratio_threshold = 0.5,
  time_diff = 0
)

str(conc_liahovden)

## ----fitting_exp_qua_lin------------------------------------------------------

slopes_exp_liahovden <- flux_fitting(
  conc_liahovden,
  conc,
  datetime,
  fit_type = "exponential"
)
str(slopes_exp_liahovden)

slopes_qua_liahovden <- flux_fitting(
  conc_liahovden,
  conc,
  datetime,
  fit_type = "quadratic"
)
str(slopes_qua_liahovden)

slopes_lin_liahovden <- flux_fitting(
  conc_liahovden,
  conc,
  datetime,
  fit_type = "linear"
)
str(slopes_lin_liahovden)

## ----quality_exp_qua_lin------------------------------------------------------

slopes_exp_liahovden_flag <- flux_quality(
  slopes_exp_liahovden,
  conc
)
str(slopes_exp_liahovden_flag)

slopes_qua_liahovden_flag <- flux_quality(
  slopes_qua_liahovden,
  conc
)
str(slopes_qua_liahovden_flag)

slopes_lin_liahovden_flag <- flux_quality(
  slopes_lin_liahovden,
  conc
)
str(slopes_lin_liahovden_flag)

## ----plot_exp_qua_lin, fig.width = 8, fig.height = 9--------------------------


slopes_exp_liahovden_flag |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxid %in% c(54, 95, 100, 101)) |>
  flux_plot(
    conc,
    datetime,
    f_ylim_upper = 600,
    f_ylim_lower = 350,
    y_text_position = 450,
    facet_wrap_args = list(
      nrow = 2,
      ncol = 2,
      scales = "free"
    )
  )

slopes_qua_liahovden_flag |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxid %in% c(54, 95, 100, 101)) |>
  flux_plot(
    conc,
    datetime,
    f_ylim_upper = 600,
    f_ylim_lower = 350,
    y_text_position = 450,
    facet_wrap_args = list(
      nrow = 2,
      ncol = 2,
      scales = "free"
    )
  )

slopes_lin_liahovden_flag |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxid %in% c(54, 95, 100, 101)) |>
  flux_plot(
    conc,
    datetime,
    f_ylim_upper = 600,
    f_ylim_lower = 350,
    y_text_position = 450,
    facet_wrap_args = list(
      nrow = 2,
      ncol = 2,
      scales = "free"
    )
  )

## ----plot_exp_cut, fig.width = 8, fig.height = 9------------------------------


slopes_exp_liahovden_flag_60 <- conc_liahovden |>
  flux_fitting(
    conc,
    datetime,
    fit_type = "exp_zhao18",
    end_cut = 60
  ) |>
  flux_quality(
    conc,
    force_discard = c(
      51, # slope is much steeper than the flux because t zero was estimated
      # at the very start of the measurement
      101 # plot starts with a high peak: accumulation in the canopy?
    )
  )

slopes_exp_liahovden_flag_60 |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxid %in% c(54, 95, 100, 101)) |>
  flux_plot(
    conc,
    datetime,
    f_ylim_upper = 600,
    f_ylim_lower = 350,
    y_text_position = 450,
    facet_wrap_args = list(
      nrow = 2,
      ncol = 2,
      scales = "free"
    )
  )

## ----plot_qua_cut, fig.width = 8, fig.height = 9------------------------------


slopes_qua_liahovden_flag_60 <- conc_liahovden |>
  flux_fitting(
    conc,
    datetime,
    fit_type = "quadratic",
    end_cut = 60
  ) |>
  flux_quality(
    conc
  )

slopes_qua_liahovden_flag_60 |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxid %in% c(54, 95, 100, 101)) |>
  flux_plot(
    conc,
    datetime,
    f_ylim_upper = 600,
    f_ylim_lower = 350,
    y_text_position = 450,
    facet_wrap_args = list(
      nrow = 2,
      ncol = 2,
      scales = "free"
    )
  )

## ----plot_lin_cut, fig.width = 8, fig.height = 9------------------------------


slopes_lin_liahovden_flag_120 <- conc_liahovden |>
  flux_fitting(
    conc,
    datetime,
    fit_type = "linear",
    end_cut = 120
  ) |>
  flux_quality(
    conc
  )

slopes_lin_liahovden_flag_120 |>
  # we just show a sample of the plots to avoid slowing down the example
  dplyr::filter(f_fluxid %in% c(54, 95, 100, 101)) |>
  flux_plot(
    conc,
    datetime,
    f_ylim_upper = 600,
    f_ylim_lower = 350,
    y_text_position = 450,
    facet_wrap_args = list(
      nrow = 2,
      ncol = 2,
      scales = "free"
    )
  )

## ----calc---------------------------------------------------------------------

fluxes_exp_liahovden_60 <- slopes_exp_liahovden_flag_60 |>
  flux_calc(
    f_slope_corr, # we use the slopes provided by flux_quality
    datetime,
    temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cols_keep = c("turfID", "type", "round"),
    cols_ave = c("temp_soil", "PAR")
  )

fluxes_qua_liahovden_60 <- slopes_qua_liahovden_flag_60 |>
  flux_calc(
    f_slope_corr, # we use the slopes provided by flux_quality
    datetime,
    temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cols_keep = c("turfID", "type", "round"),
    cols_ave = c("temp_soil", "PAR")
  )

fluxes_lin_liahovden_120 <- slopes_lin_liahovden_flag_120 |>
  flux_calc(
    f_slope_corr, # we use the slopes provided by flux_quality
    datetime,
    temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cols_keep = c("turfID", "type", "round"),
    cols_ave = c("temp_soil", "PAR")
  )

## ----gep----------------------------------------------------------------------

fluxes_exp_liahovden_60_gep <- fluxes_exp_liahovden_60 |>
  flux_gep(
    type,
    datetime,
    id_cols = c("round", "turfID"),
    cols_keep = c("temp_soil", "f_model", "PAR")
  )

str(fluxes_exp_liahovden_60_gep)

fluxes_qua_liahovden_60_gep <- fluxes_qua_liahovden_60 |>
  flux_gep(
    type,
    datetime,
    id_cols = c("round", "turfID"),
    cols_keep = c("temp_soil", "f_model", "PAR")
  )

str(fluxes_qua_liahovden_60_gep)

fluxes_lin_liahovden_120_gep <- fluxes_lin_liahovden_120 |>
  flux_gep(
    type,
    datetime,
    id_cols = c("round", "turfID"),
    cols_keep = c("temp_soil", "f_model", "PAR")
  )

str(fluxes_lin_liahovden_120_gep)

## ----24h_fluxes, fig.width = 8, fig.height = 9, warning = FALSE, message = FALSE----


library(ggplot2)
bind_rows(
  fluxes_exp_liahovden_60_gep,
  fluxes_qua_liahovden_60_gep,
  fluxes_lin_liahovden_120_gep
) |>
  ggplot(aes(x = datetime, y = f_flux, color = f_model)) +
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

