## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tidyverse.quiet = TRUE)

## ----match--------------------------------------------------------------------

library(fluxible)
library(tidyverse)

conc_twogases <- flux_match(
  raw_twogases,
  twogases_record,
  datetime,
  start,
  co2_conc,
  startcrop = 10,
  measurement_length = 180,
  ratio_threshold = 0.5,
  time_diff = 0
)

## ----fitting------------------------------------------------------------------
slopes_twogases_co2 <- flux_fitting(
  conc_twogases,
  co2_conc,
  datetime,
  fit_type = "exponential"
)

slopes_twogases_ch4 <- flux_fitting(
  conc_twogases,
  ch4_conc,
  datetime,
  fit_type = "exponential"
)

## ----quality------------------------------------------------------------------
flag_twogases_co2 <- flux_quality(
  slopes_twogases_co2,
  co2_conc,
  force_discard = "8" # there is a peak at the start that looks like an error
)

flag_twogases_ch4 <- flux_quality(
  slopes_twogases_ch4,
  ch4_conc,
  ambient_conc = 2000 # the default is for CO2
)

## ----plot-co2, fig.width = 8, fig.height = 14---------------------------------
flag_twogases_co2 |>
  flux_plot(
    co2_conc,
    datetime,
    f_ylim_upper = 500,
    f_ylim_lower = 425,
    y_text_position = 460
  )

## ----plot-ch4, fig.width = 8, fig.height = 14---------------------------------

flag_twogases_ch4 |>
  flux_plot(
    ch4_conc,
    datetime,
    f_ylim_upper = 2000,
    f_ylim_lower = 1995,
    y_text_position = 1997
  )

## ----calc---------------------------------------------------------------------
fluxes_twogases_co2 <- flux_calc(
  flag_twogases_co2,
  f_slope_corr,
  datetime,
  temp_air,
  conc_unit = "ppm",
  flux_unit = "mmol",
  chamber_volume = 6.3,
  tube_volume = 0.01,
  atm_pressure = 1,
  plot_area = 0.31,
  cols_keep = "f_quality_flag" # to use the flags of CO2 to discard CH4 fluxes
) |>
  rename( # to avoid any confusion, we rename the flux column
    flux_co2 = "f_flux"
  ) |> # and we remove the slope one
  select(!f_slope_corr)

fluxes_twogases_ch4 <- flux_calc(
  flag_twogases_ch4,
  f_slope_corr,
  datetime,
  temp_air,
  conc_unit = "ppb", # ch4 is measured in ppb
  flux_unit = "micromol", # we want a flux in umol/m2/h
  chamber_volume = 6.3,
  tube_volume = 0.01,
  atm_pressure = 1,
  plot_area = 0.31
) |>
  rename( # to avoid any confusion, we rename the flux column
    flux_ch4 = "f_flux"
  ) |> # and we remove the slope one
  select(!f_slope_corr)

## ----join---------------------------------------------------------------------

fluxes_twogases <- left_join(
  fluxes_twogases_co2,
  fluxes_twogases_ch4,
  by = c(
    "f_fluxid",
    "f_temp_air_ave",
    "datetime",
    "f_model",
    "f_volume_setup"
  )
) |>
  mutate( # we discard the CH4 fluxes based on CO2 fluxes quality flags
    flux_ch4 = case_when(
      f_quality_flag != "ok" ~ NA,
      TRUE ~ flux_ch4
    )
  )

str(fluxes_twogases) # Et voilà!

