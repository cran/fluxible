## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tidyverse.quiet = TRUE)

## ----match-twogases, message=FALSE--------------------------------------------
library(fluxible)
library(tidyverse)

conc_twogases <- flux_match(
  raw_conc = raw_twogases,
  field_record = twogases_record,
  f_datetime = datetime,
  start_col = start,
  measurement_length = 180,
  time_diff = 0
)

## ----fitting-twogases, message=FALSE------------------------------------------
slopes_twogases_co2 <- flux_fitting(
  conc_df = conc_twogases,
  f_conc = co2_conc,
  f_datetime = datetime,
  fit_type = "exp_zhao18",
  start_cut = 10
)

slopes_twogases_ch4 <- flux_fitting(
  conc_df = conc_twogases,
  f_conc = ch4_conc,
  f_datetime = datetime,
  fit_type = "exp_zhao18",
  start_cut = 10
)

## ----quality-twogases---------------------------------------------------------
flag_twogases_co2 <- flux_quality(
  slopes_df = slopes_twogases_co2,
  f_conc = co2_conc,
  force_discard = "8" # peak at the start that is probably an error
)

flag_twogases_ch4 <- flux_quality(
  slopes_df = slopes_twogases_ch4,
  f_conc = ch4_conc,
  ambient_conc = 2000 # default is for CO2
)

## ----plot-co2-twogases, fig.width=10, fig.height=10, message=FALSE, fig.cap="CO~2~ measurements with quality flags."----
flag_twogases_co2 |>
  flux_plot(
    f_conc = co2_conc,
    f_datetime = datetime,
    f_ylim_upper = 500,
    f_ylim_lower = 425,
    y_text_position = 460
  )

## ----plot-ch4-twogases, fig.width=10, fig.height=10, message=FALSE, fig.cap="CH~4~ measurements with quality flags."----
flag_twogases_ch4 |>
  flux_plot(
    f_conc = ch4_conc,
    f_datetime = datetime,
    f_ylim_upper = 2000,
    f_ylim_lower = 1995,
    y_text_position = 1997
  )

## ----calc-twogases, message=FALSE---------------------------------------------
fluxes_twogases_co2 <- flux_calc(
  slopes_df = flag_twogases_co2,
  slope_col = f_slope_corr,
  f_datetime = datetime,
  temp_air_col = temp_air,
  conc_unit = "ppm",
  flux_unit = "mmol",
  setup_volume = 6.31,
  atm_pressure = 1,
  plot_area = 0.31,
  # we want to use the quality flags of CO2 to eventally discard CH4 fluxes
  cols_keep = "f_quality_flag"
) |>
  rename( # to avoid any confusion, we rename the flux column
    flux_co2 = "f_flux"
  ) |> # and we remove the slope one
  select(-f_slope_corr)

fluxes_twogases_ch4 <- flux_calc(
  slopes_df = flag_twogases_ch4,
  slope_col = f_slope_corr,
  f_datetime = datetime,
  temp_air_col = temp_air,
  conc_unit = "ppb", # ch4 is measured in ppb
  flux_unit = "micromol", # we want a flux in umol/m2/h
  setup_volume = 6.31,
  atm_pressure = 1,
  plot_area = 0.31
) |>
  rename( # to avoid any confusion, we rename the flux column
    flux_ch4 = "f_flux"
  ) |> # and we remove the slope one
  select(-f_slope_corr)

## ----join-twogases------------------------------------------------------------
fluxes_twogases <- left_join(
  fluxes_twogases_co2,
  fluxes_twogases_ch4,
  by = c(
    # if that does not work, then it means that we did
    # something different for one of the gases
    "f_fluxid",
    "f_temp_air_ave",
    "datetime",
    "f_model"
  )
) |>
  mutate( # we discard the CH4 fluxes based on CO2 fluxes quality flags
    flux_ch4 = case_when(
      f_quality_flag %in% c("discard", "force_discard") ~ NA,
      .default = flux_ch4
    )
  )

## ----fluxes_twogases-str, echo=FALSE------------------------------------------
str(fluxes_twogases, width = 70, strict.width = "cut", give.attr = FALSE)

