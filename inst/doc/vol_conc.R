## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tidyverse.quiet = TRUE)

## ----read-data, message=FALSE-------------------------------------------------
library(tidyverse)

conc_df <- read_csv("ex_data/sample_vol_conc.csv")

head(conc_df)

## ----conc-vol, message=FALSE--------------------------------------------------
library(fluxible)

conc_df_vol <- flux_conc(
  conc_df,
  CO2,
  temp_air,
  atm_press
)

head(conc_df_vol)

## ----fitting, message=FALSE---------------------------------------------------

fit_df <- flux_fitting(
  conc_df_vol,
  f_conc_vol,
  datetime
)

quality_df <- flux_quality(
  fit_df,
  f_conc_vol,
  ambient_conc = 18, # approx ambient CO2 concentration at 20°C in umol/l
  error = 4 # this one also needs to be adapted
)

flux_plot(
  quality_df,
  f_conc_vol,
  datetime,
  f_ylim_upper = 25, # default values are for ppm
  f_ylim_lower = 10
)

## ----calc-vol, message=FALSE--------------------------------------------------
fluxes <- flux_calc(
  quality_df,
  f_slope,
  datetime,
  temp_air,
  conc_unit = "umol/l", # notice the units change
  flux_unit = "umol/m2/s",
  setup_volume = 125,
  atm_pressure = atm_press,
  plot_area = 0.25,
  cut = FALSE
)

str(fluxes)

