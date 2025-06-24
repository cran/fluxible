## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tidyverse.quiet = TRUE)

## ----co2_liahovden-str, message=FALSE, echo=FALSE-----------------------------
library(fluxible)

str(co2_liahovden, width = 70, strict.width = "cut")

## ----record_liahovden-str, message=FALSE, echo=FALSE--------------------------

str(record_liahovden, width = 70, strict.width = "cut")


## ----match, message=FALSE, echo=TRUE------------------------------------------
library(fluxible)

conc_liahovden <- flux_match(
  raw_conc = co2_liahovden, # dataframe with raw gas concentration
  field_record = record_liahovden, # dataframe with meta data
  f_datetime = datetime, # column containing date and time
  start_col = start, # start date and time of each measurement
  measurement_length = 220, # length of measurements (in seconds)
  fixed_length = TRUE, # the length of the measurement is a constant
  time_diff = 0 # time difference between f_datetime and start_col
)

## ----fitting_exp, echo=TRUE, message=FALSE, warning=FALSE---------------------
slopes_liahovden <- flux_fitting(
  conc_df = conc_liahovden, # the output of flux_match
  f_conc = conc, # gas concentration column
  f_datetime = datetime, # date and time column
  f_start = f_start, # start of each measurement, provided by flux_match
  f_end = f_end, # end of each measurement, provided by flux_match
  f_fluxid = f_fluxid, # unique ID for each measurement, provided by flux_match
  fit_type = "exp_zhao18", # the model to fit to the gas concentration
  start_cut = 0, # seconds to prune at the start before fitting
  end_cut = 0 # seconds to prune at the end of all measurements before fitting
)

## ----quality_exp--------------------------------------------------------------
flags_liahovden <- flux_quality(
  slopes_df = slopes_liahovden,
  f_conc = conc,
  # force_discard = c(),
  # force_ok = c(),
  # force_zero = c(),
  # force_lm = c(),
  # force_exp = c(),
  ambient_conc = 421,
  error = 100,
  instr_error = 5
)

flux_flag_count(flags_liahovden)

## ----explot, fig.width= 8, fig.height=9, message=FALSE, fig.cap="Output of `flux_plot` for fluxid 54, 95, 100 and 101."----
flags_liahovden |>
  # we show only a sample of the plots in this example
  dplyr::filter(f_fluxid %in% c(54, 95, 100, 101)) |>
  flux_plot(
    f_conc = conc,
    f_datetime = datetime,
    f_ylim_upper = 600, # upper limit of y-axis
    f_ylim_lower = 350, # lower limit of x-axis
    y_text_position = 450, # position of text with flags and diagnostics
    facet_wrap_args = list( # facet_wrap arguments, if different than default
      nrow = 2,
      ncol = 2,
      scales = "free"
    )
  )

## ----plot_pdf, echo=TRUE, eval=FALSE------------------------------------------
# flux_plot(
#   slopes_df = flags_liahovden,
#   f_conc = conc,
#   f_datetime = datetime,
#   print_plot = FALSE, # not printing the plots in the R session
#   output = "pdfpages", # the type of output
#   f_plotname = "plots_liahovden" # filename for the pdf file
# )

## ----fits_exp_cut, message=FALSE----------------------------------------------
fits_liahovden_60 <- conc_liahovden |>
  flux_fitting(
    conc,
    datetime,
    fit_type = "exp_zhao18",
    end_cut = 60 # we decided to cut the last 60 seconds of the measurements
  )

## ----flag_exp_cut-------------------------------------------------------------
flags_liahovden_60 <- fits_liahovden_60 |>
  flux_quality(
    conc,
    force_lm = 101 # we force the use of the linear model for fluxid 101
  )

## ----plot_exp_cut, fig.width=8, fig.height=9, message=FALSE, fig.cap="Output of `flux_plot` for fluxid 54, 95, 100 and 101 after refitting with a 60 seconds end cut."----
flags_liahovden_60 |>
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

## ----calc, message=FALSE------------------------------------------------------
fluxes_liahovden_60 <- flux_calc(
  slopes_df = flags_liahovden_60,
  slope_col = f_slope_corr, # we use the slopes provided by flux_quality
  f_datetime = datetime,
  temp_air_col = temp_air,
  conc_unit = "ppm", # unit of gas concentration
  flux_unit = "mmol/m2/h", # unit of flux
  temp_air_unit = "celsius",
  setup_volume = 24.575, # in liters, can also be a variable
  atm_pressure = 1, # in atm, can also be a variable
  plot_area = 0.0625, # in m2, can also be a variable
  cols_keep = c("turfID", "type"),
  cols_ave = c("temp_soil", "PAR")
)

## ----fluxes-str, echo=FALSE---------------------------------------------------


str(fluxes_liahovden_60, width = 70, strict.width = "cut")


## ----gpp-lia------------------------------------------------------------------
library(tidyverse)

fluxes_liahovden_60 <- fluxes_liahovden_60 |>
  mutate(
    f_fluxid = as.integer(f_fluxid),
    pairID = case_when(
      type == "NEE" ~ f_fluxid,
      type == "ER" ~ f_fluxid - 1
    ),
    f_fluxid = as_factor(f_fluxid),
    pairID = as_factor(pairID)
  )

gpp_liahovden_60 <- flux_gpp(
  fluxes_df = fluxes_liahovden_60,
  type_col = type, # the column specifying the type of measurement
  f_datetime = datetime,
  id_cols = c("pairID", "turfID"),
  cols_keep = c("temp_soil_ave", "PAR_ave"), # or "none" (default) or "all"
  nee_arg = "NEE", # default value
  er_arg = "ER" # default value
)

## ----gpp-str, echo=FALSE------------------------------------------------------


str(gpp_liahovden_60, width = 70, strict.width = "cut")


## ----24h_fluxes, fig.width=8, fig.height=9, warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE----
# 
# 
# library(ggplot2)
# fluxes_exp_liahovden_60_gep |>
#   ggplot(aes(x = datetime, y = f_flux)) +
#   geom_point() +
#   geom_smooth() +
#   labs(
#     title = "Net Ecosystem Exchange at Upper Site (Liahovden) during 24 hour",
#     x = "Datetime",
#     y = bquote(~ CO[2] ~ "flux [mmol/" * m^2 * "/h]")
#   ) +
#   theme(legend.position = "bottom") +
#   facet_grid(type ~ ., scales = "free")

