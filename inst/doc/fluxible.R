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

## ----match, message=FALSE-----------------------------------------------------
library(fluxible)

conc_liahovden <- flux_match(
  raw_conc = co2_liahovden, # dataframe with raw gas concentration
  field_record = record_liahovden, # dataframe with meta data
  f_datetime = datetime, # date and time of each gas concentration row
  start_col = start, # start date and time of each measurement
  measurement_length = 220, # length of measurements (in seconds)
  time_diff = 0 # time difference between f_datetime and start_col
)

## ----fitting_exp, message=FALSE, warning=FALSE--------------------------------
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

## ----fig-explot, fig.width=8, fig.height=7, message=FALSE, fig.cap="Output of `flux_plot` for fluxid 54, 95, 100 and 101. With quality flags and diagnostics from `flux_quality`, the slope at $t_0$ (continuous line), the model fit (dashed line), the linear fit (dotted line), and the raw gas concentration (dots). The colours show the quality flags (green for `ok`, red for `discard` and purple for `zero` with default settings) and cuts (same colour as `discard`). The gray vertical line indicates $t_0$ (a fitting parameter when using the `exp_zhao18` model, otherwise user defined in `flux_fitting`). The g-factor is calculated as slope/linear slope, and b is the b parameter inside the exponential model. Concentration is in ppm in this example. Due to poor quality (strong peak at the start), `flux_fitting` could not provide a decent fit for fluxid 101. This was detected by `flux_quality` which flagged it as discard."----
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
    ),
    f_facetid = "f_fluxid"
  )

## ----plot_pdf, eval=FALSE-----------------------------------------------------
# flux_plot(
#   slopes_df = flags_liahovden,
#   f_conc = conc,
#   f_datetime = datetime,
#   print_plot = FALSE, # not printing the plots in the R session
#   output = "longpdf", # the type of output
#   f_plotname = "plots_liahovden" # filename for the pdf file
# )

## ----ex-plot-filter, eval=FALSE-----------------------------------------------
# flags_lia |> # the output of flux_quality
#   # apply here dplyr::filter on f_quality_flags, f_fluxid,
#   # sample(f_fluxid) for random sampling of fluxid, campaigns,
#   # dates, windspeed...
#   # flux_plot will plot only the measurements passing the filter
#   # and not the entire flags_lia df
#   flux_plot(
#     f_conc = conc,
#     f_datetime = datetime
#   )

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
    conc
    # force_zero = 101 # to replace flux 101 with 0 instead of discarding
  )

## ----fig-plot_exp_cut, fig.width=8, fig.height=9, message=FALSE, fig.cap="Output of `flux_plot` for fluxid 54, 95, 100 and 101, after quality check. Concentration is in ppm in this example."----
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
  cols_keep = c("turfID", "type", "measurement_round"),
  cols_ave = c("temp_soil", "PAR")
)

## ----fluxes-str, echo=FALSE---------------------------------------------------
str(fluxes_liahovden_60, width = 70, strict.width = "cut")

## ----gpp-lia, warning=FALSE---------------------------------------------------
gpp_liahovden_60 <- flux_diff(
  fluxes_df = fluxes_liahovden_60,
  type_col = type, # the column specifying the type of measurement
  id_cols = c("measurement_round", "turfID"),
  cols_keep = c("temp_soil_ave", "PAR_ave", "datetime"), # or "none" or "all"
  type_a = "NEE", # we want the difference between NEE
  type_b = "ER", # and ER
  diff_name = "GPP" # the name of the calculated flux
)

## ----gpp-str, echo=FALSE------------------------------------------------------
str(gpp_liahovden_60, width = 70, strict.width = "cut")

## ----mg-transform-------------------------------------------------------------
gpp_liahovden_60 <- gpp_liahovden_60 |>
  dplyr::mutate(
    flux_mg = f_flux * 0.0440095
  )

## ----gpp-str2, echo=FALSE-----------------------------------------------------
str(gpp_liahovden_60, width = 70, strict.width = "cut")

