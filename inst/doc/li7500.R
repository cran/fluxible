## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tidyverse.quiet = TRUE)

## ----reading, message=FALSE---------------------------------------------------
library(licoread)
li7500_data <- import7500(
  "ex_data/li7500",
  plotinfo = c("site", "treatment", "date", "plot_id", "trial")
)

## ----wetair_co2---------------------------------------------------------------
library(fluxible)
li7500_data_co2 <- flux_drygas(li7500_data, `CO2 umol/mol`, `H2O mmol/mol`)

## ----fitting_co2, message = FALSE, warning = FALSE----------------------------
li7500_fits_co2 <- flux_fitting(li7500_data_co2,
                                f_conc = `CO2 umol/mol_dry`,
                                fit_type = "linear")

## ----flags_co2----------------------------------------------------------------
li7500_flags_co2 <- flux_quality(li7500_fits_co2,
                                 f_conc = `CO2 umol/mol_dry`,
                                 rsquared_threshold = 0.5)

## ----plotting_co2, message=FALSE, warning=FALSE, fig.cap="Output of `flux_plot` for CO~2~ fluxes"----
flux_plot(li7500_flags_co2,
          f_conc = `CO2 umol/mol_dry`,
          f_ylim_upper = 500,
          y_text_position = 450,
          print_plot = TRUE)

## ----plotting_exp_co2_pdf, eval = FALSE---------------------------------------
# flux_plot(li7500_flags_co2,
#           f_conc = `CO2 umol/mol_dry`,
#           f_ylim_upper = 500,
#           y_text_position = 450,
#           print_plot = FALSE,
#           output = "longpdf",
#           f_plotname = "li7500_co2")

## ----calc_co2-----------------------------------------------------------------
li7500_fluxes_co2 <- flux_calc(li7500_flags_co2,
                               slope_col = f_slope_corr,
                               temp_air_col = Temperature,
                               setup_volume = 2197,
                               atm_pressure = pressure_atm,
                               plot_area = 1.44,
                               conc_unit = "ppm",
                               flux_unit = "umol/m2/s",
                               cols_keep = c(
                                 "site", "treatment", "date", "plot_id", "trial"
                               ))

## ----wetair_h2o---------------------------------------------------------------
li7500_data_h2o <- flux_drygas(li7500_data, `H2O mmol/mol`, `H2O mmol/mol`)

## ----fitting_h2o, message = FALSE, warning = FALSE----------------------------
li7500_fits_h2o <- flux_fitting(li7500_data_h2o,
                                f_conc = `H2O mmol/mol_dry`,
                                fit_type = "linear",
                                start_cut = 0,
                                end_cut = 0)

## ----flags_h2o----------------------------------------------------------------
li7500_flags_h2o <- flux_quality(li7500_fits_h2o,
                                 f_conc = `H2O mmol/mol_dry`,
                                 rsquared_threshold = 0.5,
                                 ambient_conc = 10, # the default is for CO2
                                 error = 2)

## ----plotting_h2o, message=FALSE, warning=FALSE, fig.cap="Output of `flux_plot` for H~2~O fluxes"----

flux_plot(li7500_flags_h2o,
          f_conc = `H2O mmol/mol_dry`,
          print_plot = TRUE,
          f_ylim_lower = 5,
          f_ylim_upper = 15,
          y_text_position = 12)

## ----plotting_h2o_pdf, eval = FALSE-------------------------------------------
# flux_plot(li7500_flags_h2o,
#           f_conc = `H2O mmol/mol_dry`,
#           print_plot = FALSE,
#           output = "longpdf",
#           f_plotname = "li7500_h2o",
#           f_ylim_lower = 5,
#           f_ylim_upper = 15,
#           y_text_position = 12)

## ----calc_h2o-----------------------------------------------------------------
li7500_fluxes_h2o <- flux_calc(li7500_flags_h2o,
                               slope_col = f_slope_corr,
                               temp_air_col = Temperature,
                               setup_volume = 2197,
                               atm_pressure = pressure_atm,
                               plot_area = 1.44,
                               conc_unit = "mmol/mol",
                               flux_unit = "mmol/m2/s",
                               cols_keep = c(
                                 "site", "treatment", "date", "plot_id", "trial"
                               ))

## ----gpp, warning=FALSE-------------------------------------------------------
li7500_fluxes_co2 <- flux_diff(li7500_fluxes_co2, type_col = trial,
                               id_cols = c(
                                 "site", "treatment", "date", "plot_id"
                               ), type_a = "p", type_b = "r",
                               diff_name = "GPP", cols_keep = "all")

## ----trans, warning=FALSE-----------------------------------------------------
li7500_fluxes_h2o <- flux_diff(li7500_fluxes_h2o, type_col = trial,
                               id_cols = c(
                                 "site", "treatment", "date", "plot_id"
                               ), type_a = "p", type_b = "r",
                               diff_name = "T", cols_keep = "all")

## ----joining------------------------------------------------------------------
library(tidyverse)
# to avoid confusion, we add a gas column
# this might be implemented in flux_calc in the future

li7500_fluxes_co2 <- li7500_fluxes_co2 |>
  mutate(gas = "co2")

li7500_fluxes_h2o <- li7500_fluxes_h2o |>
  mutate(gas = "h2o")

li7500_fluxes <- bind_rows(li7500_fluxes_co2, li7500_fluxes_h2o)

# Now the data are in a single long df

## ----str, message=FALSE, echo=FALSE-------------------------------------------

str(li7500_fluxes, width = 70, strict.width = "cut")

