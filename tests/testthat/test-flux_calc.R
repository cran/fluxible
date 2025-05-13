test_that("flux calculation is correct", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  )) |>
    flux_quality(
      conc
    )

  output <- flux_calc(slopes0,
    f_slope,
    datetime,
    temp_air,
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    conc_unit = "ppm",
    flux_unit = "mmol",
    cut = FALSE
  )

  expect_equal(
    output$f_flux,
    co2_fluxes$f_flux,
    tolerance = 0.001
  )
})


test_that("averaging works", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  )) |>
    flux_quality(
      conc
    )

  output <- flux_calc(
    slopes0,
    f_slope,
    datetime,
    temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    cols_ave = c("PAR", "temp_soil"),
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cut = FALSE
  ) |>
    dplyr::select(f_fluxid, f_temp_air_ave, datetime, f_flux, PAR, temp_soil)


  expect_snapshot(output)
})

test_that("keeping works", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  )) |>
    flux_quality(
      conc
    )

  expect_snapshot(flux_calc(
    slopes0,
    f_slope,
    datetime,
    temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    cols_keep = c("turfID", "type", "f_start"),
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cut = FALSE
  ) |>
    dplyr::select(f_fluxid, f_flux, turfID, type, f_start, f_slope))
})

test_that("keeping and averaging work together", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  )) |>
    flux_quality(
      conc
    )

  expect_snapshot(flux_calc(
    slopes0,
    f_slope,
    datetime,
    temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    cols_keep = c("turfID", "type", "f_start"),
    cols_ave = c("PAR", "temp_soil"),
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cut = FALSE
  ) |>
    dplyr::select(f_fluxid, f_flux, turfID, type, f_start, PAR, temp_soil))
})

test_that("fahrenheit conversion works", {
  expect_snapshot(flux_calc(
    slopes0_temp,
    f_slope,
    datetime,
    temp_fahr,
    conc_unit = "ppm",
    flux_unit = "mmol",
    temp_air_unit = "fahrenheit",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cut = FALSE
  ) |>
    dplyr::select(f_fluxid, f_temp_air_ave, datetime, f_flux, f_volume_setup))
})

test_that("kelvin conversion works", {
  expect_snapshot(flux_calc(
    slopes0_temp,
    f_slope,
    datetime,
    temp_kelvin,
    conc_unit = "ppm",
    flux_unit = "mmol",
    temp_air_unit = "kelvin",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cut = FALSE
  ) |>
    dplyr::select(f_fluxid, f_temp_air_ave, datetime, f_flux, f_volume_setup))
})





test_that("error on air temp units", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  )) |>
    flux_quality(
      conc
    )

  expect_error(
    flux_calc(
      slopes0,
      f_slope,
      datetime,
      temp_air,
      conc_unit = "ppm",
      flux_unit = "mmol",
      temp_air_unit = "melvin",
      chamber_volume = 24.5,
      tube_volume = 0.075,
      atm_pressure = 1,
      plot_area = 0.0625,
      cut = FALSE
    ),
    "'arg' should be one of \"celsius\", \"fahrenheit\", \"kelvin\""
  )
  # Played for Laughs in the 1985 humor book "Science Made Stupid"
  # by Tom Weller, in which a table in its appendix lists such units as the
  # "arg" (the unit of work done incorrectly), the "galumph" (unit of waste
  # motion), the "lumpen" (unit of resistance to getting out of bed in the
  # morning), and the "melvin" (unit of temperature, "as measured from
  # absolutely perfect to absolutely awful"). A separate table of conversions
  # for weights and measures on the same page listed equivalencies such as
  # "325 cubebs = 1 furbish; 6 furbishes = 1 nautical smile;
  # 20 nautical smiles = 1 minor league; 3 minor leagues = 1 major league"
  # and "24 carrots = 1 pickelweight; 30 pickelweights = 1 tuna;
  # 1000 tuna = 1 short ton; 1.37 short tons = 1 tall ton".
  # (https://allthetropes.org/wiki/Fantastic_Measurement_System)
})

test_that("error that slope column is missing", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  )) |>
    flux_quality(
      conc
    )

  expect_error(
    suppressWarnings(flux_calc(
      slopes0,
      datetime,
      temp_air,
      conc_unit = "ppm",
      flux_unit = "mmol",
      chamber_volume = 24.5,
      tube_volume = 0.075,
      atm_pressure = 1,
      plot_area = 0.0625,
      cut = FALSE
    )),
    "Please correct the arguments"
  )
})

test_that("error slope_col cannot be found in slopes_df", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  )) |>
    flux_quality(
      conc
    )

  expect_error(
    flux_calc(
      slopes0,
      column_with_slope,
      datetime,
      temp_air,
      conc_unit = "ppm",
      flux_unit = "mmol",
      chamber_volume = 24.5,
      tube_volume = 0.075,
      atm_pressure = 1,
      plot_area = 0.0625,
      cut = FALSE
    ),
    "Can't select columns that don't exist.
x Column `column_with_slope` doesn't exist."
  )
})

test_that("error some cols_keep do not exist", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  )) |>
    flux_quality(
      conc
    )

  expect_error(
    flux_calc(
      slopes0,
      f_slope,
      datetime,
      temp_air,
      conc_unit = "ppm",
      flux_unit = "mmol",
      cols_keep = c("PAR", "site"),
      chamber_volume = 24.5,
      tube_volume = 0.075,
      atm_pressure = 1,
      plot_area = 0.0625,
      cut = FALSE
    ),
    "some names in cols_keep cannot be found in slopes_df"
  )
})

# reproducing the error I had in the example in the manuscript
# when there is a cut, flux_calc calculates a flux for keep and a flux for cut,
# which is wrong. I need to discard the cut data first.

test_that("calculating fluxes on dataset with cuts", {
  slopes30_flag <- flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18",
    end_cut = 30
  ) |>
    flux_quality(conc)

  expect_snapshot(
    flux_calc(
      slopes30_flag,
      f_slope_corr,
      datetime,
      temp_air,
      conc_unit = "ppm",
      flux_unit = "mmol",
      keep_arg = "keep",
      chamber_volume = 24.5,
      tube_volume = 0.075,
      atm_pressure = 1,
      plot_area = 0.0625
    ) |>
      dplyr::select(f_fluxid, f_temp_air_ave, datetime, f_flux, f_volume_setup)
  )
})

# testing having the chamber volume as a variable
test_that("volume can be a variable instead of a constant", {
  slopes0_vol <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  )) |>
    flux_quality(
      conc
    ) |>
    mutate(
      volume = case_when(
        f_fluxid == 1 ~ 18,
        f_fluxid == 2 ~ 28,
        f_fluxid == 3 ~ 20,
        f_fluxid == 4 ~ 24,
        f_fluxid == 5 ~ 4,
        f_fluxid == 6 ~ 35
      )
    )

  expect_snapshot(
    flux_calc(
      slopes0_vol,
      f_slope,
      datetime,
      temp_air,
      volume,
      conc_unit = "ppm",
      flux_unit = "mmol",
      tube_volume = 0.075,
      atm_pressure = 1,
      plot_area = 0.0625
    ) |>
      dplyr::select(f_fluxid, f_temp_air_ave, datetime, f_flux, f_volume_setup)
  )
})


test_that("Fluxible workflow works from start to finish", {
  conc_test <- flux_match(
    co2_df_short,
    record_short,
    datetime,
    start,
    conc,
    startcrop = 10,
    measurement_length = 180
  )
  slopes_test <- suppressWarnings(flux_fitting(
    conc_test,
    conc,
    datetime,
    start,
    fit_type = "exp_zhao18"
  ))
  slopes_flag_test <- flux_quality(
    slopes_test,
    conc
  )
  fluxes_test <- flux_calc(
    slopes_flag_test,
    f_slope_corr,
    datetime,
    temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625
  )

  expect_snapshot(
    str(fluxes_test)
  )
})

test_that("Stupeflux returns the same as step by step workflow", {
  conc_test <- flux_match(
    co2_df_short,
    record_short,
    datetime,
    start,
    conc,
    startcrop = 10,
    measurement_length = 180
  )
  slopes_test <- suppressWarnings(flux_fitting(
    conc_test,
    conc,
    datetime,
    f_start,
    fit_type = "exp_zhao18"
  ))
  slopes_flag_test <- flux_quality(
    slopes_test,
    conc
  )
  fluxes_test <- flux_calc(
    slopes_flag_test,
    f_slope_corr,
    datetime,
    temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625
  )

  expect_equal(
    stupeflux(
      raw_conc = co2_df_short,
      field_record = record_short,
      f_datetime = datetime,
      start_col = start,
      f_conc = conc,
      startcrop = 10,
      measurement_length = 180,
      fit_type = "exp_zhao18",
      temp_air_col = temp_air,
      conc_unit = "ppm",
      flux_unit = "mmol",
      chamber_volume = 24.5,
      tube_volume = 0.075,
      atm_pressure = 1,
      plot_area = 0.0625
    ),
    fluxes_test
  )
})

test_that("Stupeflux works with slope_correction = FALSE", {
  expect_snapshot(
    stupeflux(
      raw_conc = co2_df_short,
      field_record = record_short,
      f_datetime = datetime,
      start_col = start,
      f_conc = conc,
      startcrop = 10,
      measurement_length = 180,
      fit_type = "exp_zhao18",
      temp_air_col = temp_air,
      conc_unit = "ppm",
      flux_unit = "mmol",
      chamber_volume = 24.5,
      tube_volume = 0.075,
      atm_pressure = 1,
      plot_area = 0.0625,
      slope_correction = FALSE
    )
  )
})

test_that("Fluxible workflow works with kappamax", {
  conc_test <- flux_match(
    co2_df_short,
    record_short,
    datetime,
    start,
    conc,
    startcrop = 10,
    measurement_length = 220
  )
  slopes_test <- suppressWarnings(flux_fitting(
    conc_test,
    conc,
    datetime,
    start,
    end_cut = 30,
    fit_type = "exp_hm"
  ))
  slopes_flag_test <- flux_quality(
    slopes_test,
    conc,
    f_pvalue = f_pvalue_lm,
    f_rsquared = f_rsquared_lm,
    kappamax = TRUE
  )
  fluxes_test <- flux_calc(
    slopes_flag_test,
    f_slope_corr,
    datetime,
    temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625
  ) |>
    dplyr::select(!c(f_fluxid, f_slope_corr))

  expect_snapshot(
    fluxes_test
  )
})

test_that("Working with two gases", {
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

  expect_snapshot(
    str(fluxes_twogases)
  )
})

test_that("sum and median works", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exp_zhao18"
  )) |>
    flux_quality(
      conc
    )

  output <- flux_calc(
    slopes0,
    f_slope,
    datetime,
    temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    cols_sum = "PAR",
    cols_med = "temp_soil",
    chamber_volume = 24.5,
    tube_volume = 0.075,
    atm_pressure = 1,
    plot_area = 0.0625,
    cut = FALSE
  ) |>
    dplyr::select(f_fluxid, f_temp_air_ave, datetime, f_flux, PAR, temp_soil)


  expect_snapshot(output)
})
