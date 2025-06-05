test_that("quality flags count works", {
  slopes30lin_flag <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "linear",
    end_cut = 30
  )) |>
    flux_quality(
      conc
    )
  expect_snapshot(
    flux_flag_count(slopes30lin_flag)
  )
})

test_that("quality flags count works after calculating fluxes", {
  fluxes <- stupeflux(
    raw_conc = co2_df_short,
    field_record = record_short,
    f_datetime = datetime,
    start_col = start,
    f_conc = conc,
    start_cut = 10,
    measurement_length = 180,
    fit_type = "exp_zhao18",
    temp_air_col = temp_air,
    conc_unit = "ppm",
    flux_unit = "mmol",
    setup_volume = 24.575,
    atm_pressure = 1,
    plot_area = 0.0625,
    cols_keep = "f_quality_flag"
  )
  expect_snapshot(
    flux_flag_count(fluxes)
  )
})
