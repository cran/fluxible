test_that("matching works", {
  expect_snapshot(flux_match(
    co2_df_short,
    record_short,
    datetime,
    start,
    conc,
    startcrop = 10,
    measurement_length = 180
  ) |>
    dplyr::select(f_fluxid, f_n_conc, f_ratio, f_flag_match, f_length) |>
    dplyr::distinct()
  )
})

test_that("time_diff works", {
  co2_df_short_180 <- co2_df_short %>%
    dplyr::mutate(
      datetime = datetime - 180 # logger is lagging 3 minutes behind
    )

  # test

  expect_snapshot(flux_match(
    co2_df_short_180,
    record_short,
    datetime,
    start,
    conc,
    startcrop = 10,
    measurement_length = 220,
    time_diff = 180
  ))
})

test_that("renaming variables works", {
  co2_df_short <- co2_df_short %>%
    dplyr::rename(
      CO2_conc = conc,
      date_time = datetime
    )

  record_short <- record_short %>%
    dplyr::rename(
      starting = start
    )



  expect_snapshot(
    flux_match(
      co2_df_short,
      record_short,
      date_time,
      starting,
      CO2_conc,
      startcrop = 10,
      measurement_length = 220
    )
  )
})



test_that("flags on nb of data", {
  expect_snapshot(
    suppressWarnings( # warnings are expected, they are tested in another test
      flux_match(
        co2_df_missing,
        record_short,
        datetime,
        start,
        conc,
        startcrop = 10,
        measurement_length = 220
      )
    )
  )
})

# test that flags also get printed as warnings
test_that("warnings", {
  expect_warning(
    flux_match(
      co2_df_missing,
      record_short,
      datetime,
      start,
      conc,
      startcrop = 10,
      measurement_length = 220
    ),
    "fluxID 1 : nb of data too low
 fluxID 3 : nb of data too low
 fluxID 6 : no data"
  )
})

test_that("no warnings when no flags", {
  expect_no_warning(flux_match(
    co2_df_short,
    record_short,
    datetime,
    start,
    conc,
    startcrop = 10,
    measurement_length = 220
  ))
})

# test that the data type checking works (all the error messages)

test_that("error on datetime", {
  co2_df_short <- co2_df_short %>%
    dplyr::mutate(
      datetime = lubridate::date(datetime)
    )

  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      datetime,
      start,
      conc,
      startcrop = 10,
      measurement_length = 220
    ),
    "Please correct the arguments"
  )
})

test_that("error on conc variable", {
  co2_df_short <- co2_df_short %>%
    dplyr::mutate(
      conc = as.character(conc)
    )

  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      datetime,
      start,
      conc,
      startcrop = 10,
      measurement_length = 220
    ),
    "Please correct the arguments"
  )
})

test_that("error on start", {
  record_short <- record_short %>%
    dplyr::mutate(
      start = lubridate::hour(start)
    )

  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      datetime,
      start,
      conc,
      startcrop = 10,
      measurement_length = 220
    ),
    "Please correct the arguments"
  )
})

test_that("error on startcrop", {
  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      datetime,
      start,
      conc,
      startcrop = "blip",
      measurement_length = 220
    ),
    "Please correct the arguments"
  )
})

test_that("error on measurement_length", {
  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      datetime,
      start,
      conc,
      measurement_length = "blip",
      startcrop = 10
    ),
    "Please correct the arguments"
  )
})

test_that("error on ratio_threshold", {
  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      datetime,
      start,
      conc,
      ratio_threshold = 2,
      startcrop = 10,
      measurement_length = 220
    ),
    "ratio_threshold has to be a number between 0 and 1"
  )
})

test_that("error on time_diff", {
  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      datetime,
      start,
      conc,
      startcrop = 10,
      measurement_length = 220,
      time_diff = "comment est votre blanquette?"
    ),
    "Please correct the arguments"
  )
})

test_that("matching works with end col", {

  record_short_end <- record_short |>
    dplyr::mutate(
      end = dplyr::case_when(
        type == "ER" ~ start + 120,
        type == "NEE" ~ start + 180
      )
    )

  expect_snapshot(flux_match(
    co2_df_short,
    record_short_end,
    datetime,
    start,
    conc,
    end,
    startcrop = 10,
    fixed_length = FALSE
  ) |>
    dplyr::select(f_fluxid, f_ratio, f_flag_match, f_length) |>
    dplyr::distinct()
  )
})
