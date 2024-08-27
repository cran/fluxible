test_that("matching works", {
  expect_snapshot(flux_match(
    co2_df_short,
    record_short
  ))
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
      datetime_col = "date_time",
      conc_col = "CO2_conc",
      start_col = "starting"
    )
  )
})



test_that("flags on nb of data", {
  expect_snapshot(
    suppressWarnings( # warnings are expected, they are tested in another test
      flux_match(
        co2_df_missing,
        record_short
      )
    )
  )
})

# test that flags also get printed as warnings
test_that("warnings", {
  expect_warning(
    flux_match(
      co2_df_missing,
      record_short
    ),
    "fluxID 1 : nb of data too low
 fluxID 3 : nb of data too low
 fluxID 6 : no data"
  )
})

test_that("no warnings when no flags", {
  expect_no_warning(flux_match(
    co2_df_short,
    record_short
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
      record_short
    ),
    "datetime in raw_conc dataframe is not ymd_hms!"
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
      record_short
    ),
    "conc is not a double"
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
      record_short
    ),
    "start in field_record dataframe is not ymd_hms!"
  )
})

test_that("error on startcrop", {
  expect_error(
    flux_match(co2_df_short,
      record_short,
      startcrop = "blip"
    ),
    "startcrop has to be a double"
  )
})

test_that("error on measurement_length", {
  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      measurement_length = "blip"
    ),
    "measurement_length has to be a double"
  )
})

test_that("error on ratio_threshold", {
  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      ratio_threshold = 2
    ),
    "ratio_threshold has to be a number between 0 and 1"
  )
})

test_that("error on time_diff", {
  expect_error(
    flux_match(
      co2_df_short,
      record_short,
      time_diff = "comment est votre blanquette?"
    ),
    "time_diff has to be a double"
  )
})
