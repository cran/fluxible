test_that("fitting works with 0 second end cut", {
  expect_snapshot(
    flux_fitting_exp(co2_conc)
  )
})

test_that("fitting works with 30 second end cut", {
  expect_snapshot(
    flux_fitting_exp(
      co2_conc,
      end_cut = 30
    )
  )
})

test_that("fitting works with 60 second end cut", {
  expect_snapshot(
    flux_fitting_exp(
      co2_conc,
      end_cut = 60
    )
  )
})

### need to test when data are missing
# warning that NAs were dropped in some fluxID

test_that("warnings when NAs are dropped in conc", {
  expect_warning(
    flux_fitting_exp(co2_conc_missing),
    " fluxID 1 : slope was estimated on 70 points out of 210 seconds
 fluxID 2 : slope was estimated on 121 points out of 210 seconds
 fluxID 3 : slope was estimated on 102 points out of 210 seconds
 fluxID 5 : slope was estimated on 161 points out of 210 seconds",
    fixed = TRUE # need that because there parenthesis in the error message
  )
})

# warning when there is no data in some fluxID

test_that("warnings when there is no data in conc", {
  expect_warning(
    flux_fitting_exp(co2_conc_missing),
    " fluxID 6 dropped (no data in the conc column)",
    fixed = TRUE # need that because there parenthesis in the error message
  )
})

test_that("warnings with cutting", {
  expect_warning(
    flux_fitting_exp(
      co2_conc_missing,
      start_cut = 10
    ),
    " fluxID 1 : slope was estimated on 70 points out of 200 seconds
 fluxID 2 : slope was estimated on 121 points out of 200 seconds
 fluxID 3 : slope was estimated on 102 points out of 200 seconds
 fluxID 5 : slope was estimated on 151 points out of 200 seconds",
    fixed = TRUE # need that because there parenthesis in the error message
  )
})

test_that("error on arguments", {
  expect_error(
    flux_fitting_exp(
      co2_conc_missing,
      start_cut = "Voldemort"
    ),
    "start_cut has to be a double"
  )
})



test_that("cutting too much", {
  expect_error(
    flux_fitting_exp(
      co2_conc,
      start_cut = 120,
      end_cut = 100
    ),
    "You cannot cut more than the length of the measurements!",
    fixed = TRUE # need that because there parenthesis in the error message
  )
})

test_that("renaming works", {
  co2_conc_names <- co2_conc %>%
    dplyr::rename(
      date_time = f_datetime,
      finish = f_end,
      co2 = f_conc
    )

  qflux_fitting_exp <- purrr::quietly(flux_fitting_exp)
  expect_no_error(
    qflux_fitting_exp(
      co2_conc_names,
      datetime_col = "date_time",
      end_col = "finish",
      conc_col = "co2"
    )
  )
})
