test_that("fitting works with 0 second end cut", {
  expect_snapshot(
    flux_fitting(co2_conc,
                 conc,
                 datetime,
                 fit_type = "exponential") |>
      select(f_fluxid, f_slope) |>
      distinct()
  )
})

test_that("fitting works with 30 second end cut", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      end_cut = 30,
      fit_type = "exp"
    )
  )
})

test_that("fitting works with 60 second end cut", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      end_cut = 60,
      fit_type = "exp"
    )
  )
})

### need to test when data are missing
# warning that NAs were dropped in some fluxID

test_that("warnings when NAs are dropped in conc", {
  expect_warning(
    flux_fitting(co2_conc_missing,
                 conc,
                 datetime,
                 fit_type = "exp"),
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
    flux_fitting(co2_conc_missing,
                 conc,
                 datetime,
                 fit_type = "exp"),
    " fluxID 6 dropped (no data in the conc column)",
    fixed = TRUE # need that because there parenthesis in the error message
  )
})

test_that("warnings with cutting", {
  expect_warning(
    flux_fitting(
      co2_conc_missing,
      conc,
      datetime,
      start_cut = 10,
      fit_type = "exp"
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
    flux_fitting(
      co2_conc_missing,
      conc,
      datetime,
      start_cut = "Voldemort",
      fit_type = "exp"
    ),
    "Please correct the arguments"
  )
})



test_that("cutting too much", {
  expect_error(
    flux_fitting(
      co2_conc,
      conc,
      datetime,
      start_cut = 120,
      end_cut = 100,
      fit_type = "exp"
    ),
    "You cannot cut more than the length of the measurements!",
    fixed = TRUE # need that because there parenthesis in the error message
  )
})

test_that("renaming works", {
  co2_conc_names <- co2_conc %>%
    dplyr::rename(
      date_time = datetime,
      finish = f_end,
      co2 = conc
    )

  expect_snapshot(
    flux_fitting(
      co2_conc_names,
      co2,
      date_time,
      f_start,
      finish,
      fit_type = "exp"
    )
  )
})
