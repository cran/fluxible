test_that("flux LRC works", {
  output <- flux_lrc(
    fluxes_df = co2_fluxes_lrc,
    type_col = type,
    par_ave = PAR_ave,
    f_flux = f_flux,
    lrc_arg = "LRC",
    nee_arg = "NEE",
    er_arg = "ER",
    lrc_group = c("warming"),
    par_nee = 500,
    par_er = 0
  )

  expect_snapshot(output)
})

test_that("flux LRC works without groups", {
  output <- flux_lrc(
    fluxes_df = co2_fluxes_lrc,
    type_col = type,
    par_ave = PAR_ave,
    f_flux = f_flux,
    lrc_arg = "LRC",
    nee_arg = "NEE",
    er_arg = "ER",
    par_nee = 500,
    par_er = 0
  )

  expect_snapshot(output)
})

test_that("error on args", {
  expect_error(flux_lrc(
    fluxes_df = co2_fluxes_lrc,
    type_col = type,
    par_ave = PAR_ave,
    f_flux = f_flux,
    lrc_arg = "LRC",
    nee_arg = "NEE",
    er_arg = "ER",
    par_nee = 500,
    par_er = "baaaaaaaaah"
  ),
  "Please correct the arguments"
  )

})

test_that("error on cols", {
  expect_error(flux_lrc(
    fluxes_df = co2_fluxes_lrc,
    type_col = type,
    par_ave = warming,
    f_flux = f_flux,
    lrc_arg = "LRC",
    nee_arg = "NEE",
    er_arg = "ER",
    par_nee = 500,
    par_er = 0
  ),
  "Please correct the arguments"
  )

})
