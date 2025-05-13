test_that("GPP calculation", {
  expect_snapshot(
    flux_gpp(co2_fluxes,
      type,
      f_start,
      f_flux,
      id_cols = "turfID",
      cols_keep = c("temp_soil")
    )
  )
})

test_that("keeping more than one columns", {
  expect_snapshot(
    flux_gpp(co2_fluxes,
      type,
      f_start,
      f_flux,
      id_cols = "turfID",
      cols_keep = c("temp_soil", "temp_fahr")
    )
  )
})


test_that("GPP calculation works with several id cols", {
  campaign <- c(1, 1, 2, 2, 3, 3)
  turfid <- c("A", "A", "A", "A", "B", "B")
  type <- c("NEE", "ER", "NEE", "ER", "NEE", "ER")
  flux <- c(3, 5, 2, 7, 9, 11)
  datetime <- c(
    "2024-02-11 10:00:00",
    "2024-02-11 10:00:10",
    "2024-02-11 10:00:20",
    "2024-02-11 10:00:30",
    "2024-02-11 10:00:40",
    "2024-02-11 10:00:50"
  )
  par <- c(300, 2, 250, 5, 320, 1)

  fluxes <- tibble(
    campaign,
    turfid,
    type,
    flux,
    datetime,
    par
  )

  expect_snapshot(
    flux_gpp(fluxes,
      type,
      datetime,
      flux,
      id_cols = c("turfid", "campaign")
    )
  )
})

test_that("missing NEE and several id cols", {
  campaign <- c(1, 1, 2, 2, 3, 3, 3, 4, 4)
  turfid <- c("A", "A", "A", "A", "B", "B", "C", "D", "A")
  type <- c("NEE", "ER", "NEE", "ER", "NEE", "ER", "ER", "ER", "ER")
  flux <- c(3, 5, 2, 7, 9, 11, 10, 13, 8)
  datetime <- c(
    "2024-02-11 10:00:00",
    "2024-02-11 10:00:10",
    "2024-02-11 10:00:20",
    "2024-02-11 10:00:30",
    "2024-02-11 10:00:40",
    "2024-02-11 10:00:50",
    "2024-02-11 10:01:00",
    "2024-02-11 10:01:10",
    "2024-02-11 10:01:20"
  )
  par <- c(300, 2, 250, 5, 320, 1, 0, 3, 4)

  fluxes <- tibble(
    campaign,
    turfid,
    type,
    flux,
    datetime,
    par
  )

  expect_snapshot(
    flux_gpp(fluxes,
      type,
      datetime,
      flux,
      id_cols = c("turfid", "campaign")
    )
  )
})

test_that("GPP error message for non numeric flux", {
  campaign <- c(1, 1, 2, 2, 3, 3)
  turfid <- c("A", "A", "A", "A", "B", "B")
  type <- c("NEE", "ER", "NEE", "ER", "NEE", "ER")
  flux <- c(3, "jasdg", 2, "a", 9, 11)
  datetime <- c(
    "2024-02-11 10:00:00",
    "2024-02-11 10:00:10",
    "2024-02-11 10:00:20",
    "2024-02-11 10:00:30",
    "2024-02-11 10:00:40",
    "2024-02-11 10:00:50"
  )
  par <- c(300, 2, 250, 5, 320, 1)

  fluxes <- tibble(
    campaign,
    turfid,
    type,
    flux,
    datetime,
    par
  )

  expect_error(
    flux_gpp(fluxes,
      type,
      datetime,
      flux,
      id_cols = c("turfid", "campaign")
    )
  )
})

test_that("option to keep all the cols", {
  test_df <- co2_fluxes |>
    dplyr::mutate(
      treatment = c("A", "A", "A", "B", "C", "C")
    )

  expect_snapshot(
    flux_gpp(
      test_df,
      type,
      f_start,
      id_cols = "turfID",
      cols_keep = "all"
    ) |>
      select(!c(f_start, PAR, type, f_flux))
  )
})

test_that("cols keep takes values from NEE", {
  test_df <- co2_fluxes |>
    dplyr::mutate(
      test_keep = dplyr::case_when(
        f_fluxid == 2 ~ NA,
        type == "ER" ~ "ER_val",
        type == "NEE" ~ "NEE_val"
      )
    )

  expect_snapshot(
    flux_gpp(
      test_df,
      type,
      f_start,
      id_cols = "turfID",
      cols_keep = "all"
    ) |>
      select(turfID, type, test_keep)
  )
})

test_that("GPP calculation works with several id cols, and extra fluxes", {
  campaign <- c(1, 1, 2, 2, 3, 4)
  turfid <- c("A", "A", "A", "A", "B", "A")
  type <- c("NEE", "ER", "NEE", "ER", "soilR", "soilR")
  flux <- c(3, 5, 2, 7, 3, 6)
  datetime <- c(
    "2024-02-11 10:00:00",
    "2024-02-11 10:00:10",
    "2024-02-11 10:00:20",
    "2024-02-11 10:00:30",
    "2024-02-12 08:00:10",
    "2024-02-12 09:00:15"
  )
  par <- c(300, 2, 250, 5, 100, 140)

  fluxes <- tibble(
    campaign,
    turfid,
    type,
    flux,
    datetime,
    par
  )

  expect_snapshot(
    flux_gpp(fluxes,
      type,
      datetime,
      flux,
      id_cols = c("turfid", "campaign")
    )
  )
})
