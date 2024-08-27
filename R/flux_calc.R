#' calculates ecosystem gas fluxes
#' @description calculates a flux based on the rate of change
#' of gas concentration over time
#' @param slopes_df dataframe of flux slopes
#' @param slope_col column containing the slope to calculate the flux
#' (in ppm*s^(-1))
#' @param datetime_col column containing the datetime of each gas concentration
#' measurements in slopes_df. The first one after cutting will be kept as
#' datetime of each flux in the output.
#' @param cut_col column containing cutting information
#' @param keep_arg name in cut_col of data to keep
#' @param chamber_volume volume of the flux chamber in L,
#' default for Three-D project chamber (25x24.5x40cm),
#' can also be a column in case it is a variable
#' @param tube_volume volume of the tubing in L, default for summer 2020 setup,
#' can also be a column in case it is a variable
#' @param atm_pressure atmospheric pressure, assumed 1 atm,
#' can be a constant (numerical) or a variable (column name)
#' @param plot_area area of the plot in m^2, default for Three-D
#' @param cols_keep columns to keep from the input to the output.
#' Those columns need to have unique values for each flux,
#' as distinct() is applied.
#' @param cols_ave columns with values that should be averaged
#' for each flux in the output. Note that NA are removed in mean calculation.
#' @param fluxid_col column containing the fluxID
#' @param temp_air_col column containing the air temperature used
#' to calculate fluxes. Will be averaged with NA removed.
#' @param temp_air_unit units in which air temperature was measured.
#' Has to be either Celsius, Fahrenheit or Kelvin.
#' @param fit_type (optional) model used in flux_fitting, exponential,
#' quadratic or linear.
#' Will be automatically filled if slopes_df was produced using flux_quality().
#' @return a dataframe containing fluxID, fluxes (in mmol*m^(-2)*h^(-1)),
#' temperature average for each flux,
#' slope used for each flux calculation,
#' the model used in `flux_fitting`,
#' and any columns specified in cols_keep and cols_ave.
#' @importFrom rlang .data
#' @importFrom dplyr .data rename all_of select group_by summarise
#' ungroup mutate case_when distinct left_join across everything
#' @examples
#' data(slopes0)
#' flux_calc(slopes0, slope_col = "f_slope")
#' @export



flux_calc <- function(slopes_df,
                      slope_col,
                      datetime_col = "f_datetime",
                      cut_col = c(),
                      keep_arg = c(),
                      chamber_volume = 24.5,
                      tube_volume = 0.075,
                      atm_pressure = 1,
                      plot_area = 0.0625,
                      cols_keep = c(),
                      cols_ave = c(),
                      fluxid_col = "f_fluxID",
                      temp_air_col = "temp_air",
                      temp_air_unit = "celsius",
                      fit_type = c()) {
  fit_type <- flux_fit_type(
    slopes_df,
    fit_type = ((fit_type))
  )

  temp_air_unit <- match.arg(
    ((temp_air_unit)),
    c("celsius", "fahrenheit", "kelvin")
  )

  if (!is.double(((atm_pressure)))) stop("atm_pressure has to be a double")
  if (!is.double(((plot_area)))) stop("plot_area has to be a double")

  colnames <- colnames(slopes_df)
  if (!(((slope_col)) %in% ((colnames)))) {
    stop("could not find slope_col in slopes_df")
  }
  if (!(((fluxid_col)) %in% ((colnames)))) {
    stop("could not find fluxid_col in slopes_df")
  }
  if (!(((temp_air_col)) %in% ((colnames)))) {
    stop("could not find temp_air_col in slopes_df")
  }


  if (length(setdiff(((cols_keep)), ((colnames)))) > 0) {
    stop("some names in cols_keep cannot be found in slopes_df")
  }
  if (length(setdiff(((cols_ave)), ((colnames)))) > 0) {
    stop("some names in cols_ave cannot be found in slopes_df")
  }

  if (is.double((chamber_volume))) {
    slopes_df <- slopes_df |>
      mutate(
        chamber_volume = ((chamber_volume))
      )
  }

  if (is.character(((chamber_volume)))) {
    slopes_df <- slopes_df |>
      rename(
        chamber_volume = all_of(((chamber_volume)))
      )
  }

  if (is.double((tube_volume))) {
    slopes_df <- slopes_df |>
      mutate(
        tube_volume = ((tube_volume))
      )
  }

  if (is.character(((tube_volume)))) {
    slopes_df <- slopes_df |>
      rename(
        tube_volume = all_of(((tube_volume)))
      )
  }

  if (is.double((atm_pressure))) {
    slopes_df <- slopes_df |>
      mutate(
        atm_pressure = ((atm_pressure))
      )
  }

  if (is.character(((atm_pressure)))) {
    slopes_df <- slopes_df |>
      rename(
        atm_pressure = all_of(((atm_pressure)))
      )
  }



  slopes_df <- slopes_df |>
    rename(
      f_fluxID = all_of(((fluxid_col))),
      f_datetime = all_of(((datetime_col))),
      air_temp = all_of(((temp_air_col))),
      f_slope_calc = all_of(((slope_col)))
    )



  if (length(((cut_col))) > 0) {
    message("Cutting data according to 'keep_arg'...")
    slopes_df <- flux_cut(
      slopes_df,
      cut_col = ((cut_col)),
      keep_arg = ((keep_arg))
    )
  }

  message("Averaging air temperature for each flux...")
  slope_temp <- slopes_df |>
    select(
      "f_slope_calc", "f_fluxID", "air_temp", "chamber_volume",
      "tube_volume", "atm_pressure", "f_datetime"
    ) |>
    group_by(
      .data$f_fluxID, .data$f_slope_calc, .data$chamber_volume,
      .data$tube_volume, .data$atm_pressure
    ) |>
    summarise(
      temp_air_ave = mean(.data$air_temp, na.rm = TRUE),
      datetime = .data$f_datetime[1],
      .groups = "drop"
    ) |>
    mutate(
      temp_air_ave = case_when(
        ((temp_air_unit)) == "celsius" ~ .data$temp_air_ave + 273.15,
        ((temp_air_unit)) == "fahrenheit"
        ~ (.data$temp_air_ave + 459.67) * (5 / 9),
        ((temp_air_unit)) == "kelvin" ~ .data$temp_air_ave
      )
    )



  # a df with all the columns we just want to keep and join back in the end
  if (length(((cols_keep))) > 0) {
    message("Creating a df with the columns from 'cols_keep' argument...")
    slope_keep <- slopes_df |>
      select(all_of(((cols_keep))), "f_fluxID") |>
      distinct() |>
      left_join(slope_temp, by = "f_fluxID")
  } else {
    slope_keep <- slope_temp
  }

  # a df with the columns that have to be averaged
  if (length((cols_ave)) > 0) {
    message("Creating a df with the columns from 'cols_ave' argument...")
    slope_ave <- slopes_df |>
      select(all_of(((cols_ave))), "f_fluxID") |>
      group_by(.data$f_fluxID) |>
      summarise(across(
        everything(),
        ~ mean(.x, na.rm = TRUE)
      ), .groups = "drop") |>
      left_join(slope_keep, by = "f_fluxID")
  } else {
    slope_ave <- slope_keep
  }

  message("Calculating fluxes...")

  r_const <- 0.082057

  fluxes <- slope_ave |>
    mutate(
      volume_setup = .data$chamber_volume + .data$tube_volume,
      flux =
        (.data$f_slope_calc * .data$atm_pressure * .data$volume_setup)
        / (((r_const)) *
           .data$temp_air_ave
           * ((plot_area))) # flux in micromol/s/m^2
        * 3600 # secs to hours
        / 1000, # micromol to mmol flux is now in mmol/m^2/h
      temp_air_ave = case_when(
        ((temp_air_unit)) == "celsius" ~ .data$temp_air_ave - 273.15,
        ((temp_air_unit)) == "fahrenheit"
        ~ ((.data$temp_air_ave - 273.15) * (9 / 5)) + 32,
        ((temp_air_unit)) == "kelvin" ~ .data$temp_air_ave
      ),
      model = ((fit_type))
    )
  fluxes
}
