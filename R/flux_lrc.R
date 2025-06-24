#' Standardizes CO2 fluxes at fixed PAR values
#' @description
#' `r lifecycle::badge("experimental")`
#' Calculates light response curves for CO2 fluxes and
#' standardizes CO2 fluxes according to the LRC
#' @param fluxes_df a dataframe containing NEE, ER and LRC measurements
#' @param type_col column containing type of flux (NEE, ER, LRC)
#' @param par_ave column containing the PAR value for each flux
#' @param f_flux column containing flux values
#' @param nee_arg argument designating NEE fluxes in type column
#' @param er_arg argument designating ER fluxes in type column
#' @param lrc_arg argument designating LRC fluxes in type column
#' @param lrc_group character vector of columns to use to group the LRC
#' (campaign, site, treatment), if applicable
#' @param par_nee PAR value to correct the NEE fluxes to
#' @param par_er PAR value to correct the ER fluxes to
#' @details The light response curves are calculated with a quadratic of the
#' form
#' \ifelse{html}{\out{flux(PAR) = a * PAR<sup>2</sup> + b * PAR + c}}{\eqn{flux(PAR) = a * PAR^2 + b * PAR + c}{ASCII}}
#' @return the same dataframe with the additional column `PAR_corrected_flux`
#' @importFrom dplyr group_by_at filter rename vars select mutate left_join cross_join
#' case_when
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @examples
#' data(co2_fluxes_lrc)
#' flux_lrc(
#' fluxes_df = co2_fluxes_lrc,
#' type_col = type,
#' par_ave = PAR_ave,
#' f_flux = f_flux,
#' lrc_arg = "LRC",
#' nee_arg = "NEE",
#' er_arg = "ER",
#' lrc_group = c("warming"),
#' par_nee = 300,
#' par_er = 0
#' )
#' @export


flux_lrc <- function(fluxes_df,
                     type_col,
                     par_ave = par_ave,
                     f_flux = f_flux,
                     lrc_arg = "LRC",
                     nee_arg = "NEE",
                     er_arg = "ER",
                     lrc_group = c(),
                     par_nee = 300,
                     par_er = 0) {


  coefficients_lrc <- fluxes_df |>
    filter(
      {{type_col}} == ((lrc_arg))
    ) |>
    rename(
      PARavg = {{par_ave}},
      f_flux = {{f_flux}}
    ) |>
    group_by_at(vars(all_of(((lrc_group))))) |>
    nest() |>
    mutate(
      lm = map(data, ~ lm(f_flux ~ PARavg + I(PARavg^2), data = .x)),
      table = map(lm, tidy),
      table = map(table, select, "term", "estimate"),
      table = map(
        table, pivot_wider, names_from = "term", values_from = "estimate"
      )
    ) |>
    unnest(table) |>
    select(all_of(lrc_group), "PARavg", "I(PARavg^2)") |>
    rename(
      a = "I(PARavg^2)",
      b = "PARavg"
    )

  flux_corrected_par <- fluxes_df |>
    filter(
      {{type_col}} %in% c("NEE", "ER")
    )

  if (is.null(lrc_group)) {
    flux_corrected_par <- flux_corrected_par |>
      cross_join(coefficients_lrc)
  }

  if (!is.null(lrc_group)) {
    flux_corrected_par <- flux_corrected_par |>
      left_join(coefficients_lrc)
  }

  flux_corrected_par <- flux_corrected_par |>
    mutate(
      PAR_corrected_flux =
        case_when( #we correct only the NEE
          type == "NEE" ~
            {{f_flux}} +
              a * (par_nee^2 - {{par_ave}}^2) +
              b * (par_nee - {{par_ave}}),
          type == "ER" ~
            {{f_flux}} +
              a * (par_er^2 - {{par_ave}}^2) +
              b * (par_er - {{par_ave}})
        )
    ) |>
    select(!c("a", "b"))

  flux_corrected_par
}
