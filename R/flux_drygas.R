#' wet air correction
#' @description Corrects for the amount of water vapor inside the air
#' @param conc_df dataframe of gas concentration over time
#' @param gas_wet the gas to correct
#' @param h2o_wet water vapor concentration before correction (in mmol/mol)
#' @return the same dataframe with the additional column `[gas_wet]_dry` in the
#' same unit as `gas_wet`
#' @details the correction is done as follows
#' \ifelse{html}{\out{gas_dry = gas_wet / (1 - (h2o_wet / 1000))}}{\eqn{gas_dry = gas_wet / (1 - (h2o_wet / 1000}{ASCII}}
#' @importFrom rlang enquo as_name !! :=
#' @importFrom dplyr mutate
#' @export
#' @examples
#' data(wet_conc)
#' flux_drygas(wet_conc, co2, h2o)


flux_drygas <- function(conc_df,
                        gas_wet,
                        h2o_wet) {

  gas_wet <- enquo(gas_wet)
  h2o_wet <- enquo(h2o_wet)

  gas_dry_name <- paste0(as_name(gas_wet), "_dry")

  output <- conc_df |>
    mutate(
      !!gas_dry_name := (!!gas_wet) / (1 - (!!h2o_wet / 1000))
    )

  output
}
