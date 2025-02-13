#' counts quality flags
#' @description provides a table of how many fluxes were attributed
#' which quality flag.
#' This function is incorporated in flux_quality (output as a message)
#' but can be used alone to extract a dataframe with the flag count.
#' @param slopes_df dataframe of flux slopes
#' @param f_flags list of flags used in the dataset
#' (if different from default from flux_quality).
#' If not provided, it will list only the flags that are
#' present in the dataset (no showing 0).
#' @param f_fluxid column containing fluxes unique ID
#' @param f_quality_flag column containing the quality flags
#' @param f_cut column indicating which part of the flux is being cut
#' @param cut_arg argument defining that the data point should be cut out
#' @return a dataframe with the number of fluxes for each quality flags
#' and their proportion to the total
#' @importFrom dplyr .data all_of select group_by summarise
#' tibble right_join filter distinct
#' @importFrom tidyr replace_na
#' @author Vincent Belde
#' @examples
#' data(slopes30qua_flag)
#' flux_flag_count(slopes30qua_flag)
#' @export


flux_flag_count <- function(slopes_df,
                            f_fluxid = f_fluxid,
                            f_quality_flag = f_quality_flag,
                            f_cut = f_cut,
                            f_flags = c(
                              "ok",
                              "discard",
                              "zero",
                              "force_discard",
                              "start_error",
                              "no_data",
                              "force_ok"
                            ),
                            cut_arg = "cut") {

  flag_df <- slopes_df |>
    filter({{f_cut}} != cut_arg) |>
    mutate(
      f_quality_flag = as.factor({{f_quality_flag}})
    ) |>
    select({{f_fluxid}}, {{f_quality_flag}}) |>
    distinct()

  flags <- tibble({{f_quality_flag}} := factor(f_flags, levels = f_flags))

  count_table <- flag_df |>
    summarise(
      n = length({{f_quality_flag}}),
      .by = {{f_quality_flag}}
    ) |>
    right_join(flags, by = dplyr::join_by({{f_quality_flag}})) |>
    mutate(
      n = replace_na(.data$n, 0),
      ratio = .data$n / sum(.data$n)
    )

  count_table
}
