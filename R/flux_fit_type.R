#' to check the type of fit
#' @description extracts the type of fit that was applied in flux_fitting
#' or checks that the fit_type provided by the user is compatible with Fluxible
#' @param df any dataframe
#' @param fit_type type of fit that was applied in flux_fitting. Needs to be
#' filled only if the df was produced outside of the Fluxible workflow.
#' @param fit_type_list list of fit types in use with Fluxible.

flux_fit_type <- function(df,
                          fit_type = c(),
                          fit_type_list = c(
                            "exponential",
                            "linear",
                            "quadratic"
                          )) {
  if (is.null(fit_type)) {
    fit_type <- attributes(df)$fit_type
  } else {
    fit_type <- match.arg(fit_type, fit_type_list)
  }
  fit_type
}
