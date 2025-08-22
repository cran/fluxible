#' checking that arguments and columns are in the correct class
#' @param args list of arguments or dataframe to check
#' @param fn list of functions used to check (`is.numeric`, `is.character`, ...)
#' @param msg list of messages to return in case of failed check
#' @param name_df in case args is a df with selected columns to check origdf
#' is the original df to take the name from for a more obvious error message
#' @author Adam Klimes
#' @keywords internal



flux_fun_check <- function(args,
                           fn,
                           msg,
                           name_df = NA) {
  mapply(flux_check_item, args, fn, msg, names(args), df_name = name_df)
}
