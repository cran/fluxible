#' check the items inside `flux_fun_check`
#' @param arg argument to be checked by `fn`
#' @param fn function to check `arg`
#' @param msg message to display in case `arg` is the wrong class
#' @param narg name of `arg`
#' @param df_name name of `arg` in case it is a data frame
#' @author Adam Klimes
#' @keywords internal


flux_check_item <- function(arg,
                            fn,
                            msg,
                            narg,
                            df_name = NA) {
  isok <- fn(arg)
  if (!isok) {
    msg_parts <- if (!is.na(df_name))
      c("Column ", paste0(" of data frame ", df_name, " "))
    else c("Argument ", " ")
    message(paste0(msg_parts[1], narg, msg_parts[2], msg))
    FALSE
  } else {
    TRUE
  }
}
