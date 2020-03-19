#' Store user information
#'
#' These are internal functions
#' @param str the ID that the user logged into the tutorial with

store_ID <- function(str) {
  if(is.null(str)) str <- ".initializing."
 markr_env$user_ID <- str
}

get_ID  <- function() {
  if (is.null(markr_env$user_ID))  ".initializing"
  else markr_env$user_ID
}

markr_env  <-  new.env()

#' @rdname store_ID
#' @param df a dataframe containing the user IDs and passwords

store_password  <- function(df) {
  markr_env$passwd <- df
}


get_password <- function() markr_env$password

#' @rdname store_ID
#' @param key a Google Sheets key
store_submission_key <- function(key) {
  markr_env$submission_key <- key
}

get_submission_key <- function() markr_env$submission_key

#' @importFrom stats runif

session_id_init <- function() {
  set.seed(as.numeric(trunc(Sys.time(),  units="secs")))
  markr_env$session_id  <- paste0(round(10 * runif(10)), collapse = "")
}

session_id <- function() {
  markr_env$session_id
}
