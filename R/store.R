#' Store user information
#'
store_ID <- function(str) {
  if(is.null(str)) str <- ".initializing."
 markr_env$user_ID <- str
}

get_ID  <- function() {
  if (is.null(markr_env$user_ID))  ".initializing"
  else markr_env$user_ID
}

markr_env  <-  new.env()

store_password  <- function(df) {
  markr_env$passwd <- df
}

get_password <- function() markr_env$password

store_submission_key <- function(key) {
  markr_env$submission_key <- key
}

get_submission_key <- function() markr_env$submission_key

session_id_init <- function() {
  markr_env$session_id  <- paste0(round(10 * runif(10)), collapse = "")
}

session_id <- function() {
  markr_env$session_id
}
