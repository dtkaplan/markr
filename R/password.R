#' Login ID/Password question
#'
#' Creates a text box group tutorial quiz question.
#'
#' @param key Data frame of user  IDs and  passwords
#' @param store ID of sheet storing submissions
#' @param placeholder Character string giving instructions to user.
#' @export
#' @examples
#' user_ID("03829392912232232")

user_ID <- function(
  key = NULL,
  store = NULL,
  placeholder = "Enter ID here in format user_name::password"
) {
  #  set up the event handler
  options(tutorial.event_recorder = markr_event_recorder)

  # check inputs
  checkmate::assert_character(placeholder, len = 1, null.ok = TRUE, any.missing = FALSE)

  markr:::store_submission_key(store)
  markr:::session_id_init()

  if (is.data.frame(key)) {
    passwd_df = key
  } else {
    # if (length(dir(pattern =  "secret-token.RDS")) == 1) {
    #   hoo  <- readRDS("secret-token.RDS")
    #   sheets_auth(token = hoo)
    # } else {
    #   stop("There must be a file called `secret-token.RDS` in the app directory.")
    # }

    googledrive::drive_auth(path  = "learnrcache-efcb19f92072.json")
    passwd_df <-  sheets_read(key)
  }
  if (!all(names(passwd_df) %in% c("id", "password")))
    stop("password data frame must have columns 'ID' and 'password'")

  learnr::question(
    text = "ID",
    answer("bogus", correct = TRUE,  message = "Will never see this"),
    type = "learnr_userid",
    correct = "ID validated",
    incorrect = "Invalid ID",
    allow_retry = TRUE,
    random_answer_order = FALSE,
    submit_button = "Submit login credentials",
    options =
      list(
        placeholder = placeholder,
        passwd_df = passwd_df
      )

  )
}



#' @export
question_ui_initialize.learnr_userid <- function(question, value, ...) {
  textInput(
    question$ids$answer,
    label = "loginID",
    placeholder = question$options$placeholder,
    value = value
  )
}

#' @export
question_is_valid.learnr_userid <- function(question, value, ...) {
  (! is.null(value)) &&  nchar(value)  > 4
}

#' @export
question_is_correct.learnr_userid <- function(question, value, ...) {
  fields <- unlist(strsplit(value, "::", fixed  = TRUE))
  if (length(fields) != 2)
    return(mark_as(FALSE, "Use format user_id::password"))

  ID <- which(fields[1] == question$options$passwd_df$id)
  if (length(ID)  !=  1) return(mark_as(FALSE, "Invalid user ID"))
  if (fields[2]  != question$options$passwd_df$password[ID]) {
    return(
      mark_as(FALSE,
              paste("Invalid  password for user", question$options$passwd_df$id[ID]))
    )
  }

  markr:::store_ID(fields[1]) # store the user ID
  mark_as(TRUE, NULL)

}

# question_ui_completed.learnr_userid <- question_ui_completed.default
