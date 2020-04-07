#' Records events
#'
#' This function will not be called by  the user. Instead,
#' the `{learnr}` system will  call it each time an  event is generated.
#' The  arguments are in the standard format required by `{learnr}`
#' and will be constructed by `{learnr}`. All  you have to do is
#' call `user_ID()` in  a chunk in your tutorial. That will notify
#' `{learnr}` to use this event recorder.
#'
#' @param tutorial_id defined by learnr system,  but you can specify it
#' manually in the tutorials YAML. See the "access" vignette.
#' @param tutorial_version same as above
#' @param user_id same as above
#' @param event same as above
#' @param data same as above
#'
#' @importFrom utils capture.output
#'
#' @export
markr_event_recorder <- function(tutorial_id,
                                    tutorial_version,
                                    user_id,
                                    event, data) {
  this_event <-
    data.frame(time = date(), user_id = user_id,
               session_id = session_id(),
               markr_id = get_ID(),
               event = event,
               tutorial_id = tutorial_id,
               tutorial_version = tutorial_version,
               chunk_label = ifelse(is.null(data$label), "", data$label),
               submission = capture.output(data$answer),
               correct = ifelse(is.null(data$correct), "", data$correct),
               details = as.character(jsonlite::toJSON(data)),
               stringsAsFactors = FALSE)

  # Don't store the output of chunks -- it can be arbitrarily long.
  if  ( ! event %in% c("exercise_result")) {
    suppressMessages(
      googlesheets4::sheets_append(
        this_event,
        get_submission_key() )
    )
  }

  # for debugging
  #save(this_event, file="event.rda")

}
