#' Records events
#'
#' @export
markr_event_recorder <- function(tutorial_id,
                                    tutorial_version,
                                    user_id,
                                    event, data) {
  this_event <-
    data.frame(time = date(), user_id = user_id,
               session_id = session_id(),
               markr_id = markr:::get_ID(),
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
    googlesheets4::sheets_append(
      this_event,
      markr:::get_submission_key() )
  }

  # for debugging
  #save(this_event, file="event.rda")

}
