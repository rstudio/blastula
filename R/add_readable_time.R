#' Helper function for adding a humanized date/time
#'
#' Add a nicely-formatted date/time string
#' inside the body of the email with this
#' helper function. This will insert the current
#' date/time/tz based on the caller's locale
#' information at the time of the call. There
#' are options to specify whether the date, time,
#' and time zone parts are to be included.
#' @param use_date a logical value that indicates
#' whether the current date should be included.
#' @param use_time a logical value that indicates
#' whether the current time should be included.
#' @param use_tz a logical value that indicates
#' whether the locale's time zone should be
#' included.
#' @return a character object that can be placed
#' inside any message component message wherever
#' the function is called.
#' @importFrom glue glue
#' @export

add_readable_time <- function(use_date = TRUE,
                              use_time = TRUE,
                              use_tz = TRUE) {


  if (use_date) {
    current_date <-
      paste0(
        format(Sys.time(), "%A, %B "),
        format(Sys.time(), "%d") %>% as.numeric(),
        ", ",
        format(Sys.time(), "%Y"))
  } else {
    current_date <- ""
  }

  if (use_time) {
    current_time <-
      paste0(
        format(Sys.time(), "%l:%M") %>% trimws(),
        toupper(format(Sys.time(), " %p")))

    if (use_date) {
      current_time <- paste0(" at ", current_time)
    }

  } else {
    current_time <- ""
  }

  if (use_tz & (use_date | use_time)) {
    current_tz <- format(Sys.time(), " (%Z)")
  } else if (use_tz & (use_date == FALSE & use_time == FALSE)) {
    current_tz <- format(Sys.time(), "%Z")
  } else {
    current_tz <- ""
  }

  glue::glue("{current_date}{current_time}{current_tz}") %>%
    as.character()
}
