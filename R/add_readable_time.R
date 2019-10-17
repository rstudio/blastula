#' Create an HTML fragment for a humanized date/time
#'
#' Add a nicely-formatted date/time string inside the body of the email with
#' this helper function. We can provide a POSIXct date-time object or use the
#' current date/time/tz (based on the caller's locale information at the time of
#' the call). There are options to specify whether the date, time, and time zone
#' parts are to be included.
#'
#' @param time The POSIXct time to use, and to make more readable for email
#'   recipients. If a `time` is not provided, the current system time will be
#'   used.
#' @param use_date A logical value that indicates whether the current date
#'   should be included.
#' @param use_time A logical value that indicates whether the current time
#'   should be included.
#' @param use_tz A logical value that indicates whether the locale's time zone
#'   should be included.
#' @return A character object that can be placed inside any message component
#'   message wherever the function is called.
#'
#' @export
add_readable_time <- function(time = NULL,
                              use_date = TRUE,
                              use_time = TRUE,
                              use_tz = TRUE) {

  if (is.null(time)) {
    time <- Sys.time()
  } else if (!inherits(time, "POSIXct")) {
    stop("The `time` value must be a POSIXct date-time value.")
  }

  if (isTRUE(use_date)) {
    current_date <-
      paste0(
        format(time, "%A, %B "),
        format(time, "%d") %>% as.numeric(),
        ", ",
        format(time, "%Y"))
  } else {
    current_date <- ""
  }

  if (isTRUE(use_time)) {
    current_time <-
      paste0(
        gsub(" ", "", format(time, "%l:%M")),
        toupper(format(time, " %p")))

    if (isTRUE(use_date)) {
      current_time <- paste0(" at ", current_time)
    }

  } else {
    current_time <- ""
  }

  if (isTRUE(use_tz) & (isTRUE(use_date) | isTRUE(use_time))) {
    current_tz <- format(time, " (%Z)")
  } else if (isTRUE(use_tz) & (use_date == FALSE & use_time == FALSE)) {
    current_tz <- format(time, "%Z")
  } else {
    current_tz <- ""
  }

  glue::glue("{current_date}{current_time}{current_tz}") %>%
    as.character()
}
