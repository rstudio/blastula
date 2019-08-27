#' Email Attachment helpers for RStudio Connect
#'
#' These helper functions are to be used in the [connect_email()] function's
#' `attachments` arguments (and enclosed in a `list()`).
#'
#' @name attachment_helpers
#' @return a list object of with a class of either `connect_report`,
#'   `generated_files`, or `connect_files`.
NULL

#' @rdname attachment_helpers
#' @export
report_rmd <- function() {

  structure(
    class = c("connect_report"),
    list(
      name = "connect_report"
    )
  )
}

#' @rdname attachment_helpers
#' @export
generated_files <- function(files = NULL) {

  structure(
    class = c("generated_files"),
    list(
      name = files
    )
  )
}

#' @rdname attachment_helpers
#' @export
connect_files <- function(files = NULL) {

  structure(
    class = c("connect_files"),
    list(
      name = files
    )
  )
}
