#' Print the email object to the Viewer
#'
#' This function will provide a preview of the email message.
#' @param x An `email_message` object.
#' @keywords internal
#' @export
print.email_message <- function(x, ...) {

  preview_email(x)
}
