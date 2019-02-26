#' Print the email object to the Viewer
#'
#' This function will provide a preview of the email message.
#' @param x an \code{email_message} object.
#' @keywords internal
#' @export
print.blastula_message <- function(x, ...) {

  preview_email(x)
}
