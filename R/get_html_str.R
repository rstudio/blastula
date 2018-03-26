#' Get the HTML content of an email message
#'
#' Get the HTML content string from an
#' \code{email_message} object as a single-length
#' character vector.
#' @param message the email message object,
#' as created by the \code{compose_email()}
#' function. The object's class is
#' \code{email_message}
#' @return a character object containing the
#' email message's HTML content.
#' @export
get_html_str <- function(message) {

  message[["html_str"]]
}
