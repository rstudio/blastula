#' Get the HTML content of an email message
#'
#' Get the HTML content string from an `email_message` object as a single-length
#' character vector.
#' @param message The email message object, as created by the `compose_email()`
#'   function. The object's class is `email_message`
#' @return A character object containing the email message's HTML content.
#' @export
get_html_str <- function(message) {

  message[["html_str"]]
}
