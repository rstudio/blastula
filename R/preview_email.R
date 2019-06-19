#' Preview an email message
#'
#' Preview an HTML email in the Viewer before sending it out.
#' @param email An `email_message` object.
#' @export
preview_email <- function(email) {

  email[["html_html"]] %>% htmltools::html_print()
}
