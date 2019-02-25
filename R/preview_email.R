#' Preview an email message
#'
#' Preview an HTML email in the Viewer before sending it out.
#' @param email an \code{email_message} object.
#' @importFrom htmltools html_print
#' @export
preview_email <- function(email) {

  email[["html_html"]] %>% htmltools::html_print()
}
