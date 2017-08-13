#' Preview an email message
#' @description Preview an HTML email before
#' sending it out.
#' @param email an \code{email_message} object.
#' @importFrom htmltools html_print
#' @export preview_email

preview_email <- function(email) {

  email[["html_html"]] %>% html_print()
}

