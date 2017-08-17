#' Send an email message through the Mailgun API
#' @description Send an email message via
#' the Mailgun API. This requires an account
#' with Mailgun.
#' @param message the email message object,
#' as created by the \code{compose_email()}
#' function. The object's class is
#' \code{email_message}
#' @param subject the subject of the
#' email.
#' @param from the email address of the
#' sender. This does not have to be
#' the same email that is associated with
#' the account actually sending the message.
#' @param recipient the email address of the
#' recipient.
#' @param url the URL for the sending domain.
#' @param api_key the API key registered to
#' the mailgun service.
#' @import httr
#' @export send_by_mailgun

send_by_mailgun <- function(message,
                            subject = NULL,
                            from,
                            recipient,
                            url,
                            api_key) {

  # Verify that the `message` object
  # is of the class `email_message`
  if (!inherits(x = message, what = "email_message")) {
    stop("The object provided in `message` must be created by the `compose_email()` function.")
  }

  if (is.null(subject)) {
    subject_text <- "<no subject>"
  } else {
    subject_text <- glue::glue(subject)
  }

  # Post the message to Mailgun
  httr::POST(
    url = url,
    authenticate("api", api_key),
    encode = "form",
    body = list(
      from = from,
      to = recipient,
      subject = subject,
      html = message$html_html))
}
