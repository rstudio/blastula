#' Send an email message through the Mailgun API
#'
#' Send an email message via the Mailgun API. This requires an account with
#' Mailgun.
#'
#' @param message The email message object, as created by the `compose_email()`
#'   function. The object's class is `email_message`
#' @param subject The subject of the email.
#' @param from The email address of the sender. This does not have to be the
#'   same email that is associated with the account actually sending the
#'   message.
#' @param recipients A vector of email addresses.
#' @param url The URL for the sending domain.
#' @param api_key The API key registered to the Mailgun service.
#'
#' @importFrom httr POST authenticate
#'
#' @examples
#' # Create a simple email message using
#' # Markdown formatting
#'
#' # email <-
#' #   compose_email(
#' #   body = "
#' #   Hello!
#' #
#' #   ## This a section heading
#' #
#' #   We can use Markdown formatting \\
#' #   to **embolden** text or to add \\
#' #   *emphasis*. This is exciting, \\
#' #   right?
#' #
#' #   Cheers")
#'
#' # Generate a vector of recipients
#'
#' # recipient_list <-
#' #   c("person_1@site.net",
#' #     "person_2@site.net")
#'
#' # Send it to multiple people through
#' # the Mailgun API
#'
#' # email %>%
#' #   send_by_mailgun(
#' #     subject = "Sent through Mailgun",
#' #     from = "The Sender <sender@send.org>",
#' #     recipients = recipient_list,
#' #     url = "<..mailgun_sending_domain..>",
#' #     api = "<..mailgun_api_key..>")
#'
#' @export
send_by_mailgun <- function(message,
                            subject = NULL,
                            from,
                            recipients,
                            url,
                            api_key) {

  # nocov start

  # Verify that the `message` object
  # is of the class `email_message`
  if (!inherits(message, "email_message")) {
    stop("The object provided in `message` must be created by the `compose_email()` function.")
  }

  # Normalize `subject` so that a `NULL` value becomes an empty string
  subject <- subject %||% ""

  # Collapse vector of recipients to a single string
  recipients <- paste(recipients, collapse = ", ")

  # Post the message to Mailgun
  httr::POST(
    url = url,
    httr::authenticate("api", api_key),
    encode = "form",
    body = list(
      from = from,
      to = recipients,
      subject = subject,
      html = message$html_html))

  # nocov end
}
